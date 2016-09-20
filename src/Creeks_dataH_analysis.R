library(dplyr)
library(randomForest)
library(mgcv)
library(geoR)
library(lubridate)
library(data.table)
library(knitr)
library(nortest)     # for normality tests
library(car)
library(censReg)
library(ResourceSelection)
library(userfriendlyscience)
library(PMCMR)
library(usdm)     # warning: this package masks 'select' from package dplyr
library (aod)
library(lme4)
library(multcomp)
library(geepack)
library(AICcmodavg)
library(gridExtra)
library(nlme)
library(piecewiseSEM)
library(arm)
library(psych)
library("blmeco")   # Package to check for overdisperion in GLMMs
library(vcd)        # To calculate kappa
library(arm)        # To give nice model output

#detach("package:usdm", unload=TRUE)

# Load in homing data
dataH<-"dataH.csv" 
dataH<-read.csv(dataH, header=TRUE, stringsAsFactors=FALSE)

# Set correct time notation
class(dataH$Departure_time)
dataH$Arrival_time <- strptime(dataH$Arrival_time, "%Y-%m-%d %H:%M")
dataH$Arrival_time <- as.POSIXct(dataH$Arrival_time, "%Y-%m-%d %H:%M", tz="UCT")
dataH$Departure_time <- strptime(dataH$Departure_time, "%Y-%m-%d %H:%M")
dataH$Departure_time <- as.POSIXct(dataH$Departure_time, "%Y-%m-%d %H:%M", tz="UCT")

# Add month and year
dataH$Month=month(dataH$Arrival)
dataH$Year= as.numeric(format(dataH$Arrival_time,'%Y'))

# Select locations/receivers according to study area (bh-5 is still in dataset)
polder=c("bh-6","bh-7","bh-8","bh-9","bh-10","bh-11","bh-13","bh-14","bh-15","bh-16","bh-18","bh-20","bh-21","bh-22","bh-23","bh-24","bh-25","bh-26","bh-27","bh-28","bh-29","bh-30")
dataH=dataH[dataH$Station.Name %in% polder,]

# Remove eels with 14 days tracking and less (A69-1601-29923, A69-1601-31863, A69-1601-31882, A69-1601-31883)
dataH <- dataH[ ! dataH$Transmitter %in% c("A69-1601-29923", "A69-1601-31863", "A69-1601-31882", "A69-1601-31883"), ]


# ADD EEL CHARACTERISTICS
# Catch_location_type was not added in Creeks_data_analysis.R
eels<-"Eels.csv" #Filename with fish parameters (length, weight, stage, catch location)
eels<-read.csv(eels)
eels<-eels[ !duplicated(eels$Transmitter) , ]#this removes second duplicate (=in this case the pumping station eel)
eels2 = eels %>%
  select(Transmitter, Catch_location_type)
dataH=merge(dataH, eels2, by="Transmitter")

# Plot number of eel per location
detections = dataH %>%
  group_by(Station.Name, Transmitter)%>%
  select(Station.Name, unique(Transmitter))

summary=summarise(detections,
          stations=n_distinct(Station.Name),
          eels=n()
)
summary=summarise(summary,
                  stations=n_distinct(Station.Name),
                  eels=n()
)

summary$stations=NULL
summary$eels=as.numeric(summary$eels)
barplot(summary$eels, names.arg=summary$Station.Name, cex.names=0.8)

# Plot number of locations per eel 
detections = dataH %>%
  group_by(Transmitter, Station.Name)%>%
  select(Transmitter, Station.Name)

summary=summarise(detections,
                  eels=n_distinct(Transmitter),
                  stations=n()
)
summary=summarise(summary,
                  eels=n_distinct(Transmitter),
                  stations=n()
)

summary$eels=NULL
summary$stations=as.numeric(summary$stations)
par(mar=c(6,4.1,4.1,2.1))
barplot(summary$stations, names.arg=summary$Transmitter, cex.names=0.8, las=2)

dataH=merge(dataH, summary, by="Transmitter")

# Plot number of moves
detections = dataH %>%
  group_by(Transmitter)%>%
  select(Transmitter)

detections$moves="1"
detections$moves=as.numeric(detections$moves)


summary=summarise(detections,
                  eels=n_distinct(Transmitter),
                  moves=n()
)

summary$eels=NULL
summary$moves=as.numeric(summary$moves)
par(mar=c(6,4.1,4.1,2.1))
barplot(summary$moves, names.arg=summary$Transmitter, cex.names=0.8, ylim=c(0,2000),las=2)

dataH=merge(dataH, summary, by="Transmitter")

# Plot total number of detections per eel 
detections = dataH %>%
  group_by(Transmitter)%>%
  select(Transmitter, Detections)%>%
  summarize(maxdetections = sum(Detections))

detections$maxdetections=as.numeric(detections$maxdetections)
par(mar=c(6,4.1,4.1,2.1))
barplot(detections$maxdetections, names.arg=detections$Transmitter, cex.names=0.8, ylim=c(0,350000), las=2)

dataH=merge(dataH, detections, by="Transmitter")

# Calculate tracking period in days
detections = dataH %>%
  group_by(Transmitter)%>%
  select(Transmitter, Arrivalnum, Departurenum)

summary=summarise(detections,
                  days=with(detections, max(Departurenum) - min(Arrivalnum))
)


summary$days=summary$days/(60*60*24)
par(mar=c(6,4.1,4.1,2.1))
barplot(summary$days, names.arg=summary$Transmitter, cex.names=0.8, ylim=c(0,1200),las=2)

dataH=merge(dataH, summary, by="Transmitter")

# Calculate tracking period in seconds
detections = dataH %>%
  group_by(Transmitter)%>%
  select(Transmitter, Arrivalnum, Departurenum)

summary=summarise(detections,
                  seconds=with(detections, max(Departurenum) - min(Arrivalnum))
)

dataH=merge(dataH, summary, by="Transmitter")

#########################################################
# Add day, night and twilight

Diurnal<-"../resident-eel-polder-analysis//data//external//Diurnal.csv" 
Diurnal<-read.csv(Diurnal, header=TRUE, stringsAsFactors=FALSE)

# Preprocessing format
# Sunrise
Diurnal$Sunrise=Diurnal$Sunrise/100
Diurnal$Sunrisewhole=floor(Diurnal$Sunrise)
Diurnal$Sunrisefraction=Diurnal$Sunrise-Diurnal$Sunrisewhole
Diurnal$Sunrisefraction=Diurnal$Sunrisefraction*100
Diurnal$Sunrisefraction=round(Diurnal$Sunrisefraction, digits = 0)  # dit moet er bij anders staan er kommagetallen tussen (geen idee waarom of hoe R die berekent)
Diurnal$Sunrise=with(Diurnal, paste(Sunrisewhole, Sunrisefraction, sep=":"))
Diurnal$Sunrise=format(Diurnal$Sunrise, format="%H:%M")
Diurnal$Sunrise<-with(Diurnal,paste(Diurnal$Date, Diurnal$Sunrise, sep=" "))
Diurnal$Sunrise <- strptime(Diurnal$Sunrise, "%d/%m/%Y %H:%M")
Diurnal$Sunrise <- as.POSIXct(Diurnal$Sunrise, "%d/%m/%Y %H:%M", tz="UCT")

# Sunset
Diurnal$Sunset=Diurnal$Sunset/100
Diurnal$Sunsetwhole=floor(Diurnal$Sunset)
Diurnal$Sunsetfraction=Diurnal$Sunset-Diurnal$Sunsetwhole
Diurnal$Sunsetfraction=Diurnal$Sunsetfraction*100
Diurnal$Sunsetfraction=round(Diurnal$Sunsetfraction, digits = 0)  # dit moet er bij anders staan er kommagetallen tussen (geen idee waarom of hoe R die berekent)
Diurnal$Sunset=with(Diurnal, paste(Sunsetwhole, Sunsetfraction, sep=":"))
Diurnal$Sunset=format(Diurnal$Sunset, format="%H:%M")
Diurnal$Sunset<-with(Diurnal,paste(Diurnal$Date, Diurnal$Sunset, sep=" "))
Diurnal$Sunset <- strptime(Diurnal$Sunset, "%d/%m/%Y %H:%M")
Diurnal$Sunset <- as.POSIXct(Diurnal$Sunset, "%d/%m/%Y %H:%M", tz="UCT")

# Civbegin
Diurnal$Civbegin=Diurnal$Civbegin/100
Diurnal$Civbeginwhole=floor(Diurnal$Civbegin)
Diurnal$Civbeginfraction=Diurnal$Civbegin-Diurnal$Civbeginwhole
Diurnal$Civbeginfraction=Diurnal$Civbeginfraction*100
Diurnal$Civbeginfraction=round(Diurnal$Civbeginfraction, digits = 0)  # dit moet er bij anders staan er kommagetallen tussen (geen idee waarom of hoe R die berekent)
Diurnal$Civbegin=with(Diurnal, paste(Civbeginwhole, Civbeginfraction, sep=":"))
Diurnal$Civbegin=format(Diurnal$Civbegin, format="%H:%M")
Diurnal$Civbegin<-with(Diurnal,paste(Diurnal$Date, Diurnal$Civbegin, sep=" "))
Diurnal$Civbegin <- strptime(Diurnal$Civbegin, "%d/%m/%Y %H:%M")
Diurnal$Civbegin <- as.POSIXct(Diurnal$Civbegin, "%d/%m/%Y %H:%M", tz="UCT")

# Civend
Diurnal$Civend=Diurnal$Civend/100
Diurnal$Civendwhole=floor(Diurnal$Civend)
Diurnal$Civendfraction=Diurnal$Civend-Diurnal$Civendwhole
Diurnal$Civendfraction=Diurnal$Civendfraction*100
Diurnal$Civendfraction=round(Diurnal$Civendfraction, digits = 0)  # dit moet er bij anders staan er kommagetallen tussen (geen idee waarom of hoe R die berekent)
Diurnal$Civend=with(Diurnal, paste(Civendwhole, Civendfraction, sep=":"))
Diurnal$Civend=format(Diurnal$Civend, format="%H:%M")
Diurnal$Civend<-with(Diurnal,paste(Diurnal$Date, Diurnal$Civend, sep=" "))
Diurnal$Civend <- strptime(Diurnal$Civend, "%d/%m/%Y %H:%M")
Diurnal$Civend <- as.POSIXct(Diurnal$Civend, "%d/%m/%Y %H:%M", tz="UCT")

# Sunrise, sunset, civend, civbegin will be used to compare with detection time, so put these in date-time format
Diurnal$sunrise.dt <- ymd_hms(Diurnal$Sunrise)
Diurnal$sunset.dt <- ymd_hms(Diurnal$Sunset)
Diurnal$civbegin.dt <- ymd_hms(Diurnal$Civbegin)
Diurnal$civend.dt <- ymd_hms(Diurnal$Civend)

## Now create new dataframe with detection records (i.e. intervals) with diurnal phase

Diurnal$Date <- substr(Diurnal$Sunrise, 1, 10)
# A lot of unnecessary columns in Diurnal. For clarity sake, create new dataframe with only necessary columns
Diurnal.ok <- Diurnal[, c("Date", "sunrise.dt", "sunset.dt", "civbegin.dt", "civend.dt")]
# This dataframe will be merged with the detection dataset.
# First create column with date
dataH$Date <- substr(as.character(dataH$Departure_time), 1, 10)
intervals.with.daynight <- merge(x=dataH, y=Diurnal.ok, all.x=TRUE)
# We want to compare Departure_time with the 4 diurnal phases
intervals.with.daynight$Departure.dt <- ymd_hms(intervals.with.daynight$Departure_time)
# Write function for diurnal phase categorisation
a <- function(x) {
  if (x["Departure.dt"] >= x["sunrise.dt"] & x["Departure.dt"] <= x["sunset.dt"]) {
    return("Day")
  } else if (x["Departure.dt"] < x["sunrise.dt"] & x["Departure.dt"] >= x["civbegin.dt"]) {
    return("Dawn")
  } else if (x["Departure.dt"] > x["sunset.dt"] & x["Departure.dt"] <= x["civend.dt"]) {
    return("Dusk")
  } else {
    return("Night")
  }
}
# Apply this function to every row of the data frame
# Note, try to work with apply instead of for-loops; R is not so good with for-loops
r <- apply(intervals.with.daynight[, c("Departure.dt", "sunrise.dt", "sunset.dt", "civbegin.dt", "civend.dt")], 1, a)
# Paste this column to dataframe
intervals.with.daynight$daynight <- r
dataH <- intervals.with.daynight
dataH$sunrise.dt <- NULL
dataH$sunset.dt <- NULL
dataH$civbegin.dt <- NULL
dataH$civend.dt <- NULL 

# Check number of diurnal phases
table(dataH$daynight)


##########################################################################################

#################################################
# Explore environmental (explanatory) variables #
#################################################


# See Creeks_data_exploration

# 1. Test for outliers

par(mfrow= c (1,2), mar = c(5,4,2,1))
boxplot(dataH$precipitation,  ylab = "Precipitation (mm)")
dotchart(dataH$precipitation, xlab = "Precipitation (mm)",
         ylab = "Order of the data")

# Only variable 'pump' had outliers (negative values) -> set them to zero
datax=dataH[which(dataH$pump < "0"), ]

for (i in 1:dim(dataH)[1]){
  if (dataH$pump[i] < 0){  
    dataH$pump[i] <- "0"
  }
}

dataH$pump=as.numeric(dataH$pump)

# 2. Homogeneity
# Create scatterplot
par(mar = c(2, 5, 0, 5))
par(mfrow= c (2,3))
plot(dataH$precipitation, ylab = "Precipitation (mm)")
plot(dataH$pump, ylab = "Pumping discharge (m?/s)")
plot(dataH$Temp, ylab = "Temperature (?C)")
plot(dataH$daylength, ylab = "Daylength (minutes)")
plot(dataH$pressure, ylab = "Air pressure (hPa)")

# 3. Normality
dev.off()
# Create histogram and qq plot
par(mfrow= c (1,2))
hist(dataH$precipitation,  xlab = "Precipitation (mm)")
qqnorm(dataH$precipitation)
par(mfrow= c (1,2))
hist(dataH$pump, xlab = "Pumping discharge (m?/s)")
qqnorm(dataH$pump)
par(mfrow= c (1,2))
hist(dataH$Temp, xlab = "Temperature (?C)")
qqnorm(dataH$Temp)
par(mfrow= c (1,2))
hist(dataH$daylength, xlab = "Daylength (minutes)")
qqnorm(dataH$daylength)
par(mfrow= c (1,2))
hist(dataH$pressure, xlab = "Air pressure (hPa)")
qqnorm(dataH$pressure)

# 4. Lots of zero?
# You can check this with scatter plots (see 2.)
# Not applicable for these abiotic data (e.g. when precipitation = 0 -> no rain -> ecological reason)


# 5. Collinearity
# Take environmental variables and put them in new data frame datavars
datavars=dataH[,c("precipitation","Temp","pump","daylength","pressure","weir")]


#character wordt numeric
i <- sapply(datavars, is.character)
datavars[i] <- lapply(datavars[i], as.numeric)


#dim(datavars)
#head(datavars)
#summary(datavars)
#str(datavars)

#Create two matrices, one in which the correlations will be pasted and one for the significane levels --> variables = variables you want to calculate the correlation for (to define)
#Cree?r een matrix met in zowel rijen als kolommen de variablen (symmetrische matrix), dus beide 16
correlation <- matrix(data = NA, nrow = 6, ncol = 6)
significance <- matrix(data = NA, nrow = 6, ncol = 6)

colnames(correlation) <- colnames(datavars)
rownames(correlation) <- colnames(datavars)

colnames(significance) <- colnames(datavars)
rownames(significance) <- colnames(datavars)

i <- NULL
j <- NULL

# Be careful that you select the correct columns, since i and j range from 1 to n. So they will start using the first column of the 'Dataset'. If not appropriate, make a subset of the 'Dataset' or indicate in the for loop which column numbers can be selected
# ncol geeft aantal kolommen van  de dataset aan (i loopt dus van 1 tot einde aantal kolommen, wat 16 is)
for (i in 1:ncol(datavars))
{
  for (j in 1:ncol(datavars))
  {
    correlation[i,j] <- cor.test(datavars[,i],datavars[,j], use = "pairwise.complete.obs", method = "spearman")$estimate
    significance[i,j] <- cor.test(datavars[,i],datavars[,j],use = "pairwise.complete.obs", method = "spearman")$p.value          # method = "spearman"
  }
}

# To export your outcome to a csv-file
write.csv(correlation, "correlation.csv")
write.csv(significance, "significance.csv")

# Also, variation inflation factors (VIF) to check correlation
# Only on continious variables
# A high VIF value (>10) indicates a correlation problem
# If VIF values are below 3 (see Chapter 26 in Zuur et al. (2007)), this indicates there is no collinearity in these variables (at least not without the categorical variables)
dataHvars=dataH[, c("precipitation", "Temp", "daylength", "pump", "pressure","Length","Weight")]
vif(dataHvars)


# 6. Relationships
# Cfr. Fig. 9 Zuur et al. (2009)
Z <- as.vector(as.matrix(dataH[, c("pump" ,"precipitation" ,"Temp" ,"daylength" ,"pressure")]))

#Setup the data in vector format for the xyplot
Y10 <- rep(dataH$Departurenum, 5)

MyNames <- names(dataH[,c("pump" ,"precipitation" ,"Temp" ,"daylength" ,"pressure")])

ID10 <- rep(MyNames, each = length(dataH$Departurenum))
library(lattice)


ID11 <- factor(ID10, labels = c("pump" ,"precipitation" ,"Temp" ,"daylength" ,"pressure"),
               levels = c("pump" ,"precipitation" ,"Temp" ,"daylength" ,"pressure"))


xyplot(Y10 ~ Z | ID11, col = 1,
       strip = function(bg='white',...) strip.default(bg='white',...),
       scales = list(alternating = T,
                     x = list(relation = "free"),
                     y = list(relation = "same")),
       xlab = "Covariates",
       par.strip.text = list(cex = 0.8),
       ylab = "Departurenum",
       panel=function(x, y, subscripts,...){
         panel.grid(h =- 1, v = 2)
         panel.points(x, y, col = 1, pch = 16)
         if(ID10[subscripts][1] != "Tallsedge") {panel.loess(x,y,col=1,lwd=2)}
       })



# 7. Interactions



# 8. Independence
#Figure 12
#Define the time axis
Time <- seq(1,17947)

par(mar = c(4, 5, 4, 3))
plot(Time, dataH$Departurenum, type = "l", xlab = "Time (days)",
     ylab = "Departurenum")
acf(dataH$Departurenum, main = "Departurenum ACF")

# Variograms for irregularly spaced time series and spatial data
dists <- dist(dataH[,13:14])
summary(dists)


breaks = seq(0, 0.2, l = 30)
v1 <- variog(coords = dataH[,13:14], data = dataH$swimdistance, breaks = breaks)

v1.summary <- cbind(c(1:30), v1$v, v1$n)
colnames(v1.summary) <- c("lag", "semi-variance", "# of pairs")

v1.summary
plot(v1, type = "b", main = "Variogram: Departurenum")


#################################################################################

####################
# Network analysis #
####################

# First run functions in 'Creeks_network_analysis.R'

detach("package:dplyr", unload=TRUE)

par(mar = c(0, 7, 0, 5))

# Generate graph for dataH
g.intervals <- intervals2graph(dataH, "A69-1601-31858")
plot.migration.intervals(g.intervals)  # make sure unique.edges=TRUE to plot graph

#centr_degree(g.intervals, mode = c("total"), loops = TRUE,normalized = TRUE)
#V(g.intervals)

#E(g.intervals)$swimdistance
#edge_attr(g.intervals)



# ====================================================
# 1) Does the home range (i.e. range distance) differ for eels between different habitats (creek, polder, channel), lengths, weights and stadiums? And within different habitats?
# ====================================================
# Calculate 1D home range (i.e. range distance) and put it in dataframe with 10 more variables
diameter(g.intervals, directed = TRUE, unconnected = TRUE, weights = E(g.intervals)$swimdistance)

DataHdistance = dataH %>%
  group_by(Transmitter)%>%
  select(Transmitter, Length, Weight, Stadium, Catch_location, Catch_location_type, stations, moves, maxdetections, days)
DataHdistance = distinct(select(Distance, Transmitter, Length, Weight, Stadium, Catch_location, Catch_location_type, stations, moves, maxdetections, days))

#write.csv(DataHdistance, "DataHdistance.csv")

# Load in DataHdistance
DataHdistance<-"DataHdistance.csv" 
DataHdistance<-read.csv(DataHdistance, header=TRUE, stringsAsFactors=FALSE)

# Remove eels with 14 days tracking and less (A69-1601-29923, A69-1601-31863, A69-1601-31882, A69-1601-31883)
DataHdistance <- DataHdistance[ ! DataHdistance$Transmitter %in% c("A69-1601-29923", "A69-1601-31863", "A69-1601-31882", "A69-1601-31883"), ]

# Create factors
DataHdistance$Stadium <- as.factor(DataHdistance$Stadium)
DataHdistance$Catch_location_type <- as.factor(DataHdistance$Catch_location_type)
DataHdistance$Catch_location<-factor(DataHdistance$Catch_location, levels=c("9","6","10","7","4","3","1","2"))

# Response variable is numeric
DataHdistance$distance2 <- as.numeric(DataHdistance$distance2)


# Plot number of eels per catch location type
loc = DataHdistance %>%
  group_by(Catch_location_type, Transmitter)%>%
  select(Catch_location_type, unique(Transmitter))

summary=summarise(loc,
                  locations=n_distinct(Catch_location_type),
                  eels=n()
)
summary=summarise(summary,
                  locations=n_distinct(Catch_location_type),
                  eels=n()
)

summary$locations=NULL
summary$eels=as.numeric(summary$eels)
barplot(summary$eels, names.arg=summary$Catch_location_type, cex.names=0.8, main="Number of detected eels per catch location type", ylab="Number of eels")


# Create boxplot with distance for each catch location
boxplot(distance2~Catch_location, DataHdistance, xlab = "Catch location", ylab = "Distance (m)")

# Create boxplot with distance for each catch location type
boxplot(distance2~Catch_location_type, DataHdistance, xlab = "Catch location type", ylab = "Distance (m)", ylim=c(0, 20000), cex.lab=1.25, cex.axis=1.25)

# Create boxplot with distance for each stadium
boxplot(distance2~Stadium, DataHdistance, xlab = "Stadium", main="Home range per stadium", ylab = "Distance (m)", ylim=c(0, 20000), cex.lab=1.25, cex.axis=1.25)

# Plot distance to weight
plot(distance2~Weight, DataHdistance, xlab = "Weight (g)", main="Distance to weight", ylab = "Distance (m)")
abline(lm(DataHdistance$distance2~DataHdistance$Weight), col="red") # regression line (y~x)
lines(lowess(DataHdistance$Weight,DataHdistance$distance2), col="blue") # lowess line (x,y) 

# Is distance correlated with tracked time?
correlation<- cor.test(DataHdistance$days,DataHdistance$distance2, use = "pairwise.complete.obs", method = "spearman")$estimate
significance<- cor.test(DataHdistance$days,DataHdistance$distance2, use = "pairwise.complete.obs", method = "spearman")$p.value

correlation
significance

# Is distance correlated with weight?
correlation<- cor.test(DataHdistance$Weight,DataHdistance$distance2, use = "pairwise.complete.obs", method = "spearman")$estimate
significance<- cor.test(DataHdistance$Weight,DataHdistance$distance2, use = "pairwise.complete.obs", method = "spearman")$p.value

correlation
significance

# Create lm with weight as explanatory variable
lm <- lm(distance2~Weight, data=DataHdistance)
summary(lm)


# Compute summary statistics
Creek=DataHdistance[which(DataHdistance$Catch_location_type == "Creek"), ]
mean(Creek$distance2)
var(Creek$distance2)
sd(Creek$distance2)

Channel=DataHdistance[which(DataHdistance$Catch_location_type == "Channel"), ]
mean(Channel$distance2)
var(Channel$distance2)
sd(Channel$distance2)

Polder=DataHdistance[which(DataHdistance$Catch_location_type == "Polder"), ]
mean(Polder$distance2)
var(Polder$distance2)
sd(Polder$distance2)


# Create linear model
lm <- lm(tdistance~Catch_location_type, data=DataHdistance)
summary(lm)
anova(lm)

# Check assumptions
# 1) Create normal probability plot of residuals
lm.stdres = rstandard(lm)


dev.off()
par(mfrow= c (1,2))
hist(DataHdistance$distance2)
qqnorm(lm.stdres,
          ylab="Standardized Residuals",
          xlab="Normal Scores",
          main="Distance") 
qqline(lm.stdres)

shapiro.test(lm.stdres)    # p-value > 0.05: no evidence against null hypothesis (= data is normal distributed)

# 2) Check homogeneity of variance
par(mar = c(4, 10, 2, 3))
par(mfrow = c(2, 2))
plot(lm)   # Here also normality of residuals in QQ plot

# Transformation
DataHdistance$tdistance = log(DataHdistance$distance2)



# Create censored regression model
estResult <- censReg(tdistance~Catch_location_type, data=DataHdistance)
summary(estResult)
coef(estResult)
coef(estResult,logSigma = FALSE)
vcov <- as.data.frame(vcov(estResult))    # returns the variance-covariance matrix of the coefficients



# Apply one-way anova
a <- aov(tdistance~Catch_location_type, data=DataHdistance)
summary(a)











# ===========================================
# 2)  What factors determine dispersion (light, watertemp, precipitation.)? Does this differ for eels between different habitats? And within different habitats?
# ===========================================

# Categorize the explanatory variable: <= 1000m = 0 (low displacement), >1000m = 1 (high displacement)
par(mfrow= c (1,3), mar = c(5,4,2,1))
boxplot(dataH$swimdistance)
boxplot(dataH$swimdistance, ylim=c(0,2000))
boxplot(dataH$swimdistance, ylim=c(0,500))
summary(dataH$swimdistance)

dataH$movement <- NA

for (i in 1:dim(dataH)[1]){
  if (dataH$swimdistance[i] <= 500) {
    dataH$movement[i]="0"
  }
  else {
    dataH$movement[i]="1"
  }
}


table(dataH$movement)

mov = dataH[which(dataH$movement =="1"),]
mov$Transmitter = factor(mov$Transmitter)
levels(mov$Transmitter)




# weir, stadium, Year, month and daynight are factors
dataH$fweir <- factor(dataH$weir)
dataH$fstadium <- factor(dataH$Stadium)
dataH$fmonth <- factor(dataH$Month)
dataH$fyear <- factor(dataH$Year)
dataH$fdaynight <- factor(dataH$daynight)
dataH$floc <- factor(dataH$Catch_location_type)
dataH$ftag <- factor(dataH$Transmitter)

# Movement as numeric
dataH$movement <- as.numeric(dataH$movement)

# Apply regression per year
#data2=dataH[which(dataH$Year == "2013"), ]

# Apply regression per stadium
#data2=data2[which(data2$Stadium == "FII"), ]

# Apply regression per eel
#dataHind=dataH[which(dataH$Transmitter == "A69-1601-29920"), ]




#===========================================================
# Apply bootstrapping to obtain same number of 0 as 1
# Make 2 lists: list with movement = 1 and list with movement = 0
#write.csv(data2, "homing.csv")   # Write as csv and load it in
#dt <- fread("homing.csv", sep=",", header=TRUE, stringsAsFactors=FALSE)
dt<-data.table(dataH)   # Convert dataframe to datatable for following code
transmitters <- unique(dt[, Transmitter])
set.seed(600)
result <- lapply(
  transmitters, function(x) {
    list(
      dt[Transmitter==x & movement==1][sample(
        .N, min(
          nrow(dt[Transmitter==x & movement==1]), nrow(dt[Transmitter==x & movement==0])
        )
      )],
      dt[Transmitter==x & movement==0][sample(
        .N, min(
          nrow(dt[Transmitter==x & movement==1]), nrow(dt[Transmitter==x & movement==0])
        )
      )]
    )
  }
)



# Request records from first tag for first list (list with movement = 1)  
#x=as.data.frame(result[[1]][[1]])
# Request records from first tag for second list (list with movement = 0)    
#y=as.data.frame(result[[1]][[2]])

# Make 1 dataframe
t <- do.call(rbind, Map(data.frame, result))

# Select variables of interest for subset movement=1 and movement=0
t1 <- t[, c("precipitation","Temp","daylength","pump","pressure","Length","Weight","fstadium","fweir","fmonth","fyear","fdaynight","floc","ftag","movement")]
t2 <- t[, c("precipitation.1","Temp.1","daylength.1","pump.1","pressure.1","Length.1","Weight.1","fstadium.1","fweir.1","fmonth.1","fyear.1","fdaynight.1","floc.1","ftag.1","movement.1")]
colnames(t2)[1] <- "precipitation"
colnames(t2)[2] <- "Temp"
colnames(t2)[3] <- "daylength"
colnames(t2)[4] <- "pump"
colnames(t2)[5] <- "pressure"
colnames(t2)[6] <- "Length"
colnames(t2)[7] <- "Weight"
colnames(t2)[8] <- "fstadium"
colnames(t2)[9] <- "fweir"
colnames(t2)[10] <- "fmonth"
colnames(t2)[11] <- "fyear"
colnames(t2)[12] <- "fdaynight"
colnames(t2)[13] <- "floc"
colnames(t2)[14] <- "ftag"
colnames(t2)[15] <- "movement"

# Bind both subsets together
t3 <- rbind(t1,t2)


df=t3
df=na.omit(df)
sum(is.na(df))





# Data exploration
#==========================

# Create boxplot between response variable 'movement' and each explanatory variable
par(mar = c(2, 6, 2, 3))
par(mfrow = c(3, 2))

boxplot(precipitation~movement, df, xlab = "Movement", ylab = "Precipitation")
boxplot(Temp~movement, df, xlab = "Movement", ylab = "Temperature")
boxplot(daylength~movement, df, xlab = "Movement", ylab = "Daylength")
boxplot(pump~movement, df, xlab = "Movement", ylab = "Discharge")
boxplot(pressure~movement, df, xlab = "Movement", ylab = "Air pressure")
#boxplot(Length~movement, df, xlab = "Movement", ylab = "Length")
#boxplot(Weight~movement, df, xlab = "Movement", ylab = "Weight")

# Create boxplot with log(x+1) transformed explanatory variable
x <- df$precipitation
df$logprec <- log(x+1)
boxplot(logprec~movement, df, xlab = "Movement", ylab = "Log(x+1) Precipitation")



# Create plots between continuous responese variable 'swim distance' and each explanatory variable
par(mar = c(2, 6, 2, 3))
par(mfrow = c(4, 2))

plot(precipitation~swimdistance, dataH, xlab = "Movement", ylab = "Precipitation")
plot(Temp~swimdistance, dataH, xlab = "Movement", ylab = "Temperature")
plot(daylength~swimdistance, dataH, xlab = "Movement", ylab = "Daylength")
plot(pump~swimdistance, dataH, xlab = "Movement", ylab = "Discharge")
plot(pressure~swimdistance, dataH, xlab = "Movement", ylab = "Air pressure")
plot(Length~swimdistance, dataH, xlab = "Movement", ylab = "Length")
plot(Weight~swimdistance, dataH, xlab = "Movement", ylab = "Weight")


# Create plot between response variable 'movement' and each explanatory variable
par(mar = c(2, 6, 2, 3))
par(mfrow = c(4, 2))

plot(movement~precipitation, dataH, ylab = "Movement", xlab = "Precipitation")
abline(lm(dataH$movement~dataH$precipitation),col="red")
plot(movement~Temp, dataH, ylab = "Movement", xlab = "Temperature")
abline(lm(dataH$movement~dataH$Temp),col="red")
plot(movement~daylength, dataH, ylab = "Movement", xlab = "Daylength")
abline(lm(dataH$movement~dataH$daylength),col="red")
plot(movement~pump, dataH, ylab = "Movement", xlab = "Discharge")
abline(lm(dataH$movement~dataH$pump),col="red")
plot(movement~pressure, dataH, ylab = "Movement", xlab = "Air pressure")
abline(lm(dataH$movement~dataH$pressure),col="red")
plot(movement~Length, dataH, ylab = "Movement", xlab = "Length")
abline(lm(dataH$movement~dataH$Length),col="red")
plot(movement~Weight, dataH, ylab = "Movement", xlab = "Weight")
abline(lm(dataH$movement~dataH$Weight),col="red")

# Create plots to check spreading of the variables in relation with month

plot(movement~fmonth, df)
plot(precipitation~fmonth, df)
plot(Temp~fmonth, df)
plot(daylength~fmonth, df)
plot(pump~fmonth, df)
plot(pressure~fmonth, df)


#################################################
# 2. Homogeneity
# Create scatterplot
par(mar = c(2, 5, 0, 5))
par(mfrow= c (2,3))
plot(df$precipitation, ylab = "Precipitation (mm)")
plot(df$pump, ylab = "Pumping discharge (m?/s)")
plot(df$Temp, ylab = "Temperature (?C)")
plot(df$daylength, ylab = "Daylength (minutes)")
plot(df$pressure, ylab = "Air pressure (hPa)")


################################################
# Plotting Empirical Cumulative Distribution to identify link (logit or probit)
par(mar = c(4, 6, 4, 3))
par(mfrow = c(3, 2))

# extract explenatory variable data vector
temp = df$Temp
# calculate the number of non-missing values in "ozone"
n = sum(!is.na(temp))
# obtain empirical CDF values
temp.ecdf = ecdf(temp)
plot(temp.ecdf, xlab = 'Sample Quantiles of temperature', ylab = '', main = 'Empirical Cumulative Distribution\ntemp')
# Based on this plot a probit or logit link can be chosen (probit: broad, logit: narrow)

prec = df$precipitation
n = sum(!is.na(prec))
prec.ecdf = ecdf(prec)
plot(prec.ecdf, xlab = 'Sample Quantiles of precipitation', ylab = '', main = 'Empirical Cumulative Distribution\nprec')

dl = df$daylength
n = sum(!is.na(dl))
dl.ecdf = ecdf(dl)
plot(dl.ecdf, xlab = 'Sample Quantiles of daylength', ylab = '', main = 'Empirical Cumulative Distribution\ndaylength')

pump = df$pump
n = sum(!is.na(pump))
pump.ecdf = ecdf(pump)
plot(pump.ecdf, xlab = 'Sample Quantiles of discharge', ylab = '', main = 'Empirical Cumulative Distribution\ndischarge')

pressure = df$pressure
n = sum(!is.na(pressure))
pressure.ecdf = ecdf(pressure)
plot(pressure.ecdf, xlab = 'Sample Quantiles of air pressure', ylab = '', main = 'Empirical Cumulative Distribution\nair pressure')



############################################################
# Collinearity
# Take environmental variables and put them in new data frame datavars
datavars=df[,c("precipitation","Temp","pump","daylength","pressure")]
#character wordt numeric
i <- sapply(datavars, is.character)
datavars[i] <- lapply(datavars[i], as.numeric)
#dim(datavars)
#head(datavars)
#summary(datavars)
#str(datavars)
#Create two matrices, one in which the correlations will be pasted and one for the significane levels --> variables = variables you want to calculate the correlation for (to define)
#Cree?r een matrix met in zowel rijen als kolommen de variablen (symmetrische matrix), dus beide 16
correlation <- matrix(data = NA, nrow = 5, ncol = 5)
significance <- matrix(data = NA, nrow = 5, ncol = 5)
colnames(correlation) <- colnames(datavars)
rownames(correlation) <- colnames(datavars)
colnames(significance) <- colnames(datavars)
rownames(significance) <- colnames(datavars)

i <- NULL
j <- NULL

# Be careful that you select the correct columns, since i and j range from 1 to n. So they will start using the first column of the 'Dataset'. If not appropriate, make a subset of the 'Dataset' or indicate in the for loop which column numbers can be selected
# ncol geeft aantal kolommen van  de dataset aan (i loopt dus van 1 tot einde aantal kolommen, wat 16 is)
for (i in 1:ncol(datavars))
{
  for (j in 1:ncol(datavars))
  {
    correlation[i,j] <- cor.test(datavars[,i],datavars[,j], use = "pairwise.complete.obs", method = "spearman")$estimate
    significance[i,j] <- cor.test(datavars[,i],datavars[,j],use = "pairwise.complete.obs", method = "spearman")$p.value          # method = "spearman"
  }
}

# To export your outcome to a csv-file
write.csv(correlation, "correlation.csv")
write.csv(significance, "significance.csv")


###################################################################
# 'Correlation' between continuous and categorical variables
# we expect correlation between temperature and month
boxplot(Temp~fmonth, data=df)

model.lm <- lm(Temp ~ fmonth, data = df)
summary(model.lm)

rsq <- summary(model.lm)$r.squared
rsq
sqrt(rsq)
# What this actually represents is the correlation between the observed Temp, and the ones predicted (fitted) by our model. Both of these variables are numerical so we are able to correlate them.
print(model.lm$fitted)
# Apply Pearson correlation on them
cor(df$Temp, model.lm$fitted)

# We expect correlation between weight and stadium
boxplot(Weight~fstadium, data=df)
model.lm <- lm(Weight ~ fstadium, data = df)
summary(model.lm)

rsq <- summary(model.lm)$r.squared
rsq
sqrt(rsq)
print(model.lm$fitted)
cor(df$Weight, model.lm$fitted)

# We expect correlation between temperature and daynight
boxplot(Temp~fdaynight, data=df)
model.lm <- lm(Temp ~ fdaynight, data = df)
summary(model.lm)

rsq <- summary(model.lm)$r.squared
rsq
sqrt(rsq)
print(model.lm$fitted)
cor(df$Temp, model.lm$fitted)

# We expect correlation between precipitation and weir
boxplot(precipitation~fweir, data=df)
model.lm <- lm(precipitation ~ fweir, data = df)
summary(model.lm)

rsq <- summary(model.lm)$r.squared
rsq
sqrt(rsq)
print(model.lm$fitted)
cor(df$precipitation, model.lm$fitted)


# We expect correlation between weight and tag ID
plot(Weight~ftag, data=df)
model.lm <- lm(Weight ~ ftag, data = df)
summary(model.lm)

rsq <- summary(model.lm)$r.squared
rsq
sqrt(rsq)
print(model.lm$fitted)
cor(df$Weight, model.lm$fitted)





#############################################
# Model development
#############################################


#############################################
# Apply Random Forests
#############################################
set.seed(500)

# Create training and test set
#trainrows <- runif(nrow(df)) > 0.3
#train <- df[trainrows,]
#test <- df[!trainrows,]

# Apply RF; if response variable is categorical, classification is assumed; otherwise, regression is assumed
fit = randomForest(factor(movement) ~ precipitation + Temp + pressure + pump + floc, data=df, importance = TRUE, ntree=6000)
varImpPlot(fit)
importance(fit)
round(importance(fit), 2)

# Calculate kappa
x=as.data.frame(fit$confusion)
x$class.error = NULL
x=as.matrix(x)
kap <- function(x) {
  a <- (x[1,1] + x[2,2]) / sum(x)
  e <- (sum(x[1,]) / sum(x)) * (sum(x[,1]) / sum(x)) + (1 - (sum(x[1,]) / sum(x))) * (1 - (sum(x[,1]) / sum(x)))
  (a-e)/(1-e)
}
kap(x)


partialPlot(fit, df, x.var=precipitation,ylab="Class probability",xlab="Precipitation (mm)")
partialPlot(fit, df, x.var=pressure,ylab="Class probability",xlab="Air pressure (hPa)")
partialPlot(fit, df, x.var=pump,ylab="Class probability",xlab="Discharge (m?/s)")
partialPlot(fit, df, x.var=Temp,ylab="Class probability",xlab="Temperature (?C)")
partialPlot(fit, df, x.var=floc,ylab="Class probability",xlab="Catch location")
partialPlot(fit, df, x.var=daylength,ylab="Class probability",xlab="Daylength (minutes)")





#############################################
# Apply regression
#############################################

# GLM
glm = glm(movement ~ precipitation + Temp + pressure + pump + fweir + floc, family=binomial(logit), data=df)
summary(glm)
par(mar = c(4, 10, 2, 3))
par(mfrow = c(2, 2))
plot(glm)

step(glm)
drop1(glm, test="Chi")
anova(glm)

# GAM
gam = gam(movement ~ precipitation + Temp + pressure + pump + fweir + floc, family=binomial(logit), data=df)
summary(gam)



# GLMM

# Calculate summary with AIC, Marginal R? and Conditional R?
modlist = list(
  glmer(movement ~ precipitation + Temp + pressure + pump + floc + (1|fyear), family=binomial(logit), data=df),
  glmer(movement ~ precipitation + Temp + pressure + pump + floc + (1|fmonth), family=binomial(logit), data=df),
  glmer(movement ~ precipitation + Temp + pressure + pump + floc + (1|ftag), family=binomial(logit), data=df),
  glmer(movement ~ precipitation + Temp + pressure + pump + floc + (1|fyear) + (1|fmonth), family=binomial(logit), data=df),
  glmer(movement ~ precipitation + Temp + pressure + pump + floc + (1|fyear) + (1|ftag), family=binomial(logit), data=df),
  glmer(movement ~ precipitation + Temp + pressure + pump + floc + (1|fmonth) + (1|ftag), family=binomial(logit), data=df),
  glmer(movement ~ precipitation + Temp + pressure + pump + floc + (1|fyear) + (1|fmonth) + (1|ftag), family=binomial(logit), data=df)
  )

sem.model.fits(modlist)

# Model with only (1|fmonth) as random effect has lowest AIC (2174.119)
glmm = glmer(movement ~ precipitation + Temp + pressure + pump + floc + (1|fmonth), family=binomial(logit), data=df)
summary(glmm)
display(glmm)
VarCorr(glmm)      # Shows correlation between random effect and intercept

# Use this model and apply model selection based on AIC

modlist = list(
  glmer(movement ~ Temp + pressure + pump + floc + (1|fmonth), family=binomial(logit), data=df),
  glmer(movement ~ precipitation + pressure + pump + floc + (1|fmonth), family=binomial(logit), data=df),
  glmer(movement ~ precipitation + Temp + pump + floc + (1|fmonth), family=binomial(logit), data=df),
  glmer(movement ~ precipitation + Temp + pressure + floc + (1|fmonth), family=binomial(logit), data=df),
  glmer(movement ~ precipitation + Temp + pressure + pump + (1|fmonth), family=binomial(logit), data=df)
  )

sem.model.fits(modlist)

# Last model (without floc) has lowest AIC (2170.127)
modlist = list(
  glmer(movement ~ Temp + pressure + pump + (1|fmonth), family=binomial(logit), data=df),
  glmer(movement ~ precipitation + pressure + pump + (1|fmonth), family=binomial(logit), data=df),
  glmer(movement ~ precipitation + Temp + pump + (1|fmonth), family=binomial(logit), data=df),
  glmer(movement ~ precipitation + Temp + pressure + (1|fmonth), family=binomial(logit), data=df)
)

sem.model.fits(modlist)

# Third model (without pressure) has lowest AIC (2168.373)
modlist = list(
  glmer(movement ~ Temp + pump + (1|fmonth), family=binomial(logit), data=df),
  glmer(movement ~ precipitation + pump + (1|fmonth), family=binomial(logit), data=df),
  glmer(movement ~ precipitation + Temp + (1|fmonth), family=binomial(logit), data=df)
)

sem.model.fits(modlist)

# Third model (without pump) has lowest AIC (2166.654)
modlist = list(
  glmer(movement ~ Temp + (1|fmonth), family=binomial(logit), data=df),
  glmer(movement ~ precipitation + (1|fmonth), family=binomial(logit), data=df)
)

sem.model.fits(modlist)

# Second model (without temperature) has lowest AIC (2165.677)
# No more improvement of model (AIC does not lower anymore).
# Best model is:
glmm <- glmer(movement ~ precipitation + (1|fmonth), family=binomial(logit), data=df)
summary(glmm)
VarCorr(glmm)      # Shows correlation between random effect and intercept

sem.model.fits(glmm)
plot(glmm)
binnedplot(fitted(glmm),resid(glmm))

# Fine tuning selected model based on p-values: remove fweir
# Not necessary
#glmm <- glmer(movement ~ precipitation + (1|fmonth), family=binomial(logit), data=df)
#summary(glmm)

#sem.model.fits(glmm)
#plot(glmm)
#binnedplot(fitted(glmm),resid(glmm))

# Check final model for overdispersion
dispersion_glmer(glmm)   # It should not be over 1.4

# Model validation
# Create contingency table
check_model <- table(df$movement,fitted.values(glmm)>0.5)
# Calculate kappa
kap <- function(x) {
  a <- (x[1,1] + x[2,2]) / sum(x)
  e <- (sum(x[1,]) / sum(x)) * (sum(x[,1]) / sum(x)) + (1 - (sum(x[1,]) / sum(x))) * (1 - (sum(x[,1]) / sum(x)))
  (a-e)/(1-e)
}

kap(check_model)

# Create plots
par(mfrow=c(2,3))
qqnorm(resid(glmm))
hist(resid(glmm))
plot(fitted(glmm),resid(glmm))
binnedplot(fitted(glmm),resid(glmm))
# Plot residuals against continuous predictor
fitted<-predict(glmm,type="response")
resid<-resid(glmm,type="pearson")
plot(df$precipitation,resid)
# Plot residuals against fitted values
plot(fitted,resid)






# ===========================================
# 3) During what period of the day are they most active (diurnal)?
# ===========================================

#====== Departures per hour

# Select 'large movement events'
#data2=dataH[which(dataH$movement == "1"), ]

# Calculate hour lengths of diurnal periods
Diurnal.ok$daylength <- Diurnal.ok$sunset.dt - Diurnal.ok$sunrise.dt
Diurnal.ok$dawnlength <- (Diurnal.ok$sunrise.dt - Diurnal.ok$civbegin.dt)/60  # /60 to set in hour
Diurnal.ok$dusklength <- (Diurnal.ok$civend.dt - Diurnal.ok$sunset.dt)/60  # /60 to set in hour
# Calculate night length as difference between 24h and day+dawn+dusk
Diurnal.ok$daylength <- as.numeric(Diurnal.ok$daylength)
Diurnal.ok$dawnlength <- as.numeric(Diurnal.ok$dawnlength)
Diurnal.ok$dusklength <- as.numeric(Diurnal.ok$dusklength)
Diurnal.ok$dh=as.numeric(24)
Diurnal.ok$nightlength <- Diurnal.ok$dh - Diurnal.ok$daylength
Diurnal.ok$nightlength <- Diurnal.ok$nightlength - Diurnal.ok$dawnlength
Diurnal.ok$nightlength <- Diurnal.ok$nightlength - Diurnal.ok$dusklength
Diurnal.ok$dh <- NULL

# Plot number of diurnal phases per departure date
detections = dataH%>%
  group_by(Date, daynight)%>%
  select(Date, daynight)

summary=summarise(detections,
                  d=n_distinct(daynight),
                  number=n()
)

summary$d=NULL
summary$number=as.numeric(summary$number)
par(mar=c(6,4.1,4.1,2.1))
barplot(summary$number, names.arg=summary$daynight, cex.names=0.8, ylim=c(0,40),las=1)

df=merge(Diurnal.ok, summary, by="Date")
df <- df[, c("Date", "daylength", "dawnlength", "dusklength", "nightlength","daynight","number")]


# Calculate detections per hour (dph) for each diurnal phase
df$dph=NA

for (i in 1:dim(df)[1]){
  if (df$daynight[i] == "Dusk") {
    df$dph[i]=df$number[i]/df$dusklength[i]
  } else if (df$daynight[i] == "Day"){
    df$dph[i]=df$number[i]/df$daylength[i]
  } else if (df$daynight[i] == "Dawn"){
    df$dph[i]=df$number[i]/df$dawnlength[i]
  } else if (df$daynight[i] == "Night"){
    df$dph[i]=df$number[i]/df$nightlength[i]
  }
  else {
    df$dph[i]=NA
  }
}
# Check for NA's
sum(is.na(df$dph))

# Make summary
d <- df[, c("dph", "daynight")]
summary(d)

# Create boxplot with departure for each diurnal phase
df$daynight<-factor(df$daynight, levels=c("Dawn","Day","Dusk","Night"))
boxplot(dph~daynight, df, xlab = "Diurnal phase", ylab = "Number of departures per hour", ylim=c(0, 15), cex.lab=1.25, cex.axis=1.25)

# Compute summary statistics
dawn=df[which(df$daynight == "Dawn"), ]
mean(dawn$dph)
var(dawn$dph)
sd(dawn$dph)

day=df[which(df$daynight == "Day"), ]
mean(day$dph)
var(day$dph)
sd(day$dph)

dusk=df[which(df$daynight == "Dusk"), ]
mean(dusk$dph)
var(dusk$dph)
sd(dusk$dph)

night=df[which(df$daynight == "Night"), ]
mean(night$dph)
var(night$dph)
sd(night$dph)

# => Large difference in variances!

# Apply one-way ANOVA
# Test normality
# Create histogram and qq plot
dev.off()
par(mfrow= c (1,2))
hist(df$dph,  xlab = "Detections per hour")
qqnorm(df$dph)
qqline(df$dph)
shapiro.test(df$dph)

# Test homogeneity of variances (if p-value < 0.05 no homogeneity)
bartlett.test(dph ~ daynight, data=df)
leveneTest(y=df$dph, group=df$daynight)

#The advantage of using the oneway.test() function is obviously the Welch correction for nonhomogeneity.
oneway.test(dph ~ daynight, data=df, var.equal=FALSE)

aov=aov(df$dph ~ df$daynight)
summary(aov)

# Overview assumptions normality and homogeneity
par(mfrow=c(1,2))     # set graphics window to plot side-by-side
plot(aov, 1)          # graphical test of homogeneity
plot(aov, 2)          # graphical test of normality
# A similar graphical test of residuals by groups can be had this way, provided there are no missing values in the data: 
boxplot(aov$resid ~ df$daynight)

# Transformation
df$tdph=log(df$dph)

# Apply post-hoc test
TukeyHSD(aov)
posthocTGH(df$tdph, df$daynight, method=c("games-howell"), digits=3)  # post-hoc test for unequal variances

# non-parametric Kruskall Wallis test as alternative for one-way ANOVA
kw=kruskal.test(df$tdph ~ df$daynight)


#====== Swim distance per hour



# Calculate hour lengths of diurnal periods
Diurnal.ok$daylength <- Diurnal.ok$sunset.dt - Diurnal.ok$sunrise.dt
Diurnal.ok$dawnlength <- (Diurnal.ok$sunrise.dt - Diurnal.ok$civbegin.dt)/60  # /60 to set in hour
Diurnal.ok$dusklength <- (Diurnal.ok$civend.dt - Diurnal.ok$sunset.dt)/60  # /60 to set in hour
# Calculate night length as difference between 24h and day+dawn+dusk
Diurnal.ok$daylength <- as.numeric(Diurnal.ok$daylength)
Diurnal.ok$dawnlength <- as.numeric(Diurnal.ok$dawnlength)
Diurnal.ok$dusklength <- as.numeric(Diurnal.ok$dusklength)
Diurnal.ok$dh=as.numeric(24)
Diurnal.ok$nightlength <- Diurnal.ok$dh - Diurnal.ok$daylength
Diurnal.ok$nightlength <- Diurnal.ok$nightlength - Diurnal.ok$dawnlength
Diurnal.ok$nightlength <- Diurnal.ok$nightlength - Diurnal.ok$dusklength
Diurnal.ok$dh <- NULL

# Plot number of diurnal phases per swim distance
detections = dataH%>%
  group_by(Date,swimdistance, daynight)%>%
  select(Date, Transmitter, swimdistance, daynight)

# Only take into account swim distances not equal to zero
df <- detections[which(detections$swimdistance != "0"), ]

# Sum swim distance per eel per diurnal phase
detections2 = df%>%
  group_by(Transmitter, Date, daynight)%>%
  select(Transmitter,Date,daynight,swimdistance)

df=summarise(detections2,
             sum=sum(swimdistance))


par(mar=c(6,4.1,4.1,2.1))
barplot(df$sum, names.arg=df$daynight, cex.names=0.8, ylim=c(0,15000),las=1)

df=merge(Diurnal.ok, df, by="Date")
df <- df[, c("Transmitter", "Date", "daylength", "dawnlength", "dusklength", "nightlength","daynight","sum")]




# Calculate swim distance per hour (sph) for each diurnal phase
df$sph=NA

for (i in 1:dim(df)[1]){
  if (df$daynight[i] == "Dusk") {
    df$sph[i]=df$sum[i]/df$dusklength[i]
  } else if (df$daynight[i] == "Day"){
    df$sph[i]=df$sum[i]/df$daylength[i]
  } else if (df$daynight[i] == "Dawn"){
    df$sph[i]=df$sum[i]/df$dawnlength[i]
  } else if (df$daynight[i] == "Night"){
    df$sph[i]=df$sum[i]/df$nightlength[i]
  }
  else {
    df$sph[i]=NA
  }
}
# Check for NA's
sum(is.na(df$sph))

# Make summary
d <- df[, c("sph", "daynight")]
summary(d)

# Create boxplot with swim distance for each diurnal phase
df$daynight<-factor(df$daynight, levels=c("Dawn","Day","Dusk","Night"))
boxplot(sph~daynight, df, xlab = "Diurnal phase", ylab = "Swim speed (m per hour)", ylim=c(0, 10000), cex.lab=1.25, cex.axis=1.25)

# Compute summary statistics
dawn=df[which(df$daynight == "Dawn"), ]
mean(dawn$sph)
var(dawn$sph)
sd(dawn$sph)

day=df[which(df$daynight == "Day"), ]
mean(day$sph)
var(day$sph)
sd(day$sph)

dusk=df[which(df$daynight == "Dusk"), ]
mean(dusk$sph)
var(dusk$sph)
sd(dusk$sph)

night=df[which(df$daynight == "Night"), ]
mean(night$sph)
var(night$sph)
sd(night$sph)

# => Large difference in variances!

# Apply one-way ANOVA
# Test normality
# Create histogram and qq plot
dev.off()
par(mfrow= c (1,2))
hist(df$tsph,  xlab = "Detections per hour")
qqnorm(df$tsph)
qqline(df$tsph)
shapiro.test(df$sph)

# Test homogeneity of variances (if p-value < 0.05 no homogeneity)
bartlett.test(sph ~ daynight, data=df)
leveneTest(y=df$sph, group=df$daynight)

#The advantage of using the oneway.test() function is obviously the Welch correction for nonhomogeneity.
oneway.test(sph ~ daynight, data=df, var.equal=FALSE)

aov=aov(df$tsph ~ df$daynight)
summary(aov)

# Overview assumptions normality and homogeneity
par(mfrow=c(1,2))     # set graphics window to plot side-by-side
plot(aov, 1)          # graphical test of homogeneity
plot(aov, 2)          # graphical test of normality
# A similar graphical test of residuals by groups can be had this way, provided there are no missing values in the data: 
boxplot(aov$resid ~ df$daynight)

# Transformation
df$tsph=log(df$sph)

# Apply post-hoc test
TukeyHSD(aov)
posthocTGH(df$tdph, df$daynight, method=c("games-howell"), digits=3)  # post-hoc test for unequal variances

# non-parametric Kruskall Wallis test as alternative for one-way ANOVA
kw=kruskal.test(df$tsph ~ df$daynight)
kw

posthoc.kruskal.dunn.test(x=df$tsph, g=df$daynight, p.adjust.method="bonferroni")



# ===========================================
# 4) During what period of the year are they most active (seasonal)?
# ===========================================

#====== departures per eel per day

# Plot number of months per departure date
dataH$Month=as.factor(dataH$Month)
detections1 = dataH%>%
  group_by(Date, Month)%>%
  select(Date, Month)

summary1=summarise(detections1,
                 number.of.departures=n()
)

detections2 = dataH%>%
  group_by(Date, Transmitter)%>%
  select(Date, Transmitter)

summary2=summarise(detections2,
                   number.of.eels=n()
)

summary3=summarise(summary2,
                   number.of.eels=n()
)


df=merge(summary1, summary3, by="Date")

# Calculate departures per eel (dpe)
df$dpe = df$number.of.departures/df$number.of.eels


d = df%>%
  group_by(Month, dpe)%>%
  select(Month, dpe)

# Create boxplot with departures for each month
boxplot(dpe~Month, df, xlab = "Month", ylab = "Number of Departures per day", ylim=c(0, 8), cex.lab=1.25, cex.axis=1.25)

# Compute summary statistics
January=df[which(df$Month == "1"), ]
mean(January$dpe)
var(January$dpe)
sd(January$dpe)

February=df[which(df$Month == "2"), ]
mean(February$dpe)
var(February$dpe)
sd(February$dpe)

March=df[which(df$Month == "3"), ]
mean(March$dpe)
var(March$dpe)
sd(March$dpe)

April=df[which(df$Month == "4"), ]
mean(April$dpe)
var(April$dpe)
sd(April$dpe)

May=df[which(df$Month == "5"), ]
mean(May$dpe)
var(May$dpe)
sd(May$dpe)

June=df[which(df$Month == "6"), ]
mean(June$dpe)
var(June$dpe)
sd(June$dpe)

July=df[which(df$Month == "7"), ]
mean(July$dpe)
var(July$dpe)
sd(July$dpe)

August=df[which(df$Month == "8"), ]
mean(August$dpe)
var(August$dpe)
sd(August$dpe)

September=df[which(df$Month == "9"), ]
mean(September$dpe)
var(September$dpe)
sd(September$dpe)

October=df[which(df$Month == "10"), ]
mean(October$dpe)
var(October$dpe)
sd(October$dpe)

November=df[which(df$Month == "11"), ]
mean(November$dpe)
var(November$dpe)
sd(November$dpe)

December=df[which(df$Month == "12"), ]
mean(December$dpe)
var(December$dpe)
sd(December$dpe)

# => Large difference in variances!

# Apply one-way ANOVA
# Test normality
# Create histogram and qq plot
dev.off()
par(mfrow= c (1,2))
hist(df$dpe,  xlab = "Departures per eel")
qqnorm(df$dpe)
qqline(df$dpe)
shapiro.test(df$dpe)

# Test homogeneity of variances (if p-value < 0.05 no homogeneity)
bartlett.test(dpe ~ Month, data=df)
leveneTest(y=df$dpe, group=df$Month)

#The advantage of using the oneway.test() function is obviously the Welch correction for nonhomogeneity.
oneway.test(dpe ~ Month, data=df, var.equal=FALSE)

aov=aov(df$dpe ~ df$Month)
summary(aov)

# Overview assumptions normality and homogeneity
par(mfrow=c(1,2))     # set graphics window to plot side-by-side
plot(aov, 1)          # graphical test of homogeneity
plot(aov, 2)          # graphical test of normality
# A similar graphical test of residuals by groups can be had this way, provided there are no missing values in the data: 
boxplot(aov$resid ~ df$daynight)

# Transformation
df$tdpe=log(df$dpe)

# non-parametric Kruskall Wallis test as alternative for one-way ANOVA
kruskal.test(df$dpe ~ df$Month)

# Apply post-hoc test
posthoc.kruskal.nemenyi.test(dpe ~ Month, data=df, dist="Chisquare")
posthoc.kruskal.dunn.test(x=df$dpe, g=df$Month, p.adjust.method="bonferroni")
posthoc.kruskal.conover.test(x=df$dpe, g=df$Month, p.adjust.method="none")



#====== Swim speed

# Select swim distance per month
detections = dataH%>%
  group_by(Date,swimdistance, Month)%>%
  select(Date, Transmitter, swimdistance, Month)

# Only take into account swim distances not equal to zero
df <- detections[which(detections$swimdistance != "0"), ]

# Sum swim distance per eel per month
detections2 = df%>%
  group_by(Transmitter, Date, Month)%>%
  select(Transmitter,Date,Month,swimdistance)

df=summarise(detections2,
             sum=sum(swimdistance))


par(mar=c(6,4.1,4.1,2.1))
barplot(df$sum, names.arg=df$Month, cex.names=0.8, ylim=c(0,15000),las=1)


# Create boxplot with swim distance for each month
df$Month<-factor(df$Month)
boxplot(sum~Month, df, xlab = "Month", ylab = "Swim distance per month (m)", ylim=c(0, 12000), cex.lab=1.25, cex.axis=1.25)


# Compute summary statistics
January=df[which(df$Month == "1"), ]
mean(January$sum)
var(January$sum)
sd(January$sum)

February=df[which(df$Month == "2"), ]
mean(February$sum)
var(February$sum)
sd(February$sum)

March=df[which(df$Month == "3"), ]
mean(March$sum)
var(March$sum)
sd(March$sum)

April=df[which(df$Month == "4"), ]
mean(April$sum)
var(April$sum)
sd(April$sum)

May=df[which(df$Month == "5"), ]
mean(May$sum)
var(May$sum)
sd(May$sum)

June=df[which(df$Month == "6"), ]
mean(June$sum)
var(June$sum)
sd(June$sum)

July=df[which(df$Month == "7"), ]
mean(July$sum)
var(July$sum)
sd(July$sum)

August=df[which(df$Month == "8"), ]
mean(August$sum)
var(August$sum)
sd(August$sum)

September=df[which(df$Month == "9"), ]
mean(September$sum)
var(September$sum)
sd(September$sum)

October=df[which(df$Month == "10"), ]
mean(October$sum)
var(October$sum)
sd(October$sum)

November=df[which(df$Month == "11"), ]
mean(November$sum)
var(November$sum)
sd(November$sum)

December=df[which(df$Month == "12"), ]
mean(December$sum)
var(December$sum)
sd(December$sum)

# => Large difference in variances!




# Apply one-way ANOVA
# Test normality
# Create histogram and qq plot
dev.off()
par(mfrow= c (1,2))
hist(df$sum,  xlab = "Swim speed (m/h)")
qqnorm(df$sum)
qqline(df$sum)
shapiro.test(df$sum)

# Test homogeneity of variances (if p-value < 0.05 no homogeneity)
bartlett.test(swimdistance ~ Month, data=df)
leveneTest(y=df$swimdistance, group=df$Month)

#The advantage of using the oneway.test() function is obviously the Welch correction for nonhomogeneity.
oneway.test(sum ~ Month, data=df, var.equal=FALSE)

aov=aov(df$tsum ~ df$Month)
summary(aov)

# Overview assumptions normality and homogeneity
par(mfrow=c(1,2))     # set graphics window to plot side-by-side
plot(aov, 1)          # graphical test of homogeneity
plot(aov, 2)          # graphical test of normality


# non-parametric Kruskall Wallis test as alternative for one-way ANOVA
kw=kruskal.test(df$tsum ~ df$Month)
kw

posthoc.kruskal.dunn.test(x=df$tsum, g=df$Month, p.adjust.method="bonferroni")


# Transformation
df$tsum=log(df$sum)



# Individuals per month
ind.per.month=df[which(df$Month == "12"), ]
ind.per.month$Transmitter = factor(ind.per.month$Transmitter)
levels(ind.per.month$Transmitter)



# Create figure with temperature per month
# Add month 
temp$Month=month(temp$Timestamp)
temp$Month = as.factor(temp$Month)
ggplot(temp, aes(x=Month, y=Value)) + geom_point() +
  stat_summary(aes(y = Value,group=1), fun.y=mean, colour="red", geom="line",group=1)




# ------------------------------
# COMBINED PLOT:
# ------------------------------

library(ggplot2)

# OPTION 1: COMBINE 2 GGPLOT GRAPHS
#----------------------------------
temp$Month=month(temp$Timestamp)
temp$Month = as.factor(temp$Month)


#precalculate monthly averages:
month.avg <- aggregate(temp$Value, list(as.integer(temp$Month)), mean)


bxplot <- ggplot(data = df, aes(x = Month, sum)) +
  geom_boxplot() +
  theme_minimal() +
  ylab("Swim distance per month (m)") +
  theme(axis.title.y = element_text(margin = margin(r = 15))) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16))

tmplot <- ggplot(data = temp, aes(x = Month, y = Value)) +
  geom_point(colour = "grey", alpha = 0.4) + # all dots (shows the spread)
  stat_summary(fun.y = mean, colour = "red", geom = "point", group = 1,
               size = 4) +
  theme_minimal() +
  ylab(expression("Temperature ("*~degree*C*")")) +
  theme(axis.title.y = element_text(margin = margin(r = 15))) +
  theme(axis.text.y = element_text(margin = margin(r = 14.5))) +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16))

library(gridExtra)
grid.arrange(bxplot, tmplot, nrow = 2)
# The results is according to ggplot rules (no 2 variables on axis, cfr. :
# http://stackoverflow.com/questions/3099219/plot-with-2-y-axes-one-y-axis-on-the-left-and-another-y-axis-on-the-right)
# however, the alignment of the two axis is rather hacky...


# OPTION 2: OVERLAY
#-----------------------
#precalculate monthly averages:
month.avg <- aggregate(temp$Value, list(as.integer(temp$Month)), mean)

# DISTANCES
par(mar = c(5, 5, 2, 5))
with(df, boxplot(sum ~ Month, xlab = "Month",
                 ylab = "Swim distance per month (m)",
                 ylim = c(0, 12000),
                 cex.lab = 1.25,
                 cex.axis = 1.25))

# TEMPERATURE
par(new = TRUE)
with(month.avg, plot(x, axes = FALSE,
                     xlab = NA, ylab = NA, bty = "n",
                     pch = 15, cex = 1.25,
                     cex.lab = 1.25, cex.axis = 1.25,
                     type = "p", col = "red",
                     ylim = c(0, 25),
                     xlim = range(Group.1) + c(-0.5, 0.5)))
# http://stackoverflow.com/questions/24554631/combining-a-box-plot-with-a-dot-plot-using-different-y-scales
axis(side = 4, at = pretty(range(month.avg$x)))
mtext(side = 4, line = 3, col = "red",
      expression("Temperature ("*~degree*C*")"))





# Select swim distance together with the abiotic variables.

# Select swim distance and environmental variables per month
detections = dataH%>%
  group_by(Date,swimdistance, Month)%>%
  select(Date, Transmitter, swimdistance, Month,precipitation,Temp,daylength,pump,pressure)

# Only take into account swim distances not equal to zero
df <- detections[which(detections$swimdistance != "0"), ]

# Sum swim distance per eel per month
detections2 = df%>%
  group_by(Transmitter, Date, Month)%>%
  select(Transmitter,Date,Month,swimdistance,precipitation,Temp,daylength,pump,pressure)

df=summarise(detections2,
             precipitation=mean(precipitation),
             Temp=mean(Temp),
             daylength=mean(daylength),
             pump=mean(pump),
             pressure=mean(pressure),
             sum=sum(swimdistance))

# Create boxplot with swim distance for each month
df$Month<-factor(df$Month)


glm = glm(sqrt(sum) ~ precipitation+Temp+daylength+pump+pressure, family=gaussian(identity), data=df)
summary(glm)
par(mar = c(4, 10, 2, 3))
par(mfrow = c(2, 2))
plot(glm)






































#####################################
# Rest
#=================================================

# Create dataframe with total residence time to analyse site fidelity
time <- V(g.intervals)$total_time
p <- as.data.frame(paste(V(g.intervals)$name, V(g.intervals)$total_time))

colnames(p)[1]= "stationtime"
p$stations=lapply(strsplit(as.character(p$stationtime), "\\ "), "[", 1)
p$time=lapply(strsplit(as.character(p$stationtime), " "), "[", 2)
p$stationtime=NULL
p

# Calculate relative residence time (RRT = residence time / total tracking time)
RT = dataH %>%
  group_by(Transmitter)%>%
  select(Transmitter, seconds)
RT = distinct(select(test, Transmitter, seconds))
RT = filter(RT, Transmitter == 'A69-1601-31897')

p$RRT=as.numeric(p$time)/as.numeric(RT$seconds)
p

# Put RRT in dataframe
residence=as.data.frame(unique(dataH$Station.Name))
colnames(residence)[1]="stations"
residence$'31897'=NA

for (i in 1:dim(residence)[1]){
  b<-residence$stations[i]
  index<-which(b==p$stations)
  if (length(index) > 0) {
    if (is.na(p$stations[index])==FALSE) {
      residence$'31897'[i]=p$RRT[which(b==p$stations)]
    }
    else {
      residence$stations[i]=NA
    }
  }}


#==============================================
# Check normality
dev.off()
# Create histogram and qq plot
par(mfrow= c (1,2))
hist(DataHdistance$distance,  xlab = "Distance (m)")
qqnorm(DataHdistance$distance)

# Conduct linear regression to test for relationships between range distance and Catch location + Length + Weight + Stadium
lm <- lm(distance ~ Catch_location + Length + Weight + Stadium, data = DataHdistance)
summary(lm)

par(mar = c(4, 10, 2, 3))
par(mfrow = c(2, 2))
plot(lm)

lm <- lm(sqrt(distance) ~ Catch_location, data = DataHdistance)
summary(lm)

par(mar = c(4, 10, 2, 3))
par(mfrow = c(2, 2))
plot(lm)
anova(lm)
confint(lm)

# randomizing to see if the patterns are different from expected
modmat <- model.matrix(~Catch_location + Length + Weight + Stadium, data = DataHdistance)
mus <- modmat %*% coef(lm)
set.seed(1246)
# the position of the real plot in a 3x3 panel
s <- sample(1:9, size = 1)

par(mfrow = c(3, 3))
for (i in 1:9) {
  if (i == s) {
    # the real plot
    qqnorm(resid(lm))
    qqline(resid(lm))
  } else {
    # draw new y values from the fitted values with the residuals standard
    # deviation
    y <- rnorm(dim(DataHdistance)[1], mus, sd(resid(lm)))
    y <- y - fitted(lm)
    qqnorm(y)
    qqline(y)
  }
  
}


####### Playing with normality
par(mfrow= c (1,2))
hist(DataHdistance$distance,  xlab = "Distance (m)")
qqnorm(DataHdistance$distance)

shapiro.test(DataHdistance$distance)

lm <- lm(log(distance) ~ Catch_location + Length + Weight + Stadium, data = DataHdistance)
summary(lm)
anova(lm)
par(mar = c(4, 10, 2, 3))
par(mfrow = c(2, 2))
plot(lm)
a1=aov(distance ~ Catch_location, data=DataHdistance)
summary(a1)


kruskal.test(distance ~ Catch_location, data=DataHdistance)

#### Homogeneity of variance
plot(DataHdistance$distance, ylab = "Distance (m)")
bartlett.test(distance ~ Catch_location, data=DataHdistance)
leveneTest(distance ~ Catch_location, data=DataHdistance)

# Check normality within groups (i.e. categories)
DataHdistancecat=DataHdistance[which(DataHdistance$Catch_location == "4"), ]

par(mfrow= c (1,2))
hist(DataHdistancecat$distance,  xlab = "Distance (m)")
qqnorm(DataHdistancecat$distance)

shapiro.test(DataHdistancecat$distance)

#########################
# GLM

glm = glm(swimdistance ~ pump + precipitation + Temp + daylength + pressure , data=dataH)
summary(glm)
par(mar = c(4, 10, 2, 3))
par(mfrow = c(2, 2))
plot(glm)


DF=dataH

write.csv(DF, "DF.csv")
DF<-"DF.csv" 
DF<-read.csv(DF, header=TRUE, stringsAsFactors=FALSE)

par(mfrow= c (1,2))
hist(DF$swimdistance,  xlab = "Distance (m)")
qqnorm(DF$swimdistance)

shapiro.test(DF$swimdistance)

################################################
# Random Forests
set.seed(500) #what's this?

test=dataH
test=na.omit(test)
sum(is.na(test))

fit = randomForest(Departurenum ~ pump + precipitation + Temp + daylength + pressure , data=test, importance = TRUE, ntree=2000)
varImpPlot(fit)
importance(fit)


test=data2
test=na.omit(test)
sum(is.na(test))

fit = randomForest(movement ~ pump + precipitation + Temp + daylength + pressure , data=test, importance = TRUE, ntree=2000)
varImpPlot(fit)
importance(fit)

#################################################
# Model on large movements
# Select 'large movement events'
dataHlm=dataH[which(dataH$movement == "1"), ]

# Create histogram and qq plot
par(mfrow= c (1,2))
hist(dataHlm$swimdistance,  xlab = "Swim distance (m)")
qqnorm(dataHlm$swimdistance)

glm60 = glm(swimdistance ~ precipitation + Temp + weir + daynight + Month, family=Gamma(link="log"), data=dataHlm)
summary(glm60)
par(mar = c(4, 10, 2, 3))
par(mfrow = c(2, 2))
plot(glm60)
drop1(lm)

boxplot(swimdistance~Month, data=dataHlm, xlab='Month', ylab='Distance (m)')
range(dataHlm$swimdistance)       





#==============================
# Find best model
#==============================

#######################################
# Apply regression
#######################################

# Create GLM without strongly correlated variables
glm2000 = glm(movement ~ precipitation + Temp + pressure + daylength + pump + Weight + fweir + floc, family=binomial(logit), data=dataH)
summary(glm2000)
par(mar = c(4, 10, 2, 3))
par(mfrow = c(2, 2))
plot(glm2000)

step(glm1)
drop1(glm1, test="Chi")
anova(glm1)

# Backward selection
glm2 = glm(movement ~ Temp + pressure + pump + Weight + fweir + floc, family=binomial(logit), data=dataH)
glm3 = glm(movement ~ precipitation + pressure + pump + Weight + fweir + floc, family=binomial(logit), data=dataH)
glm4 = glm(movement ~ precipitation + Temp + pump + Weight + fweir + floc, family=binomial(logit), data=dataH)
glm5 = glm(movement ~ precipitation + Temp + pressure + Weight + fweir + floc, family=binomial(logit), data=dataH)
glm6 = glm(movement ~ precipitation + Temp + pressure + pump + fweir + floc, family=binomial(logit), data=dataH)
glm7 = glm(movement ~ precipitation + Temp + pressure + pump + Weight + floc, family=binomial(logit), data=dataH)
glm8 = glm(movement ~ precipitation + Temp + pressure + pump + Weight + fweir, family=binomial(logit), data=dataH)

summary(glm2)
summary(glm3)
summary(glm4)
summary(glm5)
summary(glm6)
summary(glm7)
summary(glm8)


# glm3 has lowest AIC: 8178.7
glm10 = glm(movement ~ pressure + pump + Weight + fweir + floc, family=binomial(logit), data=dataH)
glm11 = glm(movement ~ precipitation + pump + Weight + fweir + floc, family=binomial(logit), data=dataH)
glm12 = glm(movement ~ precipitation + pressure + Weight + fweir + floc, family=binomial(logit), data=dataH)
glm13 = glm(movement ~ precipitation + pressure + pump + fweir + floc, family=binomial(logit), data=dataH)
glm14 = glm(movement ~ precipitation + pressure + pump + Weight + floc, family=binomial(logit), data=dataH)
glm15 = glm(movement ~ precipitation + pressure + pump + Weight + fweir, family=binomial(logit), data=dataH)


summary(glm10)
summary(glm11)
summary(glm12)
summary(glm13)
summary(glm14)
summary(glm15)

# glm10 has lowest AIC: 8177.5
glm20 = glm(movement ~ pump + Weight + fweir + floc, family=binomial(logit), data=dataH)
glm21 = glm(movement ~ pressure + Weight + fweir + floc, family=binomial(logit), data=dataH)
glm22 = glm(movement ~ pressure + pump + fweir + floc, family=binomial(logit), data=dataH)
glm23 = glm(movement ~ pressure + pump + Weight + floc, family=binomial(logit), data=dataH)
glm24 = glm(movement ~ pressure + pump + Weight + fweir, family=binomial(logit), data=dataH)

summary(glm20)
summary(glm21)
summary(glm22)
summary(glm23)
summary(glm24)

# glm20 has lowest AIC: 8176.9
glm30 = glm(movement ~ Weight + fweir + floc, family=binomial(logit), data=dataH)
glm31 = glm(movement ~ pump + fweir + floc, family=binomial(logit), data=dataH)
glm32 = glm(movement ~ pump + Weight + floc, family=binomial(logit), data=dataH)
glm33 = glm(movement ~ pump + Weight + fweir, family=binomial(logit), data=dataH)

summary(glm30)
summary(glm31)
summary(glm32)
summary(glm33)

# AIC does not lower anymore
# glm20 is best model
glm20 = glm(movement ~ pump + Weight + fweir + floc, family=binomial(logit), data=dataH)
summary(glm20)
drop1(glm20, test='Chi')
par(mar = c(4, 10, 2, 3))
par(mfrow = c(2, 2))
plot(glm20)
# Variable 'pump' is not significant in the final model, so new model without pump
glm40 = glm(movement ~ Weight + fweir + floc, family=binomial(logit), data=dataH)
summary(glm40)     # AIC rises with only 0.1
drop1(glm40, test='Chi')
par(mar = c(4, 10, 2, 3))
par(mfrow = c(2, 2))
plot(glm40)
# Apply post hoc test on categorical variables
summary(glht(glm40, mcp(fweir="Tukey")))
summary(glht(glm40, mcp(floc="Tukey")))
# * Are we interested in the variable month??

# This should plot predicted values
plot(data2$Temp, data2$movement,
     xlab = "Temperature", ylab = "Probability of \
     movement", main = "Night data in August")
I1 <- order(data2$Temp)
Alllocs <- unique(data2$floc)
for (j in Alllocs){
  mydata <- data.frame(
    Temp = data2$Temp,
    fmonth = "8",
    fdaynight="Night",
    floc = Alllocs[j])
  n <- dim(mydata)[1]
  if (n > 10){
    glm100 <- predict(glm11, mydata,
                      type = "response")
    lines(mydata$Temp[I1], glm100[I1])}}

# This works
plot(dataH$Temp, data2$movement,
     xlab = "Temperature", ylab = "Probability of \
     movement", main = "Night data in August for Polder")
I1 <- order(dataH$Temp)
for (j in Alllocs){
  mydata <- data.frame(
    Temp = dataH$Temp,
    fmonth = "8",
    floc = "Polder",
    fdaynight="Night") 
  n <- dim(mydata)[1]
  if (n > 10){
    glm100 <- predict(glm11, mydata,
                      type = "response")
    lines(mydata$Temp[I1], glm100[I1])}}






# Create GAM without strongly correlated variables
gam1 = gam(movement ~ s(precipitation) + s(Temp) + s(pressure) + s(pump) + s(Weight) + fweir + floc, family=binomial(logit), data=dataH)
summary(gam1)
anova(gam1)
AIC(gam1)
plot(gam1)


# Backward selection
gam2 = gam(movement ~ s(Temp) + s(pressure) + s(pump) + s(Weight) + fweir + floc, family=binomial(logit), data=dataH)
gam3 = gam(movement ~ s(precipitation) + s(pressure) + s(pump) + s(Weight) + fweir + floc, family=binomial(logit), data=dataH)
gam4 = gam(movement ~ s(precipitation) + s(Temp) + s(pump) + s(Weight) + fweir + floc, family=binomial(logit), data=dataH)
gam5 = gam(movement ~ s(precipitation) + s(Temp) + s(pressure) + s(Weight) + fweir + floc, family=binomial(logit), data=dataH)
gam6 = gam(movement ~ s(precipitation) + s(Temp) + s(pressure) + s(pump) + fweir + floc, family=binomial(logit), data=dataH)
gam7 = gam(movement ~ s(precipitation) + s(Temp) + s(pressure) + s(pump) + s(Weight) + floc, family=binomial(logit), data=dataH)
gam8 = gam(movement ~ s(precipitation) + s(Temp) + s(pressure) + s(pump) + s(Weight) + fweir, family=binomial(logit), data=dataH)

AIC(gam2)
AIC(gam3)
AIC(gam4)
AIC(gam5)
AIC(gam6)
AIC(gam7)
AIC(gam8)

# gam2 has lowest AIC: 7880.578
gam10 = gam(movement ~ s(pressure) + s(pump) + s(Weight) + fweir + floc, family=binomial(logit), data=dataH)
gam11 = gam(movement ~ s(Temp) + s(pump) + s(Weight) + fweir + floc, family=binomial(logit), data=dataH)
gam12 = gam(movement ~ s(Temp) + s(pressure) + s(Weight) + fweir + floc, family=binomial(logit), data=dataH)
gam13 = gam(movement ~ s(Temp) + s(pressure) + s(pump) + fweir + floc, family=binomial(logit), data=dataH)
gam14 = gam(movement ~ s(Temp) + s(pressure) + s(pump) + s(Weight) + floc, family=binomial(logit), data=dataH)
gam15 = gam(movement ~ s(Temp) + s(pressure) + s(pump) + s(Weight) + fweir, family=binomial(logit), data=dataH)

AIC(gam10)
AIC(gam11)
AIC(gam12)
AIC(gam13)
AIC(gam14)
AIC(gam15)

# AIC does not lower anymore
# gam2 is best model
gam2 = gam(movement ~ s(Temp) + s(pressure) + s(pump) + s(Weight) + fweir + floc, family=binomial(logit), data=dataH)
AIC(gam2)
summary(gam2)
anova(gam2)
par(mar = c(4, 10, 2, 3))
par(mfrow = c(2, 2))
plot(gam2)
# Variables 'pressure' and 'pump' are not significant in the final model, so new model without pressure and pump
gam40 = gam(movement ~ s(Temp) + s(pump) + s(Weight) + fweir + floc, family=binomial(logit), data=dataH)
AIC(gam40)
summary(gam40)
anova(gam40)

gam41 = gam(movement ~ s(Temp) + s(Weight) + fweir + floc, family=binomial(logit), data=dataH)
AIC(gam41)
summary(gam41)
anova(gam41)
plot(gam41)

# Apply analysis of deviance to compare the two models, by using the anova() command
anova(glm40, gam41, test='Chi')




#######################################
# Create GEE with fstadium
gee = geeglm(movement ~ precipitation + Temp + fweir + fmonth + fdaynight + floc, family=binomial(logit), data=data2, id = fstadium, corstr = "exchangeable")
#+ fweir + fmonth + fdaynight + floc
gee2 = geeglm(movement ~ Temp + fmonth + fdaynight, family=binomial(logit), data=data2, id = floc, corstr = "exchangeable")
summary(gee2)








################################################################
################################################################
# Play with random and fixed effects

# Apply regression per year
data2=dataH[which(dataH$Year == "2015"), ]

# Apply regression per stadium
data2=dataH[which(dataH$Stadium == "FIII"), ]


ymt.model= glmer(movement ~ precipitation + Temp + pressure + daylength + pump + Weight + fweir + floc + (1|fyear) + (1|fmonth) + (1|ftag), family=binomial(logit), data=dataH)
summary(ymt.model)
sem.model.fits(ymt.model)

#################################################################
#################################################################



#################################################################
#################################################################
# GLMM per individual
unique(dataH$Transmitter)
dataHind=dataH[which(dataH$Transmitter == "A69-1601-29931"), ]
glmmind = glmer(movement ~ precipitation + Temp + pressure + daylength + pump + fweir + (1|fyear) + (1|fmonth), family=binomial(logit), data=dataHind)
summary(glmmind)
modlist = list(
  glmer(movement ~ precipitation + Temp + pressure + daylength + pump + fweir + (1|fyear) + (1|fmonth), family=binomial(logit), data=dataHind)
)
sem.model.fits(modlist)

glm = glm(movement ~ precipitation + Temp + pressure + daylength + pump + fweir, family=binomial(logit), data=dataHind)
summary(glm)
drop1(glm)
anova(glm)


#################################################################
#################################################################









# Plot model
g <- -0.7269602  - 0.0065129 * dataH$precipitation -0.0057027 * dataH$Temp - 0.85506 * data2$floc
p.averagemonth <- exp(g) / (1 + exp(g))
I1 <- order(data2$Temp) #Avoid spaghetti plot
plot(data2$Temp, data2$movement,
     ylab = "Probability of movement",
     xlab = "Temperature")
lines(data2$Temp[I1],p.averagemonth[I1],lwd=3)
p.Upp<-exp(g+1.96*0.716)/(1+exp(g+1.96*0.716))
p.Low<-exp(g-1.96*0.716)/(1+exp(g-1.96*0.716))
lines(data2$Temp[I1], p.Upp[I1])
lines(data2$Temp[I1], p.Low[I1])



#  Fit the best model with a random effect of months across years
ymt2 = glmer(movement ~ precipitation + Temp + pressure + daylength + pump + fweir + floc + (fyear|fmonth) + (1|ftag), family=binomial(logit), data=dataH)
summary(ymt2)

# Check fitting of the model
fixef(ymt2)
VarCorr(ymt2)   # -> Strong correlations -> overfitting of the model


p1 <- plot(ymt,id=0.05,idLabels=~.obs)
p2 <- plot(ymt,ylim=c(-1.5,3),type=c("p","smooth"))
grid.arrange(p1,p2,nrow=1)

drop1(ymt2, test="Chi")










# Create GAMM with only uncorrelated variables and fyear, fmonth and fstadium as random factors
gamm = gamm(movement ~ s(precipitation) + s(Temp) + s(pressure) + s(pump) + s(Weight) + fweir + floc, random = list(fyear=~1, fmonth =~ 1, fstadium=~1), family=binomial(logit), data=dataH)
summary(gamm)
summary(gamm$gam, cor = TRUE)
plot(gamm$gam)
vis.gam(gamm$gam)
summary(gamm$lme)
plot(gamm$lme)

gamm = gamm(movement ~ s(precipitation) + s(Temp) + s(Weight), random = list(fyear=~1, fmonth =~ 1, fstadium=~1), family=binomial(logit), data=dataH)
summary(gamm)
AIC(gamm$gam)

# Get residuals
E <- resid(gamm$lme, type = "normalized")











# Linear regression model
lm = lm(movement ~ pump + precipitation + Temp + daylength + pressure + weir + Length + Weight + Stadium, data=data2)
summary(lm)
par(mar = c(4, 10, 2, 3))
par(mfrow = c(2, 2))
plot(lm)

# Logit (logistic) regression model
plot(movement ~ Temp, data=data2)

glm = glm(movement ~ pump + precipitation + Temp + daylength + pressure + weir + Length + Weight + Stadium + Month + daynight, family=binomial(logit), data=data2)
summary(glm)
par(mar = c(4, 10, 2, 3))
par(mfrow = c(2, 2))
plot(glm)

# Test for an overall effect of categorical variable
# Iff p-value < 0.05, than the overall effect of the variable is statistically significant
wald.test(b=coef(glm), Sigma=vcov(glm), Terms=10:11)

# We can test factors' effects with the anova function to give an analysis of deviance table.
anova(glm)

# Use drop function
drop1(glm, test="Chi")

# Apply backwards selection
step(glm)

# Use the confint function to obtain confidence intervals for the coefficient estimates
confint(glm)

# How well our model fits depends on the difference between the model and the observed data.  
# One approach for binary data is to implement a Hosmer Lemeshow goodness of fit test.
hoslem.test(data2$movement, fitted(glm))
# Model fits well if there is no significant difference (p-value > 0.05) between model and observed data


# Plot relationship of 1 explanatory variable vs response variable
model = glm(movement ~  Temp, family=binomial, data=data2)
summary(model)
# To plot our model we need a range of values of Temp for which to produce fitted values. 
# This range of values we can establish from the actual range of values of Temp.
range(data2$Temp)
xTemp <- seq(0, 25, 0.01)  # 0 and 25 are based on min and max of range, 0.01 is the increment
# Now we use the predict() function to create the model for all of the values of xTemp.
yTemp <- predict(model, list(Temp = xTemp),type="response")
# Now plot
plot(data2$Temp, data2$movement, pch = 16, xlab = "TEMPERATURE (?c)", ylab = "MOVEMENT")
lines(xTemp, yTemp)



# Model selection
# GLM
# First stepwise backward selection based on AIC
glm = glm(movement ~ pump + precipitation + Temp + daylength + pressure + weir + daynight, family=binomial(logit), data=data2)
summary(glm)
par(mar = c(4, 10, 2, 3))
par(mfrow = c(2, 2))
plot(glm)
step(glm)
glm2 = glm(movement ~ Temp + daylength + weir + daynight, family=binomial(logit), data=data2)
summary(glm2)
# Then hypothesis testing: remove variables with p-values < 0.05
glm3 = glm(movement ~ Temp + daylength + weir, family=binomial(logit), data=data2)
summary(glm3)
glm4 = glm(movement ~ daylength + weir, family=binomial(logit), data=data2)
summary(glm4)
glm5 = glm(movement ~ weir, family=binomial(logit), data=data2)
summary(glm5)


# GAM
gam = gam(movement ~ s(pump) + s(precipitation) + s(Temp) + s(daylength) + s(pressure) + factor(weir) + factor(daynight), family=binomial(logit), data=data2)
summary(gam)
anova(gam)
AIC(gam)

gam1 = gam(movement ~ s(precipitation) + s(Temp) + s(daylength) + s(pressure) + weir + daynight, family=binomial(logit), data=data2)
gam2 = gam(movement ~ s(pump) + s(Temp) + s(daylength) + s(pressure) + weir + daynight, family=binomial(logit), data=data2)
gam3 = gam(movement ~ s(pump) + s(precipitation) + s(daylength) + s(pressure) + weir + daynight, family=binomial(logit), data=data2)
gam4 = gam(movement ~ s(pump) + s(precipitation) + s(Temp) + s(pressure) + weir + daynight, family=binomial(logit), data=data2)
gam5 = gam(movement ~ s(pump) + s(precipitation) + s(Temp) + s(daylength) + weir + daynight, family=binomial(logit), data=data2)
gam6 = gam(movement ~ s(pump) + s(precipitation) + s(Temp) + s(daylength) + s(pressure) + daynight, family=binomial(logit), data=data2)
gam7 = gam(movement ~ s(pump) + s(precipitation) + s(Temp) + s(daylength) + s(pressure) + weir, family=binomial(logit), data=data2)

AIC(gam1)
AIC(gam2)
AIC(gam3)
AIC(gam4)
AIC(gam5)
AIC(gam6)
AIC(gam7)

# GAM5 has lowest AIC (model without pressure)
gam5 = gam(movement ~ s(pump) + s(precipitation) + s(Temp) + s(daylength) + weir + daynight, family=binomial(logit), data=data2)
AIC(gam5)
gam10 = gam(movement ~ s(precipitation) + s(Temp) + s(daylength) + weir + daynight, family=binomial(logit), data=data2)
gam11 = gam(movement ~ s(pump) + s(Temp) + s(daylength) + weir + daynight, family=binomial(logit), data=data2)
gam12 = gam(movement ~ s(pump) + s(precipitation) + s(daylength) + weir + daynight, family=binomial(logit), data=data2)
gam13 = gam(movement ~ s(pump) + s(precipitation) + s(Temp) + weir + daynight, family=binomial(logit), data=data2)
gam14 = gam(movement ~ s(pump) + s(precipitation) + s(Temp) + s(daylength) + daynight, family=binomial(logit), data=data2)
gam15 = gam(movement ~ s(pump) + s(precipitation) + s(Temp) + s(daylength) + weir, family=binomial(logit), data=data2)

AIC(gam10)
AIC(gam11)
AIC(gam12)
AIC(gam13)
AIC(gam14)
AIC(gam15)

# GAM11 has lowest AIC (model without precipitation)
gam11 = gam(movement ~ s(pump) + s(Temp) + s(daylength) + weir + daynight, family=binomial(logit), data=data2)
AIC(gam11)

gam20 = gam(movement ~ s(Temp) + s(daylength) + weir + daynight, family=binomial(logit), data=data2)
gam21 = gam(movement ~ s(pump) + s(daylength) + weir + daynight, family=binomial(logit), data=data2)
gam22 = gam(movement ~ s(pump) + s(Temp) + weir + daynight, family=binomial(logit), data=data2)
gam23 = gam(movement ~ s(pump) + s(Temp) + s(daylength) + daynight, family=binomial(logit), data=data2)
gam24 = gam(movement ~ s(pump) + s(Temp) + s(daylength) + weir , family=binomial(logit), data=data2)

AIC(gam20)
AIC(gam21)
AIC(gam22)
AIC(gam23)
AIC(gam24)

# GAM23 has lowest AIC (model without weir)
gam23 = gam(movement ~ s(pump) + s(Temp) + s(daylength) + daynight, family=binomial(logit), data=data2)
AIC(gam23)

gam30 = gam(movement ~ s(Temp) + s(daylength) + daynight, family=binomial(logit), data=data2)
gam31 = gam(movement ~ s(pump) + s(daylength) + daynight, family=binomial(logit), data=data2)
gam32 = gam(movement ~ s(pump) + s(Temp) + daynight, family=binomial(logit), data=data2)
gam33 = gam(movement ~ s(pump) + s(Temp) + s(daylength), family=binomial(logit), data=data2)

AIC(gam30)
AIC(gam31)
AIC(gam32)
AIC(gam33)
# AIC rises again, so best model is gam23
summary(gam23)
# Then hypothesis testing: remove variables with p-values < 0.05
gam40 = gam(movement ~ s(pump) + s(Temp) + s(daylength), family=binomial(logit), data=data2)
AIC(gam40)
summary(gam40)
gam41 = gam(movement ~ s(Temp) + s(daylength), family=binomial(logit), data=data2)
summary(gam41)
AIC(gam41)
plot(gam41)



# GLM model validation
glm = glm(movement ~ pump + precipitation + Temp + daylength + pressure + weir + daynight, family=binomial(logit), data=data2)
summary(glm)
par(mar = c(4, 10, 2, 3))
par(mfrow = c(2, 2))
plot(glm)    #standard graphical output
#Check for normality
E <- rstandard(glm)
hist(E)
qqnorm(E)
#Check for independence and homogeneity: residuals
#versus individual explanatory variables
data3 = data2[-1,]   # Remove first row of data2, so x and y have equal length
plot(y = E, x = data3$pump, xlab = "Explanatory variable", ylab = "Residuals")
abline(0,0)
plot(E ~ data3$precipitation, xlab = "Explanatory variable",ylab = "Residuals")
abline(0, 0)
par(op)


# GAM model validation
# Plot residuals versus fitted values
E.GAM41 <- resid(gam41)
Fit.GAM41 <- fitted(gam41)
plot(x = Fit.GAM41, y = E.GAM41, xlab = "Fitted values",
     ylab = "Residuals")

# Plot residuals versus each explanatory variable
plot(x = data2$Temp, y = E.GAM41, xlab = "Explanatory variable",
     ylab = "Residuals")
plot(x = data2$daylength, y = E.GAM41, xlab = "Explanatory variable",
     ylab = "Residuals")
plot(x = data2$pump, y = E.GAM41, xlab = "Explanatory variable",
     ylab = "Residuals")
plot(x = data2$precipitation, y = E.GAM41, xlab = "Explanatory variable",
     ylab = "Residuals")
plot(x = data2$pressure, y = E.GAM41, xlab = "Explanatory variable",
     ylab = "Residuals")
plot(x = data2$weir, y = E.GAM41, xlab = "Explanatory variable",
     ylab = "Residuals")
plot(x = data2$daynight, y = E.GAM41, xlab = "Explanatory variable",
     ylab = "Residuals")

# Final selected glm and gam have different explenatory variables
# However, to still test if there is a significant difference between the model,
# we apply an anova on the full models
anova(glm, gam, test ="F")

anova(gam23, gam41, test="F")


par(mar = c(2, 2, 2, 2))
vis.gam(gam23, theta = 120, color = "heat")







#================================================
# Work with real swim distance
# Select swim distance >300 (measurement error = receiver detection range = 300)
dataH$movement <- NA

for (i in 1:dim(dataH)[1]){
  if (dataH$swimdistance[i] <= 300) {
    dataH$movement[i]="0"
  }
  else {
    dataH$movement[i]="1"
  }
}

# weir, stadium, Year, month and daynight are factors
dataH$fweir <- factor(dataH$weir)
dataH$fstadium <- factor(dataH$Stadium)
dataH$fmonth <- factor(dataH$Month)
dataH$fyear <- factor(dataH$Year)
dataH$fdaynight <- factor(dataH$daynight)
dataH$floc <- factor(dataH$Catch_location_type)

# Movement as numeric
dataH$movement <- as.numeric(dataH$movement)

# Create subset with only swim distances >= 300
table(dataH$movement)  # So you should get dataframe with equal number of rows as there are 1s
dataH2=dataH[which(dataH$movement == 1), ]
boxplot(dataH2$swimdistance)
range(dataH2$swimdistance)

# Create plots between continuous responese variable 'swim distance' and each explanatory variable
par(mar = c(3, 6, 0, 3))
par(mfrow = c(4, 2))

plot(precipitation~swimdistance, dataH2, xlab = "Movement", ylab = "Precipitation")
plot(Temp~swimdistance, dataH2, xlab = "Movement", ylab = "Temperature")
plot(daylength~swimdistance, dataH2, xlab = "Movement", ylab = "Daylength")
plot(pump~swimdistance, dataH2, xlab = "Movement", ylab = "Discharge")
plot(pressure~swimdistance, dataH2, xlab = "Movement", ylab = "Air pressure")
plot(Length~swimdistance, dataH2, xlab = "Movement", ylab = "Length")
plot(Weight~swimdistance, dataH2, xlab = "Movement", ylab = "Weight")


#===========================================================================
# Collinearity
# Take environmental variables and put them in new data frame datavars
datavars=dataH2[,c("precipitation","Temp","pump","daylength","pressure","Length","Weight")]
#character wordt numeric
i <- sapply(datavars, is.character)
datavars[i] <- lapply(datavars[i], as.numeric)
#dim(datavars)
#head(datavars)
#summary(datavars)
#str(datavars)
#Create two matrices, one in which the correlations will be pasted and one for the significane levels --> variables = variables you want to calculate the correlation for (to define)
#Cree?r een matrix met in zowel rijen als kolommen de variablen (symmetrische matrix), dus beide 16
correlation <- matrix(data = NA, nrow = 7, ncol = 7)
significance <- matrix(data = NA, nrow = 7, ncol = 7)
colnames(correlation) <- colnames(datavars)
rownames(correlation) <- colnames(datavars)
colnames(significance) <- colnames(datavars)
rownames(significance) <- colnames(datavars)

i <- NULL
j <- NULL

# Be careful that you select the correct columns, since i and j range from 1 to n. So they will start using the first column of the 'Dataset'. If not appropriate, make a subset of the 'Dataset' or indicate in the for loop which column numbers can be selected
# ncol geeft aantal kolommen van  de dataset aan (i loopt dus van 1 tot einde aantal kolommen, wat 16 is)
for (i in 1:ncol(datavars))
{
  for (j in 1:ncol(datavars))
  {
    correlation[i,j] <- cor.test(datavars[,i],datavars[,j], use = "pairwise.complete.obs", method = "spearman")$estimate
    significance[i,j] <- cor.test(datavars[,i],datavars[,j],use = "pairwise.complete.obs", method = "spearman")$p.value          # method = "spearman"
  }
}

# To export your outcome to a csv-file
write.csv(correlation, "correlation.csv")
write.csv(significance, "significance.csv")


#=================================================================
# 'Correlation' between continuous and categorical variables
# we expect correlation between temperature and month
boxplot(Temp~fmonth, data=dataH2)
model.lm <- lm(Temp ~ fmonth, data = dataH2)
summary(model.lm)
rsq <- summary(model.lm)$r.squared
rsq
sqrt(rsq)
# What this actually represents is the correlation between the observed Temp, and the ones predicted (fitted) by our model. Both of these variables are numerical so we are able to correlate them.
print(model.lm$fitted)
# Apply Pearson correlation on them
cor(dataH2$Temp, model.lm$fitted)

# We expect correlation between weight and stadium
boxplot(Weight~fstadium, data=dataH2)
model.lm <- lm(Weight ~ fstadium, data = dataH2)
summary(model.lm)
rsq <- summary(model.lm)$r.squared
rsq
sqrt(rsq)
print(model.lm$fitted)
cor(dataH2$Temp, model.lm$fitted)

# We expect correlation between temperature and daynight
boxplot(Temp~fdaynight, data=dataH2)
model.lm <- lm(Temp ~ fdaynight, data = dataH2)
summary(model.lm)
rsq <- summary(model.lm)$r.squared
rsq
sqrt(rsq)
print(model.lm$fitted)
cor(dataH2$Temp, model.lm$fitted)

# We expect correlation between precipitation and weir
boxplot(precipitation~fweir, data=dataH2)
model.lm <- lm(precipitation ~ fweir, data = dataH2)
summary(model.lm)
rsq <- summary(model.lm)$r.squared
rsq
sqrt(rsq)
print(model.lm$fitted)
cor(dataH2$precipitation, model.lm$fitted)



#=================================================================
# Test normality
# Create histogram and qq plot
dev.off()
par(mfrow= c (1,2))
hist(dataH2$swimdistance,  xlab = "Swim distance")
qqnorm(dataH$swimdistance)
qqline(dataH$swimdistance)
shapiro.test(dataH$swimdistance)

# Test homogeneity of variances (if p-value < 0.05 no homogeneity)
bartlett.test(swimdistance ~ daynight, data=df)
leveneTest(y=df$dph, group=df$daynight)

#==========================================
# Find best model
#==============================
# Create GLM without strongly correlated variables
glm = glm(movement ~ precipitation + Temp + pressure + daylength + pump + fweir + floc, family=binomial(link = "log"), data=test)
summary(glm)
par(mar = c(4, 10, 2, 3))
par(mfrow = c(2, 2))
plot(glm)

step(glm)
drop1(glm, test="Chi")
anova(glm)


# Create GLMM without strongly correlated variables
# Play with random effects
y = glmer(swimdistance ~ precipitation + Temp + pressure + daylength + pump + fweir + floc + (1|fyear), family=Gamma(link = "log"), data=dataH2)
summary(y)
m = glmer(swimdistance ~ precipitation + Temp + pressure + daylength + pump + fweir + floc +  (1|fmonth), family=Gamma(link = "log"), data=dataH2)
summary(m)
s = glmer(swimdistance ~ precipitation + Temp + pressure + daylength + pump + fweir + floc + (1|fstadium), family=Gamma(link = "log"), data=dataH2)
summary(s)
t = glmer(swimdistance ~ precipitation + Temp + pressure + daylength + pump + fweir + floc + (1|ftag), family=Gamma(link = "log"), data=dataH2)
summary(t)
ym = glmer(swimdistance ~ precipitation + Temp + pressure + daylength + pump + fweir + floc + (1|fyear) + (1|fmonth), family=Gamma(link = "log"), data=dataH2)
summary(ym)
ys = glmer(swimdistance ~ precipitation + Temp + pressure + daylength + pump + fweir + floc + (1|fyear) + (1|fstadium), family=Gamma(link = "log"), data=dataH2)
summary(ys)
yt = glmer(swimdistance ~ precipitation + Temp + pressure + daylength + pump + fweir + floc + (1|fyear) + (1|ftag), family=Gamma(link = "log"), data=dataH2)
summary(yt)
ms = glmer(swimdistance ~ precipitation + Temp + pressure + daylength + pump + fweir + floc + (1|fmonth) + (1|fstadium), family=Gamma(link = "log"), data=dataH2)
summary(ms)
mt = glmer(swimdistance ~ precipitation + Temp + pressure + daylength + pump + fweir + floc + (1|fmonth) + (1|ftag), family=Gamma(link = "log"), data=dataH2)
summary(mt)
st = glmer(swimdistance ~ precipitation + Temp + pressure + daylength + pump + fweir + floc + (1|fstadium) + (1|ftag), family=Gamma(link = "log"), data=dataH2)
summary(st)
yms = glmer(swimdistance ~ precipitation + Temp + pressure + daylength + pump + fweir + floc + (1|fyear) + (1|fmonth) + (1|fstadium), family=Gamma(link = "log"), data=dataH2)
summary(yms)
ymt = glmer(swimdistance ~ precipitation + Temp + pressure + daylength + pump + fweir + floc + (1|fyear) + (1|fmonth) + (1|ftag), family=Gamma(link = "log"), data=dataH2)
summary(ymt)
mst = glmer(swimdistance ~ precipitation + Temp + pressure + daylength + pump + fweir + floc + (1|fmonth) + (1|fstadium) + (1|ftag), family=Gamma(link = "log"), data=dataH2)
summary(mst)
yst = glmer(swimdistance ~ precipitation + Temp + pressure + daylength + pump + fweir + floc + (1|fyear) + (1|fstadium) + (1|ftag), family=Gamma(link = "log"), data=dataH2)
summary(yst)
ymst = glmer(swimdistance ~ precipitation + Temp + pressure + daylength + pump + fweir + floc + (1|fyear) + (1|fmonth) + (1|fstadium) + (1|ftag), family=Gamma(link = "log"), data=dataH2)
summary(ymst)


# ymt (AIC: 12526.8) and ymst (AIC: 12528.8) have lowest AIC and close to each other
anova(ymt, ymst, test="F")
# non-significant difference, so take model with lowest AIC (ymt)

# Create GAMM without strongly correlated variables
# Play with random effects
gamm.y = gamm(swimdistance ~ s(precipitation) + s(Temp) + s(pressure) + s(daylength) + s(pump) + fweir + floc,  random = list(fyear=~1), family=Gamma(link = "log"), data=dataH2)
summary(gamm.y$gam)
gamm.m = gamm(swimdistance ~ s(precipitation) + s(Temp) + s(pressure) + s(daylength) + s(pump) + fweir + floc, random = list(fmonth=~1), family=Gamma(link = "log"), data=dataH2)
summary(gamm.m$gam)
gamm.s = glmer(swimdistance ~ s(precipitation) + s(Temp) + s(pressure) + s(daylength) + s(pump) + fweir + floc, random = list(fstadium=~1), family=Gamma(link = "log"), data=dataH2)
summary(gamm.s$gam)
gamm.t = glmer(swimdistance ~ s(precipitation) + s(Temp) + s(pressure) + s(daylength) + s(pump) + fweir + floc, random = list(ftag=~1), family=Gamma(link = "log"), data=dataH2)
summary(gamm.t$gam)
ym = glmer(swimdistance ~ precipitation + Temp + pressure + daylength + pump + fweir + floc + (1|fyear) + (1|fmonth), family=Gamma(link = "log"), data=dataH2)
summary(ym)
ys = glmer(swimdistance ~ precipitation + Temp + pressure + daylength + pump + fweir + floc + (1|fyear) + (1|fstadium), family=Gamma(link = "log"), data=dataH2)
summary(ys)
yt = glmer(swimdistance ~ precipitation + Temp + pressure + daylength + pump + fweir + floc + (1|fyear) + (1|ftag), family=Gamma(link = "log"), data=dataH2)
summary(yt)
ms = glmer(swimdistance ~ precipitation + Temp + pressure + daylength + pump + fweir + floc + (1|fmonth) + (1|fstadium), family=Gamma(link = "log"), data=dataH2)
summary(ms)
mt = glmer(swimdistance ~ precipitation + Temp + pressure + daylength + pump + fweir + floc + (1|fmonth) + (1|ftag), family=Gamma(link = "log"), data=dataH2)
summary(mt)
st = glmer(swimdistance ~ precipitation + Temp + pressure + daylength + pump + fweir + floc + (1|fstadium) + (1|ftag), family=Gamma(link = "log"), data=dataH2)
summary(st)
yms = glmer(swimdistance ~ precipitation + Temp + pressure + daylength + pump + fweir + floc + (1|fyear) + (1|fmonth) + (1|fstadium), family=Gamma(link = "log"), data=dataH2)
summary(yms)
gamm.ymt = gamm(swimdistance ~ s(precipitation) + s(Temp) + s(pressure) + s(daylength) + s(pump) + fweir + floc + (1|fyear) + (1|fmonth) + (1|ftag), family=Gamma(link = "log"), data=dataH2)
summary(gamm.ymt)
mst = glmer(swimdistance ~ precipitation + Temp + pressure + daylength + pump + fweir + floc + (1|fmonth) + (1|fstadium) + (1|ftag), family=Gamma(link = "log"), data=dataH2)
summary(mst)
yst = glmer(swimdistance ~ precipitation + Temp + pressure + daylength + pump + fweir + floc + (1|fyear) + (1|fstadium) + (1|ftag), family=Gamma(link = "log"), data=dataH2)
summary(yst)
ymst = glmer(swimdistance ~ precipitation + Temp + pressure + daylength + pump + fweir + floc + (1|fyear) + (1|fmonth) + (1|fstadium) + (1|ftag), family=Gamma(link = "log"), data=dataH2)
summary(ymst)















