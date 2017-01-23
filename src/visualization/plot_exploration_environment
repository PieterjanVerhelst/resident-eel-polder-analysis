# DATA EXPLORATION OF ENVIRONMENTAL VARIABLES FOR GLMM AND RF ANALYSIS

# PIETERJAN VERHELST

# FOLLOWS ON ANALYSIS_ENVIRONMENT
source(Analysis_Environment.R)

## RELATIONSHIP RESPONSE VARIABLE 'MOVEMENT' AND EACH EXPLANATORY VARIABLE
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


## HOMOGENEITY
# Create scatterplot

par(mar = c(2, 5, 0, 5))
par(mfrow= c (2,3))
plot(df$precipitation, ylab = "Precipitation (mm)")
plot(df$pump, ylab = "Pumping discharge (m?/s)")
plot(df$Temp, ylab = "Temperature (?C)")
plot(df$daylength, ylab = "Daylength (minutes)")
plot(df$pressure, ylab = "Air pressure (hPa)")



## PLOTTING EMPIRICAL CUMULATIVE DISTRIBUTION
## TO IDENTIFY LINK (LOGIT OR PROBIT)

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

## COLLINEARITY
# Take environmental variables and put them in new data frame datavars
datavars=df[,c("precipitation","Temp","pump","daylength","pressure")]
#character becomes numeric

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

correlation
significance

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
