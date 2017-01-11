# ANALYSIS OF ENVIRONMENTAL FACTORS AFFECTING EEL DISPERSION - GLMM & RF

# PIETERJAN VERHELST

library(randomForest)
library(mgcv)
library(userfriendlyscience)
library(lme4)
library(piecewiseSEM)
library(blmeco)



# Categorize the explanatory variable:
# <= 500 m = 0 (low displacement) & >500 m = 1 (high displacement)
# Apply this for the categories 500 m, 1000 m and 1500 m
# Below the elaborated analysis for the 500 m threshold

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

#===========================================================
# Apply bootstrapping to obtain same number of 0 as 1
# Make 2 lists: list with movement = 1 and list with movement = 0

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


####################
# APPLY GLMM
####################
# Calculate summary with AIC
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

# # Apply stepwise backwards selection based on AIC

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


#############################################
# Apply Random Forests
#############################################
set.seed(500)

# Apply RF; if response variable is categorical, classification is assumed; otherwise, regression is assumed
fit = randomForest(factor(movement) ~ precipitation + Temp + pressure + pump + floc, data=df, importance = TRUE, ntree=6000)
varImpPlot(fit)
importance(fit)


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


