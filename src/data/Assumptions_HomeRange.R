# CHECKING ASSUMPTIONS FOR LINEAR MODEL OF HOME RANGE ANALYSIS

# PIETERJAN VERHELST

# Follows ON 'Analysis_HomeRange'
source(Analysis_HomeRange.R)

###########################
# Create linear model
###########################
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

shapiro.test(lm.stdres) # p-value > 0.05: no evidence against null hypothesis (= data is normal distributed)

# 2) Check homogeneity of variance
par(mar = c(4, 10, 2, 3))
par(mfrow = c(2, 2))
plot(lm) # Here also normality of residuals in QQ plot
