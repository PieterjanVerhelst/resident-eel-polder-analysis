# CHECKING ASSUMPTIONS FOR LINEAR MODEL OF SEASONAL ACTIVITY ANALYSIS

# PIETERJAN VERHELST

# Follows ON 'Analysis_SeasonalActivity'
source(Analysis_SeasonalActivity.R)


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


aov=aov(df$sum ~ df$Month)
summary(aov)

# Overview assumptions normality and homogeneity
par(mfrow=c(1,2))     # set graphics window to plot side-by-side
plot(aov, 1)          # graphical test of homogeneity
plot(aov, 2) # graphical test of normality



