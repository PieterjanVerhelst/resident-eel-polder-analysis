# ANALYSIS OF SEASONAL ACTIVITY AS THE TOTAL SWIM DISTANCE (M) PER MONTH

# PIETERJAN VERHELST

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

# Transformation
df$tsum=log(df$sum)

# non-parametric Kruskall Wallis test as alternative for one-way ANOVA
kw=kruskal.test(df$tsum ~ df$Month)
kw

# pairwise comparison
posthoc.kruskal.dunn.test(x=df$tsum, g=df$Month, p.adjust.method="bonferroni")





