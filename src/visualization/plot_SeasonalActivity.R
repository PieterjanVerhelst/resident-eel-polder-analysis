# PLOT FOR THE SEASONAL ACTIVITY

# STIJN VAN HOEY & PIETERJAN VERHELST

# FOLLOWS 'ENRICH FILE'
source(enrich.R)


library(ggplot2)
library(gridExtra)



# ------------------------------
# COMBINED PLOT: COMBINE 2 GGPLOT GRAPHS
# ------------------------------


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

grid.arrange(bxplot, tmplot, nrow = 2)
# The results is according to ggplot rules (no 2 variables on axis, cfr. :
# http://stackoverflow.com/questions/3099219/plot-with-2-y-axes-one-y-axis-on-the-left-and-another-y-axis-on-the-right)



