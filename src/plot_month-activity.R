# Select swim distance per month

library(dplyr)
# library(geoR)
library(lubridate)
library(data.table)
library(ggplot2)

setwd("~/projecten/2016_pieterjan_shortpath/month_activity_plots")

# Read data files
temp <- read.csv("tempdata.csv")
dataH <- read.csv("dataH.csv")

dataH$Date <- substr(as.character(dataH$Departure_time), 1, 10)
dataH$Month <- as.integer(substr(as.character(dataH$Departure_time), 6, 7))

#--------------------------------------
# HOW TO CALCULATE THE DATE COLUMN???
#--------------------------------------
detections = dataH %>%
    group_by(Date, swimdistance, Month) %>%
    select(Date, Transmitter, swimdistance, Month)

# Only take into account swim distances not equal to zero
df <- detections[which(detections$swimdistance != "0"), ]

# Sum swim distance per eel per month
detections2 <- df %>%
    group_by(Transmitter, Date, Month) %>%
    select(Transmitter, Date, Month, swimdistance)

df = summarise(detections2, sum = sum(swimdistance))


par(mar = c(6,4.1,4.1,2.1))
barplot(df$sum, names.arg = df$Month, cex.names = 0.8,
        ylim = c(0,15000), las = 1)

# Create boxplot with swim distance for each month
df$Month <- factor(df$Month)
boxplot(sum ~ Month, df, xlab = "Month",
        ylab = "Swim distance per month (m)",
        ylim = c(0, 12000), cex.lab = 1.25,
        cex.axis = 1.25)

# Create figure with temperature per month

# Add month
temp$Month = month(temp$Timestamp)
temp$Month = as.factor(temp$Month)
ggplot(temp, aes(x = Month, y = Value)) +
    stat_summary(aes(y = Value, group = 1), fun.y = mean,
                 colour = "black", geom = "point", group = 1) # +
    # geom_boxplot(data=df)

# ------------------------------
# COMBINED PLOT:
# ------------------------------

# OPTION 1: COMBINE 2 GGPLOT GRAPHS
#----------------------------------
month.avg <- aggregate(temp$Value, list(as.integer(temp$Month)), mean)

# location preparation for the graph text
number_of_eels_placement <- rep(12500, 12)
names(number_of_eels_placement) <- levels(temp$Month)

# FOR NOW: hard-code the numbers: should be integrated with other script and
# calculated as such!
counts_eels <- c("6", "5", "6", "13", "19", "12", "21", "25", "XX", "23", "13", "10")

bxplot <- ggplot(data = df, aes(x = Month, sum)) +
    geom_boxplot() +
    theme_minimal() +
    ylab("Swim distance per month (m)") +
    scale_y_continuous(breaks = seq(0, 9000, by = 3000)) +
    theme(axis.title.y = element_text(margin = margin(r = 15))) +
    geom_text(data = data.frame(),
              aes(x = names(number_of_eels_placement),
                  y = number_of_eels_placement,
                  label = counts_eels),
              col = 'black', size = 6) +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()) +
    theme(axis.text = element_text(size = 14),
          axis.title = element_text(size = 14))

tmplot <- ggplot() +
    geom_point(data = temp, aes(x = Month, y = Value),
               colour = "grey", alpha = 0.4) + # all dots (shows the spread)
    geom_point(data = month.avg, aes(x = Group.1, y = x),
               colour = "blue") + # all dots (shows the spread)
    theme_minimal() +
    ylab(expression("Temperature ("*~degree*C*")")) +
    theme(axis.title.y = element_text(margin = margin(r = 14))) +
    theme(axis.text.y = element_text(margin = margin(r = 14.5))) +
    theme(axis.text = element_text(size = 14),
          axis.title = element_text(size = 14))

library(gridExtra)
swim_plot <- grid.arrange(bxplot, tmplot, nrow = 2)
swim_plot

ggsave(swim_plot, file = './swim_distance_plot.pdf')
ggsave(swim_plot, file = './swim_distance_plot.png')
# The results is according to ggplot rules (no 2 variables on axis, cfr. :
# http://stackoverflow.com/questions/3099219/plot-with-2-y-axes-one-y-axis-on-the-left-and-another-y-axis-on-the-right)
# however, the alignment of the two axis is rather hacky...


# OPTION 2: OVERLAY
#-----------------------
#precalculate monthly averages:
month.avg <- aggregate(temp$Value, list(as.integer(temp$Month)), mean)
#dist.avg <- aggregate(df$sum, list(as.integer(df$Month)), mean)

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




