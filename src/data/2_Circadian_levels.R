# LINK CIRCADIAN LEVELS TO THE DATASET

# PIETERJAN VERHELST & BART AELTERMAN


library(data.table)
library(lubridate)

Diurnal<-"./data/external/Diurnal.csv"
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
