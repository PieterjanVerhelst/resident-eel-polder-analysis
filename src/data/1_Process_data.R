# PROCESS DATASET

# PIETERJAN VERHELST

library(dplyr)
library(lubridate)



# Load in homing data
dataH <- "data/interim/dataH.csv"
dataH <- read.csv(dataH, header = TRUE, stringsAsFactors = FALSE)

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
polder=c("bh-6","bh-7","bh-8","bh-9","bh-10","bh-11","bh-13","bh-14","bh-15",
         "bh-16","bh-18","bh-20","bh-21","bh-22","bh-23","bh-24","bh-25",
         "bh-26","bh-27","bh-28","bh-29","bh-30")
dataH=dataH[dataH$Station.Name %in% polder,]

# Remove eels with 14 days tracking and less (A69-1601-29923, A69-1601-31863, A69-1601-31882, A69-1601-31883)
dataH <- dataH[ ! dataH$Transmitter %in% c("A69-1601-29923", "A69-1601-31863", "A69-1601-31882", "A69-1601-31883"), ]


