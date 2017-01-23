# LINK EEL CHARACTERISTICS TO DATA

# PIETERJAN VERHELST

library(dplyr)

eels<-"data/interim/Eels.csv" #Filename with fish parameters (length, weight, stage, catch location)
eels<-read.csv(eels)
eels<-eels[ !duplicated(eels$Transmitter) , ]#this removes second duplicate (=in this case the pumping station eel)
eels2 = eels %>%
  select(Transmitter, Catch_location_type)
dataH=merge(dataH, eels2, by="Transmitter")
