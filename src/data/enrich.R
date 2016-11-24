# Preprocessing functions
#
# author: PieterJan Verhelst
#

source("src/data/support_functions.R")

########################
# Load in homing data
########################
dataH <- "data/interim/dataH.csv"
dataH <- read.csv(dataH, header = TRUE, stringsAsFactors = FALSE)

########################
# TIME
########################
dataH <- convert_datecols(dataH, c("Departure_time", "Arrival_time"),
                          format = "%Y-%m-%d %H:%M")

dataH <- convert_datecols(dataH, c("Departure", "Arrival"),
                          format = "%d/%m/%Y %H:%M")

# Add month and year to data set
dataH <- add_yearmonth(dataH, "Arrival", name_prefix = "Arrival_")

########################
# SUBSET DATA
########################

# Select locations/receivers according to study area (bh-5 is still in dataset)
polders <- c("bh-6", "bh-7", "bh-8", "bh-9", "bh-10", "bh-11", "bh-13", "bh-14",
            "bh-15", "bh-16", "bh-18", "bh-20", "bh-21", "bh-22", "bh-23",
            "bh-24", "bh-25", "bh-26", "bh-27", "bh-28", "bh-29", "bh-30")
dataH <- dataH[dataH$Station.Name %in% polders, ]

# Remove eels with 14 days tracking and less
# (A69-1601-29923, A69-1601-31863, A69-1601-31882, A69-1601-31883)
eels_too_short_tracking <- c("A69-1601-29923", "A69-1601-31863",
                    "A69-1601-31882", "A69-1601-31883")
dataH <- dataH[!dataH$Transmitter %in% eels_too_short_tracking, ]

#############################
# COMBINE EEL CHARACTERISTICS
# File with fish parameters (length, weight, stage, catch location)
# for this analysis, the interest is the Catch_location_type, which will be
# merged with the dataH, using the transmitter data
#############################

eels <- "data/interim/Eels.csv"
eels <- read.csv(eels)
# remove duplicate, i.e. the pumping station eel
eels <- eels[ !duplicated(eels$Transmitter), ]
# update dataH with the new column catch location type
dataH <- merge(dataH, eels[, c("Transmitter", "Catch_location_type")],
               by = "Transmitter")

# addition of $stations, $moves and $maxdetections not in enrich,
# as not used for further analysis

#############################
# ADD the total tracking period
#############################

# detections <- dataH %>%
#     group_by(Transmitter) %>%
#     select(Transmitter, Arrivalnum, Departurenum)
#
# summary=summarise(detections,
#                   seconds=with(detections, max(Departurenum) - min(Arrivalnum))
# )

tracking_length <- dataH %>%
                    group_by(Transmitter) %>%
                    select(Transmitter, Arrivalnum, Departurenum) %>%
                    mutate(total_tracking = max(Departurenum) - min(Arrivalnum))

# Add the total lengths to
dataH=merge(dataH, summary, by="Transmitter")








