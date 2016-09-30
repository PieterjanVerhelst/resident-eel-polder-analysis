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
#(A69-1601-29923, A69-1601-31863, A69-1601-31882, A69-1601-31883)
eels_too_short_tracking <- c("A69-1601-29923", "A69-1601-31863",
                    "A69-1601-31882", "A69-1601-31883")
dataH <- dataH[!dataH$Transmitter %in% eels_too_short_tracking, ]

#############################
# COMBINE EEL CHARACTERISTICS
#############################





