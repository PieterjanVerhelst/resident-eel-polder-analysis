# ANALYSIS OF HOME RANGE SIZE BETWEEN EELS OF DIFFERENT CATCH LOCATIONS

# PIETERJAN VERHELST


library(censReg)

# Load in DataHdistance
DataHdistance <- "./data/external/DataHdistance.csv"
DataHdistance <- read.csv(DataHdistance, header = TRUE, stringsAsFactors = FALSE)

# Remove eels with 14 days tracking and less (A69-1601-29923, A69-1601-31863, A69-1601-31882, A69-1601-31883)
DataHdistance <- DataHdistance[ ! DataHdistance$Transmitter %in% c("A69-1601-29923", "A69-1601-31863", "A69-1601-31882", "A69-1601-31883"), ]

# Create factors
DataHdistance$Stadium <- as.factor(DataHdistance$Stadium)
DataHdistance$Catch_location_type <- as.factor(DataHdistance$Catch_location_type)
DataHdistance$Catch_location<-factor(DataHdistance$Catch_location, levels=c("9","6","10","7","4","3","1","2"))

# Response variable is numeric
DataHdistance$distance2 <- as.numeric(DataHdistance$distance2)

# Log transformation
DataHdistance$tdistance = log(DataHdistance$distance2)

# Create censored regression model
estResult <- censReg(tdistance~Catch_location_type, data=DataHdistance)
summary(estResult)
