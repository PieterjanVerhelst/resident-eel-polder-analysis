
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

# Create boxplot with distance for each catch location type
# TODO: add number of eels (transmitter IDs) taken into account at the top of the boxplot?

#
# make a named list for the location of the number of eels
eel_per_loc <- summary
eels_per_loc_list <- rep(20000, 3)
names(eels_per_loc_list) <- eel_per_loc$Catch_location_type
# create ggplot (cfr. styling earlier plot)
fig_homerange <- ggplot(DataHdistance, aes(x = Catch_location_type,
                                           y = distance2)) +
                    geom_boxplot() +
                    scale_y_continuous(breaks = seq(0, 15000, by = 5000)) +
                    theme_minimal() +
                    ylab("Distance (m)") +
                    geom_text(data = data.frame(),
                              aes(x = names(eels_per_loc_list),
                                  y = eels_per_loc_list,
                                  label = as.character(eel_per_loc$eels)),
                              col = 'black', size = 6) +
                    xlab("") +
                    theme(axis.title.y = element_text(margin = margin(r = 5))) +
                    theme(axis.text = element_text(size = 14),
                          axis.title = element_text(size = 16))
ggsave(fig_homerange, file = './home_range_boxplot.png')
#--------------------------------

boxplot(distance2~Catch_location_type, DataHdistance,
        xlab = "Catch location type",
        ylab = "Distance (m)",
ylim=c(0, 20000), cex.lab=1.25, cex.axis=1.25)

