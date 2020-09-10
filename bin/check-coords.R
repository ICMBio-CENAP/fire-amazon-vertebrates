# Generate spatial distributions
# Code to check if Latitude and Longitude are OK

# Adapted from a previous script by Jorge Ahumada
# Elildo Carvalho Jr @ ICMBio/CENAP, 2020-04-02

### Generate spatial distributions
check.coord <- function(data) {
  
  ##----- 1 - Load libraries-----
  library(ggmap)
  library(here)
  
  ##-----2 - Generate spatial distributions -----
  
  # Start with provide the lon/lat range of the data
  lon <- range(data$Longitude, na.rm=T)
  lat <- range(data$Latitude, na.rm=T)
  
  # Extract the unique lat/lons and put them on a data frame
  locations.data <- unique(cbind(as.character(data$Camera.Trap.Name), data$Latitude, data$Longitude))
  
  locations.data <- data.frame(Camera.Trap.Name = locations.data[,1], Latitude = as.numeric(locations.data[,2]), Longitude = as.numeric(locations.data[,3]))
  
  locations.data <- dplyr::arrange(locations.data, Camera.Trap.Name)
  
  # If you have internet: Download the map from google
  map <- get_map(location = c(c(lon[1],lat[1]),c(lon[2],lat[2])), zoom = 10, source = "google", maptype = "terrain")
  
  # Plot the locations of Camera traps
  ggmap(map, extent = "normal", maprange = T) + geom_point(data=locations.data, aes(x = Longitude, y = Latitude), colour="black", size = 0.1)
  
} # end of function
