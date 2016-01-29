# Hack Week January 2016
# January 28, 2016
# Anita Mehrotra

# packages
library(classInt)
library(geoR)
library(rgdal)


prepData = function(data) {
  "
  Create nonstationary geodata.
  
  :param String filename: assumed empty

  :rtype: geodata object
  :returns nonstationary_geodata:
  "
  # get variables of interest
  standardized_pageviews = data$Pageviews
  longitude = data$Longitude
  latitude = data$Latitude
  coordinates = cbind(longitude, latitude)
  
  # jitter coordinates to handle nonstationarity in one way
  jittered_coordinates = jitterDupCoords(coordinates, max=0.01)  # ensure nonstationarity
  nonstationary_data = cbind(standardized_pageviews, jittered_coordinates)
  nonstationary_geodata = as.geodata(nonstationary_data, coords.col=2:3, data.col=1)
  
  return(nonstationary_geodata)
}