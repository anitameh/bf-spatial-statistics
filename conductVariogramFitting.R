# Hack Week January 2016
# January 28, 2016
# Anita Mehrotra

## Explore the data to determine values for kriging.

# packages
library(automap)
library(classInt)
library(geoR)
library(maptools)
library(rgdal)
library(RColorBrewer)
library(spatial)

# working dir
wd_loc = '/Users/anitamehrotra/Documents/hackweek-jan2016'
setwd(wd_loc)


conductVariogramFitting = function(nonstationary_geodata, covariance_model_type, initial_sill, initial_range) {
  "
  Fits empirical variogram to inputted data.
  :param nonstationary_geodata:
  :param String covariance_model_type:
  :param float initial_sill:
  :param float initial_range:
  
  :rtype: vector
  :returns: (nonstationary_geodata, tausq, sigmasq, phi)
  "
  # compute variogram
  variog_for_pageviews = variog(nonstationary_geodata, max.dist=50, estimator.type='modulus')
  fitted_empirical_variogram = variofit(variog_for_pageviews, c(initial_sill, initial_range), 
                                        cov.model=covariance_model_type, 
                                        weights='cressie')
  # output relevant params
  tausq = data.frame(summary(fitted_empirical_variogram)$estimated.pars)[1,1]
  sigmasq = data.frame(summary(fitted_empirical_variogram)$estimated.pars)[2,1]
  phi = data.frame(summary(fitted_empirical_variogram)$estimated.pars)[3,1]
  
  return(c(tausq, sigmasq, phi))
}