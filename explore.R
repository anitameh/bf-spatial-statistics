# Hack Week January 2016
# January 26, 2016
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

# variables
covariance_model_types = c('spherical', 'gaussian', 'wave', 'exponential', 'cubic', 'circular')
colors = c('red', 'green', 'black', 'orange', 'yellow', 'purple')


"
Read in data.
"
data = read.csv(paste(wd_loc, '/data/training_data_us.csv', sep=''))
standardized_pageviews = data$Pageviews


"
Update the data to combat nonstationarity.
"
# work with absolute values for page views
# absolute_standardized_pageviews = abs(standardized_pageviews)

# jitter coordinates
longitude = data$Longitude; latitude = data$Latitude
coordinates = cbind(longitude, latitude)
jittered_coordinates = jitterDupCoords(coordinates, max=0.01)  # ensure nonstationarity

# append jittered coordinates with absolute values of standardized pageviews (order matters!)
nonstationary_data = cbind(standardized_pageviews, jittered_coordinates)
nonstationary_geodata = as.geodata(nonstationary_data, coords.col=2:3, data.col=1)


"
Explore the data.
"
# bin the standardized PV's & study (where does most of the data fall?)
variog_for_pageviews = variog(nonstationary_geodata, estimator.type='modulus')
plot(variog_for_pageviews$uvec, 
     cumsum(variog_for_pageviews$n)/sum(variog_for_pageviews$n),
     main='Percent of Standardized PVs Captured within a Bin', 
     xlab='Bins (degree)',
     ylab='Amount of Data (%)')

# Note: In studying the outputted plot, nearly 100% of the data is captured within 50 degrees.
variog_for_pageviews = variog(nonstationary_geodata, max.dist=50, estimator.type='modulus')

# plot variog cloud with restricted data
variog_cloud = variog(nonstationary_geodata, max.dist=50, 
                      option='cloud', estimator.type='modulus')
length_of_vectors = length(variog_cloud$u)  # u=vector with distances, v=estimated semivariance vals

png('img/restricted_variog_cloud.png', width=4000, height=4000)
plot(variog_cloud$u[seq(1, length_of_vectors, 100)], 
     variog_cloud$v[seq(1, length_of_vectors, 100)], 
     pch=16, cex=3,
     col=rgb(red=0, green=0, blue=1.0, alpha=0.4), 
     xlab='Distance', 
     ylab='Semivariance', 
     main='Cressie estimator, Distance <= 50 degrees')
dev.off()
# NOTE: Withstands stationarity test (consistent variance over points - notice horizontal lines.)


"
Explore different variograms.
"
fit_variograms = function(covariance_model_type, data, initial_sill, initial_range) {
  "
  Fits variogram to inputted geodata.
  :param geodata data:
  :param String covariance_model_type:
  :param int initial_sill:
  :param int initial_range:
  
  :rtype:
  :returns: fitted variogram
  "
  fitted_variogram = variofit(data, c(initial_sill, initial_range), 
                              cov.model=covariance_model_type, 
                              weights='cressie')
  print(covariance_model_type)
  print(fitted_variogram)
  return(fitted_variogram)
}

# fit variograms & plot to determine best one
plot((variog_for_pageviews), main='Empirical Variograms')  # use this to determine initial_sill
initial_sill=0.1; initial_range=50

for (i in 1:length(covariance_model_types)) {
  fitted_variogram = fit_variograms(covariance_model_types[i], 
                                    variog_for_pageviews,
                                    initial_sill, 
                                    initial_range)
  lines(fitted_variogram, col=colors[i], lwd=2)
}
legend(x=35, y=0.05, fill=colors, legend=covariance_model_types)
# NOTE: Looks like Cubic fits the best.


"
Check the empirical variogram against the data you will be using for predictions.
"
# envelope plotting
cubic_variogram = variofit(variog_for_pageviews, 
                              c(initial_sill, initial_range), 
                              cov.model='cubic', 
                              weights='cressie')
pageviews_envelope = variog.model.env(nonstationary_geodata, 
                                      obj.var=variog_for_pageviews,
                                      model.pars=cubic_variogram, 
                                      nsim=10)
plot(variog_for_pageviews, envelope=pageviews_envelope, main='Envelope with Cubic Params')

tausq = data.frame(summary(cubic_variogram)$estimated.pars)[1,1]
sigmasq = data.frame(summary(cubic_variogram)$estimated.pars)[2,1]
phi = data.frame(summary(cubic_variogram)$estimated.pars)[3,1]


"
For Residuals.
"
# for residuals
variog_for_residuals = variog(nonstationary_geodata, estimator.type='modulus')
plot(variog_for_residuals$uvec, 
     cumsum(variog_for_residuals$n)/sum(variog_for_residuals$n),
     main='Percent of Standardized PVs Captured within a Bin', 
     xlab='Bins (degree)',
     ylab='Amount of Data (%)')
variog_for_residuals = variog(residuals_geodata, max.dist=20, estimator.type='modulus')

# fit variograms & plot to determine best one
plot((variog_for_residuals), main='Empirical Variograms for Residuals')  # use this to determine initial_sill
initial_sill=0.025; initial_range=20

for (i in 1:length(covariance_model_types)) {
  fitted_variogram = fit_variograms(covariance_model_types[i], 
                                    variog_for_residuals,
                                    initial_sill, 
                                    initial_range)
  lines(fitted_variogram, col=colors[i], lwd=2)
}
legend(x=35, y=0.05, fill=colors, legend=covariance_model_types)
# NOTE: Looks like Gaussian fits the best.


# envelope plotting
gaussian_variogram = variofit(variog_for_residuals, 
                           c(initial_sill, initial_range), 
                           cov.model='gaussian', 
                           weights='cressie')
pageviews_envelope = variog.model.env(residuals_geodata, 
                                      obj.var=variog_for_residuals,
                                      model.pars=gaussian_variogram, 
                                      nsim=10)
plot(variog_for_residuals, envelope=pageviews_envelope, main='Envelope with Cubic Params')

tausq = data.frame(summary(gaussian_variogram)$estimated.pars)[1,1]
sigmasq = data.frame(summary(gaussian_variogram)$estimated.pars)[2,1]
phi = data.frame(summary(gaussian_variogram)$estimated.pars)[3,1]