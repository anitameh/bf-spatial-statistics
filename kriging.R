# Hack Week January 2016
# January 27, 2016
# Anita Mehrotra

## Conduct kriging.

# packages
library(automap)
library(classInt)
library(maptools)
library(geoR)
library(rgdal)
library(RColorBrewer)
library(rworldmap)
library(spatial)

# working dir
wd_loc = '/Users/anitamehrotra/Documents/hackweek-jan2016'
setwd(wd_loc)

# my functions
source('modifyShapefile.R')
source('conductVariogramFitting.R')
source('prepData.R')

# initial parameters (these come from explore.R!)
covariance_model_type = 'cubic'
initial_sill = 0.1
initial_range = 50


"
Read in data & create variogram based on results from explore.R.
"
# prep data
filename = paste(wd_loc, '/data/training_data_us.csv', sep='')
data = read.csv(filename)
nonstationary_geodata = prepData(data)

# create variogram
estimated_params = conductVariogramFitting(nonstationary_geodata, 
                                           covariance_model_type, initial_sill, initial_range)
tausq = estimated_params[1]; sigmasq = estimated_params[2]; phi = estimated_params[3]
print(tausq); print(sigmasq); print(phi)

"
Predict.
"
# Conduct ordinary kriging with estimated params and plot predicted values on surface
# for USA, the dimensions are such: -125 <= lat <= -65, 20 <= long <= 50
prediction_grid = expand.grid(seq(-125, -65, 1), seq(20, 50, 1))
krige_control = krige.control(type.krige='ok', cov.model=covariance_model_type, 
                              cov.pars=c(sigmasq, phi), nugget=tausq)
predicted_values = krige.conv(nonstationary_geodata, locations=prediction_grid, krige=krige_control)


"
Plot.
"
newmap = getMap(resolution='low')

# predicted values
image(predicted_values, prediction_grid, col=heat.colors(200), xlab='Latitude', 
      ylab='Longitude', main='Ordinary Kriging')  # heatmap
contour(predicted_values, prediction_grid, col='white', lwd=3, add=TRUE)  # add contours
points(data$Longitude, data$Latitude, col=rgb(0, 0, 1, alpha=0.35), 
       cex=0.6, pch=19)  # add points
plot(newmap, xlim=c(-125,-65), ylim=c(35,36), asp=1, lwd=2, add=TRUE)  # map of USA
legend.krige(x.leg=c(-123, -100), y.leg=c(10, 12), values=predicted_values$predict)

# standard errors
image(predicted_values, val=predicted_values$krige.var, col=heat.colors(200), 
      xlab='Latitude', ylab='Longitude', main='Variance on Predicted Values from Ordinary Kriging')
contour(predicted_values, prediction_grid, col='white', lwd=3, add=TRUE)
points(data$Longitude, data$Latitude, col='lightblue', cex=0.6, pch=16)  # add points
plot(newmap, xlim=c(-125,-65), ylim=c(35,36), asp=1, lwd=2, add=TRUE)  # map of USA
legend.krige(x.leg=c(-124, -93), y.leg=c(10, 12), values=predicted_values$krige.var)


"
Output predicted values to .csv.
"
predicted_data = cbind(prediction_grid, predicted_values$predict, predicted_values$krige.var)
colnames(predicted_data) = c('Longitude', 'Latitude', 'Predictions', 'Variance')
write.table(predicted_data, file=paste(wd_loc, '/data/predicted_values.csv', sep=''), 
            sep=',', row.names=FALSE)
