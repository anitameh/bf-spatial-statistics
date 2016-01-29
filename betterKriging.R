# Hack Week January 2016
# January 28, 2016
# Anita Mehrotra

## Better kriging.

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
covariance_model_type = 'gaussian'
initial_sill = 0.03
initial_range = 50

"
Read in data & run linear regression to control for residuals.
"
# prep data
filename = paste(wd_loc, '/data/training_data_us.csv', sep='')
# filename = paste(wd_loc, '/data/training_data_europe.csv', sep='')
data = read.csv(filename)

# linear regression
# linear_reg = lm(data$Pageviews ~ data[,3] + data[,4] + data[,5] + data[,6])
linear_reg = lm(data$Pageviews ~ data[,3] + data[,4] + data[,5] + data[,6] +
                  data[,7] + data[,8] + data[,9] + data[,10] + data[,11] +
                  data[,12] + data[,13] + data[,14] + data[,15] + data[,16] +
                  data[,17] + data[,18] + data[,19] + data[,20] + data[,21] +
                  data[,22] + data[,23] + data[,24] + data[,25] + data[,26] +
                  data[,27] + data[,28] + data[,29] + data[,30] + data[,31] + 
                  data[,32] + data[,33] + data[,34] + data[,35] + data[,36] + 
                  data[,37] + data[,38] + data[,39] + data[,40] + data[,41] + 
                  data[,42] + data[,43] + data[,44] + data[,45] + data[,46] + 
                  data[,47] + data[,48] + data[,49] + data[,50] + data[,51] + 
                  data[,52] + data[,53])
# linear_reg = lm(data$Pageviews ~ data[,4] + data[,5] + data[,6] + data[,7] + 
#                  data[,8] + data[,9])

"
Now conduct kriging on residuals.
"

# prep data
new_data = cbind(data$Longitude, data$Latitude, linear_reg$residuals)
colnames(new_data) = c('Longitude', 'Latitude', 'Pageviews')
new_data = data.frame(new_data)

# convert to geodata
residuals_geodata = prepData(new_data)

# create variogram
estimated_params = conductVariogramFitting(residuals_geodata, covariance_model_type, 
                                           initial_sill, initial_range)

# get necessary params
tausq = estimated_params[1]; sigmasq = estimated_params[2]; phi = estimated_params[3]
print(tausq); print(sigmasq); print(phi)

############################
# if any of the vals == 0, add epsilon small to ensure invertibility of matrix
# epsilon = 0.001
# sigmasq = sigmasq + epsilon
############################

"
Predict.
"
# Conduct ordinary kriging with estimated params and plot predicted values on surface
# for USA, the dimensions are such: -125 <= lat <= -65, 20 <= long <= 50
prediction_grid = expand.grid(seq(-125, -65, 1), seq(20, 50, 1))
krige_control = krige.control(type.krige='ok', cov.model=covariance_model_type, 
                              cov.pars=c(sigmasq, phi), nugget=tausq)
predicted_values = krige.conv(residuals_geodata, locations=prediction_grid, krige=krige_control)

# for Europe, the dimensions are: -20 <= lat <= 30; 25 <= long <= 60
prediction_grid = expand.grid(seq(-20, 30, 1), seq(25, 60, 1))
krige_control = krige.control(type.krige='ok', cov.model=covariance_model_type, 
                              cov.pars=c(sigmasq, phi), nugget=tausq)
predicted_values = krige.conv(residuals_geodata, locations=prediction_grid, krige=krige_control)

"
Plot.
"
newmap = getMap(resolution='low')

# predicted values
image(predicted_values, prediction_grid, col=heat.colors(200), xlab='Latitude', 
      ylab='Longitude', main='Ordinary Kriging for Residuals')  # heatmap
contour(predicted_values, prediction_grid, col='white', lwd=3, add=TRUE)  # add contours
points(data$Longitude, data$Latitude, col='lightblue', 
       cex=0.6, pch=19)  # add points

plot(newmap, xlim=c(-125,-65), ylim=c(35,36), asp=1, lwd=2, add=TRUE)  # map of USA
legend.krige(x.leg=c(-124, -93), y.leg=c(10, 12), values=predicted_values$predict) # USA legend

# plot(newmap, xlim=c(-20,30), ylim=c(25,60), asp=1, lwd=2, add=TRUE)  # map of Europe
# legend.krige(x.leg=c(-18, -5), y.leg=c(20, 22), values=predicted_values$predict)

# standard errors
image(predicted_values, val=sqrt(predicted_values$krige.var), col=heat.colors(200), 
      xlab='Latitude', ylab='Longitude', main='Standard Errors from Ordinary Kriging for Residuals')
contour(predicted_values, prediction_grid, col='white', lwd=3, add=TRUE)
points(data$Longitude, data$Latitude, col='lightblue', 
       cex=0.6, pch=19)  # add points
plot(newmap, xlim=c(-125,-65), ylim=c(35,36), asp=1, lwd=2, add=TRUE)  # map of USA
legend.krige(x.leg=c(-124, -93), y.leg=c(10, 12), values=predicted_values$krige.var) # USA legend

# plot(newmap, xlim=c(-20,30), ylim=c(25,60), asp=1, lwd=1, add=TRUE)  # map of Europe
# legend.krige(x.leg=c(-18, 10), y.leg=c(20, 22), values=predicted_values$krige.var) # Europe legend


"
Output predicted values to .csv.
"
predicted_data = cbind(prediction_grid, predicted_values$predict, sqrt(predicted_values$krige.var))
colnames(predicted_data) = c('Longitude', 'Latitude', 'Predictions', 'SE')
write.table(predicted_data, file=paste(wd_loc, '/data/residuals_state_predicted_values.csv', sep=''), 
            sep=',', row.names=FALSE)

