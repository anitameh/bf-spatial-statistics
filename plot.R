# Hack Week January 2016
# January 26, 2016
# Anita Mehrotra

## Plot data on contiguous US map.

# packages
library(classInt)
library(geoR)
library(maptools)
library(rgdal)
library(RColorBrewer)
library(spatial)

# working dir
wd_loc = '/Users/anitamehrotra/Documents/hackweek-jan2016'
setwd(wd_loc)

source('modifyShapefile.R')


# function to plot data
plot_geodata = function(shape, data_to_plot, image_name) {
  "
  Plot geo data on a map of the US.

  :param shape:
  :param data_to_plot:
  :param image_name:
  
  :rtype:
  :returns:
  "
  # project data onto map of US
  proj4string(shape) = CRS('+proj=longlat')
  projection = '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'
  USA_projection = spTransform(shape, CRS(projection))
  
  # establish colors
  number_of_colors = 9
  plot_colors = rev(brewer.pal(number_of_colors, 'YlOrRd'))
  class_intervals = classIntervals(data_to_plot, number_of_colors, style='quantile')
  color_code = findColours(class_intervals, plot_colors)
  
  # plot standardized page views
  png(image_name, width=4500, height=4500)
  plot(USA_projection, col=color_code, border="grey", lwd=0.5)
  legend(-75, 35, legend=names(attr(color_code, "table")), fill=attr(color_code, "palette"), cex=5, bty="n")
  dev.off()
}

"
Execute code
"
# get shapefile for US counties
shape = readShapePoly(paste(wd_loc, 
                            '/data/cb_2014_us_county_500k/cb_2014_us_county_500k.shp', 
                            sep=''))
updated_shape = modifyShapefile(shape)  # contiguous region only

# get page view data
standardized_data = read.csv(paste(wd_loc, '/data/training_data_us.csv', sep=''))
standardized_pageviews = standardized_data$Pageviews

# run
plot_geodata(updated_shape, standardized_pageviews, 'img/pageviews.png')
