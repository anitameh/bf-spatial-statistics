# Hack Week January 2016
# January 26, 2016
# Anita Mehrotra

## Remove non-contiguous regions from shapefile data.

# packages
library(classInt)
library(geoR)
library(maptools)
library(rgdal)
library(RColorBrewer)
library(spatial)

modifyShapefile = function(shape) {
  "
  Modifies inputted shapefile for US to only contain contiguous region.
  :param shape: path to shape file
  
  :rtype:
  :returns: updated shapefile data
  "
  
  # counties to remove
  virgin_islands = c("St. Croix", "St. John", "St. Thomas")
  hawaii = c("Hawaii", "Honolulu", "Kalawao", "Kauai", "Maui")
  alaska = c("Aleutians East", "Aleutians West", "Anchorage", "Bethel", "Bristol Bay",
             "Denali", "Dillingham", "Fairbanks North Star", "Haines", "Juneau",
             "Kenai Peninsula", "Ketchikan Gateway", "Kodiak Island", "Matanuska-Susitna",
             "Nome", "North Slope", "Northwest Arctic", "Lake and Peninsula",
             "Haines", "Sitka", "Hoonah-Angoon", "Skagway", "Southeast Fairbanks", 
             "Valdez-Cordova", "Wade Hampton", "Wrangell", "Petersburg", "Yakutat", 
             "Yukon-Koyukuk", "Prince of Wales-Hyder")
  puerto_rico = c("Adjuntas", "Aguada", "Aguadilla", "Aguas Buenas", "Aibonito", "A\xf1asco",
                  "Arecibo", "Arroyo", "Barceloneta", "Barranquitas", "Bayam\xf3n", "Cabo Rojo",
                  "Caguas", "Camuy", "Can\xf3vanas", "Carolina", "Cata\xf1o", "Cayey", "Ceiba",
                  "Ciales", "Cidra", "Coamo", "Comer\xedo", "Corozal", "Culebra", "Dorado", "Fajardo",
                  "Florida", "Gu\xe1nica", "Guayama", "Guayanilla", "Guaynabo", "Gurabo", "Hatillo",
                  "Hormigueros", "Humacao", "Isabela", "Jayuya", "Juana D\xedaz", "Juncos", "Lajas",
                  "Lares", "Las Mar\xedas", "Las Piedras", "Lo\xedza", "Luquillo", "Manat\xed", "Maricao",
                  "Maunabo", "Mayag\xfcez", "Moca", "Morovis", "Naguabo", "Naranjito", "Orocovis",
                  "Patillas", "Pe\xf1uelas", "Ponce", "Quebradillas", "Rinc\xf3n", "R\xedo Grande", "Sabana Grande",
                  "Salinas", "San Germ\xe1n", "San Juan", "San Lorenzo", "San Sebasti\xe1n", "Santa Isabel",
                  "Toa Alta", "Toa Baja", "Trujillo Alto", "Utuado", "Vega Alta", "Vega Baja", "Vieques",
                  "Villalba", "Yabucoa", "Yauco")
  
  # get indices for all but Puerto Rico
  allcounties = c(virgin_islands, hawaii, alaska)
  indices_to_remove = match(allcounties, shape$NAME)
  
  # handle Puerto Rico separately
  puerto_rico_indices_to_remove = match(puerto_rico, shape$NAME)
  puerto_rico = append(sort(puerto_rico_indices_to_remove)[2:length(puerto_rico)], 3156)
  
  all_indices_to_remove = c(indices_to_remove, puerto_rico_indices_to_remove)
  final_shape_data = shape[-na.omit(all_indices_to_remove),]
  return(final_shape_data)
}


