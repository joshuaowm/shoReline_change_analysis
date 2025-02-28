calculate_ndwi <- function(raster) {
  green <- raster[[2]]  
  nir <- raster[[4]]    
  
  ndwi <- (green - nir) / (green + nir)
  return(ndwi)
}

create_masks <- function(ndwi) {
  land <- ndwi < 0
  water <- ndwi >= 0
  return(list(land = land, water = water))
}

create_shoreline <- function(raster_mask) {
  shoreline <- as.polygons(raster_mask, dissolve = TRUE)
  shoreline <- st_as_sf(shoreline)
  shoreline_sf <- st_transform(crs = 4326)
  return(shoreline_sf)
}