library(raster)
library(sf)
library(tidyverse)

aoi <- st_read("output/aoi.gpkg")

rs <- list.files("C:/Science/R_projects/Evo/dems/", pattern = "tif$", full.names = T)

dems <- c()
for(ir in rs){
  r <- raster(ir)
  
  aoi2 <- st_transform(aoi, as.character(crs(r)))
  # aoi2 <- st_as_sf(st_as_sfc(st_bbox(aoi2)+c(-3000,-3000,3000,3000)))
  
  if(length(st_intersects(aoi2, st_as_sf(st_as_sfc(st_bbox(r))))[[1]])>0){
    dems <- c(dems, ir)
  }
  
}

if(length(dems)>1){
  rast.list <- list()
  for(ii in 1:length(dems)) { rast.list[ii] <- raster(dems[ii]) }
  rast.list$fun <- mean
  rast.list$tolerance <- 0.5
  rast.mosaic <- do.call(mosaic,rast.list)
} else {
  rast.mosaic <- raster(dems)
}

aoi2 <- st_transform(aoi, as.character(crs(rast.mosaic)))
aoi2 <- st_as_sf(st_as_sfc(st_bbox(aoi2)+c(-3000,-3000,3000,3000)))
rast.mosaic <- crop(rast.mosaic, aoi2)

if(as.character(crs(aoi)) != as.character(crs(rast.mosaic))){
  rast.mosaic <- projectRaster(rast.mosaic, res = 2, crs = as.character(crs(aoi)))
}

rast.mosaic <- crop(rast.mosaic, aoi)

plot(rast.mosaic)

if(maxValue(rast.mosaic) < 650){
  writeRaster(round(rast.mosaic*100), paste0("output/dem.tif"),
              format = "GTiff", datatype = "INT2U", overwrite = T)
} else {
  writeRaster(round(rast.mosaic*100), paste0("output/dem.tif"),
              format = "GTiff", datatype = "INT4U", overwrite = T)
}


