library(raster)
library(sf)
library(tidyverse)

aoi <- st_read("output/aoi.gpkg")

rs <- list.files("E:/TomstGIS/luke_data/2019/", pattern = ".tif$", full.names = T, recursive = T)

roi <- st_as_sf(st_as_sfc(st_bbox(aoi)+c(-2000,-2000,2000,2000)))

# FOREST AGE

rs2 <- rs[grepl("ika_", rs)]

dems <- c()
for(ir in rs2){
  r <- raster(ir)
  
  p2 <- st_transform(aoi, as.character(crs(r)))
  
  roi2 <- st_as_sf(st_as_sfc(st_bbox(p2)+c(-5000,-5000,5000,5000)))
  
  if(length(st_intersects(roi2, st_as_sf(st_as_sfc(st_bbox(r))))[[1]])>0){
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

p2 <- st_transform(aoi, as.character(crs(rast.mosaic)))
roi2 <- st_as_sf(st_as_sfc(st_bbox(p2)+c(-3000,-3000,3000,3000)))
rast.mosaic <- crop(rast.mosaic, roi2)

if(as.character(crs(aoi)) != as.character(crs(rast.mosaic))){
  rast.mosaic <- projectRaster(rast.mosaic, res = 16, crs = as.character(crs(aoi)))
}

rast.mosaic <- crop(rast.mosaic, aoi)
rast.mosaic[rast.mosaic > 10000] <- NA

plot(rast.mosaic)

writeRaster(rast.mosaic, paste0("output/forest_age.tif"),
            format = "GTiff", datatype = "INT2U", overwrite = T)


# FOREST HEIGHT

rs2 <- rs[grepl("keskipituus_", rs)]

dems <- c()
for(ir in rs2){
  r <- raster(ir)
  
  p2 <- st_transform(aoi, as.character(crs(r)))
  
  roi2 <- st_as_sf(st_as_sfc(st_bbox(p2)+c(-3000,-3000,3000,3000)))
  
  if(length(st_intersects(roi2, st_as_sf(st_as_sfc(st_bbox(r))))[[1]])>0){
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

p2 <- st_transform(aoi, as.character(crs(rast.mosaic)))
roi2 <- st_as_sf(st_as_sfc(st_bbox(p2)+c(-3000,-3000,3000,3000)))
rast.mosaic <- crop(rast.mosaic, roi2)

if(as.character(crs(aoi)) != as.character(crs(rast.mosaic))){
  rast.mosaic <- projectRaster(rast.mosaic, res = 16, crs = as.character(crs(p)))
}

rast.mosaic <- crop(rast.mosaic, aoi)
rast.mosaic[rast.mosaic > 10000] <- NA

plot(rast.mosaic)

writeRaster(rast.mosaic, paste0("output/forest_height.tif"),
            format = "GTiff", datatype = "INT2U", overwrite = T)

# CANOPY COVER

rs2 <- rs[grepl("/latvuspeitto_", rs)]

dems <- c()
for(ir in rs2){
  r <- raster(ir)
  
  p2 <- st_transform(aoi, as.character(crs(r)))
  
  roi2 <- st_as_sf(st_as_sfc(st_bbox(p2)+c(-3000,-3000,3000,3000)))
  
  if(length(st_intersects(roi2, st_as_sf(st_as_sfc(st_bbox(r))))[[1]])>0){
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

p2 <- st_transform(aoi, as.character(crs(rast.mosaic)))
roi2 <- st_as_sf(st_as_sfc(st_bbox(p2)+c(-3000,-3000,3000,3000)))
rast.mosaic <- crop(rast.mosaic, roi2)

if(as.character(crs(aoi)) != as.character(crs(rast.mosaic))){
  rast.mosaic <- projectRaster(rast.mosaic, res = 16, crs = as.character(crs(p)))
}

rast.mosaic <- crop(rast.mosaic, aoi)
rast.mosaic[rast.mosaic > 10000] <- NA

plot(rast.mosaic)

writeRaster(rast.mosaic, paste0("output/canopy_cover.tif"),
            format = "GTiff", datatype = "INT1U", overwrite = T)

# CANOPY COVER DECIDIOUS

rs2 <- rs[grepl("lehtip_latvuspeitto_", rs)]

dems <- c()
for(ir in rs2){
  r <- raster(ir)
  
  p2 <- st_transform(aoi, as.character(crs(r)))
  
  roi2 <- st_as_sf(st_as_sfc(st_bbox(p2)+c(-3000,-3000,3000,3000)))
  
  if(length(st_intersects(roi2, st_as_sf(st_as_sfc(st_bbox(r))))[[1]])>0){
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

p2 <- st_transform(aoi, as.character(crs(rast.mosaic)))
roi2 <- st_as_sf(st_as_sfc(st_bbox(p2)+c(-3000,-3000,3000,3000)))
rast.mosaic <- crop(rast.mosaic, roi2)

if(as.character(crs(aoi)) != as.character(crs(rast.mosaic))){
  rast.mosaic <- projectRaster(rast.mosaic, res = 16, crs = as.character(crs(p)))
}

rast.mosaic <- crop(rast.mosaic, aoi)
rast.mosaic[rast.mosaic > 10000] <- NA

plot(rast.mosaic)

writeRaster(rast.mosaic, paste0("output/canopy_cover_decid.tif"),
            format = "GTiff", datatype = "INT1U", overwrite = T)

havu <- raster(paste0("output/canopy_cover.tif")) - rast.mosaic
havu[havu < 0] <- 0
writeRaster(havu, paste0("output/canopy_cover_conif.tif"),
            format = "GTiff", datatype = "INT1U", overwrite = T)

havu <- havu/raster(paste0("output/canopy_cover.tif"))*100
havu[havu < 0] <- 0
havu[raster(paste0("output/canopy_cover.tif")) == 0] <- 0
writeRaster(havu, paste0("output/canopy_portion_conif.tif"),
            format = "GTiff", datatype = "INT1U", overwrite = T)

havu <- 100-havu
havu[raster(paste0("output/canopy_cover.tif")) == 0] <- 0
writeRaster(havu, paste0("output/canopy_portion_decid.tif"),
            format = "GTiff", datatype = "INT1U", overwrite = T)


# FOREST VOLUME

rs2 <- rs[grepl("tilavuus_", rs)]

dems <- c()
for(ir in rs2){
  r <- raster(ir)
  
  p2 <- st_transform(aoi, as.character(crs(r)))
  
  roi2 <- st_as_sf(st_as_sfc(st_bbox(p2)+c(-3000,-3000,3000,3000)))
  
  if(length(st_intersects(roi2, st_as_sf(st_as_sfc(st_bbox(r))))[[1]])>0){
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

p2 <- st_transform(aoi, as.character(crs(rast.mosaic)))
roi2 <- st_as_sf(st_as_sfc(st_bbox(p2)+c(-3000,-3000,3000,3000)))
rast.mosaic <- crop(rast.mosaic, roi2)

if(as.character(crs(aoi)) != as.character(crs(rast.mosaic))){
  rast.mosaic <- projectRaster(rast.mosaic, res = 16, crs = as.character(crs(p)))
}

rast.mosaic <- crop(rast.mosaic, aoi)
rast.mosaic[rast.mosaic > 10000] <- NA

plot(rast.mosaic)

writeRaster(rast.mosaic, paste0("output/forest_volume.tif"),
            format = "GTiff", datatype = "INT2U", overwrite = T)
