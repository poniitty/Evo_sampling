library(raster)
library(sf)
library(tidyverse)
library(fasterize)

r <- raster("output/dem.tif")

# Clip focus areas

nr <- st_read("output/nature_reserves.gpkg")
nr$id <- 1
nr <- fasterize(nr, r, "id")
plot(nr)


tm <- st_read("data/other_shapes/HAMK_Evo.shp")
tm$id <- 1
tm <- fasterize(tm, r, "id")
plot(tm)

# Clip roads and buildings

ro <- st_read("output/roads.gpkg")
ro <- st_buffer(ro, 50) %>% st_cast("MULTIPOLYGON")
ro$id <- 1
ro <- fasterize(ro, r, "id")
plot(ro)


bu <- st_read("output/buildings.gpkg")
bu <- st_buffer(bu, 100) %>% st_cast("MULTIPOLYGON")
bu$id <- 1
bu <- fasterize(bu, r, "id")
plot(bu)

# FINALIZE AREAS OF INTEREST

aoi <- sum(stack(nr,tm), na.rm = T)
aoi[aoi > 1] <- 1
aoi[aoi == 0] <- NA

built <- sum(stack(ro,bu), na.rm = T)
built[built > 0] <- NA

plot(aoi)
plot(built)

aoi <- mask(aoi, built)
plot(aoi)

canopy <- raster("output/canopy_cover.tif")
canopy[!is.na(canopy)] <- 1
canopy <- resample(canopy, aoi, method = "ngb")

aoi <- mask(aoi, canopy)
plot(aoi)

writeRaster(aoi, "output/aoi.tif", datatype = "INT1U", format = "GTiff")

