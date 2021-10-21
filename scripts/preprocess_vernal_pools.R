
library(raster)
library(sf)
library(tidyverse)
library(Rsagacmd)

saga <- saga_gis()

dem <- raster("output/dem.tif")

p <- st_read("data/other_shapes/vernal_pools_evo.shp")
p$id <- 1

p <- rasterize(p, dem, field = "id")

pd <- saga$grid_tools$proximity_grid(features = p)

plot(log(pd$distance))

writeRaster(round(pd$distance), "output/distance_to_pools.tif",
            datatype = "INT2U", format = "GTiff")
