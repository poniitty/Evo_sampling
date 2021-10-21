
# Topo indices

library(raster)
library(sf)
library(tidyverse)

library(Rsagacmd)

saga <- saga_gis()

dem <- raster("output/dem.tif")/100

dem <- saga$ta_preprocessor$fill_sinks_xxl_wang_liu(elev = dem)
swi <- saga$ta_hydrology$saga_wetness_index(dem = dem)

plot(swi$twi)

writeRaster(swi$twi, "output/swi.tif", format = "GTiff")

# Radiation

dem <- raster("output/dem.tif")/100
dem <- aggregate(dem, 5)

svi <- saga$ta_lighting$sky_view_factor(dem = dem, radius = 500)
plot(svi$svf)

rad <- saga$ta_lighting$potential_incoming_solar_radiation(grd_dem = dem,
                                                           grd_svf = svi$svf,
                                                           latitude = 61,
                                                           period = 2,
                                                           day = "2020-01-01",
                                                           day_stop = "2020-12-31",
                                                           days_step = 15,
                                                           hour_step = 2)

plot(rad$grd_total)

writeRaster(rad$grd_total, "output/pisr.tif", format = "GTiff")


# TPI

dem <- raster("output/dem.tif")/100

tpi <- saga$ta_morphometry$topographic_position_index_tpi(dem = dem)
plot(tpi)

writeRaster(tpi, "output/tpi.tif", format = "GTiff")

