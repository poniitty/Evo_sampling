

library(tidyverse)
library(sf)
library(dplyr)
library(raster)
library(maptools)
library(fasterize)
library(sp)
library(rgdal)

###################################################################################
#

list.files(pattern = ".tif$", full.names = T)

aoi <- st_read(paste0("study_area_", lyh, ".shp"))

swi <- raster("output/swi.tif")
dem <- raster("output/dem.tif")/100
pisr <- raster("output/pisr.tif")
tpi <- raster("output/tpi.tif")
puu <- raster("output/canopy_cover.tif")
lpuu <- raster("output/canopy_portion_decid.tif")
edge <- log(raster("output/distance_to_pools.tif"))

lpuu <- resample(lpuu, swi)
puu <- resample(puu, swi)
pisr <- resample(pisr, swi)

plot(pisr)

s <- stack(swi, dem, lpuu, puu, edge, pisr, tpi)
names(s) <- c("swi", "dem", "lpuu", "puu", "edge", "pisr", "tpi")
plot(s)

aoi <- raster("output/aoi.tif")

s <- mask(s, aoi)

###################################################################################
#install.packages("iSDM")
library(iSDM)

s <- aggregate(s, 5)

reps <- 50

probs <- stack()
for(i in 1:reps){
  print(i)
  
  EnvS <- s
  SCD2 <- s[[1]]
  EnvS[sample(1:ncell(SCD2), ncell(SCD2)*0.5)] <- NA
  
  Mysampling1<-eSample(EnvS, nExpect=100, plot=F,
                       saveShape=F, nf=3, lowerLim=0, upperLim=1)
  
  Mysampling1[[1]]$ID2 <- 1
  
  tempr <- rasterize(Mysampling1[[1]], EnvS[[1]])
  
  probs <- stack(probs, tempr[["ID2"]])
}

grepl("ID2", names(probs))
probR <- sum(probs[[names(probs)[grepl("ID2", names(probs))]]], na.rm = T)
probR <- probR/maxValue(probR)
probR[probR == 0] <- NA
plot(probR)

names(probR) <- "prob"

writeRaster(probR, "samples/sample_prob_surface.tif", format = "GTiff", overwrite = T)

probdf <- rasterToPoints(probR, spatial = T, fun=function(x){!is.na(x)})
plot(probdf)

Empty <- probdf[probdf$prob == max(probdf$prob)+1,]

tempdf <- probdf

nbrpersqr <- 100

repeat {
  samp <- tempdf[sample(1:length(tempdf), 1, prob = tempdf$prob),]
  
  temp <- rbind.SpatialPointsDataFrame(Empty, samp)
  
  if(length(temp) == 1){
    Empty <- rbind.SpatialPointsDataFrame(Empty, samp)
  } else {
    if(min(dist(temp@coords, method = "euclidean")) >= 100){
      Empty <- rbind.SpatialPointsDataFrame(Empty, samp)
    }
  }
  
  if (NROW(Empty) == nbrpersqr) break
}


plot(s[[1]], col=rev(bpy.colors(100)))
plot(Empty, add=TRUE, col=1, pch=19)

values <- extract(s, Empty)

hist(values[,1])
hist(values[,2])
hist(values[,3])
hist(values[,4])
hist(values[,5])
hist(values[,6])
hist(values[,7])

Empty$id <- 1:nrow(Empty)

st_write(st_as_sf(Empty), "samples/Sample100_100m.gpkg")


