
library(tidyverse)
library(sf)
library(raster)
library(ggforce)
library(scales)

# read points
p <- st_read("samples/Evo_Tutkimuspisteet_FMI.shp")
p %>% filter(fid_ %in% c(0,2)) -> p

# read roads
ro <- st_read("output/roads.gpkg")

# rasters

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


# Extract raster info to points

p <- bind_cols(p, as_data_frame(raster::extract(s, p))) %>% 
  drop_na()

plot(s[[1]])
plot(st_geometry(p), pch = 20, add = T)

# PCA scores based on env data

pca <- princomp(st_drop_geometry(p)[names(s)])
pca$scores

p <- bind_cols(p, as_data_frame(pca$scores))

p %>% mutate(across(starts_with("Comp"), ~rescale(.x, to = c(0, 1)),
                    .names = "scaled1_{.col}")) %>% 
  mutate(across(starts_with("Comp"), ~rescale(.x, to = c(0, 1), from = range(p$Comp.1)),
                .names = "scaled2_{.col}")) %>% 
  mutate(across(starts_with("Comp"), ~scale(.x),
                .names = "scaled3_{.col}")) -> p

# PCA scores to RGB codes
p %>% rowwise() %>% 
  mutate(rgb1 = rgb(red = scaled1_Comp.1, green = scaled1_Comp.2, blue = scaled1_Comp.3, maxColorValue = 1)) %>% 
  mutate(rgb2 = rgb(red = scaled2_Comp.1, green = scaled2_Comp.2, blue = scaled2_Comp.3, maxColorValue = 1)) %>% 
  mutate(outlier_score = sum(abs(across(starts_with("scaled3"))))) %>%
  ungroup() -> p

# Unsupervised clustering based on points' locations

p$kmeans <- kmeans(st_coordinates(p), centers = 15, nstart = 100)$cluster
plot(p["kmeans"], pch = 20)


p %>% mutate(x = st_coordinates(p)[,"X"],
             y = st_coordinates(p)[,"Y"]) -> p


st_write(p, "samples/sites_to_consider.gpkg")

# Plots
dev.off()
plot(crop(s[[2]], st_bbox(p)+c(-50,-50,50,50)), box = F, axes = F)
plot(st_geometry(p), pch = 20, add = T)
plot(st_geometry(ro %>% st_crop(., st_bbox(p)+c(-50,-50,50,50))), add = T, col = "gray50")

p %>% 
  mutate(kmeans = as_factor(kmeans)) %>% 
  ggplot(aes(x = x, y = y, color = kmeans))+
  geom_point()+
  geom_mark_ellipse()+
  theme_void()+
  theme(legend.position="none")

p %>% 
  mutate(kmeans = as_factor(kmeans)) %>% 
  ggplot(aes(x = x, y = y, color = kmeans))+
  geom_point(color = p$rgb1, size = 3) +
  geom_mark_ellipse(aes(label=kmeans),label.colour="grey30")+
  theme_void()+
  theme(legend.position="none")

p %>% 
  mutate(kmeans = as_factor(kmeans)) %>% 
  ggplot(aes(x = x, y = y))+
  geom_mark_ellipse(aes(group = kmeans))+
  geom_point(aes(color = outlier_score), size = 3)+
  scale_colour_gradient2(midpoint = 6.5,
                         low = "white",
                         mid = "blue",
                         high = "red")+
  theme_void()
  