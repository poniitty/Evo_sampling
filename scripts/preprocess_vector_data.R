
library(sf)
library(tidyverse)

aoi <- st_read("output/aoi.gpkg")

shps <- list.files("data/maastotietokanta/", pattern = "^s_", full.names = T)
shps <- shps[grepl("\\.shp$", shps)]

for(i in shps){
  print(i)
  
  p <- st_read(i) %>% 
    st_transform(st_crs(aoi))
  
  if(i == shps[1]){
    nature_reserves <- p
  } else {
    nature_reserves <- bind_rows(nature_reserves, p)
  }
  
}

nature_reserves <- nature_reserves %>% filter(TASTAR != 10000) %>% 
  filter(!(TASTAR == 30000 & LUOKKA == 72200)) %>% 
  group_by(RYHMA) %>% 
  summarise(RYHMA = median(RYHMA, na.rm = TRUE))

nature_reserves <- st_crop(nature_reserves, aoi)


plot(st_geometry(nature_reserves))

st_write(nature_reserves, "output/nature_reserves.gpkg")

# Roads


shps <- list.files("data/maastotietokanta/", pattern = "^l_", full.names = T)
shps <- shps[grepl("\\.shp$", shps)]

for(i in shps){
  print(i)
  
  p <- st_read(i) %>% 
    st_transform(st_crs(aoi))
  
  if(i == shps[1]){
    roads <- p
  } else {
    roads <- bind_rows(roads, p)
  }
  
}

roads <- st_crop(roads, aoi)


plot(st_geometry(roads))

st_write(roads, "output/roads.gpkg")

# buildings


shps <- list.files("data/maastotietokanta/", pattern = "^r_", full.names = T)
shps <- shps[grepl("\\.shp$", shps)]

for(i in shps){
  print(i)
  
  p <- st_read(i) %>% 
    st_transform(st_crs(aoi))
  
  if(i == shps[1]){
    buildings <- p
  } else {
    buildings <- bind_rows(buildings, p)
  }
  
}

buildings <- st_crop(buildings, aoi)


plot(st_geometry(buildings))

st_write(buildings, "output/buildings.gpkg")



