

library(sf)
library(tidyverse)

outer = matrix(c(392600,6783000,
                 404400,6783000,
                 404400,6792700,
                 392600,6792700,
                 392600,6783000), ncol=2, byrow=TRUE)

df <- data.frame(
  lon = c(392600, 404400, 404400, 392600, 392600),
  lat = c(6783000, 6783000, 6792700, 6792700, 6783000)
)

polygon <- df %>%
  st_as_sf(coords = c("lon", "lat"), crs = 3067) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")

st_write(polygon, "output/aoi.gpkg")
