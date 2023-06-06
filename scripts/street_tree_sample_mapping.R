
# setup -------------------------------------------------------------------

library(sf)
library(tmap)
library(tidyverse)

tmap_options(
  basemaps = 
    c(
      Canvas = "Esri.WorldGrayCanvas",
      Map = "OpenStreetMap", 
      Imagery = "Esri.WorldImagery"))

# The starting data:

street_trees <-
  read_csv("data/processed/sampled_street_trees.csv") %>% 
  mutate(
    `Cover class` = 
      factor(
        cover_class,
        labels = c("Maple dominated, no oak",
                   "Maple dominated, some oak",
                   "Oak dominateed, no maple",
                   "Oak dominated, some maple"))) %>% 
  select(!cover_class)

# Number of samples by metro and cover_class:

street_trees %>% 
  summarize(
    sample_count = n(), 
    .by = c(metro, `Cover class`))

# Mapping the data:

street_trees %>% 
  st_as_sf(
    coords = c("lon", "lat"),
    crs = 4326) %>% 
  tm_shape(name = "Street trees") +
  tm_dots(col = "Cover class",
          size = 0.08,
          palette = "Spectral",
          border.lwd = 2)
  
