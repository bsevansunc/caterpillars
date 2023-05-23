# Simple script to view NN sites being used for caterpillar counting. Sites
# do not include ZURLBECMD2, but I can add that later.

# setup -------------------------------------------------------------------

# Load libraries:

library(sf)
library(tmap)
library(tidyverse)

# Set mode to view (must use RStudio here):

tmap_mode("view")


# prep data ---------------------------------------------------------------

# Read in file:

cc_nn_sites <-
  write_csv(
    selected_sites,
    "data/caterpillar_nn_sites.csv") %>% 
  
  # Convert to spatial:
  
  st_as_sf(
    coords = c("long", "lat"),
    crs = 4326) %>% 
  
  # Show distance to the zoo:
  
  mutate(
    zoo_dist =
      st_distance(
        ., 
        tibble(
          long = -77.05305, 
          lat = 38.92837) %>% 
          st_as_sf(
            coords = c("long", "lat"),
            crs = 4326)) %>% 
      {./1000} %>% 
      as.numeric())

# make interactive map ----------------------------------------------------

selected_sites %>% 
  st_as_sf(
    coords = c("long", "lat"),
    crs = 4326) %>% 
  tm_shape() +
  tm_dots()
