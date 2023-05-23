
# setup -------------------------------------------------------------------

library(sf)
library(spatialsample)
library(tmap)
library(tidyverse)

tmap_mode("view")

# initial data processing -------------------------------------------------

# Metro stations to subset to walkable locations:

metro <-
  st_read("data/raw/Metro_Stations_Regional.geojson") %>% 
  
  # Subset to stations of interest:
  
  filter(
    NAME %in% c(
      "Cleveland Park",
      "Columbia Heights",
      "Dupont Circle",
      "Eastern Market",
      "Farragut North",
      "Farragut West",
      "Foggy Bottom-GWU",
      "Gallery Pl-Chinatown",
      "Judiciary Square",
      "L'Enfant Plaza",
      "Mt Vernon Sq 7th St-Convention Center",
      "NoMa-Gallaudet U",
      "Smithsonian",
      "Takoma",
      "Tenleytown-AU",
      "U Street/African-Amer Civil War Memorial/Cardozo",
      "Union Station",
      "Van Ness-UDC",
      "Woodley Park-Zoo/Adams Morgan")) %>% 

# Generate a 1 km maximum walking distance from a metro stop:

st_buffer(dist = 1000)

# Get trees:

trees <-
  st_read("data/processed/dc_street_trees.geojson") %>% 
  
  # Subset to variables of interest:
  
  select(objectid, sci_nm, cmmn_nm) %>% 
  
  # Subset to trees within 1.5 km of the metro:
  
  st_filter(metro) %>% 
  
  # Convert to UTM 18N for faster processing:
  
  st_transform(crs = 32618)

# Remove metro layer:

rm(metro)

# Get oaks and maples as centroids:

oak_maple <-
  trees %>% 
  filter(
    str_detect(sci_nm,
               "^Quercus|^Acer"))

# subset with oaks/maples as centroids ------------------------------------

oak_maple_subset <-
  oak_maple %>% 
  semi_join(
    oak_maple %>% 
      
      # Get number of trees within 25 m of centroid:
      
      st_buffer(dist = 25) %>% 
      st_join(
        trees %>% 
          select(join_id = objectid)) %>% 
      as_tibble() %>% 
      summarize(
        n = n(),
        .by = objectid) %>% 
      
      # Subset to center trees with at least 5 nearby trees:
      
      filter(n >= 5),
    by = "objectid")

# Aggregate points to reduce the number of potential sampling
# locations:

hex_grid <-
  st_make_grid(oak_maple_subset,
               cellsize = 200,
               what = "polygons",
               square = FALSE) %>% 
  st_as_sf() %>% 
  st_filter(oak_maple_subset) %>% 
  mutate(grid_id = row_number())

# Take a look at the number of samples available per grid cell:

hex_grid %>% 
  inner_join(
    st_join(
      oak_maple_subset,
      hex_grid) %>% 
      as_tibble() %>% 
      summarize(
        n = n(),
        .by = grid_id)) %>% 
  filter(n > 10) %>% 
  tm_shape(name = "hex") +
  tm_polygons(col = "n")

