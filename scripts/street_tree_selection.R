
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

# Generate a 1.5 km maximum walking distance from a metro stop:

st_buffer(dist = 1500)

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

# Conduct spatial clustering to reduce the number of potential sampling
# locations:

oak_maple_clust <-
  oak_maple_subset %>% 
  spatialsample::spatial_clustering_cv(
    v = 100,
    cluster_function = "hclust")

oak_maple_clust <-
  oak_maple_subset %>% 
  spatialsample::spatial_block_cv(
    v = 100)

tm_shape(oak_maple_subset) +
  tm_markers() 


