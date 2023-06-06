# This script determines potential sampling locations using DC street tree 
# data. The output is potential sampling points where:
#
# * The centroids of each sampling circle are oak or maple;
# * Centroids are located at a minimum distance of 50 m from each other 
#.  (Note: The minimum distance, in practice is 106.6 m);
# * The basal area of the sampled trees are at or above the median basal area
#.  across the data;
# * Sampling locations are classified by proportion of the total basal area
#.  made up by oaks and maples.

# setup -------------------------------------------------------------------

library(sf)
library(spatialsample)
library(tmap)
library(tidyverse)

tmap_mode("view")

# metro stop data ---------------------------------------------------------

# Metro station points:

metro_pts <-
  st_read("data/raw/Metro_Stations_Regional.geojson") %>% 
  
  # Subset to stations of interest:
  
  filter(
    NAME %in% c(
      "Eastern Market",
      "Woodley Park-Zoo/Adams Morgan")) %>% 
  
  # Better names:
  
  mutate(
    metro = 
      if_else(
        str_detect(.$NAME, "Eastern"), 
        NAME,
        "Woodley Park"),
    geometry,
    .keep = "none") %>% 
  
  # Convert to UTM 18N for faster processing:
  
  st_transform(crs = 32618)

# Metro stations buffered to walkable locations:

metro_buffer <-
  metro_pts %>% 
  
  # Generate a 1 km maximum walking distance from a metro stop:
  
  st_buffer(dist = 1000)

# tree data ---------------------------------------------------------------

# Get trees:

trees <-
  st_read("data/processed/dc_street_trees.geojson") %>% 
  
  # Subset to variables of interest:
  
  select(objectid, 
         sci_nm, 
         cmmn_nm, 
         dbh) %>% 
  
  # Convert to UTM 18N for faster processing:
  
  st_transform(crs = 32618) %>% 
  
  # Subset to trees within buffer distance of the metro:
  
  st_filter(metro_buffer) %>% 
  
  st_join(metro_buffer)

# defining circles --------------------------------------------------------

# Get oaks and maples as potential centroids:

oak_maple <-
  filter(
    trees,
    str_detect(sci_nm, "^Quercus|^Acer"))

# Subset to the five nearest trees to an oak or a maple and define the
# circle by the center tree (takes a long time to run):

potential_circles <- 
  oak_maple %>% 
  pull(objectid) %>% 
  map_dfr(
    ~ trees %>% 
      
      # Remove trees that could not be identified:
      
      filter(
        !str_detect(sci_nm, "Other"),
        !str_detect(sci_nm, "species")) %>% 
      
      # Calculate the distance to the centroid:
      
      mutate(
        center_dist = 
          st_distance(., 
                      {oak_maple %>% 
                          filter(objectid == .x)})) %>% 
      
      # Subset to the 5 nearest trees:
      
      slice_min(center_dist, n = 5) %>% 
      
      # Add the centroid_id:
      
      mutate(centroid_id = .x, .before = objectid))

# classifying circles -----------------------------------------------------

# For each circle, define composition by oaks and maples and basal area:

circles_classified <-
  potential_circles %>% 
  as_tibble() %>% 
  
  # Calculate the total basal area and basal area of oaks and maples:
  
  summarize(
    basal_area_total = sum(dbh^2 * 0.005454),
    basal_area_oak = sum(dbh[str_detect(sci_nm, "Quercus")]^2 * 0.005454),
    basal_area_maple = sum(dbh[str_detect(sci_nm, "Acer")]^2 * 0.005454),
    .by = centroid_id) %>% 
  
  # Convert to a percentage:
  
  mutate(
    across(
      basal_area_oak:basal_area_maple,
      ~ .x/basal_area_total * 100)) %>% 
  
  # Subset to circles with at least the median total basal area:
  
  filter(
    basal_area_total >= median(basal_area_total)) %>% 
  
  # Assign classes:
  
  mutate(
    cover_class =
      case_when(
        basal_area_maple > 50 & basal_area_oak > 0 ~
          "maple_dom_some_oak",
        basal_area_maple > 50 ~
          "maple_dom_no_oak",
        basal_area_oak > 50 &  basal_area_maple > 0 ~
          "oak_dom_some_maple",
        basal_area_oak > 50 ~
          "oak_dom_no_maple",
        TRUE ~ "other")) %>% 
  
  # Remove other:
  
  filter(cover_class != "other") %>% 
  
  # Convert cover_class to factor and return only relevant columns:
  
  mutate(
    centroid_id,
    cover_class = factor(cover_class),
    .keep = "none")

# Subset oaks and maples to classified circles:

oak_maple_subset <-
  oak_maple %>%
  inner_join(
    circles_classified, 
    by = join_by(objectid == centroid_id)) 

# sample circles ----------------------------------------------------------

# Sample 10 potential centroids per metro and cover class:

oak_maple_sampled <-
  oak_maple_subset %>% 
  slice_sample(
    n = 10,
    by = c(metro, cover_class))

# Buffer centroids to 50 meters and combine geometries:

centroid_buffers <-
  oak_maple_sampled %>% 
  st_buffer(50) %>% 
  st_union() %>% 
  st_cast("POLYGON") %>% 
  st_sf() %>% 
  mutate(buffer_id = row_number())

# Grab one sample per combined buffer:

potential_sample_locations <-
  oak_maple_sampled %>% 
  st_join(centroid_buffers) %>% 
  slice_sample(n = 1, by = buffer_id)

# Have a look at the data:

tm_shape(potential_sample_locations) +
  tm_dots(col = "cover_class")

# write output ------------------------------------------------------------

# Get tabular data:

potential_sample_locations %>% 
  as_tibble() %>% 
  select(objectid:cover_class) %>% 
  
  # Bind with geographic coordinates:
  
  bind_cols(
    potential_sample_locations %>% 
      
      # Convert coordinates to longitude and latitude, WGS84:
      
      st_transform(crs = 4326) %>% 
      st_coordinates()) %>% 
  
  # Rename and rearrange columns:
  
  select(
    object_id = objectid,
    cover_class,
    lon = X,
    lat = Y,
    everything()) %>% 
  
  # Write to file:
  
  write_csv("data/processed/sampled_street_trees.csv")




