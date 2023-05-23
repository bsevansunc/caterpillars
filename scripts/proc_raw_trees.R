# This file generates dc_street_trees.geojson from the original
# Urban_Forestry_Street_Trees.geojson at
# https://opendata.dc.gov/datasets/DCGIS::urban-forestry-street-trees/explore?location=38.904146%2C-77.011668%2C13.37

# setup -------------------------------------------------------------------

library(sf)
library(tidyverse)


# Read in file:

st_read("data/raw/Urban_Forestry_Street_Trees.geojson") %>% 
  
  # Names to lower:
  
  set_names(
    tolower(
      names(.))) %>% 
  
  # Subset by row: 
  
  filter(
    
    # Keep only trees with recorded measurements:
    
    if_all(
      c(dbh, mbg_width:crown_area),
      ~ !is.na(.x)),
    
    # Remove trees that are on private land:
    
    ownership != "Private",
    
    # Keep only wards 1-6 (safety and travel):
    
    ward %in% 1:6) %>% 
  
  # Subset by variable:
  
  select(objectid, 
         vicinity,
         sci_nm,
         cmmn_nm,
         dbh,
         max_crown_height:crown_area) %>% 
  
  # Write to file:
  
  st_write("data/processed/dc_street_trees.geojson")
