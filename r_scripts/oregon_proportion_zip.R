# ------------------------------------------------------------------------------
# Title: Oregon dataframe of proportion of each zipcode in each WRF-Grid cell 
# Author: Jingyang Liu
# Date Created: Mar 10, 2017
# R version: 3.3.2
# ------------------------------------------------------------------------------

library(rgdal) # package for shape files
library(sp)
library(rgeos) # rgeos package contains the intersect and area commands I need
library(tidyverse)
library(data.table)
library(readxl)

getwd()
setwd("C:/Users/jyliu/Desktop/local_git_repo/oregon_wildfire")

# Import Shapefiles  -----------------------------------------------------------
# WRF Grid
grid_dir <- paste0('./shapefile/oregon_grid.shp')

smoke_grid <- readOGR(dsn = grid_dir, layer = 'oregon_grid') 

summary(smoke_grid) 
plot(smoke_grid) 

# Zipcode shapefile
shp_dir <- paste0('./shapefile/tl_2013_us_zcta510/tl_2013_us_zcta510.shp')

us_zip_2013 <- readOGR(dsn = shp_dir, layer = 'tl_2013_us_zcta510')
summary(us_zip_2013)
# plot(us_zip_2013)

## import data
# because it use overall ZIP, so I filter the full data set with same requirements 
# of separate disease.
var_list <- c('respiratory', 'asthma', 'pneumonia',  'acute_bronch', 'copd', 
              'cvd', 'isch_heart_dis', 'arrhythmia', 'heart_failure', 
              'cerbrovas_dis', 'myocardial_infarc', 'broken_arm')

setwd("C:/Users/jyliu/Desktop/local_git_repo/oregon_wildfire/data_new")

or_zip_2013 <- data_frame()
n <- 0

start <- Sys.time()
for(m in var_list){ # begin first loop of variable names (outcomes)
  
  read_path <- paste('oregon', m, 'jul_to_oct_claim.csv', sep='_')
  or_disease <- read_csv( read_path)
  
  or_disease$dx11 <- as.character(or_disease$dx11)
  or_disease$dx12 <- as.character(or_disease$dx12)
  
  # iteration which binds rows of unique ids
  or_zip_2013 <- bind_rows(or_zip_2013, or_disease)
  n <- n + length((or_disease$personkey))
  
}  

total_time <- Sys.time() - start
total_time # Time difference of 0.8644149 secs



### ----------------------------------------------------------------------------
# convert to character vector
or_zip_2013$ZIP <- as.character(or_zip_2013$ZIP)

# check on zip code in colorado range
# removing anyways
# co_zip_2012 <- filter(co_zip_2012, ZIP>=80001&ZIP<=81658)

# limit to just Colorado state zipcodes
or_zip_map <- us_zip_2013[us_zip_2013$ZCTA5CE10 %in% or_zip_2013$ZIP,]

# output zipcodes from washington zipcode map to bind values to
# plot map to check
plot(or_zip_map)

# saving wash shapefile ----
# save shapefile to use in future work
summary(or_zip_map)

# save the Colorado zips to a shapefile to use later
# create save path
save_path <- paste0('../shapefile/or_zip_2013_shape_files')

writeOGR(or_zip_map, layer = 'or_zip_2013_shape_files', save_path, driver = "ESRI Shapefile")






















#
# Set coordinate reference system for smoke gird
nad83 <- '+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0'
proj4string(smoke_grid) <- nad83

# Plot overlay
plot(smoke_grid)
plot(co_zip_map, add=T)
# looks like they overlay pretty well, same projections

# Test code to figure out proportion calculations in each WRF-Grid -------------
# Trying 'over' function in sp package
# limit to a specific zip code
test_zip <- c(80521)
test_zip_map <- co_zip_map[co_zip_map$ZCTA5CE10 %in% test_zip,]

plot(test_zip_map)
plot(smoke_grid, add = T)
invisible(text(getSpPPolygonsLabptSlots(smoke_grid), 
               labels=as.character(smoke_grid$WRFGRID_ID)))













