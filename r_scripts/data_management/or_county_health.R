#-------------------------------------------------------------------------------
# Title: Check and exploring the unreasonable plot
# Author: Jingyang Liu                                                                   
# Date Created: April 11, 2017                                                  
# R version: 3.3.3                                                       
#-------------------------------------------------------------------------------

library(rgdal) # package for shape files
library(sp)
library(rgeos) # rgeos package contains the intersect and area commands I need
library(tidyverse)
library(data.table)
library(readxl)

getwd()
setwd("C:/Users/jyliu/Desktop/local_git_repo/oregon_wildfire")

grid_dir <- paste0('./data/Oregon/oregon_new_grid.shp')
smoke_grid <- readOGR(dsn = grid_dir, layer = 'oregon_new_grid') 

shp_dir <- paste0('./shapefile/cb_2013_us_county_500k/cb_2013_us_county_500k.shp')
us_county_2013 <- readOGR(dsn = shp_dir, layer = 'cb_2013_us_county_500k')

plot(or_county_2013)

nad83 <- '+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0'
proj4string(smoke_grid) <- nad83
plot(smoke_grid)
plot(or_county_2013, add=T)

df <- data.frame(or_county_2013)
unique(df$COUSUBNS)

or_county_2013 <- us_county_2013[us_county_2013$STATEFP =="41",]
plot(or_county_2013)




