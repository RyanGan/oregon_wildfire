# ------------------------------------------------------------------------------
# Title: Oregon dataframe of proportion of each zipcode in each WRF-Grid cell 
# Author: Jingyang Liu
# Date Created: Mar 10, 2017
# R version: 3.3.2
# ------------------------------------------------------------------------------

library(rgdal) # package for shape files
library(sp)
library(rgeos) # rgeos package contains the intersect and area commands I need
library(dplyr)
library(data.table)
library(readxl)
# parallel computing libraries
library(foreach) 
library(doParallel)
library(readr)

getwd()
# "/home/jyliu/wildfire/local_git_repo/oregon_wildfire/r_scripts"

# Import Shapefiles  -----------------------------------------------------------
# WRF Grid
grid_dir <- paste0('../../../data/data_new/or_shapefile/oregon_grid.shp')

smoke_grid <- readOGR(dsn = grid_dir, layer = 'oregon_grid') 

summary(smoke_grid) 
summary(smoke_grid@data$WRFGRID_ID)

# Zipcode shapefile
shp_dir <- paste0('../../../data/data_original/', 
                 'tl_2013_us_zcta510/tl_2013_us_zcta510.shp')

us_zip_2013 <- readOGR(dsn = shp_dir, layer = 'tl_2013_us_zcta510')
summary(us_zip_2013)
# plot(us_zip_2013)


### whole oregon zip code
read_path <- paste0('../../../data/data_original/oregon_zipcode.csv')
or_zip <- read_csv(read_path)
names(or_zip) <- c('zip','type','city','county','area')

# check on zip code in colorado range
# removing anyways

# limit to just Colorado state zipcodes
or_zip_map <- us_zip_2013[us_zip_2013$ZCTA5CE10 %in% or_zip$zip,]


# output zipcodes from washington zipcode map to bind values to
# plot map to check
plot(or_zip_map)

# saving wash shapefile ----
# save shapefile to use in future work
summary(or_zip_map)
or_zip <- as.character(sort(or_zip_map@data$ZCTA5CE10))


# save the Oregon zips to a shapefile to use later
# create save path
save_path <- paste0('../../../data/data_new/or_zip_2013_shape_files')

writeOGR(or_zip_map, layer = 'or_zip_2013_shape_files', save_path, driver = "ESRI Shapefile")


# Set coordinate reference system for smoke gird
nad83 <- '+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0'
proj4string(smoke_grid) <- nad83

# Plot overlay
plot(smoke_grid)
plot(or_zip_map, add=T)
# looks like they overlay pretty well, same projections
summary(smoke_grid)

# Test code to figure out proportion calculations in each WRF-Grid -------------
# Trying 'over' function in sp package
# limit to a specific zip code
test_zip <- c(97405)
test_zip_map <- or_zip_map[or_zip_map$ZCTA5CE10 %in% test_zip,]

plot(test_zip_map)
plot(smoke_grid, add = T)
invisible(text(getSpPPolygonsLabptSlots(smoke_grid), 
               labels=as.character(smoke_grid$WRFGRID_ID)))


# smoke grid over test zip map
test_grid <- over(smoke_grid, test_zip_map)
summary(test_grid) # 14 grids over the zip code
test_grid2 <- test_grid %>% filter(!is.na(ZCTA5CE10))
test_grid2


# test zip map over zip grid
plot(smoke_grid)
plot(test_zip_map, add = T)
zip_over_grid <- over(test_zip_map, smoke_grid)
summary(zip_over_grid) # this way retains the values

# try gIntersection function from rgeos
# http://stackoverflow.com/questions/35039614/r-calculate-overlapping-section-
# polygon-intersection-the-fast-way

shape_zip <- SpatialPolygons(test_zip_map@polygons)
shape_grid <- SpatialPolygons(smoke_grid@polygons)
plot(shape_zip)
plot(shape_grid, add = T)
# try and plot the values for this zipcode for 9/21
invisible(text(getSpPPolygonsLabptSlots(smoke_grid), 
               labels=as.character(smoke_grid$WRFGRID_ID)))

# output the WRF Grid 261 and 262 for test
wrf_grid_261 <- smoke_grid[smoke_grid@data$WRFGRID_ID == 261, ]
plot(wrf_grid_261)

wrf_grid_262 <- smoke_grid[smoke_grid@data$WRFGRID_ID == 262, ]
plot(wrf_grid_262)

# calculate the area of grid 297
gArea(SpatialPolygons(wrf_grid_261@polygons))
gArea(SpatialPolygons(wrf_grid_262@polygons))


# Subset to one zip code and two different WRF grids ---------------------------
# plot zip and 2 grids
plot(test_zip_map)
#invisible(text(getSpPPolygonsLabptSlots(test_zip_map), 
#               labels=as.character(test_zip_map$ZCTA5CE10)))
plot(wrf_grid_261, add = T)
invisible(text(getSpPPolygonsLabptSlots(wrf_grid_261), 
               labels=as.character(wrf_grid_261$WRFGRID_ID)))
plot(wrf_grid_262, add=T)
invisible(text(getSpPPolygonsLabptSlots(wrf_grid_262), 
               labels=as.character(wrf_grid_262$WRFGRID_ID)))

# Start with the intersection with wrf grid 297 and zipcode
# first I need to convert the spatial polygon to just polygon
poly_261 <- SpatialPolygons(wrf_grid_261@polygons)
shape_zip <- SpatialPolygons(test_zip_map@polygons)

zip_261_int <- gIntersection(poly_261, shape_zip)

plot(zip_261_int)
prop_261_int <- gArea(zip_261_int)/gArea(poly_261)
prop_261_int # 41.8% of grid is covered by zip

# now what about grid 296
poly_262 <- SpatialPolygons(wrf_grid_262@polygons)
shape_zip <- SpatialPolygons(test_zip_map@polygons)

zip_262_int <- gIntersection(poly_262, shape_zip)

plot(zip_262_int)
prop_int_262 <- gArea(zip_262_int)/gArea(poly_262)
prop_int_262 # 6.0% of grid is covered by zip



# Setup for parallel computing before for loop ---------------------------------
cores <- detectCores() # 48
cl <- makeCluster(cores) # use half the cores on the vet cluster
registerDoParallel(cl)
# load packages on each cluster
clusterCall(cl, function() library(rgdal))
clusterCall(cl, function() library(sp))
clusterCall(cl, function() library(rgeos))
# since I have another foreach loop, I need to load foreach on the clusters
#clusterCall(cl, function() library(doParallel))
#clusterCall(cl, function() library(foreach))

clusterExport(cl, "or_zip_map")
clusterExport(cl, "smoke_grid")

# Loop to estimate proportion of area covered by each grid for each zip --------
# I'm expecting a matrix of 489 zipcodes * 1610 wrf_grids 
or_zip_name <- or_zip
length(or_zip_name)
wrf_grid_name <- as.character(smoke_grid@data$WRFGRID_ID)
length(wrf_grid_name)
tail(wrf_grid_name, 50L)
# empty matrix
zip_wrf_proportion <- matrix(nrow = 417, ncol = 1610, byrow = T,
                             dimnames = list(or_zip_name, wrf_grid_name))

# matrix should be faster and less memory than a df
# start time

start <- Sys.time()

# first I want to subset out each zipcode shapefile
foreach(i=1:10) %dopar% {
  # output value of zipcode
  zipcode <- as.character(or_zip_name[i]) 
  # limit shapefile to particular zipcode
  zip_shape <- or_zip_map[or_zip_map$ZCTA5CE10 %in% zipcode, ]
  # convert to polygon
  zip_poly <-SpatialPolygons(zip_shape@polygons)

  plot(zip_poly)
}
  


  # now I can create the second loop that finds the proportion of the area of
  # the zipcode polygon that overlaps with each WRF-Grid
  for(j in 1:length(wrf_grid_name)){
    # output each grid and create a polygon
    wrf_grid <- smoke_grid[smoke_grid@data$WRFGRID_ID == j, ]
    # now what about grid 719; should be much less
    wrf_poly <- SpatialPolygons(wrf_grid@polygons)
    
    
    zip_wrf_intersect <- gIntersection(wrf_poly, zip_poly)
    # if empty, then set to 0, else find the proportion
    grid_prop <- ifelse(is.null(zip_wrf_intersect),
                        0, gArea(zip_wrf_intersect)/gArea(wrf_poly))
    # populate the matrix based on i position and j position
    zip_wrf_proportion[[i,j]] <- grid_prop
  }
}

# stop cluster
stopCluster(cl)

# stop time
stop <- Sys.time() - start
stop # 33.97498 mins

zip_proportion_df <- data.frame(zip_wrf_proportion)

write_path <- paste0('../../../data/data_new/',
                     'zip_wrf_proportion.csv')
write_csv(zip_proportion_df, paste0(write_path))










