# ------------------------------------------------------------------------------
# Title: Oregon dataframe of proportion of each county in each WRF-Grid cell 
# Author: Jingyang Liu
# Date Created: Apr 27, 2017
# R version: 3.3.2
# ------------------------------------------------------------------------------

library(rgdal) # package for shape files
library(sp)
library(rgeos) # rgeos package contains the intersect and area commands I need
library(dplyr)
# library(data.table)
library(readr)

setwd("C:/Users/jyliu/Desktop/local_git_repo/oregon_wildfire_new/")

### First step: join the zipcode of health data with county

colnames(or_zip_county) <- c("ZIP", "county")

read_path <- paste0('./instructions/oregon_zip_county.csv')
or_zip_county <- read_csv(read_path)

# or_zip_county$county_name[which(or_zip_county$county_name=="Hood.River")] <- "Hood River"

or_zip_county <- or_zip_county %>%
  select(zip, county_name) %>%
  rename(county = county_name) %>%
  rename(ZIP = zip) %>%
  select(ZIP, county) 

county_new <- c("Washington", "Douglas", "Lane", "Deschutes")

zip_new <- data.frame(c(97003, 97471, 97475, 97703))
colnames(zip_new) <- "ZIP"

zip_new <- zip_new %>%
  mutate(county = county_new)

or_zip_county_new <- or_zip_county %>%
  bind_rows(zip_new) # 488 zip codes

grid_dir <- paste0('./shapefile/oregon_new_grid/oregon_new_grid.shp')
smoke_grid <- readOGR(dsn = grid_dir, layer = 'oregon_new_grid') 

county_dir <- paste0('./shapefile/tl_2013_us_county/tl_2013_us_county.shp')
us_county_2013 <- readOGR(dsn = county_dir, layer = 'tl_2013_us_county')
or_county_2013 <- us_county_2013[us_county_2013$STATEFP =="41",]

nad83 <- '+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0'
proj4string(or_county_2013) <- nad83

plot(or_county_2013,border="red")
plot(smoke_grid, add=T)

### Choose one sample for testing ----------------------------------------------
test_county <- or_county_2013[or_county_2013$NAME == "Yamhill",]
plot(test_county, border="red")
plot(smoke_grid, add = T)
invisible(text(getSpPPolygonsLabptSlots(smoke_grid), 
               labels=as.character(smoke_grid$WRFGRID_ID)))

wrf_grid_306 <- smoke_grid[smoke_grid@data$WRFGRID_ID == 306, ]
wrf_grid_307 <- smoke_grid[smoke_grid@data$WRFGRID_ID == 307, ]

poly_306 <- SpatialPolygons(wrf_grid_306@polygons)
shape_county <- SpatialPolygons(test_county@polygons)

county_306_int <- gIntersection(poly_306, shape_county)

prop_306_int <- gArea(county_306_int)/gArea(poly_306)
prop_306_int # 79.24% of grid is covered by zip

# now what about grid 307
poly_307 <- SpatialPolygons(wrf_grid_307@polygons)
shape_county <- SpatialPolygons(test_county@polygons)

county_307_int <- gIntersection(poly_307, shape_county)

prop_307_int <- gArea(county_307_int)/gArea(poly_307)
prop_307_int # 22.85% of grid is covered by zip



### Calculate all the proportion of WRF Grid and each county -------------------
or_county_name <- sort(unique(or_zip_county$county))
length(or_county_name)
wrf_grid_name <- as.character(smoke_grid@data$WRFGRID_ID)
length(wrf_grid_name)
tail(wrf_grid_name, 50L)

# empty matrix
county_wrf_proportion <- matrix(nrow = 36, ncol = 1575, byrow = T,
                             dimnames = list(or_county_name, wrf_grid_name))


start <- proc.time()

# first I want to subset out each county shapefile
for(i in 1:36){
  # output each grid and create a polygon
  county_shape <- or_county_2013[or_county_2013$NAME == or_county_name[i], ]
  # convert to polygon
  county_poly <- SpatialPolygons(county_shape@polygons)
  
  # now I can create the second loop that finds the proportion of the area of
  # the county polygon that overlaps with each WRF-Grid
  for(j in 1:1575){
    # output each grid and create a polygon
    wrf_grid <- smoke_grid[smoke_grid@data$WRFGRID_ID == j, ]
    # now what about grid 719; should be much less
    wrf_poly <- SpatialPolygons(wrf_grid@polygons)
    
    
    county_wrf_intersect <- gIntersection(wrf_poly, county_poly)
    # if empty, then set to 0, else find the proportion
    grid_prop <- ifelse(is.null(county_wrf_intersect),
                        0, gArea(county_wrf_intersect)/gArea(wrf_poly))
    # populate the matrix based on i position and j position
    county_wrf_proportion[[i,j]] <- grid_prop
  }
}

# stop time
stop <- proc.time() - start
stop # 60.98 secs

### Check
which(or_county_name=="Yamhill") # 36
county_wrf_proportion[36, 306:307]
# 306       307 
# 0.7923660 0.2285243 

county_wrf_prop_df <- data.frame(county_wrf_proportion)

county_wrf_prop_df <- county_wrf_prop_df %>%
  mutate(county = rownames(county_wrf_prop_df)) %>%
  select(county, 1:1575)

# file_name <- paste('./data_new/county_data/or_county_wrf_prop.csv') # previous file

file_name <- paste('C:/Users/jyliu/Desktop/local_git_repo/oregon_wildfire_new/data_new/county_data/or_county_wrf_prop.csv') # new shapefile
write_csv(county_wrf_prop_df, file_name)



