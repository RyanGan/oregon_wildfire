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
setwd("C:/Users/jyliu/Desktop/local_git_repo/oregon_wildfire_new")

grid_dir <- paste0('./shapefile/oregon_new_grid/oregon_new_grid.shp')
smoke_grid <- readOGR(dsn = grid_dir, layer = 'oregon_new_grid') 

county_dir <- paste0('./shapefile/tl_2013_us_county/tl_2013_us_county.shp')
us_county_2013 <- readOGR(dsn = county_dir, layer = 'tl_2013_us_county')
or_county_2013 <- us_county_2013[us_county_2013$STATEFP =="41",]

### From previous work, the zip shapefie can be filter by us zip shapefile------
# zip_dir <- paste0('./shapefile/tl_2013_us_zcta510/tl_2013_us_zcta510.shp')
# us_zip_2013 <- readOGR(dsn = zip_dir, layer = 'tl_2013_us_zcta510')

# read_path <- paste0('./instructions/oregon_zipcode.csv')
# or_zip <- read_csv(read_path)
# names(or_zip) <- c('zip','type','city','county','area')
# or_zip_2013 <- us_zip_2013[us_zip_2013$ZCTA5CE10 %in% or_zip$zip,]

# save_path <- paste0('./shapefile/or_zip_2013')
# writeOGR(or_zip_2013, layer = 'or_zip_2013', save_path, driver = "ESRI Shapefile")
### ----------------------------------------------------------------------------

zip_dir <- paste0('./shapefile/or_zip_2013/or_zip_2013.shp')
or_zip_2013 <- readOGR(dsn = zip_dir, layer = 'or_zip_2013')

nad83 <- '+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0'
proj4string(smoke_grid) <- nad83

plot(or_county_2013, border="red")
plot(or_zip_2013, add=T)

county_df <- data.frame(or_county_2013)
zip_df <- data.frame(or_zip_2013)


### Test for one county: "Lane"
test_county <- or_county_2013[or_county_2013$NAME == "Lane",]
plot(test_county, border="red", lwd = 3)
plot(or_zip_2013, add = T, lwd= 2)
# invisible(text(getSpPPolygonsLabptSlots(or_zip_2013), 
#                labels=as.character(or_zip_2013$ZCTA5CE10)))

# zip code over test county
zip_over_county <- over(or_zip_2013, test_county)
summary(zip_over_county) 


# test county over zip code
plot(or_zip_2013)
plot(test_county, add = T, border="red", lwd=3)
county_over_grid <- over(test_county, or_zip_2013)
summary(county_over_grid) 


shape_county <- SpatialPolygons(test_county@polygons)
shape_zip <- SpatialPolygons(or_zip_2013@polygons)
plot(shape_county, border="red", lwd=3)
plot(shape_zip, add = T)
# try and plot the values for this zipcode for 9/21
invisible(text(getSpPPolygonsLabptSlots(or_zip_2013), 
               labels=as.character(or_zip_2013$ZCTA5CE10)))

# output the zip 97439 and 97448 for test
zip_97439 <- or_zip_2013[or_zip_2013$ZCTA5CE10 %in% c(97439), ]
plot(zip_97439)

zip_97448 <- or_zip_2013[or_zip_2013$ZCTA5CE10 %in% c(97448), ]
plot(zip_97448)

# calculate the area of zip
gArea(SpatialPolygons(zip_97439@polygons))
gArea(SpatialPolygons(zip_97448@polygons))


# Subset to one zip code and two different WRF grids ---------------------------
# plot zip and 2 grids
plot(test_county, border="red", lwd=3)
plot(zip_97439, add = T)
invisible(text(getSpPPolygonsLabptSlots(zip_97439), 
               labels=as.character(zip_97439$ZCTA5CE10)))
plot(zip_97448, add=T)
invisible(text(getSpPPolygonsLabptSlots(zip_97448), 
               labels=as.character(zip_97448$ZCTA5CE10)))

# Start with the intersection with zip 97439 and county
# first I need to convert the spatial polygon to just polygon
poly_97439 <- SpatialPolygons(zip_97439@polygons)
shape_97439 <- SpatialPolygons(test_county@polygons)

zip_97439_int <- gIntersection(poly_97439, shape_97439)

plot(zip_97439_int)
prop_97439_int <- gArea(zip_97439_int)/gArea(poly_97439)
prop_97439_int # 95.91% of grid is covered by zip

# now what about zip 97448
poly_97448 <- SpatialPolygons(zip_97448@polygons)
shape_97448 <- SpatialPolygons(test_county@polygons)

zip_97448_int <- gIntersection(poly_97448, shape_97448)

plot(zip_97448_int)
prop_97448_int <- gArea(zip_97448_int)/gArea(poly_97448)
prop_97448_int # 93.79% of grid is covered by zip


### Now calculate for all zipcode and county intersections
or_zip_name <- sort(as.character(or_zip_2013$ZCTA5CE10))
length(or_zip_name) # 417
or_county_name <- sort(as.character(or_county_2013$NAME))
length(or_county_name) # 36
tail(or_county_name, 50L)
# empty matrix
zip_county_proportion <- matrix(nrow = 417, ncol = 36, byrow = T,
                                dimnames = list(or_zip_name, or_county_name))


start <- proc.time()

# first I want to subset out each zipcode shapefile
for(i in 1:417){
  # output value of zipcode
  # i = 226 (97439)
  zipcode <- as.character(or_zip_name[i]) 
  # limit shapefile to particular zipcode
  zip_shape <- or_zip_2013[or_zip_2013$ZCTA5CE10 %in% zipcode, ]
  # convert to polygon
  zip_poly <-SpatialPolygons(zip_shape@polygons)
  
  # now I can create the second loop that finds the proportion of the area of
  # the zipcode polygon that overlaps with each WRF-Grid
  for(j in 1:36){
    # j = 20 (Lane)
    # output each grid and create a polygon
    county_shape <- or_county_2013[or_county_2013$NAME == or_county_name[j], ]
    # convert to polygon
    county_poly <- SpatialPolygons(county_shape@polygons)
    
    
    zip_county_intersect <- gIntersection(zip_poly, county_poly)
    # if empty, then set to 0, else find the proportion
    grid_prop <- ifelse(is.null(zip_county_intersect),
                        0, gArea(zip_county_intersect)/gArea(zip_poly))
    # populate the matrix based on i position and j position
    zip_county_proportion[[i,j]] <- grid_prop
  }
}

# stop time
stop <- proc.time() - start
stop # 33.17 secs

not_match <- which((zip_county_proportion > 0)&(zip_county_proportion < 1), arr.ind=T )
dim(not_match) # 220

zip_county_prop_df <- data.frame(zip_county_proportion)

zip_county_prop_df2 <- zip_county_prop_df %>%
  mutate(county_order = NA) %>%
  mutate(county_name = NA)


for (i in 1:length(zip_county_prop_df[,1])){
  find_order <- which(zip_county_prop_df[i,]==(max(zip_county_prop_df[i,], na.rm=TRUE)))
  zip_county_prop_df2$county_order[i] <- find_order
  zip_county_prop_df2$county_name[i] <- colnames(zip_county_prop_df2[zip_county_prop_df2$county_order[i]])
}

zip_county_prop_df2 <- zip_county_prop_df2 %>%
  mutate(zip=rownames(zip_county_prop_df))

### Check using the zip county file from website
or_check_county <- read_csv('./instructions/oregon_zip_county.csv')
head(or_check_county)
names(or_check_county) <- c('zip','city','county','area')

or_check_county_df <- data.frame(or_check_county)

or_check <- zip_county_prop_df2 %>%
  select(zip, county_name) 

or_check2 <- or_check_county %>%
  select(zip, city, county)
  
or_check3 <- or_check2%>%
  filter(zip %in% or_check$zip)
  

or_check4 <- or_check %>%
  filter(zip %in% or_check3$zip)

or_check5 <- or_check%>%
  filter(!zip %in% or_check3$zip) # 97471

a <- which(or_check4$county_name=="Hood.River") # 27, 36
or_check4$county_name[a] <- "Hood River"

or_check_combine <- or_check %>%
  left_join(or_check, or_check2, by = "zip")

b <- which(or_check3$county!=or_check4$county_name)
# 11 147 160 162 167 168 184 346 347 348 349

or_check3$county[b]
or_check4$county[b]

or_check3$zip[b]
#  [1] 97014 97326 97347 97350 97358 97360 97378 97758 97759 97760 97761

write_path <- paste('./data_new/county_data/or_zip_county_prop_new.csv') 
write_csv(zip_county_prop_df2, write_path)



