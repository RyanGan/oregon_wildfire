#-------------------------------------------------------------------------------
#     Title: Creating visuals of the wrf grids over oregon for May 1 to Sep 30, 2013
#     Purpose: Nice sequence maps for presentations
#     Author: Jingyang Liu
#     Date Created: Apr 27 2017
#     R version: 3.2.3
#-------------------------------------------------------------------------------

# libraries used
library(dplyr)
library(ggmap)
library(ggplot2)
library(rgdal) # package for shape files
library(sp)
library(raster) # need for crs (but masks select in dplyr and extract in tidyr)
library(maptools) # need for fortify by region
library(rgeos)
library(readr)

# set working directory
getwd()
# relative path
setwd('C:/Users/jyliu/Desktop/local_git_repo/oregon_wildfire_new/data/Oregon_PM')
list.files()

# read csv smoke files
wrf_chem <- paste0('oregon_wrffirePM_2013.csv')
wrf_chem_pm <- read_csv(wrf_chem)

# Rename all columns to denote wrf estimates
x <- colnames(wrf_chem_pm[, 4:156])
x2 <- colnames(wrf_chem_pm[, 1:3])
x3 <- paste('wrf', x, sep = '')
x4 <- c(x2, x3)
colnames(wrf_chem_pm) <- c(x4)
# all renamed
summary(wrf_chem_pm)

# read in shape files of Bonne's and Will's smoke grid -------------------------
# pc
grid_dir <- paste0("../../shapefile/oregon_new_grid")

# mac
# grid_dir <- paste0('/Users/ryangan/Google Drive/CSU/wild_fire/shape_files/',
#                   'wash_grid_shapefile')

smoke_grid <- readOGR(dsn = grid_dir, layer = "oregon_new_grid")
summary(smoke_grid) # has 1107 here
plot(smoke_grid) # check out the grid

# output the id to merge
smoke_grid@data$id <- rownames(smoke_grid@data)
smoke_grid_points <- fortify(smoke_grid, region = 'id')
str(smoke_grid_points)
str(wrf_chem_pm)
wrf_chem_pm <- mutate(wrf_chem_pm, id = as.character(GRID_ID))

# merge in wrf_meteor values where WRFGRID_ID == id
smoke_grid_df <- smoke_grid_points %>% full_join(wrf_chem_pm, by = 'id')
smoke_grid_df[ , 164] <- NULL

# might change high value bound
smoke_grid_df <- smoke_grid_df %>% 
  mutate_each(funs(ifelse( . > 250, 250, .)),
              wrf20130501:wrf20130930)

summary(smoke_grid_df)

# set projection string same as county polygon projectons (nad83)
nad83 <- '+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0'
crs(smoke_grid) <- nad83

# fortify smoke_grid shp to data frame by wrfgrid id (need)
smoke_data <- fortify(smoke_grid, region = 'WRFGRID_ID')
summary(as.factor(smoke_data$id))

# Creating maps ----------------------------------------------------------------

# Set base map layer
# find max and min values of raster grid
xmn=min(wrf_chem_pm[,2]) # min longitude
xmx=max(wrf_chem_pm[,2]) # max longitude
ymn=min(wrf_chem_pm[,3]) # min latitude
ymx=max(wrf_chem_pm[,3]) # max latitude


bbox <- c(-125, 41.52, -116.32, 46.69)

oregon_map <- get_map(location= bbox, source = 'stamen', 
                      maptype = 'toner', crop = TRUE)

# create loop to add pm values for each day
# Date labels for title
date_2013 <- data.frame(seq(as.Date('2013-05-01'), as.Date('2013-09-30'),
                            by = 'day'))

setwd('../../plot/map_images') # set directory to save maps
getwd()
# note projection seems a bit off.
# map loop 
for(i in 11:163){ # start of loop
  date <- date_2013[i-10,]
  # print all maps
  ggmap(oregon_map) + geom_polygon(data = smoke_grid_df, aes(x = long, y = lat,
                                                             group = id, fill = smoke_grid_df[i]), alpha = 0.6) +
    scale_fill_gradient(expression('WRF-Chem PM'[2.5]),
                        low = 'white', high = 'red',
                        # limits looks like it works to set same scale
                        limits= c(0,250)) +
    ggtitle(date) + labs(x = "Longitude", y = "Latitude")
  # close out print loop
  ggsave(paste(i,'pdf', sep = '.'))
} # end loop
