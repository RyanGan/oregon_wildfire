#-------------------------------------------------------------------------------
# Title: Washington population-weighted smoke PM2.5 by day and zip
# Author: Jingyang Liu                                                                   
# Date Created: April 3, 2017                                                  
# R version: 3.3.3                                                       
#-------------------------------------------------------------------------------

# Note: This is general code that could be submitted in batches to handle 
#       multiple imputs (dataframes)

# Note 8/8/16: I think the matrix approach actually runs pretty quick; doesn't
#              need to be submitted in batches

# load libraries ---------------------------------------------------------------
library(tidyverse)
library(dplyr)

# Setting Working Directory ----------------------------------------------------
#dir <- paste0("C:/Users/RGan/Google Drive/CSU/wild_fire/washington/",
#               "smoke_data/created_pm_estimates")

# relative path 
dir <- paste0("./data/Oregon/")
setwd(dir)
getwd()
list.files()

# Import and prepare smoke PM 2.5 data for loop --------------------------------

# These files never change and can take place outside the loop
# Import population weight csv 
pop_path <- paste0('./popdensity_oregon.csv')
pop_grid <- read_csv(pop_path)

# rename missing column
colnames(pop_grid) <- c('WRFGRID_ID', 'Longitude', 'Latitude', 'pop_density')
summary(pop_grid)

# calculate the estimated population for each grid (multiply density by grid area)
population_grid <- pop_grid %>% 
  mutate(pop_n = pop_density *(15^2)) ##??
summary(population_grid)


# Import the file that contains the proportion of each grid that overlaps a zip
zip_grid_path <- paste0('zip_wrf_proportion.csv')
zip_grid_proportion <- read_csv(zip_grid_path)

# Importing files that I would like to loop through
# Geo weighted regresssion (Atmos model of choice)
geo_wt_path <- paste0("oregon_gwrPM_2013.csv")
geo_wt <- read_csv(geo_wt_path)

# Kriging 
krig_path <- paste0("oregon_krigedPM_2013.csv")
krig <- read_csv(krig_path)

# Background PM2.5
back_path <- paste0("oregon_backgroundPM_2013.csv")
background <- read_csv(back_path)

# WRF-nofire
wrf_nf_path <- paste0("oregon_wrfnofirePM_2013.csv")
wrf_nf <- read_csv(wrf_nf_path)

# WRF-fire
wrf_f_path <- paste0("oregon_wrffirePM_2013.csv")
wrf_f <- read_csv(wrf_f_path)

# WRF temperature
wrf_temp_path <- paste0("oregon_wrf_temperature_2013.csv")
wrf_temp <- read_csv(wrf_temp_path)


# Dataframes of PM2.5 attributed to wildfire smoke -----------------------------
# _smk indicates that we somehow account for background levels 
# in the case of WRF models, the WRF-nofire model is subtracted off
# in other cases, the background pm is subtracted off

head(wrf_f[, 1:10])
head(wrf_nf[, 1:10])

# estimating WRF-Fire smoke model
wrf_smk <- wrf_f[, 4:156] - wrf_nf[, 4:156]
# set all values <0 to 0
wrf_smk[wrf_smk < 0] <- 0
# grid id, lat and long
grid_id <- wrf_f[,1:3]
# final estimates of grid pm due to smoke
wrf_smk_pm <- cbind(grid_id, wrf_smk)
head(wrf_smk_pm[, 1:10])

# The background only has one column data for the average of full period, so 
# repeat 153 columns in total.
background[,5:156] <- background[,4]
colnames(background) <- colnames(krig)

# Geo weight and krig dataframes subtracting off background pm2.5
# Geo-weighted regression smoke
geo_smk <- geo_wt[, 4:156] - background[, 4:156] # not sure
# set all values <0 to 0
geo_smk[geo_smk < 0] <- 0
# grid id, lat and long
grid_id <- geo_wt[, 1:3]
# final estimates of grid pm due to smoke
geo_smk_pm <- cbind(grid_id, geo_smk)
head(geo_smk_pm[, 1:10])

# krig smoke
krig_smk <- krig[, 4:156] - background[, 4:156]
krig_smk[krig_smk <0] <- 0
grid_id <- krig[, 1:3]
krig_smk_pm <- cbind(grid_id, krig_smk)
head(krig_smk_pm[, 1:10])

# Setting up dataframes for loop -----------------------------------------------
# create a list of the dataframes I want to loop through
df_list <- list(wrf_f = wrf_f, wrf_nf = wrf_nf, wrf_smk_pm = wrf_smk_pm, 
                geo_wt = geo_wt,  krig = krig, background = background, 
                geo_smk_pm = geo_smk_pm, krig_smk_pm = krig_smk_pm, 
                wrf_temp = wrf_temp)

df_name <- c('wrf_f', 'wrf_nf', 'wrf_smk_pm', 'geo_wt', 'krig', 'background', 
             'geo_smk_pm', 'krig_smk_pm', "wrf_temp")


# General approach to producing population-weighted zipcode-specific PM2.5 -----

# The best thing to do is mutiply the population grid by the concentration
# matrices first, which I believe should give you the population weighted
# concentrations for each grid (dim should be 1575 by 156 I think). Then take
# that matrix and multiply by zipcode by grid matrix to get the population
# weighted concentration for each grid, which I can then divide by
# the zip_pop_matrix to get the zip code specific population weighted average.


# zip and wrf grid overlap matrix ----------------------------------------------
zip_grid_to_matrix <- zip_grid_proportion[ , 1:1575]
zip_grid_matrix <- matrix(as.numeric(unlist(zip_grid_to_matrix)), 
                          nrow=nrow(zip_grid_to_matrix))
dim(zip_grid_matrix) # (dimension of matrix 417 x 1575)


# creating population grid matrix ----------------------------------------------
# matrix is a 1575 row, 1 column matrix that contains the grid estimated number
# of people
summary(population_grid)
population_to_matrix <- population_grid[, 5]
# Set NAs of population to 0
population_to_matrix$pop_n <- ifelse(is.na(population_to_matrix$pop_n), 0,
                                     population_to_matrix$pop_n) 

summary(population_to_matrix)

# covert to matrix
pop_grid_matrix <- matrix(as.numeric(unlist(population_to_matrix)), 
                          nrow=nrow(population_to_matrix))
dim(pop_grid_matrix) # (1575 x 1 dimension matrix)

# Matrix algebra that can be done outside the list
# Multiply the pop_grid matrix by the zip_grid matrix to get a sum of
# the population in each zipcode
zip_pop_matrix <-  zip_grid_matrix %*% pop_grid_matrix
dim(zip_pop_matrix) # 417 x 1 matrix (column is population for each zip code)

# convert zip_pop_matrix to a vector to use to divide later in the loop
zip_pop_vector <- as.vector(zip_pop_matrix)

# Convert zip population matrix to a vector (easier to apply formula col by col)
# easiest to create a vector to multiply each column (date) of the matrix
grid_population_vector <- as.vector(pop_grid_matrix)
tail(grid_population_vector)

# output zipcode column from zip_grid_proportion for naming purposes later
# From last proportion calculation code ----------------------------------------
read_path <- paste0('../../data_new/or_zip.csv')
or_zip <- read_csv(read_path)

zipcode <- as.character(or_zip[[1]])
head(zipcode)


# start timer
start <- proc.time()

# set the dataframe of interest to a general dataframe to loop through
for(i in 1:length(df_list)){
  
  daily_pm_grid  <- data.frame(df_list[[i]]) # easier to use double brackets so 
  # X comes before each date for each dataframe, then I can remove X when I tidy
  data_frame_name <- df_name[i]
  
  # Matrix Multiplication --------------------------------------------------------
  
  # For each day, I need to multiply the PM2.5 concentration for each zip code 
  
  # convert dataframes to matrix (need to conver to matrix and remove variables like
  # zip code and lat long)
  # daily PM 2.5 concentration for each grid 
  daily_pm_to_matrix <- daily_pm_grid[, 4:156]
  
  
  pm_matrix <- matrix(as.numeric(unlist(daily_pm_to_matrix)), 
                      nrow=nrow(daily_pm_to_matrix))
  dim(pm_matrix) # 1575 (pm val in wrf_grid) by 153 (days) matrix
  
  # Multiply the matrix of zip_grid proportion by the PM concentration matrix
  # This matrix contains the summed PM2.5 concentrations for each zip code for
  # each day
  
  # Check -----
  # is this redundant, and should it be removed? I overwrite below
  # zip_grid_wt_pm_matrix <- zip_grid_matrix %*% pm_matrix
  
  # dim(zip_grid_wt_pm_matrix) # 417 x 123 matrix (417 zipcodes by 123 days)
  
  # multiply the population vector by the pm concentration matrix for each day
  # (column in the matrix)
  
  pm_pop_matrix <- diag(grid_population_vector) %*% pm_matrix 
  dim(pm_pop_matrix) # 1575 (grid) x 153 (days)
  
  # now I want to multiply the pm_pop_matrix by the zip_gird matrix
  zip_grid_wt_pm_matrix <-  zip_grid_matrix %*% pm_pop_matrix
  # this gives me a matrix of the sum of values for each zipcode for each day 
  dim(zip_grid_wt_pm_matrix) # 417 (summed pm2.5 in each zipcodes) x 153 (days)
  
  # now I think each daily value of this matrix needs to be divided by
  # the summed population of each zip code
  # need to multiply by the inverse of the zip_pop_vector for each column 
  # since you cannot divide with matrix algebra
  zip_pop_and_grid_wt_pm <- diag(1/zip_pop_vector) %*% zip_grid_wt_pm_matrix 
  dim(zip_pop_and_grid_wt_pm) # 417 (population wted avg of PM2.5 ) x 153 (days)
  # this should work
  summary(zip_pop_and_grid_wt_pm)
  # There are two zipcodes with 0 people living in it, therefore there will be
  # 2 missing values for each day as dividing by 0 is not possible
  
  
  # bind the zipcode column in with the matrix (I think this works)
  zip_pm_conc <- cbind(zipcode, zip_pop_and_grid_wt_pm)
  
  # now give every other column the header of the date of the geo_wt value
  x <- colnames(daily_pm_grid[, 4:156]) # 417 row x 154 col
  x # check date column names
  x2 <- c('ZIPCODE', x) # concate with zipcode column name
  x2
  # assign column names to matrix
  colnames(zip_pm_conc) <- c(x2)
  
  head(zip_pm_conc)
  
  # create environment dataframe with name of original dataset hia is based off
  matrix_name <- paste(data_frame_name, 'df', sep = '_')
  assign(matrix_name, zip_pm_conc)
  
  # write permanent dataset
  write_path <- paste('./zip_population_weighted_pm_', matrix_name, '.csv', sep = '')
  write.csv(matrix_name, file = write_path)
  
  
} # end loop

stop <- proc.time() - start
stop # 5.92 sec

# very fast loop, much easier than my extract function

# I think this might work and be more efficent than my extract raster loop
# it still needs to be tested and confirmed against some of my older datasets

# subset zipcode 98858 # row 124 i think, column 84 for the 21st of september

# Creation of dataframe to join with health estimates --------------------------

# take each dataframe and create one large dataframe of zip code, date, and then
# each population weighted value
tidy_loop <- list(wrf_f_df = wrf_f_df, wrf_nf_df = wrf_nf_df,
                  wrf_smk_pm_df = wrf_smk_pm_df,
                  geo_wt_df = geo_wt_df, krig_df = krig_df, background_df = background_df,
                  geo_smk_pm_df = geo_smk_pm_df, krig_smk_pm_df = krig_smk_pm_df, 
                  wrf_temp_df = wrf_temp_df)


pm_name <- c('wrf_f_pm', 'wrf_nf_pm', 'wrf_smk_pm', 'geo_wt_pm',
             'krig_pm', 'background_pm', 'geo_smk_pm', 'krig_smk_pm', "wrf_temp")

# empty dataframe
or_pm_pop_wt_2013 <- data.frame(matrix(vector(), 63801, 11, # rows, columns (417*153)
                                       dimnames = list(c(), c("ZIPCODE", "date",
                                                              pm_name))), stringsAsFactors = F)
# probably not efficient to fill cols 1 and 2 on each loop, but eh, it's fast anyways

for(k in 1:length(tidy_loop)){
  df_to_tidy <- data.frame(tidy_loop[[k]])
  pm_col_name <- pm_name[k]
  
  or_zip_pm_pop_wt <- df_to_tidy %>% gather(date, pm_method, -ZIPCODE) %>% 
    arrange(ZIPCODE, date)
  # convert character to date
  or_zip_pm_pop_wt$date <- as.Date(or_zip_pm_pop_wt$date, "X%Y%m%d")
  # rename the column 'pm_method' to the stored pm_col_name
  colnames(or_zip_pm_pop_wt) <- c("ZIPCODE", "date", pm_col_name)
  
  
  # final dataset
  or_pm_pop_wt_2013[, 1] <- or_zip_pm_pop_wt[, 1]
  or_pm_pop_wt_2013[, 2] <- or_zip_pm_pop_wt[, 2]
  or_pm_pop_wt_2013[, k+2] <- or_zip_pm_pop_wt[, 3]
  
} # end of loop

summary(or_pm_pop_wt_2013) 
head(or_pm_pop_wt_2013)

df_check <- or_pm_pop_wt_2013 %>% filter(ZIPCODE == 97005 & date == '2013-09-21')
df_check

write_path <- paste0('./zip_pm_to_merge_with_chars.csv')
write_csv(or_pm_pop_wt_2013, write_path)

# looks good; write premanent file to merge with health data
