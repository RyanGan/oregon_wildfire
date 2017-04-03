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
