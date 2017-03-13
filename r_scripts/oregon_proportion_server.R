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

getwd()
# "/home/jyliu/wildfire/local_git_repo/oregon_wildfire/r_scripts"


