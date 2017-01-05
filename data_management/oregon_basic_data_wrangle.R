# ------------------------------------------------------------------------------
# Title: Basic data wrangle and cleaning of Oregon 2013 claims data
# Author: Ryan Gan
# Date Created: December 30, 2016
# R Version: 3.3.2
# ------------------------------------------------------------------------------

# Note: Datasets are pretty large. I had to come up with some different ways of
# processing these data. I checked the number of rows in the command line and
# found there were 77,069,321 rows. 

# Helpful sources: 
# Reading in specific sequences of a large data set
# http://stackoverflow.com/questions/21798930/how-to-read-specific-rows-of-csv-file-with-fread-function

# load libraries ---------------------------------------------------------------
library(tidyverse)
library(data.table)


# import data ------------------------------------------------------------------

# Read in first 100000 lines to mess around with in R until I figure out another
# way. I will then delete the text file 
#read_path <- paste0("./data/gan_episodes_of_care.txt")

#oregon_df <- fread(read_path, sep = "|", nrows = 200000, showProgress = T)

#fwrite(oregon_df, "./data/oregon_epis_care_reduced.csv")
start_time <- Sys.time()

oregon_df <- fread("./data/oregon_epis_care_reduced.csv", sep = ",", 
                   colClasses = rep("character", 72))

stop_time <-  Sys.time() - start_time 
# time it took
stop_time

oregon_df[1:10, 64]

# code chunk to output rows of a specific outcome ------------------------------
# see if your lists work with the oregon_df
# primary diagnosis
test_df <- oregon_df %>% 
  # select columns to search through
  select(matches("dx")) %>% 
  # apply the %in% function to evaluate list of icd9 codes to subset by
  lapply("%in%", outcome_icd9_list[[2]]) %>% 
  # reduce the funciton to 'or' across all logic values
  Reduce(f = "|", .) %>% 
  # identifies the rows where this statement is true
  which %>% 
  slice(.data = oregon_df)
# this chunk of code works for subsetting the rows that meet specific icd9 codes

head(test_df)

# get column df names
df_col_names <- colnames(oregon_df)


# code chunk to sequentially read in smaller chunks of dataframes --------------

# figure out a test script to read chunks of the file and move on
# number of lines to read

# I need a dataframe of start row and length to read after
n_start<- seq(from = 1, to = 200000, by = 10000) 

#n_start<- seq(from = 1, to = 100, by = 5)   
# create dataframe
n_read <- data_frame(n_start) %>% # turn the sequence in to a dataframe
  mutate(n_length = 10000) # set length to read 

# test dataframe
start_time <- Sys.time()

test_df <- do.call(rbind, apply(n_read,1, function(x) 
  return(fread("./data/oregon_epis_care_reduced.csv", sep =",",
               nrows=x[2], skip=x[1], header = F, 
               # keep all the variables as character to avoid import errors
               # 72 variables
               colClasses = rep("character", 72))) %>% 
    # allows me to subset (1st of Jan in this case)
    # i want to filter by scanning through each dx column and looking 
    # for specific outcomes
    filter(V24 == "2013-11-01")))

colnames(test_df) <- df_col_names

stop_time <-  Sys.time() - start_time 

stop_time

glimpse(test_df)
# find locations of variables
which(colnames(test_df) == 'dx13')

# check to make sure dataset imported
head(test_df$V1)
head(oregon_df$personkey)
tail(oregon_df$personkey)
tail(test_df$V1)

# combo -----
# now try to work the two chunks together to identify records that contain icd9s
# of a certain outcome and then output those rows in chunks of data.


# I need a dataframe of start row and length to read after
n_start<- seq(from = 1, to = 200000, by = 10000) 

#n_start<- seq(from = 1, to = 100, by = 5)   
# create dataframe
n_read <- data_frame(n_start) %>% # turn the sequence in to a dataframe
  mutate(n_length = 10000) # set length to read 

# test dataframe
start_time <- Sys.time()

asthma_icd9_vector <- outcome_icd9_list[[2]]

test_df2 <- do.call(rbind, apply(n_read,1, function(x) 
  return(
  fread("./data/oregon_epis_care_reduced.csv", sep =",",
    nrows=x[2], skip=x[1], header = F, 
    # keep all the variables as character to avoid import errors
    # 72 variables
    colClasses = rep("character", 72)) %>% 
    # now search for icd9 codes
    filter(V30 %in% asthma_icd9_vector |
           V32 %in% asthma_icd9_vector |
           V33 %in% asthma_icd9_vector |
           V34 %in% asthma_icd9_vector |
           V35 %in% asthma_icd9_vector |
           V36 %in% asthma_icd9_vector |
           V37 %in% asthma_icd9_vector |
           V38 %in% asthma_icd9_vector |
           V39 %in% asthma_icd9_vector |
           V40 %in% asthma_icd9_vector |
           V41 %in% asthma_icd9_vector |
           V42 %in% asthma_icd9_vector | 
           V43 %in% asthma_icd9_vector)  
  ) # end of apply
  ) # end of rbind
  ) # end of do.call

colnames(test_df2) <- df_col_names

stop_time <-  Sys.time() - start_time 

stop_time


