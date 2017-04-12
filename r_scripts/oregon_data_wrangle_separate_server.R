# ------------------------------------------------------------------------------
# Title: Basic data wrangling  for 2013 oregon data
# Author: Jingyang Liu
# Date Created: Mar 03, 2016
# R Version: 3.3.2
# ------------------------------------------------------------------------------

library(dplyr)
library(data.table)
library(readxl) 
library(readr)

# import data ------------------------------------------------------------------

read_path <- paste0("../../../data/data_original/gan_episodes_of_care.txt")
start_time <- Sys.time()
oregon_df <- fread(read_path, sep = "|", showProgress = T) 
stop_time <-  Sys.time() - start_time 
# time it took
stop_time # 7 mins

# basic cleaning ---------------------------------------------------------------
start_time <- Sys.time()
oregon_df <- oregon_df %>% filter(!is.na(dx1)) %>% 
  # filter State is Oregon
  filter(STATE=="OR") %>%
  # filter urgent or ED visits (pos = 20 or 23)
  filter(pos=="20"|pos=="23") 
stop_time <-  Sys.time() - start_time 
# time it took
stop_time # 2.5 mins

write_path <- paste0('../../../data/data_original/',
                     'oregon_epis_care_reduced.csv')
write_csv(oregon_df, write_path)



# Define read paths ------------ -----------------------------------------------
# import r list of outcome-specific icd9 codes
# 1514642 obs
load_path <- paste0("../../../data/data_original/outcome_list.RData")
load(load_path)

# path and file name of data
read_path <- paste0("../../../data/data_original/oregon_epis_care_reduced.csv")
start_time <- Sys.time()
oregon_df <- fread(read_path, sep = ",", 
                   colClasses = rep("character", 72)) 
stop_time <-  Sys.time() - start_time 
# time it took
stop_time # 14 sec



# now try to work the two chunks together to identify records that contain icd9s
# of a certain outcome and then output those rows in chunks of data.

# I need a dataframe of start row and length to read after
n_start<- seq(from = 1, to = length(oregon_df$personkey), by = 10000) 

# create dataframe
n_read <- data_frame(n_start) %>% # turn the sequence in to a dataframe
  mutate(n_length = ifelse(row_number()!=n(), 10000,
                    ifelse(row_number()==n(), length(oregon_df$personkey)%%10000, 
                           NA))) # set length to read 

# read first line and get column names
df_col_names <- fread(read_path, nrows = 0, header = T, 
                      colClasses = rep("character", 72))

col_names <- colnames(df_col_names) # output column names

# loop to output dataframes of outcomes ----------------------------------------
start_time <- Sys.time() # start clock time

for(i in 1:length(outcome_icd9_list)) { # start loop
  
  outcome_name <- names(outcome_icd9_list)[i] # output the name of the list/outcome
  icd9_vector <- outcome_icd9_list[[i]] # output the icd9 codes of the list
  
  outcome_df <- do.call(rbind, apply(n_read,1, function(x) # 1 means rows
    return(
      fread(read_path, sep =",",
            nrows=x[2], skip=x[1], header = F, 
            # keep all the variables as character to avoid import errors
            # 72 variables
            colClasses = rep("character", 72)) %>% 
        # now search for icd9 codes
        filter(V30 %in% icd9_vector)  
    ) # end of apply
  ) # end of rbind
  ) # end of do.call
  
  colnames(outcome_df) <- col_names # assign column names to dataframe
  
  # write csv file and assign outcome name to df name
  save_name <- paste0("../../../data/data_new/oregon_",outcome_name, "_claims_2013.csv")
  fwrite(outcome_df, save_name)
  
} # end of loop
# stop time of clock
stop_time <-  Sys.time() - start_time 
stop_time # 10 mins


