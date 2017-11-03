# ------------------------------------------------------------------------------
# Title: Basic data wrangle and cleaning of Oregon 2013 claims data
# Author: Ryan Gan
# Date Created: December 30, 2016
# R Version: 3.3.2
# ------------------------------------------------------------------------------

# Note: Datasets are pretty large. I had to come up with some different ways of
# processing these data. I checked the number of rows in the command line and
# found there were 77,069,321 rows. 

# load libraries ---------------------------------------------------------------
library(dplyr)
library(data.table)

# import data ------------------------------------------------------------------

# Read in first 100000 lines for exploration.

#read_path <- paste0("./data/gan_episodes_of_care.txt")
#oregon_df <- fread(read_path, sep = "|", nrows = 200000, showProgress = T)
#fwrite(oregon_df, "./data/oregon_epis_care_reduced.csv")

# I wrote a reduced dataframe of 200,000 rows to get an idea of data structure

# import the reduced dataframe
start_time <- Sys.time()

oregon_df <- fread("./data/oregon_epis_care_reduced.csv", sep = ",", 
                   colClasses = rep("character", 72))

stop_time <-  Sys.time() - start_time 
# time it took
stop_time

#oregon_df[1:10, 64]

# Define read paths ------------ -----------------------------------------------
# import r list of outcome-specific icd9 codes
load_path <- paste0("./data/outcome_list.RData")
load(load_path)

# path and file name of data
read_path <- paste0("./data/oregon_epis_care_reduced.csv")

# now try to work the two chunks together to identify records that contain icd9s
# of a certain outcome and then output those rows in chunks of data.

# I need a dataframe of start row and length to read after
n_start<- seq(from = 1, to = 200000, by = 10000) 

#n_start<- seq(from = 1, to = 100, by = 5)   
# create dataframe
n_read <- data_frame(n_start) %>% # turn the sequence in to a dataframe
  mutate(n_length = 10000) # set length to read 

# read first line and get column names
df_col_names <- fread(read_path, nrows = 0, header = T, 
                      colClasses = rep("character", 72))

col_names <- colnames(df_col_names) # output column names

# loop to output dataframes of outcomes ----------------------------------------
start_time <- Sys.time() # start clock time

for(i in 1:length(outcome_icd9_list)) { # start loop

outcome_name <- names(outcome_icd9_list)[i] # output the name of the list/outcome
icd9_vector <- outcome_icd9_list[[i]] # output the icd9 codes of the list

outcome_df <- do.call(rbind, apply(n_read,1, function(x) 
  return(
  fread(read_path, sep =",",
    nrows=x[2], skip=x[1], header = F, 
    # keep all the variables as character to avoid import errors
    # 72 variables
    colClasses = rep("character", 72)) %>% 
    # now search for icd9 codes
    filter(V30 %in% icd9_vector |
           V32 %in% icd9_vector |
           V33 %in% icd9_vector |
           V34 %in% icd9_vector |
           V35 %in% icd9_vector |
           V36 %in% icd9_vector |
           V37 %in% icd9_vector |
           V38 %in% icd9_vector |
           V39 %in% icd9_vector |
           V40 %in% icd9_vector |
           V41 %in% icd9_vector |
           V42 %in% icd9_vector | 
           V43 %in% icd9_vector)  
  ) # end of apply
  ) # end of rbind
  ) # end of do.call

colnames(outcome_df) <- col_names # assign column names to dataframe

# write csv file and assign outcome name to df name
save_name <- paste0("./data/oregon_",outcome_name, "_claims_2013.csv")
fwrite(outcome_df, save_name)

} # end of loop
# stop time of clock
stop_time <-  Sys.time() - start_time 
stop_time

# estimated time to run through entire file
#((33.5/200000)*27000000)/60
# 75 minutes

hf_df <- fread("./data/oregon_heart_failure_claims_2013.csv", sep = ",")
#head(hf_df)
