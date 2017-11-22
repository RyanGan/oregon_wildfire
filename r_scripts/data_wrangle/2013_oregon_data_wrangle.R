# ------------------------------------------------------------------------------
# Title: Basic data wrangling  for 2013 Oregon data and seperating by ICD9 lists
# Author: Jingyang Liu
# Date Created: Mar 03, 2016
# Date Checked: Nov 20, 2017
# R Version: 3.3.2
# ------------------------------------------------------------------------------

# This file is for 
# Also it separates the data into 12 disease files due to the ICD9 lists.

# This runs on server.

# library
library(dplyr)
library(data.table)
library(readxl) 
library(readr)

# import original data ---------------------------------------------------------
read_path <- paste0("../../../data/data_original/gan_episodes_of_care.txt")
start_time <- Sys.time()
oregon_df <- fread(read_path, sep = "|", showProgress = T) 
stop_time <-  Sys.time() - start_time 
# time it took
stop_time # 7 mins

# basic cleaning ---------------------------------------------------------------

oregon_df <- oregon_df %>% 
  # filter(!is.na(dx1)) %>% 
  # filter State is Oregon
  filter(STATE=="OR") 
# filter urgent or ED visits (pos = 20 or 23)
# filter(pos=="20"|pos=="23") 

write_path <- paste0('../../../data/data_original/',
                     '2013_oregon_epis_care.csv')
write_csv(oregon_df, write_path)



# Define read paths ------------ -----------------------------------------------
# import r list of outcome-specific icd9 codes
load_path <- paste0("../../../data/data_original/outcome_list.RData")
load(load_path)

# path and file name of data
read_path <- paste0("../../../data/data_original/2013_oregon_epis_care.csv")
start_time <- Sys.time()
oregon_df <- fread(read_path, sep = ",", 
                   colClasses = rep("character", 72)) 
stop_time <-  Sys.time() - start_time 
# time it took
stop_time # 20 min


## make SABA data set(ndc is beta-2-agonist; personkey) ------------------------
read_path2 <- paste0("../../../data/data_original/2013_ndc_beta2_agonists.csv")
ndc_df <- read_csv(read_path2, col_names = FALSE)

colnames(ndc_df) <- c("ndc_code", "brand_name", "generic_product_name", 
                      "route", "category", "drug_id")

# convert to vector
ndc_codes <- as.vector(as.matrix(ndc_df$ndc_code))

ndc_in_oregon_df <- oregon_df %>%
  filter(ndc %in% ndc_codes) # 300752, unique personkey 109483

oregon_saba_index <- ndc_in_oregon_df %>%
  select(personkey) %>%
  unique()

write_path2 <- paste0('../../../data/data_new/health/2013_oregon_saba_index.csv')
write_csv(oregon_saba_index, write_path2)

saba_df <- oregon_df %>%
  filter(personkey %in% oregon_saba_index$personkey) # 11358660

write_path3 <- paste0('../../../data/data_new/health/2013_oregon_saba_claims.csv')
write_csv(saba_df, write_path3)

# ------------------------------------------------------------------------------

# now try to work the two chunks together to identify records that contain icd9s
# of a certain outcome and then output those rows in chunks of data.

# I need a dataframe of start row and length to read after
n_start<- seq(from = 1, to = length(oregon_df$personkey), by = 200000) 

# create dataframe
n_read <- data_frame(n_start) %>% # turn the sequence in to a dataframe
  mutate(n_length = ifelse(row_number()!=n(), 200000,
                           ifelse(row_number()==n(), length(oregon_df$personkey)%%200000, 
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
        filter(V30 %in% icd9_vector |
                 V31 %in% icd9_vector |
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
                 V42 %in% icd9_vector)  
    ) # end of apply
  ) # end of rbind
  ) # end of do.call
  
  colnames(outcome_df) <- col_names # assign column names to dataframe
  
  # write csv file and assign outcome name to df name
  save_name <- paste0("../../../data/data_new/health/2013_oregon_",outcome_name, "_claims.csv")
  write_csv(outcome_df, save_name)
  
} # end of loop
# stop time of clock
stop_time <-  Sys.time() - start_time 
stop_time # 10 mins