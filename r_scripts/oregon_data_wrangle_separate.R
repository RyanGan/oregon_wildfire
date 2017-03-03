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

# Read in first 200000 lines for exploration.

read_path <- paste0("./data/gan_episodes_of_care.txt")
oregon_df <- fread(read_path, sep = "|", nrows = 200000, showProgress = T)
fwrite(oregon_df, "./data/oregon_epis_care_reduced.csv")

# I wrote a reduced dataframe of 200,000 rows to get an idea of data structure


# Define read paths ------------ -----------------------------------------------
# import r list of outcome-specific icd9 codes
load_path <- paste0("./data/outcome_list.RData")
load(load_path)

# path and file name of data
read_path <- paste0("./data/oregon_epis_care_reduced.csv")
start_time <- Sys.time()
oregon_df <- fread(read_path, sep = ",", 
                   colClasses = rep("character", 72)) 
stop_time <-  Sys.time() - start_time 
# time it took
stop_time # 20 mins

## 

oregon_df$yob <- as.numeric(oregon_df$yob)
oregon_df[oregon_df=="*NULL*"] <- NA

### data rough cleaning (remove some NA and blank values) and add index---------
# at least a primary ICD9 code present, urgent or ED visit
# whole data still have 1514387 observations after cleaning
# small data 2926 left
or_hosp_w_outcome_df <- oregon_df %>% filter(!is.na(dx1)) %>% 
  # filter sex is known
  filter(gender!="U") %>%
  # add age, filter age not missing
  mutate(age = 2013-yob) %>%
  filter(!is.na(age)) %>%
  # filter race
  filter(race!="") %>%
  # filter enthnicity
  filter(ethn!="") %>%
  # filter State is Oregon
  filter(STATE=="OR") %>%
  # filter ZIP non missing
  filter(!is.na(ZIP)) %>%
  # filter MSA
  filter(!is.na(MSA)) %>%
  # filter POS
  filter(!is.na(pos)) %>%
  # filter urgent or ED visits (pos = 20 or 23)
  filter(pos=="20"|pos=="23") %>%
  # indicator for male=0, female=1; age
  mutate(sex_ind =ifelse(gender == "F", 1, 
                         ifelse(gender == "M", 0, NA)),
         age_ind = ifelse(age < 15, 0,
                          ifelse(age >= 15 & age < 65, 1,
                                 ifelse(age >= 65 & age <=120, 2, NA)))
  ) # end of mutate

# for server
# write_path <- paste0('../../../data/data_original/',
#                     'or_hosp_w_outcome_df')
# write_csv(or_hosp_w_outcome_df, write_path)


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
  save_name <- paste0("./instructions/oregon_",outcome_name, "_claims_2013.csv")
  fwrite(outcome_df, save_name)
  
} # end of loop
# stop time of clock
stop_time <-  Sys.time() - start_time 
stop_time

# estimated time to run through entire file
#((33.5/200000)*27000000)/60
# 75 minutes

hf_df <- fread("./instructions/oregon_heart_failure_claims_2013.csv", sep = ",")
#head(hf_df)
