gitpull
# ------------------------------------------------------------------------------
# Title: Basic data wrangling  for 2013 Oregon data and seperating by ICD9 lists
# Author: Jingyang Liu
# Date Created: Mar 03, 2016
# Date Checked and Modified: Nov 20, 2017
# R Version: 3.3.2 (now 3.4.2)
# ------------------------------------------------------------------------------

# This file is for filtering saba outcomes.
# And combine an asthma at-risk data set with ICD9 of asthma or saba.
# original data sets and instrunction files such as index are stored in data_original file,
# the new data sets we made are stored in data_new, which contain health and smoke
# data sets. Now the outcomes are all stored in data_new/health.

# This runs on server because of its huge computing volume.


# library
library(dplyr)
library(data.table)
library(readxl) 
library(readr)

### Prepare work

# import original data ---------------------------------------------------------
read_path <- paste0("../../../data/data_original/gan_episodes_of_care.txt") # 77million
start_time <- Sys.time()
oregon_df <- fread(read_path, sep = "|", showProgress = T) 
stop_time <-  Sys.time() - start_time 
# time it took
stop_time # 7 mins

write_path <- paste0('../../../data/data_original/',
                     '2013_oregon_epis_care.csv') # csv file, for future use
write_csv(oregon_df, write_path)
### ----------------------------------------------------------------------------


### Make saba index

# the ndc beta-2 agonist file are made in SQLite previously.
read_path2 <- paste0("../../../data/data_original/2013_ndc_beta2_agonists.csv")
ndc_df <- read_csv(read_path2, col_names = FALSE)

# change to nice varibale name
colnames(ndc_df) <- c("ndc_code", "brand_name", "generic_product_name", 
                      "route", "category", "drug_id")

# convert to vector
ndc_codes <- as.vector(as.matrix(ndc_df$ndc_code))

ndc_in_oregon_df <- oregon_df %>%
  filter(ndc %in% ndc_codes) # 300752, unique personkey 109483

oregon_saba_index <- ndc_in_oregon_df %>%
  select(personkey) %>%
  unique() # unique personkey, 109483

write_path <- paste0('../../../data/data_new/health/2013_oregon_saba_index.csv')
write_csv(oregon_saba_index, write_csv)
### ----------------------------------------------------------------------------



### Make data set of saba or asthma
# path and file name of data
read_path <- paste0("../../../data/data_original/2013_oregon_epis_care.csv")
start_time <- Sys.time()
oregon_df <- fread(read_path, sep = ",", 
                   colClasses = rep("character", 72)) 
stop_time <-  Sys.time() - start_time 
# time it took
stop_time # 14 min

## Import ICD9 code-------------------------------------------------------------
icd9_key <- read_excel("../../../data/data_original/CMS32_DESC_LONG_SHORT_DX.xlsx") %>% 
  # rename the terrible variable names
  select(dx_code = 1, long_desc = 2, short_desc = 3)

# sort by icd9 code add row variable 
icd9_key$X <- NULL
icd9_key <- arrange(icd9_key, dx_code) %>%
  mutate(n = as.numeric(row.names(icd9_key)))

which(icd9_key$dx_code == '49300') # start of asthma is row 5206
which(icd9_key$dx_code == '49392') # end of asthma is row 5219

asthma_icd9 <- filter(icd9_key, n >= 5206 & n <= 5219) %>%
  select(dx_code)
# convert to vector
asthma_icd9 <- as.vector(as.matrix(asthma_icd9))   

## -----------------------------------------------------------------------------

## data set with saba or icd
saba_asthma_df <- oregon_df %>%
  filter(STATE == "OR")
  filter(personkey %in% oregon_saba_index$personkey |
           dx1 %in% asthma_icd9 |
           dx2 %in% asthma_icd9 |
           dx3 %in% asthma_icd9 |
           dx4 %in% asthma_icd9 |
           dx5 %in% asthma_icd9 |
           dx6 %in% asthma_icd9 |
           dx7 %in% asthma_icd9 |
           dx8 %in% asthma_icd9 |
           dx9 %in% asthma_icd9 |
           dx10 %in% asthma_icd9 |
           dx11 %in% asthma_icd9 |
           dx12 %in% asthma_icd9 |
           dx13 %in% asthma_icd9) # 11358660

write_path <- paste0('../../../data/data_new/2013_oregon_atrisk_asthma.csv')
write_csv(saba_asthma_df, write_path)
