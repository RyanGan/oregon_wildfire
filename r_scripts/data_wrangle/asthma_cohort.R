# ------------------------------------------------------------------------------
# Title: Creation of asthma and beta2 agonist cohort
# Author: Ryan Gan
# Date Created: 2017-11-29
# R Version: 3.4
# ------------------------------------------------------------------------------

# note 2017-12-01: memory issues with this process; going to have to chunk the 
# reads in to smaller parts
# oregon txt file has 77069321 rows including header

# load library tidyverse -----
library(tidyverse)

# path 
#read_path <- paste0("./data/health/gan_episodes_of_care.txt")
# test path
read_path <- paste0("./data/health/oregon_subset.txt")

# read sections dataframe -------
# n_start <- seq(from=1, to=77069321, by=200000)
n_start <- seq(from=1, to=10000, by = 100)
n_length <- rep(100, each=length(n_start))
# make a dataframe to apply to 
n_read <- data.frame(cbind(n_start,n_length))
# I will use fread function from data.table to read in chunks at a time and
# it doesn't matter if the read length is not exact

# read first line and get column names
df_col_names <- data.table::fread(read_path, nrows = 0, header = T, 
                      colClasses = rep("character", 72))

col_names <- colnames(df_col_names) # output column names

# read in with fread
oregon_df <- data.table::fread(read_path, sep = "|",
  colClasses = c(rep("character", 72)), stringsAsFactors = FALSE) 


# already written ------
# print first 6 rows as check
head(oregon_df)

# read in Rdata icd9 outcome vectors and limit to asthma
load("./data/health/outcome_list.RData")
# print outcomes list as check
class(outcome_icd9_list)

# subset to asthma icd9
asthma_icd9 <- pluck(outcome_icd9_list, "asthma")
# subset to ndc saba
saba_ndc <- pluck(outcome_icd9_list, "saba")

# filter oregon epidsodes of care to dx of asthma or ndc of saba ------
# find ids with at least one dx of asthma
asthma_id <- oregon_df %>% 
  filter_at(vars(contains("dx")), any_vars(. %in% asthma_icd9)) %>% 
  select(personkey) %>% 
  as_vector() 

saba_id <- oregon_df %>% 
  filter(ndc %in% saba_ndc) %>% 
  select(personkey) %>% 
  as_vector()

# print 6 obs of ids
head(asthma_id)
tail(saba_id)

# bind two id vectors together
asthma_saba_personkey <- unique(c(asthma_id, saba_id))

head(asthma_saba_personkey)
tail(asthma_saba_personkey)

# filter large dataset to persons with asthma or saba fills ------
# creating persons at risk (with asthma or a saba fill) for adverse events
asthma_saba_at_risk <- oregon_df %>% 
  filter(personkey %in% asthma_saba_personkey) %>% 
  # make a indicator if visit is asthma hosp, saba fill, or neither
  mutate(visit_type = ifelse(dx1 %in% asthma_icd9, "asthma", 
    ifelse(ndc %in% saba_ndc, "saba", "other")))

head(asthma_saba_at_risk)

# save file
write_path <- paste0("./data/health/2013-oregon_asthma_cohort.csv")
write_csv(asthma_saba_at_risk, write_path)

# list files as final check
list.files("./data/health/")






