
# ------------------------------------------------------------------------------
# Title: Creation of asthma and beta2 agonist cohort
# Author: Ryan Gan
# Date Created: 2017-11-29
# R Version: 3.4
# ------------------------------------------------------------------------------


# load library tidyverse
library(tidyverse)

# read in dataframe ----
read_path <- paste0("./data/health/oregon_subset.txt")
# read in with fread
oregon_df <- data.table::fread(read_path, sep = "|",
  colClasses = c(rep("character", 72)), stringsAsFactors = FALSE)

# set *NULL* and blank to NA
# i may be able to integrate this to the first part
oregon_df <- oregon_df %>% 
  # replace *NULL* with NA
  mutate_all(funs(replace(., .== "*NULL*" | . == "", NA)))

# read in Rdata icd9 outcome vectors and limit to asthma
outcome_icd9_list<- load("./data/health/outcome_list.RData")
# subset to asthma icd9
asthma_icd9 <- pluck(outcome_icd9_list, "asthma")

beta2_ndc <- read_csv("./data/health/2014-hedis_asthma_code.csv")

# limit to short-acting beta2 agonist vector
saba_ndc <- beta2_ndc %>% 
  filter(category == "short-acting inhaled beta-2 agonists") %>% 
  select(ndc_code) %>% 
  as_vector()

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

# bind two id vectors together
asthma_saba_personkey <- unique(c(asthma_id, saba_id))

# filter large dataset to persons with asthma or saba fills ------
# creating persons at risk (with asthma or a saba fill) for adverse events
asthma_saba_at_risk <- oregon_df %>% 
  filter(personkey %in% asthma_saba_personkey) %>% 
  # make a indicator if visit is asthma hosp, saba fill, or neither
  mutate(visit_type = ifelse(dx1 %in% asthma_icd9, "asthma", 
    ifelse(ndc %in% saba_ndc, "saba", "other")))

# save file
write_path <- paste0("./data/health/asthma_saba_cohort.csv")
write_csv(asthma_saba_at_risk, write_path)
