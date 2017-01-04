# ------------------------------------------------------------------------------
# Title: Basic data wrangle and cleaning of Oregon 2013 claims data
# Author: Ryan Gan
# Date Created: December 30, 2016
# R Version: 3.3.2
# ------------------------------------------------------------------------------

# The purpose of this code is to easily group the ICD-9 codes that make up
# a specific outcome of interest using the ICD-9-CM version 32 list

# Data files can be found here: 
# https://www.cms.gov/Medicare/Coding/ICD9ProviderDiagnosticCodes/codes.html

# load library -----------------------------------------------------------------
library(tidyverse)
library(readxl) # read excel files


# read in icd9 data and create lists of vectors of icd9 codes ----
# read in xlsx icd9 codes and description
icd9_df <- read_excel("./data/CMS32_DESC_LONG_SHORT_DX.xlsx") %>% 
  # rename the terrible variable names
  select(dx_code = 1, long_desc = 2, short_desc = 3)

# look at the first 6 rows of the icd9 codes; comes in sorted
head(icd9_df)
glimpse(icd9_df)

# Overall goal is to make a list of vectors for each outcome that contains the
# character strings of icd9 codes that make up the outcome of interest.
# Some outcomes like COPD, where icd9 codes are not sequential, will not work
# and will need to be entered by hand.

# Create a dataframe of the outcome name, start, and stop rows
outcome_name <- c("respiratory", "asthma", "pneumonia", "acute_bronch",
                  "cvd", "arrhythmia", "cerbrovas_dis", "heart_failure",
                  "isch_heart_dis", "myocardial_infarc", "broken_arm")
# first icd9 code of outcome
start_icd9 <- c("460", "49300", "4800", "4660", "390", "4270", "430", "4280",
                "41000", "41000", "81300")

# last icd9 code of outcome
stop_icd9 <- c("5199", "49392", "486", "46619", "4599", "4279", "4389", "4289",
               "4139", "41092", "81393")

# dataframe of outcomes and start stop of icd9 codes
outcome_icd9_range <- data_frame(outcome_name, start_icd9, stop_icd9)

# list of icd9 codes ----
outcome_icd9_list <- apply(outcome_icd9_range, 1, 
  function(x) { # start of function
    return(icd9_df %>% 
      slice(which(icd9_df$dx_code == x[2]): which(icd9_df$dx_code == x[3])) %>%
      select(1) %>%
      as_vector()
      )  
  } )

names(outcome_icd9_list) <- outcome_name

outcome_icd9_list

# All Respiratory Diseases ICD9 460 to 519 -------------------------------------
start <- which(icd9_df$dx_code == '460') # row 5067 (cold) start of resp outcomes
icd9_df[5067,] # looks correct
# last icd9 code in resp disease is 519.9 (unsp dis of resp sys)
stop <- which(icd9_df$dx_code == '5199') # row 5321 end of resp outcomes
icd9_df[5321,]

# all respiratory vector based on row position
resp_icd9 <- icd9_df %>% slice(start:stop) %>%
  select(dx_code) %>% 
  as_vector()



# COPD, ICD9 490 to 492, 494, and 496 ------------------------------------------
copd_icd9 <- c('490', '4910','4911','49120','49121','49122','4918','4919', '4920',
               '4928', '4940', '4941', '496') 
