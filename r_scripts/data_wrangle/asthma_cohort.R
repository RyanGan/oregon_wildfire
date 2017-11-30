# ------------------------------------------------------------------------------
# Title: Creation of asthma and beta2 agonist cohort
# Author: Ryan Gan
# Date Created: 2017-11-29
# R Version: 3.4
# ------------------------------------------------------------------------------

# working with a example txt file of 10,000 rows to get a feel for data

# load library tidyverse
library(tidyverse)

# read in dataframe ----
read_path <- paste0("./data/health/oregon_subset.txt")
# read in with fread
oregon_df <- data.table::fread(read_path, sep = "|",
                               colClasses = c(rep("character", 72)), stringsAsFactors = FALSE)

head(oregon_df)
tail(oregon_df)

# set *NULL* and blank to NA
oregon_df <- oregon_df %>% 
  # replace *NULL* with NA
  mutate_all(funs(replace(., .== "*NULL*" | . == "", NA)))


