# ------------------------------------------------------------------------------
# Title: Basic data wrangle and cleaning of Oregon 2013 claims data
# Author: Ryan Gan
# Date Created: November 22, 2016
# ------------------------------------------------------------------------------

# Datasets are pretty large, R might not work

# load libraries ----
library(tidyverse)
library(data.table)

list.files("./data")
# lets see if readr can import the txt file
read_path <- paste0("./data/gan_episodes_of_care.txt")
# reading only the first 100 rows to get a feel for these data
oregon_df <- fread(read_path, sep = "|", nrows = 100)
summary(oregon_df)
getwd()
