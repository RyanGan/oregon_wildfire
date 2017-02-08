# ------------------------------------------------------------------------------
# Title: Oregon 2013 claims data
# Author: Jingyang Liu
# Date Created: Feb 8th, 2017
# R Version: 3.3.2
# ------------------------------------------------------------------------------

library(dplyr)
library(data.table)


# import data ------------------------------------------------------------------

# Read in first 2000000 lines for exploration.

read_path <- paste0("./data/gan_episodes_of_care.txt")
oregon_df <- fread(read_path, sep = "|", nrows = 2000000, showProgress = T)
fwrite(oregon_df, "./data/oregon_epis_care_reduced.csv")

# I wrote a reduced dataframe of 2000,000 rows to get an idea of data structure

# import the reduced dataframe
start_time <- Sys.time()

oregon_df <- fread("./data/oregon_epis_care_reduced.csv", sep = ",", 
                   colClasses = rep("character", 72))

stop_time <-  Sys.time() - start_time 
# time it took
stop_time # 19.55287 secs

#oregon_df[1:10, 64]

# Define read paths ------------ -----------------------------------------------
# import r list of outcome-specific icd9 codes
load_path <- paste0("./data/outcome_list.RData")
load(load_path)

# path and file name of data
read_path <- paste0("./data/oregon_epis_care_reduced.csv")

summary(as.factor(oregon_df$dx1))
summary(as.factor(oregon_df$ndc))



