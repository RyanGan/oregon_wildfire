# ------------------------------------------------------------------------------
# Title: Creation asthma cohort using Oregon APAC
# Author: Ryan Gan
# Date Created: 2017-12-05
# R Version: 3.4
# ------------------------------------------------------------------------------

# load library tidyverse -----
# parallel pacakges
library(parallel) 
# tidyverse
library(tidyverse)

# setup ----
# read vector of personkeys with asthma
vector_path <- paste0("./data/health/2013-oregon_asthma_personkey.csv")
asthma_personkey <- data.table::fread(vector_path, 
                                      colClasses = "character")
# convert to vector
asthma_personkey_v <- asthma_personkey %>% 
  as_vector()

# read path for APAC dataframe
read_path <- paste0("./data/health/gan_episodes_of_care.txt")
# test path
#read_path <- paste0("./data/health/oregon_subset.txt")

# read sections dataframe -------
# there are 77069321 rows (including header) in the Oregon APAC
# I am going to read 200000 rows at a time in parallel to find unique persons
# with an asthma icd9 code in any of the dx variables
# define number of lines
read_length <- 200000
n_start <- seq(from=1, to=77069321, by=read_length)

# test set
# read_length <- 100
# n_start <- seq(from=1, to=10000, by = read_length)

# read first line and get column names
df_col_names <- data.table::fread(read_path, nrows = 0, header = T, 
                                  colClasses = rep("character", 72))
col_names <- colnames(df_col_names) # output column names
# remove 
rm(df_col_names)

# set up parallel computing ----
# find number of processing nodes (cores)
cores <- detectCores() # should be 16 per node
# make cluster/node
cl <- makeCluster(cores)

# load packages on each processor of the node/cluster
clusterCall(cl, function() library(tidyverse))
# export read path to each core
clusterExport(cl, c("read_path", "read_length", "n_start", "col_names", 
  "asthma_personkey_v"), envir = .GlobalEnv)

# register start time
start_time <- proc.time()
# run in parallel -----
# read in rows of the oregon file
df_list <- parLapply(cl, n_start, function(x){
  read_df <- data.table::fread(read_path, sep = "|", header = F,
          # read set number of rows and skip what has already been read
          nrows=read_length, skip=x, colClasses = rep("character", 72))
  # assign column names
  colnames(read_df) <- col_names
  # filter df to subset of asthma icd9s in any dx column
  out_df <- read_df %>% 
    filter(personkey %in% asthma_personkey_v)
  # return vector of personkeys with asthma ids
  return(out_df)
  } # end function
) # end parsapply

# stop cluster
stopCluster(cl)
# find time process took
stop_time <- proc.time() - start_time
stop_time

# create dataframe of subjects with at least one diagnosis of asthma ----
# bind the subsets of lists in to one dataframe
asthma_cohort <- df_list %>% 
  bind_rows()

# print first keys
head(asthma_cohort[,1:10])
# save as csv ----
write_path <- paste0("./data/health/2013-oregon_asthma_cohort.csv")
data.table::fwrite(asthma_cohort, write_path)

