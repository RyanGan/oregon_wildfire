# ------------------------------------------------------------------------------
# Title: Basic data wrangle and cleaning of Oregon 2013 claims data
# Author: Ryan Gan
# Date Created: December 30, 2016
# ------------------------------------------------------------------------------

# Note: Datasets are pretty large, R might not work

# load libraries ---------------------------------------------------------------
library(tidyverse)
library(data.table)

# import data ------------------------------------------------------------------
# zip path
zip_path <- paste0("./data/gan_episodes_of_care.zip")
testconnection <- file(zip_path, open="r")
readsize <- 20000
n_lines <- 0
(while((linesread <- length(readLines(testconnection, readsize))) > 0) 
  n_lines <- n_lines+linesread)
close(testconnection)
n_lines # 20,720,000, there are probably more records than this


# Read in first 100000 lines to mess around with in R until I figure out another
# way. I will then delete the text file 
#read_path <- paste0("./data/gan_episodes_of_care.txt")

#oregon_df <- fread(read_path, sep = "|", nrows = 200000, showProgress = T)

#fwrite(oregon_df, "./data/oregon_epis_care_reduced.csv")
start_time <- Sys.time()

oregon_df <- fread("./data/oregon_epis_care_reduced.csv", sep = ",", 
                   colClasses = rep("character", 72))

stop_time <-  Sys.time() - start_time 
# time it took
stop_time

oregon_df[1:10, 64]

# Notes on first import
head(oregon_df)
glimpse(oregon_df)
xtabs(~dx1, oregon_df)
getwd()
object.size(oregon_df)

# figure out a test script to read chunks of the file and move on
# number of lines to read

# I need a dataframe of start row and length to read after
n_start<- seq(from = 1, to = 200000, by = 10000) 

#n_start<- seq(from = 1, to = 100, by = 5)   
# create dataframe
n_read <- data_frame(n_start) %>% # turn the sequence in to a dataframe
  mutate(n_length = 10000) # set length to read 

# test dataframe
start_time <- Sys.time()

test_df <- do.call(rbind, apply(n_read,1, function(x) 
  return(fread("./data/oregon_epis_care_reduced.csv", sep =",",
               nrows=x[2], skip=x[1], header = F, 
               # keep all the variables as character to avoid import errors
               # 72 variables
               colClasses = rep("character", 72))) %>% 
    # allows me to subset (1st of Jan in this case)
    # i want to filter by scanning through each dx column and looking 
    # for specific outcomes
    filter(V24 == "2013-11-01")))

colnames(test_df) <- df_col_names

stop_time <-  Sys.time() - start_time 

stop_time

df_col_names <- colnames(oregon_df)

# check to make sure dataset imported
head(test_df$V1)
head(oregon_df$personkey)
tail(oregon_df$personkey)
tail(test_df$V1)

# the script above will sequentially import the oregon data. I need to figure
# out if I can screen for certain icd9 codes

# here is a way to read sequences of rows
# create sample dataset
set.seed(1)
m   <- matrix(rnorm(1e5),ncol=10)
csv <- data.frame(x=1:1e4,m)
write.csv(csv,"test.csv")
# s: rows we want to read
s <- c(1:50,53, 65,77,90,100:200,350:500, 5000:6000)
# v: logical, T means read this row (equivalent to your read_vec)
v <- (1:1e4 %in% s)

seq  <- rle(v)

idx  <- c(0, cumsum(seq$lengths))[which(seq$values)] + 1
idx
# indx: start = starting row of sequence, length = length of sequence (compare to s)
indx <- data.frame(start=idx, length=seq$length[which(seq$values)])

result <- do.call(rbind,apply(indx,1, function(x) return(fread("test.csv",nrows=x[2],skip=x[1]))))



