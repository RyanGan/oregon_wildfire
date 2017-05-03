# ------------------------------------------------------------------------------
# Title: Case crossover for diseases (Oregon State) for server
# Author: Jingyang Liu
# Date Created: May 3, 2017
# R version: 3.3.2
# ------------------------------------------------------------------------------

## Case crossover for each disease
## We can get the unique personkey with the first visit from data_new/casecrossover/ file

# library(tidyverse)
library(data.table)
library(readr)
library(dplyr)

getwd() # [1] "/home/jyliu/wildfire/local_git_repo/oregon_wildfire/r_scripts"
setwd("../../../data/data_new/casecrossover")

var_list <- c('respiratory', 'asthma', 'pneumonia',  'acute_bronch', 'copd', 
              'cvd', 'isch_heart_dis', 'arrhythmia', 'heart_failure', 
              'cerbrovas_dis', 'myocardial_infarc', 'broken_arm')


start <- Sys.time()
for(m in var_list){ # begin first loop of variable names (outcomes)

  read_path <- paste('oregon', m, 'may_to_sep_claim.csv', sep='_')
  disease <- read_csv( read_path)
  
  outcome_id <- disease %>%
    # arrange with dates
    arrange(dates) %>%
    mutate(id = seq(1, nrow(.), by = 1))  # create subject id
   
  # create dataset to populate
  id_date_df <- data_frame()
  
  
  # begin second loop to create counterfactual observations for each case subject
  for (k in 1:nrow(outcome_id)){
    
    # find the replicate times of weeks
    dates_l <- outcome_id[[k,74]] 
    n1 <- 0
    d=as.Date("2013-05-01")
    i=1
    while (dates_l >= "2013-05-01"){
      dates_l <- dates_l - 7
      d[i] = dates_l
      i = i+1
      n1 = n1+1
    }
    d[1:n1-1] # shows character(0) when the first week
    n1-1
    
    dates_l <- outcome_id[[k,74]] 
    n2=0
    e=as.Date("2013-09-30")
    j=1
    while (dates_l <= "2013-09-30"){
      dates_l <- dates_l + 7
      e[j]=dates_l
      j=j+1
      n2 = n2 + 1
    }
    e[1:n2-1] # shows character(0) when the last week
    n2-1
    
    # replicate covariates length of counterfactual dates
    # and make conuterfactual dates
    if (n1==1){
      cov_df <- do.call("bind_rows", replicate(n1+n2-1, outcome_id[k,],simplify = F))
      cov_df$dates <- c(outcome_id[[k,74]], e[1:n2-1])
    } else if (n2==1){
      cov_df <- do.call("bind_rows", replicate(n1+n2-1, outcome_id[k,],simplify = F))
      cov_df$dates <- c(outcome_id[[k,74]], d[1:(n1-1)])
    }else{
      cov_df <- do.call("bind_rows", replicate(n1+n2-1, outcome_id[k,],simplify = F))
      cov_df$dates <- c(outcome_id[[k,74]], d[1:(n1-1)], e[1:n2-1])
    }
    
    # bind unique id and date of the year with covariates
    id_date <- bind_cols(cov_df)
    # iteration which binds rows of unique ids
    id_date_df <- bind_rows(id_date_df, id_date)
    
  } # End of inner loop
  
  # make outcome variable, 1 means asthma1 and 0 means no asthma1
  outcome_casecross <- id_date_df %>%
    mutate(outcome = ifelse(dates == as.Date(fromdate, "%m/%d/%Y"), 1, 0)) %>%
    arrange(id, dates) # order by id and date
  
  # Create a permanent case-cross over dataset
  file_name <- paste('or', m, 'may_to_sep_casecross.csv', sep = '_')
  
  # write permanent dataset
  write_csv(outcome_casecross, paste0("./", file_name))
} # End of the overall loop

# sweet this works
total_time <- Sys.time() - start
total_time # Time difference of 1.046643 hours


