# ------------------------------------------------------------------------------
# Title: Case crossover for diseases (Oregon State) for server
# Author: Jingyang Liu
# Date Created: Nov 27, 2017
# R version: 3.3.2
# ------------------------------------------------------------------------------

#Creating case crossover dataframes
library(dplyr) 
library(data.table)
library(readxl)
library(readr)
getwd()
setwd("../../../data/data_new/health/")

### ---------------------------------------------------------------------
var_list <- c('respiratory', 'asthma', 'pneumonia',  'acute_bronch', 'copd', 
              'cvd', 'isch_heart_dis', 'arrhythmia', 'heart_failure', 
              'cerbrovas_dis', 'myocardial_infarc', 'broken_arm')

start <- Sys.time()
for (m in var_list){
  m = "broken_arm"
  read_path <- paste('2013_oregon', m, 'claims.csv', sep='_')
  
  disease <- read_csv(read_path) %>%
    select(personkey, clmid, line, gender, yob, race, ethn, STATE, ZIP, 
           MSA, fromdate, todate, pos, dx1, px1) %>%
    mutate(age = 2017 - yob, 
           dx_outcome = m) %>%
    arrange(personkey, clmid, line, fromdate)
  
  outcome_id <- disease %>%
    # arrange with dates
    arrange(fromdate) %>%
    mutate(id = seq(1, nrow(.), by = 1))  # create subject id
  
  # create dataset to populate
  id_date_df <- data_frame()
  
  
  # begin second loop to create counterfactual observations for each case subject
  for (k in 1:nrow(outcome_id)){
    
    # find the replicate times of weeks
    dates_l <- outcome_id[[k,11]] 
    n1 <- 0
    d=as.Date("2013-01-01")
    i=1
    while (dates_l >= "2013-01-01"){
      dates_l <- dates_l - 7
      d[i] = dates_l
      i = i+1
      n1 = n1+1
    }
    d[1:n1-1] # shows character(0) when the first week
    n1-1
    
    dates_l <- outcome_id[[k,11]] 
    n2=0
    e=as.Date("2013-12-31")
    j=1
    while (dates_l <= "2013-12-31"){
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
      cov_df$fromdate <- c(outcome_id[[k,11]], e[1:n2-1])
    } else if (n2==1){
      cov_df <- do.call("bind_rows", replicate(n1+n2-1, outcome_id[k,],simplify = F))
      cov_df$fromdate <- c(outcome_id[[k,11]], d[1:(n1-1)])
    }else{
      cov_df <- do.call("bind_rows", replicate(n1+n2-1, outcome_id[k,],simplify = F))
      cov_df$fromdate <- c(outcome_id[[k,11]], d[1:(n1-1)], e[1:n2-1])
    }
    
    # bind unique id and date of the year with covariates
    id_date <- bind_cols(cov_df)
    # iteration which binds rows of unique ids
    id_date_df <- bind_rows(id_date_df, id_date)
    
  } # End of inner loop
  
  # make outcome variable, 1 means asthma1 and 0 means no asthma1
  outcome_casecross <- id_date_df %>%
    mutate(outcome = ifelse(fromdate == as.Date(fromdate, "%m/%d/%Y"), 1, 0)) %>%
    arrange(id, fromdate) # order by id and date
  
  # Create a permanent case-cross over dataset
  file_name <- paste('2013_oregon', m, 'casecross.csv', sep = '_')
  
  # write permanent dataset
  write_csv(outcome_casecross, paste0("./", file_name))
} # End of the overall loop

# sweet this works
total_time <- Sys.time() - start
total_time # Time difference of 2.225269 hours
