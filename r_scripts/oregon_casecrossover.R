# ------------------------------------------------------------------------------
# Title: Case crossover for diseases (Oregon State) for server
# Author: Jingyang Liu
# Date Created: Feb 24, 2017
# R version: 3.3.2
# ------------------------------------------------------------------------------

#Creating case crossover dataframes
library(tidyverse)
library(data.table)
library(readxl)
getwd()
setwd("C:/Users/jyliu/Desktop/local_git_repo/oregon_wildfire/data_disease")


read_path <- paste0('../data_new/or_hosp_w_outcome_df.csv')
disease <- read_csv(read_path) 

disease <- disease %>%
  # add new transverted from date
  mutate(dates = as.Date(fromdate, "%m/%d/%Y"))



### ---------------------------------------------------------------------
var_list <- c('respiratory', 'asthma', 'pneumonia',  'acute_bronch', 'copd', 
              'cvd', 'isch_heart_dis', 'arrhythmia', 'heart_failure', 
              'cerbrovas_dis', 'myocardial_infarc', 'broken_arm')

start <- Sys.time()
for (i in var_list){
  read_path <- paste('oregon', i, 'claims_2013.csv', sep='_')
  or_disease <- read_csv( read_path)
    
  or_disease <- or_disease %>% 
    group_by(personkey) %>% 
    arrange(personkey, fromdate, line) %>% 
    mutate(num_visit = dense_rank(fromdate)) %>%
    arrange(personkey, num_visit) %>% 
    select(personkey, clmid, num_visit) %>% 
    unique() %>% 
    full_join(or_disease, by = c("personkey", "clmid")) %>% 
    arrange(personkey, clmid, line, fromdate) %>% 
    filter(row_number() == 1) %>%
    mutate(dates = as.Date(fromdate, "%m/%d/%Y")) %>%
    filter(dates >= '2013-07-01' & 
           dates <= '2013-10-31') %>%
    mutate(outcome = 1)
  
  # Create a permanent case-cross over dataset
  file_name <- paste('oregon', i, 'jul_to_oct_claim.csv', sep = '_')
  
  # write permanent dataset
  write_csv(or_disease, paste0("../data_new/", file_name))
  
}
total_time <- Sys.time() - start
total_time # Time difference of 5 mins

setwd("../data_new/")

## Case crossover for each disease
var_list2 <- c('resp1', 'asthma1', 'pneum1',  'acute_bronch1', 'copd1', 'cvd1', 
              'ihd1', 'arrhythmia1', 'hf1', 'cereb_vas1', 'mi1', 'broken_arm1')

start <- Sys.time()
for(m in var_list2){ # begin first loop of variable names (outcomes)
  
  # Case-Crossover loop --------------------------------------------------------
  outcome_col <- which(colnames(disease) == m) # use to keep outcome var
  
  outcome_id <- disease %>%
    # filter total outcome for jth disease
    filter(disease[[m]] == 1) %>% 
    # filter date in July 1, 2012 and Oct 31, 2012
    filter(dates >= '2013-07-01' & 
             dates <= '2013-10-31') %>%
    # arrange with dates
    arrange(dates) %>%
    mutate(id = seq(1, nrow(.), by = 1)) %>% # create subject id
    # select cols, the 4th is disease name
    select(personkey, id, fromdate, dates, 
           (outcome_col), # keep in bracket for outcome var num
           dx1, ZIP, race, ethn, MSA, pos, sex_ind, age_ind)
  
  outcome_col2 <- which(colnames(outcome_id) == m) # use to keep outcome var
  
  # create dataset to populate
  id_date_df <- data_frame(personkey = NA, id =NA, fromdate = NA, dates = NA, 
                           dx1 = NA, ZIP = NA, race = NA, ethn = NA, 
                           MSA = NA, pos = NA, sex_ind = NA, age_ind = NA)
  
  
  # begin second loop to create counterfactual observations for each case subject
  for (k in 1:nrow(outcome_id)){
    
    # find the replicate times of weeks
    dates_l <- outcome_id[[k,4]] 
    n1 <- 0
    d=as.Date("2013-07-01")
    i=1
    while (dates_l >= "2013-07-01"){
      dates_l <- dates_l - 7
      d[i] = dates_l
      i = i+1
      n1 = n1+1
    }
    d[1:n1-1] # shows character(0) when the first week
    n1-1
    
    dates_l <- outcome_id[[k,4]] 
    n2=0
    e=as.Date("2013-10-31")
    j=1
    while (dates_l <= "2013-10-31"){
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
      cov_df$dates <- c(outcome_id[[k,4]], e[1:n2-1])
    } else if (n2==1){
      cov_df <- do.call("bind_rows", replicate(n1+n2-1, outcome_id[k,],simplify = F))
      cov_df$dates <- c(outcome_id[[k,4]], d[1:(n1-1)])
    }else{
      cov_df <- do.call("bind_rows", replicate(n1+n2-1, outcome_id[k,],simplify = F))
      cov_df$dates <- c(outcome_id[[k,4]], d[1:(n1-1)], e[1:n2-1])
    }
    
    # bind unique id and date of the year with covariates
    id_date <- bind_cols(cov_df)
    # iteration which binds rows of unique ids
    id_date_df <- na.omit(bind_rows(id_date_df, id_date))
    
  } # End of inner loop
  
  # make outcome variable, 1 means asthma1 and 0 means no asthma1
  outcome_casecross <- id_date_df %>%
    mutate(outcome = ifelse(dates == as.Date(fromdate, "%m/%d/%Y"), 1, 0)) %>%
    arrange(id, dates) # order by id and date
  
  # Create a permanent case-cross over dataset
  file_name <- paste(m, 'jul_to_oct_casecross.csv', sep = '_')
  
  # write permanent dataset
  write_csv(outcome_casecross, paste0("../data_new/", file_name))
} # End of the overall loop

# sweet this works
total_time <- Sys.time() - start
total_time # Time difference of 48.35907 mins





# Asthma1 ----------------------------------------------------------------------
read_path_ast <- paste0('oregon_asthma_jul_to_oct_claim.csv')
asthma_df <- read_csv(read_path_ast)

disease_asthma1 <- asthma_df %>%
  # add ID
  mutate(id = seq(1, nrow(.), by = 1))

one_asthma1_case <- sample_n(disease_asthma1, 1) 

one_asthma1_case

# find the replicate times of weeks
dates <- one_asthma1_case[[1,74]] 
n1 <- 0
d=as.Date("2013-07-01")
i=1
while (dates >= "2013-07-01"){
  dates <- dates - 7
  d[i] = dates
  i = i+1
  n1 = n1+1
}
d[1:n1-1] # shows character(0) when the first week
n1-1

dates <- one_asthma1_case[[1,74]] 
n2=0
e=as.Date("2013-10-31")
j=1
while (dates <= "2013-10-31"){
  dates <- dates + 7
  e[j]=dates
  j=j+1
  n2 = n2 + 1
}
e[1:n2-1] # shows character(0) when the last week
n2-1

# replicate covariates length of counterfactual dates
# and make conuterfactual dates
if (n1==1){
  cov_df <- do.call("bind_rows", replicate(n1+n2, one_asthma1_case,simplify = F))
  cov_df$dates <- c(one_asthma1_case[[1,3]], e[1:n2-1])
} else if (n2==1){
  cov_df <- do.call("bind_rows", replicate(n1+n2, one_asthma1_case,simplify = F))
  cov_df$dates <- c(one_asthma1_case[[1,3]], d[1:(n1-1)])
}else{
  cov_df <- do.call("bind_rows", replicate(n1+n2-1, one_asthma1_case,simplify = F))
  cov_df$dates <- c(one_asthma1_case[[1,3]], d[1:(n1-1)], e[1:n2-1])
}

# make outcome variable, 1 means asthma1 and 0 means no asthma1
cov_df <- cov_df %>%
  mutate(outcome = ifelse(dates == one_asthma1_case[[1,3]], 1, 0)) %>%
  arrange(id, dates) # order by id and date



### Every diseases--------------------------------------------------------------
# Make 12 separate data frames (excel) for each disease for dx1
# 01/31/2017-02/01/2017

var_list <- c('resp1', 'asthma1', 'pneum1',  'acute_bronch1', 'copd1', 'cvd1', 
              'ihd1', 'arrhythmia1', 'hf1', 'cereb_vas1', 'mi1', 'broken_arm1')

start <- Sys.time()
for(m in var_list){ # begin first loop of variable names (outcomes)
  
  # Case-Crossover loop --------------------------------------------------------
  outcome_col <- which(colnames(disease) == m) # use to keep outcome var
  
  outcome_id <- disease %>%
    # filter total outcome for jth disease
    filter(disease[[m]] == 1) %>% 
    # filter date in July 1, 2012 and Oct 31, 2012
    filter(dates >= '2013-07-01' & 
             dates <= '2013-10-31') %>%
    # arrange with dates
    arrange(dates) %>%
    mutate(id = seq(1, nrow(.), by = 1)) %>% # create subject id
    # select cols, the 4th is disease name
    select(personkey, id, fromdate, dates, 
           (outcome_col), # keep in bracket for outcome var num
           dx1, ZIP, race, ethn, MSA, pos, sex_ind, age_ind)
  
  outcome_col2 <- which(colnames(outcome_id) == m) # use to keep outcome var
  
  # create dataset to populate
  id_date_df <- data_frame(personkey = NA, id =NA, fromdate = NA, dates = NA, 
                           dx1 = NA, ZIP = NA, race = NA, ethn = NA, 
                           MSA = NA, pos = NA, sex_ind = NA, age_ind = NA)
  
  
  # begin second loop to create counterfactual observations for each case subject
  for (k in 1:nrow(outcome_id)){
    
    # find the replicate times of weeks
    dates_l <- outcome_id[[k,4]] 
    n1 <- 0
    d=as.Date("2013-07-01")
    i=1
    while (dates_l >= "2013-07-01"){
      dates_l <- dates_l - 7
      d[i] = dates_l
      i = i+1
      n1 = n1+1
    }
    d[1:n1-1] # shows character(0) when the first week
    n1-1
    
    dates_l <- outcome_id[[k,4]] 
    n2=0
    e=as.Date("2013-10-31")
    j=1
    while (dates_l <= "2013-10-31"){
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
      cov_df$dates <- c(outcome_id[[k,4]], e[1:n2-1])
    } else if (n2==1){
      cov_df <- do.call("bind_rows", replicate(n1+n2-1, outcome_id[k,],simplify = F))
      cov_df$dates <- c(outcome_id[[k,4]], d[1:(n1-1)])
    }else{
      cov_df <- do.call("bind_rows", replicate(n1+n2-1, outcome_id[k,],simplify = F))
      cov_df$dates <- c(outcome_id[[k,4]], d[1:(n1-1)], e[1:n2-1])
    }
    
    # bind unique id and date of the year with covariates
    id_date <- bind_cols(cov_df)
    # iteration which binds rows of unique ids
    id_date_df <- na.omit(bind_rows(id_date_df, id_date))
    
  } # End of inner loop
  
  # make outcome variable, 1 means asthma1 and 0 means no asthma1
  outcome_casecross <- id_date_df %>%
    mutate(outcome = ifelse(dates == as.Date(fromdate, "%m/%d/%Y"), 1, 0)) %>%
    arrange(id, dates) # order by id and date
  
  # Create a permanent case-cross over dataset
  file_name <- paste(m, 'jul_to_oct_casecross.csv', sep = '_')
  
  # write permanent dataset
  write_csv(outcome_casecross, paste0("../data_new/", file_name))
} # End of the overall loop

# sweet this works
total_time <- Sys.time() - start
total_time # Time difference of 48.35907 mins


getwd()
# check
aaa <- disease %>% filter(dates >= '2013-07-01' & 
                            dates <= '2013-10-31')
which(colnames(aaa)=='broken_arm1') # 153
check_broken <- sum(aaa[[153]] == 1) # 74
file_path <- paste0("../data_new/broken_arm1_jul_to_oct_casecross.csv")

# read in dataframe 
check <- read_csv(file_path)


