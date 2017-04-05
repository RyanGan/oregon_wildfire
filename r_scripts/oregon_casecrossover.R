# ------------------------------------------------------------------------------
# Title: Case crossover for diseases (Oregon State)
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

### ---------------------------------------------------------------------
var_list <- c('respiratory', 'asthma', 'pneumonia',  'acute_bronch', 'copd', 
              'cvd', 'isch_heart_dis', 'arrhythmia', 'heart_failure', 
              'cerbrovas_dis', 'myocardial_infarc', 'broken_arm')

start <- Sys.time()
for (i in var_list){
  read_path <- paste('oregon', i, 'claims_2013.csv', sep='_')
  or_disease <- read_csv(read_path)
    
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
    # add new transverted from date
    mutate(dates = as.Date(fromdate, "%m/%d/%Y")) %>%
    filter(dates >= '2013-05-01' & 
           dates <= '2013-09-30') 
  
  # Create a permanent case-cross over dataset
  file_name <- paste('oregon', i, 'may_to_sep_claim.csv', sep = '_')
  
  # write permanent dataset
  write_csv(or_disease, paste0("../data_new/", file_name))
  
}
total_time <- Sys.time() - start
total_time # Time difference of 5.154874 mins

setwd("../data_new/update")



## Case crossover for each disease

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
total_time # Time difference of 2.225269 hours


## Check
read_path <- paste0("or_asthma_may_to_sep_casecross.csv")
sam_1 <- read_csv(read_path)

head(sam_1, 20L)

check <- sam_1 %>% filter(outcome == 1) %>% 
  group_by(personkey) %>% 
  summarise(n = n())

summary(check)

check2 <- sam_1 %>% filter(personkey=="409060") %>% 
  select(personkey, fromdate, todate, dates, outcome, dx1, ZIP)

read_path2 <- paste0()



## Try -------------------------------------------------------------------------
# Asthma1 ----------------------------------------------------------------------
read_path_ast <- paste0('oregon_asthma_jul_to_oct_claim.csv')
asthma_df <- read_csv(read_path_ast)

disease_asthma1 <- asthma_df %>%
  # add ID
  mutate(id = seq(1, nrow(.), by = 1))

one_asthma1_case <- sample_n(disease_asthma1, 1) 

one_asthma1_case

# find the replicate times of weeks
sam_date <- one_asthma1_case[[1,74]] 
n1 <- 0
d=as.Date("2013-07-01")
i=1
while (sam_date >= "2013-07-01"){
  sam_date <- sam_date - 7
  d[i] = sam_date
  i = i+1
  n1 = n1+1
}
d[1:n1-1] # shows character(0) when the first week
n1-1

sam_date <- one_asthma1_case[[1,74]] 
n2=0
e=as.Date("2013-10-31")
j=1
while (sam_date <= "2013-10-31"){
  sam_date <- sam_date + 7
  e[j]=sam_date
  j=j+1
  n2 = n2 + 1
}
e[1:n2-1] # shows character(0) when the last week
n2-1

# replicate covariates length of counterfactual dates
# and make conuterfactual dates
if (n1==1){
  cov_df <- do.call("bind_rows", replicate(n1+n2, one_asthma1_case,simplify = F))
  cov_df$dates <- c(one_asthma1_case[[1,74]], e[1:n2-1])
} else if (n2==1){
  cov_df <- do.call("bind_rows", replicate(n1+n2, one_asthma1_case,simplify = F))
  cov_df$dates <- c(one_asthma1_case[[1,74]], d[1:(n1-1)])
}else{
  cov_df <- do.call("bind_rows", replicate(n1+n2-1, one_asthma1_case,simplify = F))
  cov_df$dates <- c(one_asthma1_case[[1,74]], d[1:(n1-1)], e[1:n2-1])
}

# make outcome variable, 1 means asthma1 and 0 means no asthma1
cov_df <- cov_df %>%
  mutate(outcome2 = ifelse(dates == one_asthma1_case[[1,74]], 1, 0)) %>%
  arrange(id, dates) # order by id and date

