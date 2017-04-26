# ------------------------------------------------------------------------------
# Title: Oregon Case-crossover dataframe creation (only patients with single obs)
#        and joining with and zip-level PM2.5 values. 
# Author: Jingyang Liu
# Date Created: 04/05/2017     
# R version: 3.3.3
# ------------------------------------------------------------------------------

# Notes ----
# Notes 8/16/16: I updated smoke data with population weighted averages for zip
# This code also assumed only one visit, so I remove the people with multiple
# admissions for the outcome

# Notes 10/3/16: I'm retaining the county assignment of the observation so I
# can join the county population weighted averages as well for comparison
# and to analyze morbidity similar to Rish

# Libraries ----
# load libraries
#library(tidyverse) # tidyverse not on the server
# if tidyverse is not available, use dplyr and readr
library(dplyr) 
library(readr)
library(lubridate) # working with date
# parallel computing libraries
# library(foreach) 
# library(doParallel)

# Read in smoke data created in loop -------------------------------------------

# read in zipcode level populatoin-weighted pm
read_path <- paste0('C:/Users/jyliu/Desktop/local_git_repo/oregon_wildfire_new/data/Oregon_PM/zip_pm_to_merge_with_acap.csv')

zip_smoke <- read_csv(read_path) # 63801 rows

# descriptives of the two smoke datasets
summary(zip_smoke)

# Zipcode PM2.5 estimates
# create lag variables that take smoke values from n previous days for zipcodes
zip_smoke_w_lag <- zip_smoke %>% arrange(ZIPCODE, date) %>%
  # group by zipcode
  group_by(ZIPCODE) %>% 
  # wrf
  mutate(wrf_f_pm_lag1 = lag(wrf_f_pm, 1, order_by = ZIPCODE), 
         wrf_f_pm_lag2 = lag(wrf_f_pm, 2, order_by = ZIPCODE),
         wrf_f_pm_lag3 = lag(wrf_f_pm, 3, order_by = ZIPCODE),
         wrf_f_pm_lag4 = lag(wrf_f_pm, 4, order_by = ZIPCODE),
         wrf_f_pm_lag5 = lag(wrf_f_pm, 5, order_by = ZIPCODE),
         # wrf no fire lag
         wrf_nf_pm_lag1 = lag(wrf_nf_pm, 1, order_by = ZIPCODE),
         wrf_nf_pm_lag2 = lag(wrf_nf_pm, 2, order_by = ZIPCODE),
         wrf_nf_pm_lag3 = lag(wrf_nf_pm, 3, order_by = ZIPCODE),
         wrf_nf_pm_lag4 = lag(wrf_nf_pm, 4, order_by = ZIPCODE),
         wrf_nf_pm_lag5 = lag(wrf_nf_pm, 5, order_by = ZIPCODE),
         # wrf_smk_pm
         wrf_smk_pm_lag1 = lag(wrf_smk_pm, 1, order_by = ZIPCODE),
         wrf_smk_pm_lag2 = lag(wrf_smk_pm, 2, order_by = ZIPCODE),
         wrf_smk_pm_lag3 = lag(wrf_smk_pm, 3, order_by = ZIPCODE),
         wrf_smk_pm_lag4 = lag(wrf_smk_pm, 4, order_by = ZIPCODE),
         wrf_smk_pm_lag5 = lag(wrf_smk_pm, 5, order_by = ZIPCODE),
         # geo weighted pm
         geo_wt_pm_lag1 = lag(geo_wt_pm, 1, order_by = ZIPCODE),
         geo_wt_pm_lag2 = lag(geo_wt_pm, 2, order_by = ZIPCODE),
         geo_wt_pm_lag3 = lag(geo_wt_pm, 3, order_by = ZIPCODE),
         geo_wt_pm_lag4 = lag(geo_wt_pm, 4, order_by = ZIPCODE),
         geo_wt_pm_lag5 = lag(geo_wt_pm, 5, order_by = ZIPCODE),
         # krig pm
         krig_pm_lag1 = lag(krig_pm, 1, order_by = ZIPCODE),
         krig_pm_lag2 = lag(krig_pm, 2, order_by = ZIPCODE),
         krig_pm_lag3 = lag(krig_pm, 3, order_by = ZIPCODE),
         krig_pm_lag4 = lag(krig_pm, 4, order_by = ZIPCODE),
         krig_pm_lag5 = lag(krig_pm, 5, order_by = ZIPCODE),   
         # background pm
         background_pm_lag1 = lag(background_pm, 1, order_by = ZIPCODE),
         background_pm_lag2 = lag(background_pm, 2, order_by = ZIPCODE),
         background_pm_lag3 = lag(background_pm, 3, order_by = ZIPCODE),
         background_pm_lag4 = lag(background_pm, 4, order_by = ZIPCODE),
         background_pm_lag5 = lag(background_pm, 5, order_by = ZIPCODE),   
         # geo_smk_pm 
         geo_smk_pm_lag1 = lag(geo_smk_pm, 1, order_by = ZIPCODE),
         geo_smk_pm_lag2 = lag(geo_smk_pm, 2, order_by = ZIPCODE),
         geo_smk_pm_lag3 = lag(geo_smk_pm, 3, order_by = ZIPCODE),
         geo_smk_pm_lag4 = lag(geo_smk_pm, 4, order_by = ZIPCODE),
         geo_smk_pm_lag5 = lag(geo_smk_pm, 5, order_by = ZIPCODE),
         # krig smk pm
         krig_smk_pm_lag1 = lag(krig_smk_pm, 1, order_by = ZIPCODE),
         krig_smk_pm_lag2 = lag(krig_smk_pm, 2, order_by = ZIPCODE),
         krig_smk_pm_lag3 = lag(krig_smk_pm, 3, order_by = ZIPCODE),
         krig_smk_pm_lag4 = lag(krig_smk_pm, 4, order_by = ZIPCODE),
         krig_smk_pm_lag5 = lag(krig_smk_pm, 5, order_by = ZIPCODE),
         # temp
         wrf_temp_lag1 = lag(wrf_temp, 1, order_by = ZIPCODE),
         wrf_temp_lag2 = lag(wrf_temp, 2, order_by = ZIPCODE),
         wrf_temp_lag3 = lag(wrf_temp, 3, order_by = ZIPCODE),
         wrf_temp_lag4 = lag(wrf_temp, 4, order_by = ZIPCODE),
         wrf_temp_lag5 = lag(wrf_temp, 5, order_by = ZIPCODE)) %>% 
  # ungroup by zip
  ungroup(ZIPCODE) %>% 
  # attach a zip indicator for each smoke variable
  setNames(paste(colnames(.), "zip", sep="_")) %>% 
  # remove the '_zip' from the zipcode and date variable 
  rename(ZIPCODE = ZIPCODE_zip, date = date_zip)

# # check 
 check <- zip_smoke_w_lag %>% 
   select(ZIPCODE, date, geo_smk_pm_zip, geo_smk_pm_lag1_zip,
         geo_smk_pm_lag2_zip, geo_smk_pm_lag3_zip) # looks good



setwd("../../data_new/update/")

# set variable list
var_list <- c('respiratory', 'asthma', 'pneumonia',  'acute_bronch', 'copd', 
              'cvd', 'isch_heart_dis', 'arrhythmia', 'heart_failure', 
              'cerbrovas_dis', 'myocardial_infarc', 'broken_arm')

start <- Sys.time()
for(j in var_list) { # begin first loop of variable names (outcomes)
  
  # standard for statement without parallel computing
  #for(j in 1:length(var_list)){
  read_path <- paste('or', j, 'may_to_sep_casecross.csv', sep='_')
  or_disease <- read_csv(read_path)
  
  ### try join
  colnames(or_disease)[24] <- c("ZIPCODE")
  colnames(or_disease)[74] <- c("date")
  
  outcome_casecross <- or_disease %>%
    # indicator for male=0, female=1, unknown = 2
    mutate(age = 2013-yob,
           sex_ind =ifelse(gender == "F", 1, 
                           ifelse(gender == "M", 0, 2)),
           age_ind = ifelse(age < 15, 0,
                            ifelse(age >= 15 & age < 65, 1,
                                   ifelse(age >= 65 & age <=105, 2, NA)))
    ) %>% # end of mutate 
    # create variables
    mutate(day = as.factor(weekdays(fromdate)),
           day_admit = as.factor(weekdays(date)),
           month_smk = month(fromdate),
           month_admit = month(date),
           season_smk = ifelse(fromdate >= "2013-03-20" &  
                                 fromdate <= "2013-06-21", "spring",
                               ifelse(fromdate >= "2013-06-22" &  
                                        fromdate <= "2013-09-22", "summer",
                                      ifelse(fromdate >= "2013-09-23" & 
                                               fromdate <= "2013-12-21", "fall", "other"))),
           season_admit = ifelse(date >= "2013-03-20" &  
                                   date <= "2013-06-21", "spring",
                                 ifelse(date >= "2013-06-22" &  
                                          date <= "2013-09-22", "summer",
                                        ifelse(date >= "2013-09-23" & 
                                                 date <= "2013-12-21", "fall", "other")))) %>%
    # join with zip-level pm estimates
    left_join(zip_smoke_w_lag, by = c("date", "ZIPCODE"))%>%
    arrange(personkey, fromdate) # order by id and fromdate
  
  
  
  
  # checks
  # glimpse(outcome_casecross)
  # which(colnames(outcome_casecross)=='geo_wt_pm_zip')
  # check <- outcome_casecross[, c(1:16, 20, 89, 111:133)]
  
  # Create a permanent case-cross over datasets
  file_name <- paste0('../health_data/', j, 
                      '_may_to_sep_time_strat_casecross.csv')
  
  # write permanent dataset
  write_csv(outcome_casecross, file_name)
  
} # End of the overall loop

total_time <- Sys.time() - start
total_time # 8.097143 mins



