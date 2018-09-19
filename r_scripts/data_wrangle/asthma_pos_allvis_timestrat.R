# ------------------------------------------------------------------------------
# Title: Asthma place of service time-stratified case-crossover using all visits
# Author: Ryan Gan
# Date Created: 2018-09-18
# ------------------------------------------------------------------------------

# Description: This script subsets the asthma claims to specific service places
# and creates time-stratified case-crossover dataframes


# library ----
library(tidyverse)
library(case.crossover)
library(parallel)
library(lubridate)
library(survival) # for clogit

# asthma place of service ----
# read in pm2.5 data
pm <- read_csv("./data/pm/2013-oregon_zip_pm25.csv") %>% 
  # create 10 unit variables
  mutate(geo_pm10 = geo_wt_pm/10,
         geo_smk10 = geo_smk_pm/10,
         ZIPCODE = as.character(ZIPCODE)) %>% 
  rename(ZIP = ZIPCODE)

# read in asthma claims
asthma_claims <- read_csv('./data/health/2013-oregon_asthma_fireseason_cohort.csv',
                          col_types = cols(.default = "c")) %>% 
  # filter to just primary diagnosis or saba fill
  filter(visit_type != 'dx_asthma_not_primary')%>% 
  # filter to date of PM2.5 data (wildfire season)
  filter(fromdate >= "2013-05-01" & fromdate <= "2013-09-30") %>% 
  # filter to oregon
  filter(STATE == "OR") %>%
  # filter to places of service
  filter(pos_simple %in% c('Ambulance', 'Emergency Room Hospital', 
                           'Inpatient Hospital', 'Office', 'Outpatient Hospital',
                           'Pharmacy', 'Urgent Care')) %>% 
  # group by person id and date
  group_by(personkey, service_place, fromdate) %>% 
  filter(row_number()==1) %>% 
  mutate(date = as.Date(fromdate), 
         ZIPCODE = as.character(ZIP))


# count of each place of service for each patient
# service_utilization <- asthma_claims %>% 
#   group_by(personkey, pos_simple) %>% 
#   summarize(count = n())
# 
# # write event counts
# event_stats <- service_utilization %>% 
#   group_by(pos_simple) %>% 
#   summarize(n = n(), total_vis = sum(count), mean_vis = mean(count), 
#             med_vis = median(count), min_vis = min(count), max_vis = max(count))

# write_csv(event_stats, './data/health/asthma_saba_event_sum_stats.csv')

# split out each place of service observation to list
# transform pos_subset to list
asthma_list <- asthma_claims %>% 
  # split lists
  split(.$pos_simple) %>% 
  as.list() 

# check asthma list
head(asthma_list[[1]])

# set up cores
cl <- makeCluster(6)

# load packages on each processor of the node/cluster
clusterCall(cl, function() c(library(tidyverse), library(case.crossover),
                             library(survival), library(lubridate)))

# export new set of objects to global objects to cluster
clusterExport(cl, c("asthma_list"), 
              envir = .GlobalEnv)

# parallel creation of casecrossover list -----
# start time
start <- Sys.time()

#asthma_cc_pos_list <- parLapply(cl, asthma_list, function(x){
asthma_cc_pos_list <- lapply(asthma_list[1:2], function(x){
    pos <- unique(x$pos_simple)
    print(pos)
    # set up full period timestrat df
    fullperiod_df <- time_stratified(data = x, id = "clmid", 
      covariate = c("personkey", "gender", "age", "MSA", "ZIP", 
                    "pos", "dx1", "pos_simple"), 
      admit_date = "fromdate", 
      start_date = "2013-05-01", end_date = "2013-09-30", interval = 7) %>% 
      left_join(pm, by = c("date", "ZIP")) %>% 
      filter(!is.na(geo_smk10))
    # check
    head(fullperiod_df)
    # conditional logistic model
    fullperiod_mod <- clogit(outcome ~ geo_smk10 + wrf_temp + strata(identifier), 
                  data = fullperiod_df)
    
    summary(fullperiod_mod)
    # n events
    n_events <- fullperiod_mod$nevent
    # odds ratio and 95% CI
    estimate_full <- broom::tidy(fullperiod_mod) %>% 
      filter(term == "geo_smk10") %>% 
      dplyr::select(term, estimate, conf.low, conf.high) %>% 
      mutate_at(2:4, funs(round(exp(.),3))) %>% 
      cbind(pos, n_events, .) %>% 
    mutate(ref_period = "Wildfire Season") %>% 
    dplyr::select(ref_period, pos:conf.high)
    
    # remove fullperiod_df to save space
    rm(fullperiod_df, fullperiod_mod)

    # month timestrat
    month_df <- casecross(data = x, id = 'clmid', date = "date", 
                          covariate = c("personkey", "gender", "age", "MSA", "ZIP", 
                                        "pos", "dx1", "pos_simple"), 
                          period = 'month') %>% 
      mutate(date = as.Date(date),
             outcome = as.numeric(outcome)) %>% 
      left_join(pm, by = c("date", "ZIP")) %>% 
      filter(!is.na(geo_smk10))
    
    # conditional logistic model
    month_mod <- clogit(outcome ~ geo_smk10 + wrf_temp + strata(id), 
                             data = month_df)
    
    # n events
    n_events <- month_mod$nevent
    # odds ratio and 95% CI
    estimate_month <- broom::tidy(month_mod) %>% 
      filter(term == "geo_smk10") %>% 
      dplyr::select(term, estimate, conf.low, conf.high) %>% 
      mutate_at(2:4, funs(round(exp(.),3))) %>% 
      cbind(pos, n_events, .) %>% 
      mutate(ref_period = "Month") %>% 
      dplyr::select(ref_period, pos:conf.high)
    
    # bind full and month mods together
    estimates <- rbind(estimate_full, estimate_month)

})

# bind final list to results csv
results <- map_dfr(asthma_cc_pos_list, rbind)

stop <- Sys.time()
time <- stop - start
print(time)


# close cores 
stopCluster(cl)

# save results as csv
save(results, file = "./data/health/asthma_care_results_sensitivity.csv")
