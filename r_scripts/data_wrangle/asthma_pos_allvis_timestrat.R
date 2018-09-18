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

# asthma place of service ----

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


# set up cores
cl <- makeCluster(6)

# load packages on each processor of the node/cluster
clusterCall(cl, function() c(library(tidyverse), library(case.crossover)))

# export new set of objects to global objects to cluster
clusterExport(cl, c("asthma_list"), 
              envir = .GlobalEnv)

# parallel creation of casecrossover list -----
# start time
start <- Sys.time()

asthma_cc_pos_list <- parLapply(cl, asthma_list, function(x){
#asthma_cc_pos_list <- lapply(asthma_list, function(x){
  pos_df <- x %>% 
      # find the first observation and first claim line of each person 
      group_by(clmid) %>%
      arrange(fromdate, line) %>%
      ungroup() 
    
    ts_df <- time_stratified(data = x, id = "clmid", 
      covariate = c("personkey", "gender", "age", "MSA", "ZIP", 
                    "pos", "dx1", "service_place"), 
      admit_date = "fromdate", 
      start_date = "2013-05-01", end_date = "2013-09-30", interval = 7)
})

stop <- Sys.time()
time <- stop - start
print(time)


# close cores 
stopCluster(cl)

# save casecross list as r data object
save(asthma_cc_pos_list, file = "./data/health/asthma_cc_pos_allobs_list.RData")
