# ------------------------------------------------------------------------------
# Title: Creation of asthma and saba fill Oregon and MSA time series
# Author: Ryan Gan
# Date Created: 2018-09-12
# ------------------------------------------------------------------------------

# Script purpose is to create time series data frames for Oregon, and each
# Oregon MSA for analysis

# load tidyverse library
library(tidyverse)

# I am able to create the time series I need using the asthma fireseason cohort
# dataset created. This contains all primary diagnoses of asthma and saba fills
# during the study period.

# read asthma cohort (n = 550610)
asthma_c <- read_csv('./data/health/2013-oregon_asthma_fireseason_cohort.csv',
                     col_types = cols(.default = "c")) %>% 
  # filter to just primary diagnosis or saba fill
  filter(visit_type != 'dx_asthma_not_primary')

# find zipcodes in each metroarea
zip <- asthma_c %>% 
  select(ZIP, MSA) %>% 
  rename(ZIPCODE = ZIP) %>% 
  unique()

# read zip pm; join with msa
msa_pm = read_csv('./data/pm/2013-oregon_zip_pm25.csv', 
                  col_types = cols(ZIPCODE = 'c')) %>% 
  # join with unique MSA vector
  left_join(zip, by = 'ZIPCODE') %>%
  # mutate
  mutate(ZIPCODE = as.character(ZIPCODE),
         MSA = as.factor(MSA),
         # assign metro name to number
         metroarea = case_when(MSA == 13460 ~ "Bend",
                               MSA == 18700 ~ "Corvallis",
                               MSA == 21660 ~ "Eugene",
                               MSA == 32780 ~ "Medford",
                               MSA == 38900 ~ "Portland",
                               MSA == 41420 ~ "Salem"))  %>% 
  # filter to zips in an MSA only
  filter(!is.na(metroarea))

# It's possible we could duplicate claimids; not sure I want to count these
claim_count <- asthma_c %>% 
  group_by(clmid) %>% 
  summarize(count = n())
# looking at an example where a person had multiple asthma inhaler fills with
# same claimid. It looks like they are all filled on unique dates, so I'm going
# to assume that even though it has same claimid they are unique fills
check <- filter(asthma_c, clmid == '162416557')

# this is a case where it's a duplicate claim I think
check2 <- filter(asthma_c, clmid == '289349081')

# claim with no place of service
check3 <- filter(asthma_c, clmid == '258561201')
# solution may be to take only one observation forunique dates for person/claims

# n unique asthma and saba events during the time period
# n = 161329 total
event_count <- asthma_saba_unique_visit %>% 
  group_by(pos_simple) %>% 
  summarize(count = n()) %>% 
  filter(pos_simple %in% c('Ambulance', 'Emergency Room Hospital', 
                           'Inpatient Hospital', 'Office', 'Outpatient Hospital',
                           'Pharmacy', 'Urgent Care'))
         
event_count

event_stats <- event_count %>% 
  group_by(pos_simple) %>% 
  summarize(total_vis = sum(count), mean_vis = mean(count), med_vis = median(vis),
            min_vis = min(count), max_vis = max(count))

# write event counts to csv file
write_csv(event_count, './data/health/2013-fireseason_asthma_counts.csv')

# limit to unique claims based on unique date and place of service
asthma_saba_unique_visit <- asthma_c %>% 
  # group by person id and date
  group_by(personkey, service_place, fromdate) %>% 
  filter(row_number()==1) %>% 
  mutate(date = as.Date(fromdate), 
         ZIPCODE = as.character(ZIP)) %>% 
  select(-metroarea) %>% 
  # join with pm values 
  left_join(msa_pm, by = c('ZIPCODE', 'date', 'MSA')) %>% 
  # filter to only MSAs 
  filter(!is.na(metroarea)) %>% 
  # filter to following places of service
  filter(pos_simple %in% c('Ambulance', 'Emergency Room Hospital', 
                           'Inpatient Hospital', 'Office', 'Outpatient Hospital',
                           'Pharmacy', 'Urgent Care'))

# read in population denom for msa
population <- read_csv("./data/health/saba_month_counts.csv") %>% 
  dplyr::select(msa_name, POPESTIMATE2013) %>% 
  rename(metroarea = msa_name,
         pop = POPESTIMATE2013) %>% 
  unique()

# time series counts
asthma_msa_ts <- asthma_saba_unique_visit %>% 
  # rename pharamcy to saba fill
  mutate(pos_simple = case_when(pos_simple == 'Pharmacy' ~ 'SABA Fill',
                                pos_simple == 'Emergency Room Hospital' ~ 'Emergency Department',
                                TRUE ~ pos_simple)) %>% 
  left_join(population, by = 'metroarea') %>% 
  # group by date, metroarea and place of service
  group_by(date, metroarea, pos_simple) %>% 
  summarize(n_events = n(), pop = max(pop), avg_smk_pm = mean(geo_smk_pm),
            avg_temp = mean(wrf_temp)) %>% 
  # set missing value to 0
  mutate(n_events = ifelse(is.na(n_events), 0, n_events)) %>% 
  # identify day and weekend
  mutate(day = lubridate::wday(date, label = T), 
         weekend = ifelse(day %in% c('Sat', 'Sun'), 1, 0), 
         month = as.factor(lubridate::month(date))) %>% 
  # rename place of service
  rename(service_place = pos_simple)

# write file
write_csv(asthma_msa_ts, './data/health/2013-asthma_msa_smk.csv')

summary(asthma_msa_ts$date)


