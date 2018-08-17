# ------------------------------------------------------------------------------
# Title: Prep of MSA asthma care utilization time series
# Author: Ryan Gan
# Date Create: 2018-08-17
# ------------------------------------------------------------------------------

# load library
library(tidyverse)

# load asthma casecross place of service list
load("./data/health/asthma_cc_pos_list.RData")
# extract names
vars_keep <- names(asthma_cc_pos_list[[4]])

# read in saba case-crossover dataset
saba <- read_csv("./data/health/2013-oregon_casecross_saba.csv") %>% 
  rename(identifier = personkey, ZIP = ZIPCODE) %>% 
  mutate(service_place = "SABA Fill",
         dx1 = ndc) %>% 
  # keeping only vars in the other casecrossover dataframes
  dplyr::select(vars_keep, ndc) %>% 
  # turn in to list
  list()

# assign name
names(saba) = "saba"

# adding in age category to each dataframe in list 
# (i should have done this earlier)
asthma_pos_count <- c(asthma_cc_pos_list, saba) %>% 
  map_dfr(., function(df){
    return_df <- df %>% 
      # remove unknown gender
      filter(outcome ==1 ) %>% 
      mutate(metroarea = case_when(MSA == 13460 ~ "Bend",
                                   MSA == 18700 ~ "Corvallis",
                                   MSA == 21660 ~ "Eugene",
                                   MSA == 32780 ~ "Medford",
                                   MSA == 38900 ~ "Portland",
                                   MSA == 41420 ~ "Salem",
                                   MSA == 41999 ~ "Not in MSA"),
             service_place = case_when(service_place == "Emergency Room – Hospital" ~ 
                                         "Emergency Department", 
                                       TRUE ~ service_place)) %>% 
      # group by date, metroarea, and service place
      group_by(date, metroarea, service_place) %>% 
      summarise(n_events = sum(outcome))
  })

# read saba and asthma ed count# read  
population <- read_csv("./data/health/saba_month_counts.csv") %>% 
  dplyr::select(msa_name, POPESTIMATE2013) %>% 
  rename(metroarea = msa_name,
         pop = POPESTIMATE2013) %>% 
  unique()


# read respiratory casecross and find unique zipcodes by MSA values
zip_msa = saba[[1]] %>% 
  select(ZIP, MSA) %>% 
  rename(ZIPCODE = ZIP) %>% 
  mutate(ZIPCODE = as.character(ZIPCODE)) %>% 
  unique()


# read zip pm; join with msa# read  
msa_pm <- read_csv('./data/pm/2013-oregon_zip_pm25.csv', 
                   col_types = cols(ZIPCODE = 'c')) %>% 
  # join with unique MSA vector
  left_join(zip_msa, by = 'ZIPCODE') %>%
  # mutate
  mutate(ZIPCODE = as.factor(ZIPCODE),
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

glimpse(msa_pm)

# join pm to counts ----

# vector of service place
service_place <- unique(asthma_pos_count$service_place)
# vector of msa
metroarea <- sort(unique(msa_pm$metroarea))
# vector of dates
date <- sort(unique(msa_pm$date))


# expand dates so I have equal number of dates for the time series by place of service# expand 
metro_dates <- expand.grid(date, metroarea, service_place) %>% 
  rename(date = Var1, metroarea = Var2, service_place = Var3) %>% 
  mutate(metroarea = as.character(metroarea),
         service_place = as.character(service_place))


# aggregate pm to msa# aggreg 
metro_smk <- msa_pm %>% 
  # groupby metroarea and date
  group_by(metroarea, date) %>% 
  # summarise values of pm2.5 smoke and binary smoke
  # using min for smoke where if any zipcode is exposed then msa had smoke
  summarize(avg_smk_pm = mean(geo_smk_pm), min_smk_pm = min(geo_smk_pm),
            avg_pm = mean(geo_wt_pm), min_pm = min(geo_wt_pm))

# join smoke values and populations to asthma smoke
asthma_smk_ts <- metro_dates %>% 
  left_join(asthma_pos_count, by = c('date', 'metroarea', 'service_place')) %>% 
  # set missing value to 0
  mutate(n_events = ifelse(is.na(n_events), 0, n_events)) %>% 
  # merge population values
  left_join(population, by = 'metroarea') %>% 
  # merge smoke values
  left_join(metro_smk, by = c('metroarea', 'date')) %>% 
  # identify day and weekend
  mutate(day = lubridate::wday(date, label = T), 
         weekend = ifelse(day %in% c('Sat', 'Sun'), 1, 0), 
         month = as.factor(lubridate::month(date)))

glimpse(asthma_smk_ts)

# write permanent file
write_csv(asthma_smk_ts, path = "./data/health/2013-asthma_msa_smk.csv")
