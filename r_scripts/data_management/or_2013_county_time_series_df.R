# ------------------------------------------------------------------------------
# Title: Creation of county-level time-series dataframe for Oregon 2013
# Author: Jingyang Liu
# Date Created: 9/28/16
# ------------------------------------------------------------------------------

# Load libraries -----
library(tidyverse)

# Import dataframe with outcome ---- -------------------------------------------

# Note: file is saved on my work computer or the secured CSU server
# create read path
var_list <- c('respiratory', 'asthma', 'pneumonia',  'acute_bronch', 'copd', 
              'cvd', 'isch_heart_dis', 'arrhythmia', 'heart_failure', 
              'cerbrovas_dis', 'myocardial_infarc', 'broken_arm')

or_disease <- data.frame()
n = 1

setwd("C:/Users/jyliu/Desktop/local_git_repo/oregon_wildfire_new/data_new/update")
start <- Sys.time()
for (i in var_list){
  i <- "asthma"
  read_path <- paste('or', i, 'may_to_sep_casecross.csv', sep='_')
    or_disease <- read_csv(read_path)

}
total_time <- Sys.time() - start
total_time # Time difference of 14 sec


summary(or_disease)

# Join data with names of Oregon counties 

oregon_fips <- read_csv('../../instructions/oregon_FIPS.csv')
which(oregon_fips=="Hood River")
oregon_fips$county[14] <- "Hood.River"
oregon_fips$fips <- as.character(oregon_fips$fips)

read_path <- paste0('../../data_new/update/or_zip_county_prop.csv')
or_zip_county <- read_csv(read_path)

or_zip_county <- or_zip_county %>%
  select(zip, county_name) %>%
  rename(county = county_name) %>%
  rename(ZIP = zip) %>%
  full_join(oregon_fips, by = "county")


# Import population weighted county PM2.5
pm_path <- paste0("../../data_new/county_data/or_county_pop_wt_pm.csv")

county_pm <- read_csv(pm_path)

summary(as.factor(county_pm$county))

# Note: Think about how to work in patient id; single or multiobs?

# create a working dataset 'chars_2012'
or_2012_county_time_series_df <- or_disease %>% 
  # filter to 2012 dates (prevents parsing failures later on)
  # filter(admit_date_impute >= "2012-01-01" & admit_date_impute <= "2012-12-31") %>% 
  # join in the washington county name based on CHARS county identifier
  full_join(or_zip_county, by = "ZIP") %>% 
  # rename 'admit_date_impute' to 'date'
  rename(date = dates) %>%
  # rename(date = admit_date_impute) %>% 
  # group by county of residence and date
  group_by(county, date) %>% 
  # sum up each primary diagnosis for each outcome for each day for each county
  summarise(n_obs = n(), asthma_n = sum(outcome)) %>% 
  # join daily county estimates of pm2.5
  full_join(county_pm, by = c('date', 'county'))
  

summary(or_2012_county_time_series_df)

# write permanent dataframe

write_path <- paste0("../../or_asthma_county_time_series.csv")

write_csv(or_2012_county_time_series_df, write_path)
