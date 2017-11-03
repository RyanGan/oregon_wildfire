# ------------------------------------------------------------------------------
# Title: Creation of county-level time-series only er visit dataframe for Oregon 2013
# Author: Jingyang Liu
# Date Created: May 2 2017
# ------------------------------------------------------------------------------

# Load libraries -----
library(tidyverse)

# Import dataframe with outcome ---- -------------------------------------------

# Note: file is saved on my work computer or the secured CSU server
# create read path
getwd()
setwd("C:/Users/jyliu/Desktop/local_git_repo/oregon_wildfire_new/data_disease")

### ---------------------------------------------------------------------
var_list <- c('respiratory', 'asthma', 'pneumonia',  'acute_bronch', 'copd', 
              'cvd', 'isch_heart_dis', 'arrhythmia', 'heart_failure', 
              'cerbrovas_dis', 'myocardial_infarc', 'broken_arm')
or_list <- list()

start <- Sys.time()
for (i in var_list){
  read_path <- paste('oregon', i, 'claims_2013.csv', sep='_')
  or_disease <- read_csv(read_path)
  
  or_disease1 <- or_disease %>%
    filter(pos == 23) %>%
    group_by(personkey) %>% 
    arrange(personkey, fromdate, line) %>% 
    mutate(num_visit = dense_rank(fromdate)) %>%
    group_by(personkey, fromdate) %>%
    select(personkey, fromdate, num_visit, clmid, line, ZIP) %>% 
    filter(row_number() == 1) %>%
    mutate(dates = as.Date(fromdate, "%m/%d/%Y")) %>%
    mutate(outcome = 1)
  
  colnames(or_disease1)[colnames(or_disease1)=="outcome"] <- i
  or_list[[which(var_list==i)]] <- or_disease1

}
total_time <- Sys.time() - start
total_time # Time difference of 12 sec

summary(or_list)

or_2013_disease <- or_list[[1]]%>%
  full_join(or_list[[2]], by=c("personkey", "fromdate", "num_visit", "clmid", "line", "ZIP", "dates")) 

for(j in 3:12){
  or_2013_disease <- or_2013_disease %>%
    full_join(data.frame(or_list[[j]]), by=c("personkey", "fromdate", "num_visit", "clmid", "line", "ZIP", "dates")) 
}

or_2013_disease[is.na(or_2013_disease)] <- 0

# Join data with names of Oregon counties 
oregon_fips <- read_csv('../instructions/oregon_FIPS.csv')
# remove the "County" character
# factor(oregon_fips$county)
oregon_fips$county <- gsub(" County", "", as.character(factor(oregon_fips$county)))

which(oregon_fips$county=="Hood River")
oregon_fips$county[14] <- "Hood.River"

oregon_fips <- oregon_fips %>%
  mutate(st_county_fips = with(oregon_fips, paste0(st_code, fips)))

### Join county with zip
read_path <- paste0('../data_new/update/or_zip_county_prop.csv')
or_zip_county <- read_csv(read_path)

or_zip_county <- or_zip_county %>%
  select(zip, county_name) %>%
  rename(county = county_name) %>%
  rename(ZIP = zip) %>%
  full_join(oregon_fips, by = "county") %>%
  select(ZIP, county, state, st_county_fips) %>%
  rename(fips = st_county_fips)

# Note: Think about how to work in patient id; single or multiobs?

# create a working dataset 'or_2013'
or_2013_county_er_time_series <- or_2013_disease %>% 
  # join in the washington county name based on CHARS county identifier
  left_join(or_zip_county, by = "ZIP") %>% 
  # rename 'admit_date_impute' to 'date'
  rename(date = dates) %>%
  # rename(date = admit_date_impute) %>% 
  # group by county of residence and date
  group_by(county, date) %>% 
  # sum up each primary diagnosis for each outcome for each day for each county
  summarise(n_obs = n(), resp_n = sum(respiratory), asthma_n = sum(asthma), 
            pneum_n = sum(pneumonia), acute_bronch_n = sum(acute_bronch), 
            copd_n = sum(copd), cvd_n = sum(cvd), ihd_n = sum(isch_heart_dis), 
            arrythmia_n = sum(arrhythmia), hf_n = sum(heart_failure), 
            cereb_vas_n = sum(cerbrovas_dis), mi_n = sum(myocardial_infarc), 
            broken_arm_n = sum(broken_arm))

portl <- or_2013_county_er_time_series%>%
  filter(county=="Multnomah")

summary(or_2013_county_er_time_series)

# write permanent dataframe

write_path <- paste0("../data_new/county_data/or_2013_county_er_time_series.csv")

write_csv(or_2013_county_er_time_series, write_path)



### Change county "Hood.River" to "Hood River"
library(readr)
setwd("C:/Users/jyliu/Desktop/local_git_repo/oregon_wildfire_new/data_new/county_data/")
read_path1 <- paste("./or_2013_county_er_time_series.csv")
read_path2 <- paste("./or_county_pop_wt_pm.csv")

or_1 <- read_csv(read_path1)
or_2 <- read_csv(read_path2)

or_1$county[which(or_1$county=="Hood.River")] <- "Hood River"
or_2$county[which(or_2$county=="Hood.River")] <- "Hood River"

write_path1 <- paste0("./or_2013_county_er_time_series.csv")
write_csv(or_1, write_path1)

write_path2 <- paste0("./or_county_pop_wt_pm.csv")
write_csv(or_2, write_path2)




