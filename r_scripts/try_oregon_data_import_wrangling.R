# ------------------------------------------------------------------------------
# Title: Small data set case crossover studies (Practice)
# Author: Jingyang Liu
# Date Created: Feb 24, 2017
# R Version: 3.3.2
# ------------------------------------------------------------------------------

library(tidyverse)
library(data.table)
library(dplyr)
library(readxl) 
getwd()

### import data ----------------------------------------------------------------
# Read in first 200,000 lines for exploration.

read_path <- paste0("./data/gan_episodes_of_care.txt") # R Markdown is ".." because working directory is r_script
oregon_df <- fread(read_path, sep = "|", nrows = 200000, showProgress = T)
fwrite(oregon_df, "./data/oregon_epis_care_reduced.csv")

# I wrote a reduced dataframe of 200,000 rows to get an idea of data structure

# import the reduced dataframe
start_time <- Sys.time()
oregon_df <- fread("./data/oregon_epis_care_reduced.csv", sep = ",", 
                   colClasses = rep("character", 72))
stop_time <-  Sys.time() - start_time 

# time it took
stop_time # 1.968219 secs

# convert variable type to appropriate functional form 
oregon_df$yob <- as.numeric(oregon_df$yob)
oregon_df[oregon_df=="*NULL*"] <- NA

### data rough cleaning (remove some NA and blank values) and add index---------
# at least a primary ICD9 code present
# whole data still have 76552034 observations
or_hosp_w_outcome_df <- oregon_df %>% filter(!is.na(dx1)) %>% 
  # filter sex is known
  filter(gender!="U") %>%
  # add age, filter age not missing
  mutate(age = 2013-yob) %>%
  filter(!is.na(age)) %>%
  # filter race
  filter(race!="") %>%
  # filter enthnicity
  filter(ethn!="") %>%
  # filter State is Oregon
  filter(STATE=="OR") %>%
  # filter ZIP non missing
  filter(!is.na(ZIP)) %>%
  # filter MSA
  filter(!is.na(MSA)) %>%
  # filter POS
  filter(!is.na(pos)) %>%
  # indicator for male=0, female=1; age
  mutate(sex_ind =ifelse(gender == "F", 1, 
                         ifelse(gender == "M", 0, NA)),
         age_ind = ifelse(age < 15, 0,
                          ifelse(age >= 15 & age < 65, 1,
                                 ifelse(age >= 65 & age <=120, 2, NA)))
  ) # end of mutate

# age density
ggplot(or_hosp_w_outcome_df, aes(or_hosp_w_outcome_df$age)) +
  geom_density(aes(group=or_hosp_w_outcome_df$age_ind,
                   color=or_hosp_w_outcome_df$age_ind)) + xlim(0,110)



### Creating vectors of outcome claims -----------------------------------------
# import chars diagnosis code key 
icd9_key <- read_excel("./instructions/CMS32_DESC_LONG_SHORT_DX.xlsx")

## for shell server
## icd9_key <- read_excel("../data/CMS32_DESC_LONG_SHORT_DX.xlsx")

# changing variable name, 'code' for 'diagonsis code'.
# summary(icd9_key)
names(icd9_key)[1:3] <- c("code","long","short")


# Coding Respiratory Disease ICD-9 Outcomes ------------------------------------
# All Respiratory Diseases 460 to 519 ------------------------------------------
which(icd9_key$code == '460') # row 5067 (cold) start of resp outcomes
# last icd9 code in resp disease is 519.9 (unsp dis of resp sys)
which(icd9_key$code == '5199') # row 5321 end of resp outcomes

# sort by icd9 code add row variable 
icd9_key$X <- NULL
icd9_key <- arrange(icd9_key, code) %>%
  mutate(n = as.numeric(row.names(icd9_key)))

resp_icd9 <- filter(icd9_key, n >= 5067 & n <= 5321) %>%
  select(code)
# convert to vector
resp_icd9 <- as.vector(as.matrix(resp_icd9))   

# CHARS indicator of resp_icd9
or_hosp_w_outcome_df <- or_hosp_w_outcome_df %>%
  mutate(resp1 = ifelse(dx1 %in% resp_icd9, 1, 0),
         resp2 = ifelse(dx2 %in% resp_icd9, 1, 0),
         resp3 = ifelse(dx3 %in% resp_icd9, 1, 0),
         resp4 = ifelse(dx4 %in% resp_icd9, 1, 0),
         resp5 = ifelse(dx5 %in% resp_icd9, 1, 0),
         # sum up the indicators
         resp_sum = (resp1 + resp2 + resp3 + resp4 +resp5 ),
         resp_dx = ifelse(resp_sum>0, 1, 0)
  ) # end of mutate

# primary dx of any resp outcome
xtabs(~ resp1, or_hosp_w_outcome_df)

# Asthma, ICD-9 493 ------------------------------------------------------------
# try asthma 493 to 49392; identify rows with following code
which(icd9_key$code == '49300') # start of asthma is row 5206
which(icd9_key$code == '49392') # end of asthma is row 5219

# limit just to asthma code and just the diagnosis column
icd9_check <- filter(icd9_key, n >= 5206 & n <= 5219) 
icd9_check

asthma_icd9 <- filter(icd9_key, n >= 5206 & n <= 5219) %>%
  select(code)
# convert to vector
asthma_icd9 <- as.vector(as.matrix(asthma_icd9))   

# now can I make a new variable, asthma1, that indicates an asthma claim?
or_hosp_w_outcome_df <- or_hosp_w_outcome_df %>%
  mutate(asthma1 = ifelse(dx1 %in% asthma_icd9, 1, 0),
         asthma2 = ifelse(dx2 %in% asthma_icd9, 1, 0),
         asthma3 = ifelse(dx3 %in% asthma_icd9, 1, 0),
         asthma4 = ifelse(dx4 %in% asthma_icd9, 1, 0),
         asthma5 = ifelse(dx5 %in% asthma_icd9, 1, 0),
         # sum up the asthma indicators
         asthma_sum = (asthma1 + asthma2 + asthma3 + asthma4 + asthma5),
         asthma_dx = ifelse(asthma_sum>0, 1, 0)
  ) # end of mutate

# check if the binary asthma_dx code aligns with the sum
asthma_dx_check <- table(or_hosp_w_outcome_df$asthma_dx, or_hosp_w_outcome_df$asthma_sum)
asthma_dx_check

summary(or_hosp_w_outcome_df)

# seems to do the same thing, but both codes could be incorrect. i should check
# the number of asthma claims in diagnoses 1 
asthma_claims <- subset(or_hosp_w_outcome_df, dx1 %in% asthma_icd9)
asthma_claims$dx1 <- as.factor(asthma_claims$dx1)
# check asthma claims subset
summary(asthma_claims$dx1)

# number of asthma claims; first convert claims to as.factor ???
# or_hosp_w_outcome_df$asthma_dx <- as.factor(or_hosp_w_outcome_df$asthma_dx)

summary(or_hosp_w_outcome_df$asthma_dx)

# checking if all data are in right range
asthma_check <- or_hosp_w_outcome_df %>% filter(asthma1 == 1)
xtabs(~asthma1 + dx1, asthma_check )
glimpse(asthma_check)

rm(asthma_claims) # remove to save space


# Pneumonia ICD-9 code 480-486 -------------------------------------------------
# id the rows of interest
which(icd9_key$code == '4800') # row 5149, start pneumonia
which(icd9_key$code == '486') # row 5183, end pneumonia
# code df 
pneumonia_icd9 <- filter(icd9_key, n >= 5149 & n <= 5183) %>%
  select(code)
# convert to vector
pneumonia_icd9 <- as.vector(as.matrix(pneumonia_icd9))

# make indicator of pneumonia
or_hosp_w_outcome_df <- or_hosp_w_outcome_df %>%
  mutate(pneum1 = ifelse(dx1 %in% pneumonia_icd9, 1, 0),
         pneum2 = ifelse(dx2 %in% pneumonia_icd9, 1, 0),
         pneum3 = ifelse(dx3 %in% pneumonia_icd9, 1, 0),
         pneum4 = ifelse(dx4 %in% pneumonia_icd9, 1, 0),
         pneum5 = ifelse(dx5 %in% pneumonia_icd9, 1, 0),
         # sum up the pneumonia indicators
         pneum_sum = (pneum1 + pneum2 + pneum3 + pneum4 + pneum5),
         pneum_dx = ifelse(pneum_sum>0, 1, 0)
  ) #end of mutate


# Acute bronchitis ICD-9 codes 466 to 466.19 -----------------------------------
which(icd9_key$code == '4660') # row 5090, start acute bronch
which(icd9_key$code == '46619') # row 5092, end acute bronch

# code df of acute bronchitis codes
acute_bronch_icd9 <- filter(icd9_key, n >= 5090 & n <= 5092) %>% 
  select(code)
# convert to vector
acute_bronch_icd9 <- as.vector(as.matrix(acute_bronch_icd9))  

# code variables of acute bronchitis
or_hosp_w_outcome_df <- or_hosp_w_outcome_df %>%
  mutate(acute_bronch1 = ifelse(dx1 %in% acute_bronch_icd9, 1, 0),
         acute_bronch2 = ifelse(dx2 %in% acute_bronch_icd9, 1, 0),
         acute_bronch3 = ifelse(dx3 %in% acute_bronch_icd9, 1, 0),
         acute_bronch4 = ifelse(dx4 %in% acute_bronch_icd9, 1, 0),
         acute_bronch5 = ifelse(dx5 %in% acute_bronch_icd9, 1, 0),
         # sum up the pneumonia indicators
         acute_bronch_sum = (acute_bronch1 + acute_bronch2 + acute_bronch3 + 
                               acute_bronch4 +acute_bronch5),
         acute_bronch_dx = ifelse(acute_bronch_sum>0, 1, 0)
  ) #end of mutate


# check coding outcome
check <- filter(or_hosp_w_outcome_df, acute_bronch1 == 1)
xtabs(~dx1 + acute_bronch1, check)
rm(check)

# COPD, ICD9 490 to 492, 494, and 496 ------------------------------------------
copd_icd9 <- c('490', '4910','4911','49120','49121','49122','4918','4919', '4920',
               '4928', '4940', '4941', '496') 

# now can I make a new variable, copdn, that indicates an asthma claim?
or_hosp_w_outcome_df <- or_hosp_w_outcome_df %>%
  mutate(copd1 = ifelse(dx1 %in% copd_icd9, 1, 0),
         copd2 = ifelse(dx2 %in% copd_icd9, 1, 0),
         copd3 = ifelse(dx3 %in% copd_icd9, 1, 0),
         copd4 = ifelse(dx4 %in% copd_icd9, 1, 0),
         copd5 = ifelse(dx5 %in% copd_icd9, 1, 0),
         # sum up the pneumonia indicators
         copd_sum = (copd1 + copd2 + copd3 + copd4 +copd5),
         copd_dx = ifelse(copd_sum>0, 1, 0)
  ) #end of mutate

# check copd coding
xtabs(~copd_dx, or_hosp_w_outcome_df)

xtabs(~copd1, or_hosp_w_outcome_df)

copd_claims <- filter(or_hosp_w_outcome_df, dx1 %in% copd_icd9)
copd_claims$dx1 <- as.factor(copd_claims$dx1)
# check asthma claims subset
summary(copd_claims$dx1)
rm(copd_claims) # remove to save space

# Cardiovascular Diseases (general CVD), ICD-9: 390 to 459 ---------------------
which(icd9_key$code == '390') # starts at 4593, rheumatic fever
which(icd9_key$code == '4599') # ends at 5066, unsp circulatory disorder

# code df of stroke codes 
# code check
icd9_check <- filter(icd9_key, n >= 4593 & n <= 5066)
icd9_check # check codes

cvd_icd9 <- filter(icd9_key, n >= 4593 & n <= 5066) %>%
  select(code)
# convert to vector
cvd_icd9 <- as.vector(as.matrix(cvd_icd9)) 

# make indicator of cvd_icd9
or_hosp_w_outcome_df <- or_hosp_w_outcome_df %>%
  mutate(cvd1 = ifelse(dx1 %in% cvd_icd9, 1, 0),
         cvd2 = ifelse(dx2 %in% cvd_icd9, 1, 0),
         cvd3 = ifelse(dx3 %in% cvd_icd9, 1, 0),
         cvd4 = ifelse(dx4 %in% cvd_icd9, 1, 0),
         cvd5 = ifelse(dx5 %in% cvd_icd9, 1, 0),
         # sum up the pneumonia indicators
         cvd_sum = (cvd1 + cvd2 + cvd3 + cvd4 + cvd5),
         cvd_dx = ifelse(cvd_sum>0, 1, 0)
  ) #end of mutate

xtabs(~ cvd1, or_hosp_w_outcome_df)

# Ischemic heart diseases, ICD-9 codes 410-413 ---------------------------------
which(icd9_key$code == '41000') # starts at 4656, acute myo infarc
which(icd9_key$code == '4139') # ends 4693, other us angina pectoris

# code df of ihd codes (includes mi)
ihd_icd9 <- filter(icd9_key, n >= 4656 & n <= 4693)
ihd_icd9 # check codes

ihd_icd9 <- filter(icd9_key, n >= 4656 & n <= 4693) %>% 
  select(code)
# convert to vector
ihd_icd9 <- as.vector(as.matrix(ihd_icd9))  

# make indicator of ihd_icd9
or_hosp_w_outcome_df <- or_hosp_w_outcome_df %>%
  mutate(ihd1 = ifelse(dx1 %in% ihd_icd9, 1, 0),
         ihd2 = ifelse(dx2 %in% ihd_icd9, 1, 0),
         ihd3 = ifelse(dx3 %in% ihd_icd9, 1, 0),
         ihd4 = ifelse(dx4 %in% ihd_icd9, 1, 0),
         ihd5 = ifelse(dx5 %in% ihd_icd9, 1, 0),
         # sum up the pneumonia indicators
         ihd_sum = (ihd1 + ihd2 + ihd3 + ihd4 + ihd5),
         ihd_dx = ifelse(ihd_sum>0, 1, 0)
  ) #end of mutate

xtabs(~ ihd1, or_hosp_w_outcome_df) # 35401 primary events

# Arrhythmias ICD-9 code 427 ---------------------------------------------------
which(icd9_key$code == '4270') # starts at 4780, acute myo infarc
which(icd9_key$code == '4279') # ends 4793, other us angina pectoris

# code df of arrhyth codes 
icd9_check <- filter(icd9_key, n >= 4780 & n <= 4793)
icd9_check # check codes

arrhythmia_icd9 <- filter(icd9_key, n >= 4780 & n <= 4793) %>%
  select(code)
# convert to vector
arrhythmia_icd9 <- as.vector(as.matrix(arrhythmia_icd9))

# make indicator of arrhythmia_icd9
or_hosp_w_outcome_df <- or_hosp_w_outcome_df %>%
  mutate(arrhythmia1 = ifelse(dx1 %in% arrhythmia_icd9, 1, 0),
         arrhythmia2 = ifelse(dx2 %in% arrhythmia_icd9, 1, 0),
         arrhythmia3 = ifelse(dx3 %in% arrhythmia_icd9, 1, 0),
         arrhythmia4 = ifelse(dx4 %in% arrhythmia_icd9, 1, 0),
         arrhythmia5 = ifelse(dx5 %in% arrhythmia_icd9, 1, 0),
         # sum up the pneumonia indicators
         arrhythmia_sum = (arrhythmia1 + arrhythmia2 + arrhythmia3 + arrhythmia4 + arrhythmia5),
         arrhythmia_dx = ifelse(arrhythmia_sum>0, 1, 0)
  ) #end of mutate

xtabs(~ arrhythmia_dx, or_hosp_w_outcome_df)

# Heart Failure, ICD-9 428 -----------------------------------------------------
which(icd9_key$code == '4280') # starts at 4794, congestive heart failure
which(icd9_key$code == '4289') # ends at 4808, heart failure

# code df of hf codes 
# code check
icd9_check <- filter(icd9_key, n >= 4794 & n <= 4808)
icd9_check # check codes

hf_icd9 <- filter(icd9_key, n >= 4794 & n <= 4808) %>%
  select(code)
# convert to vector
hf_icd9 <- as.vector(as.matrix(hf_icd9))  

# make indicator of hf_icd9
or_hosp_w_outcome_df <- or_hosp_w_outcome_df %>%
  mutate(hf1 = ifelse(dx1 %in% hf_icd9, 1, 0),
         hf2 = ifelse(dx2 %in% hf_icd9, 1, 0),
         hf3 = ifelse(dx3 %in% hf_icd9, 1, 0),
         hf4 = ifelse(dx4 %in% hf_icd9, 1, 0),
         hf5 = ifelse(dx5 %in% hf_icd9, 1, 0),
         # sum up the pneumonia indicators
         hf_sum = (hf1 + hf2 + hf3 + hf4 + hf5),
         hf_dx = ifelse(hf_sum>0, 1, 0)
  ) #end of mutate

xtabs(~ hf1, or_hosp_w_outcome_df)

# Cerebrovascular Diseases, ICD-9 430-438 --------------------------------------
# Consider refining outcome definition to be more specific
which(icd9_key$code == '430') # starts at 4823, brain hemorage
which(icd9_key$code == '4389') # ends at 4891, late effects of cerevas dis

# code df of stroke codes 
# code check
icd9_check <- filter(icd9_key, n >= 4823 & n <= 4891)
icd9_check # check codes

cereb_vas_icd9 <- filter(icd9_key, n >= 4823 & n <= 4891) %>%
  select(code)
# convert to vector
cereb_vas_icd9 <- as.vector(as.matrix(cereb_vas_icd9)) 

# make indicator of cereb_vas_icd9
or_hosp_w_outcome_df <- or_hosp_w_outcome_df %>%
  mutate(cereb_vas1 = ifelse(dx1 %in% cereb_vas_icd9, 1, 0),
         cereb_vas2 = ifelse(dx2 %in% cereb_vas_icd9, 1, 0),
         cereb_vas3 = ifelse(dx3 %in% cereb_vas_icd9, 1, 0),
         cereb_vas4 = ifelse(dx4 %in% cereb_vas_icd9, 1, 0),
         cereb_vas5 = ifelse(dx5 %in% cereb_vas_icd9, 1, 0),
         # sum up the pneumonia indicators
         cereb_vas_sum = (cereb_vas1 + cereb_vas2 + cereb_vas3 + cereb_vas4 + cereb_vas5),
         cereb_vas_dx = ifelse(cereb_vas_sum>0, 1, 0)
  ) #end of mutate

xtabs(~ cereb_vas1, or_hosp_w_outcome_df)

# Myocardial Infarction ICD-9 code 410.00 - 410.92 -----------------------------
which(icd9_key$code == '41000') # starts at 4656, congestive heart failure
which(icd9_key$code == '41092') # ends at 4685, heart failure

# code check
icd9_check <- filter(icd9_key, n >= 4656 & n <= 4685)
icd9_check # check codes

mi_icd9 <- filter(icd9_key, n >= 4656 & n <= 4685) %>%
  select(code)
# convert to vector
mi_icd9 <- as.vector(as.matrix(mi_icd9))  

# variables indicating MI claim
or_hosp_w_outcome_df <- or_hosp_w_outcome_df %>%
  mutate(mi1 = ifelse(dx1 %in% mi_icd9, 1, 0),
         mi2 = ifelse(dx2 %in% mi_icd9, 1, 0),
         mi3 = ifelse(dx3 %in% mi_icd9, 1, 0),
         mi4 = ifelse(dx4 %in% mi_icd9, 1, 0),
         mi5 = ifelse(dx5 %in% mi_icd9, 1, 0),
         # sum up the pneumonia indicators
         mi_sum = (mi1 + mi2 + mi3 + mi4 + mi5),
         mi_dx = ifelse(mi_sum>0, 1, 0)
  ) #end of mutate

# check if the binary mi_dx code aligns with the sum
mi_dx_check <- table(or_hosp_w_outcome_df$mi_dx, or_hosp_w_outcome_df$mi_sum)
mi_dx_check

summary(as.factor(or_hosp_w_outcome_df$mi1))

# seems to do the same thing, but both codes could be incorrect. i should check
# the number of mi claims in diagnoses 1 (10,459 claims)
mi_claims <- subset(or_hosp_w_outcome_df, dx1 %in% mi_icd9)
# check asthma claims subset
summary(as.factor(mi_claims$dx1))
# very few with 2 designation indicating subsequent visit, but I still might
# need to remove due to possible misclassification

# Coding in a outcomes that should not be associated with smoke ----------------

# Broken Arm ICD-9 813 ---------------------------------------------------------
which(icd9_key$code == '81300') # starts at 10094, fracture of radius and ulna
which(icd9_key$code == '81393') # ends at 10140, late effects of cerevas dis

# code df of broken arms
# code check
icd9_check <- filter(icd9_key, n >= 10094 & n <= 10140)
icd9_check # check codes

broken_arm_icd9 <- filter(icd9_key, n >= 10094 & n <= 10140) %>%
  select(code)
# convert to vector
broken_arm_icd9 <- as.vector(as.matrix(broken_arm_icd9)) 

# code variables of broken arm
or_hosp_w_outcome_df <- or_hosp_w_outcome_df %>%
  mutate(broken_arm1 = ifelse(dx1 %in% broken_arm_icd9, 1, 0),
         broken_arm2 = ifelse(dx2 %in% broken_arm_icd9, 1, 0),
         broken_arm3 = ifelse(dx3 %in% broken_arm_icd9, 1, 0),
         broken_arm4 = ifelse(dx4 %in% broken_arm_icd9, 1, 0),
         broken_arm5 = ifelse(dx5 %in% broken_arm_icd9, 1, 0),
         # sum up the pneumonia indicators
         broken_arm_sum = (broken_arm1 + broken_arm2 + broken_arm3 + broken_arm4 + broken_arm5),
         broken_arm_dx = ifelse(broken_arm_sum>0, 1, 0)
  ) #end of mutate

# primary diagnosis (1944 events)
xtabs(~ broken_arm1, or_hosp_w_outcome_df)
# any diagnosis
xtabs(~ broken_arm_dx, or_hosp_w_outcome_df)
# check coding outcome (3277 events)
check <- filter(or_hosp_w_outcome_df, broken_arm1 == 1)
xtabs(~dx1 + broken_arm1, check)

# end outcome coding for ICD-9 codes -------------------------------------------

# checking if the variables of diseases is missing or wrong
summary(or_hosp_w_outcome_df)


# Creating a Permanent DataFrame -----------------------------------------------
# write a permanent chars confidential dataset
write_path <- paste0('C:/Users/jyliu/Desktop/local_git_repo/oregon_wildfire/data1/',
                     'or_hosp_w_outcome_df')
write_csv(or_hosp_w_outcome_df, write_path)


