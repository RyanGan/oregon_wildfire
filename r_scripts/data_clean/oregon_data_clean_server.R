---
title: "Oregon_Wildfire_explore_full_server"
author: "Jingyang Liu"
date: "February 15, 2017"
---
  
# Explore Data Structure and Cleaning
  
The Oregon data contains 77069320 obsversations in total in year of 2013, which is a big data set. We want to figure out the structure for this data set and do some basic cleaning. 

```{r}
library(dplyr)
library(data.table)
library(knitr)
library(readxl)
library(ggplot2)
getwd()

# import data ------------------------------------------------------------------

read_path <- paste0("../../../data/data_original/gan_episodes_of_care.txt")
start_time <- Sys.time()
oregon_df <- fread(read_path, sep = "|", showProgress = T, 
                   colClasses = rep("character", 72)) 
stop_time <-  Sys.time() - start_time 
# time it took
stop_time # about 20 mins

```


## The overall summary  of data set
72 variables in total.

```{r overall summary}
# Check data structure
# summary(oregon_df)
```

## convert variable type to appropriate functional form 
(i.e. zipcode leave as character, age as numeric, date as date, dx leave as character, but *NULL* == NA)
```{r}
oregon_df$yob <- as.numeric(oregon_df$yob)
oregon_df[oregon_df=="*NULL*"] <- NA
```

# summary statistics key variables for study 
date, dx, need to know urgent and emergency department visits, sex, age, patientid, state of residence, zipcode, county, race

## 1. Personkey 
```{r}
personkey2 <- oregon_df %>% 
  select(personkey) %>%
  mutate(personkey_cat = ifelse(is.na(personkey), "Missing",
                         ifelse(!is.na(personkey), "Non-Missing", NA))) %>% 
  group_by(personkey_cat) %>%
  summarise(n = n()) %>%
  mutate(total_n =sum(n),
         percent = round(n/total_n, 4)*100) %>%
  select(personkey_cat, n, percent)

# table ----
knitr::kable(personkey2, align = "c", caption = "Patient ID")

```


## How many visits does unique personkey have?
```{r}
# pick up personkey and claim id column, make new column of unique number of visit
visit2 <- oregon_df %>%
  select(personkey, clmid) %>%
  distinct(clmid,personkey) %>%
  select(personkey, clmid) %>%
  group_by(personkey) %>%
  summarise(num_visit=n()) 

visit3 <- visit2 %>%
  group_by(num_visit) %>%
  summarise(n = n()) %>%
  mutate(total_n =sum(n),
  percent = round(n/total_n, 4)*100) %>% 
  select(num_visit, n, percent)

m <- cbind(sum(visit2$num_visit)/n_distinct(oregon_df$personkey), median(visit2$num_visit), max(visit2$num_visit))
colnames(m) <- c("Mean", "Median", "Max")

knitr::kable(m, align = "c", caption = "Statistics")

ggplot(visit3, aes(n)) + geom_density()

```


## 2. Patid
```{r}
patid2 <- oregon_df %>% 
  select(patid) %>%
  mutate(patid_cat = ifelse(is.na(patid), "Missing",
                     ifelse(!is.na(patid), "Non-Missing", NA))) %>% 
  group_by(patid_cat) %>%
  summarise(n = n()) %>%
  mutate(total_n =sum(n),
         percent = round(n/total_n, 4)*100) %>%
      select(patid_cat, n, percent)

# table ----
knitr::kable(patid2, align = "c", caption = "Encrypted Patient ID")

```

## 3. Gender
```{r}
gender2 <- oregon_df %>%
  select(gender) %>%
  group_by(gender) %>%
  summarise(n = n()) %>%
  mutate(total_n = sum(n),
         percent = round(n/total_n, 4)*100,
         gender_cat = ifelse(gender == "F", "Female",
                      ifelse(gender == "M", "Male", 
                      ifelse(gender == "U", "Unknown", NA)))) %>%
  select(gender, gender_cat, n, percent)

knitr::kable(gender2, align = "c", caption = "Gender")

```

## 4. Age
```{r}
oregon_df$yob <- as.numeric(oregon_df$yob)
age2 <- oregon_df %>%
  select(yob) %>%
  mutate(age = 2013-yob) %>%
  mutate(age_ind = ifelse(age < 15, 0,
                   ifelse(age >= 15 & age < 65, 1,
                   ifelse(age >= 65 & age <=120, 2, NA)))
  ) %>%
  group_by(age_ind) %>% 
  summarise(n = n()) %>% 
  mutate(total_n = sum(n),
         percent = round(n/total_n, 4)*100,
         age_cat = ifelse(is.na(age_ind), "Missing",
                   ifelse(age_ind == 0, "Age <15",
                   ifelse(age_ind == 1, "Age 15 to 65",
                   ifelse(age_ind == 2, "Age >= 65",NA))))) %>% 
  select(age_cat, age_ind, n, percent)

knitr::kable(age2, align = "c", caption = "Age")
 
```

## 5. Race
```{r}
race2 <- oregon_df %>%
  select(race) %>%
  group_by(race) %>%
  summarise(n = n()) %>%
  mutate(total_n = sum(n),
         percent = round(n/total_n, 4)*100,
         race_cat = ifelse(race == 1, "White",
                    ifelse(race == 2, "Black or African American",
                    ifelse(race == 3, "American Indian or Alaska Native",
                    ifelse(race == 4, "Asian",
                    ifelse(race == 5, "Native Hawaiian or Pacific Islander",
                    ifelse(race == 6, "Some other race",
                    ifelse(race == 7, "Two or more races",
                    ifelse(race == 9, "Unknown",
                    ifelse(race == "", "Missing", NA)))))))))) %>%
    select(race, race_cat, n, percent)

knitr::kable(race2, align = "c", caption = "Race")

```

## 6. Enthicity
```{r}
ethn2 <- oregon_df %>%
  select(ethn) %>%
  group_by(ethn) %>%
  summarise(n = n()) %>%
  mutate(total_n = sum(n),
         percent = round(n/total_n, 4)*100,
         ethn_cat = ifelse(ethn == 1, "Hispanic",
                    ifelse(ethn == 2, "Not Hispanic",
                    ifelse(ethn == 3, "Unknown",
                    ifelse(ethn == "", "Missing", NA))))) %>%
    select(ethn, ethn_cat, n, percent)

knitr::kable(ethn2, align = "c", caption = "Ethnicity")

```

## 7. State 
```{r}
state2 <- oregon_df %>%
  select(STATE) %>%
  group_by(STATE) %>%
  summarise(n = n()) %>%
  mutate(total_n = sum(n),
         percent = round(n/total_n, 4)*100,
         state_cat = ifelse(STATE == "", "Missing",
                     ifelse(STATE == "OR", "Oregon",
                     ifelse(STATE != "OR", "Not Oregon",NA)))) %>%
  arrange(desc(state_cat)) %>%
  select(STATE, state_cat, n, percent)

knitr::kable(state2, align = "c", caption = "State")

```

## 8. ZIP 
```{r}
# summary(as.factor(oregon_df$ZIP))

# which(oregon_df$ZIP == "")

zip2 <- oregon_df %>% 
  select(ZIP) %>%
  mutate(zip_cat = ifelse(ZIP == "", "Missing", 
                   ifelse(!is.na(ZIP), "Non-Missing", NA))) %>% 
  group_by(zip_cat) %>%
  summarise(n = n()) %>%
  mutate(total_n =sum(n),
         percent = round(n/total_n, 4)*100) %>%
      select(zip_cat, n, percent)

knitr::kable(zip2, align = "c", caption = "ZIP COde")

```

## 9. MSA (Metropolitan and Micropolitan Statistical Areas Main)
```{r}
msa2 <- oregon_df %>% 
  select(MSA) %>%
  mutate(msa_cat = ifelse(MSA=="", "Missing",
                   ifelse(!is.na(MSA), "Non-Missing", 
                   NA))) %>% 
  group_by(msa_cat) %>%
  summarise(n = n()) %>%
  mutate(total_n =sum(n),
         percent = round(n/total_n, 4)*100) %>%
      select(msa_cat, n, percent)

summary(as.factor(oregon_df$MSA))

knitr::kable(msa2, align = "c", caption = "MSA")

```

## 10. Pos (Urgent and Emergency) NA: 2974
```{r}
pos_key <- read_excel("../LT_pos.xlsx")

pos2 <- oregon_df %>%
  select(pos) 

which(pos2$pos=="0S") # 14553
as.numeric(pos2$pos[14553])

pos2$pos <- as.numeric(pos2$pos)

pos2 <- pos2 %>%
  group_by(pos) %>%
  summarise(n = n()) %>%
  mutate(total_n = sum(n),
         percent = round(n/total_n, 4)*100,
         Place_of_service = ifelse(is.na(pos), "Missing",
                            ifelse(pos == pos_key$Code[1], pos_key$Value[1],
                            ifelse(pos == pos_key$Code[2], pos_key$Value[2],
                            ifelse(pos == pos_key$Code[3], pos_key$Value[3],       
                            ifelse(pos == pos_key$Code[4], pos_key$Value[4],       
                            ifelse(pos == pos_key$Code[5], pos_key$Value[5],       
                            ifelse(pos == pos_key$Code[6], pos_key$Value[6],        
                            ifelse(pos == pos_key$Code[7], pos_key$Value[7],
                            ifelse(pos == pos_key$Code[8], pos_key$Value[8],     
                            ifelse(pos == pos_key$Code[9], pos_key$Value[9],      
                            ifelse(pos == pos_key$Code[10], pos_key$Value[10],      
                            ifelse(pos == pos_key$Code[11], pos_key$Value[11],      
                            ifelse(pos == pos_key$Code[12], pos_key$Value[12],    
                            ifelse(pos == pos_key$Code[13], pos_key$Value[13],     
                            ifelse(pos == pos_key$Code[14], pos_key$Value[14],        
                            ifelse(pos == pos_key$Code[15], pos_key$Value[15],      
                            ifelse(pos == pos_key$Code[16], pos_key$Value[16],     
                            ifelse(pos == pos_key$Code[17], pos_key$Value[17],      
                            ifelse(pos == pos_key$Code[18], pos_key$Value[18],     
                            ifelse(pos == pos_key$Code[19], pos_key$Value[19],      
                            ifelse(pos == pos_key$Code[20], pos_key$Value[20],       
                            ifelse(pos == pos_key$Code[21], pos_key$Value[21],     
                            ifelse(pos == pos_key$Code[22], pos_key$Value[22],      
                            ifelse(pos == pos_key$Code[23], pos_key$Value[23],      
                            ifelse(pos == pos_key$Code[24], pos_key$Value[24],      
                            ifelse(pos == pos_key$Code[25], pos_key$Value[25],     
                            ifelse(pos == pos_key$Code[26], pos_key$Value[26],      
                            ifelse(pos == pos_key$Code[27], pos_key$Value[27],      
                            ifelse(pos == pos_key$Code[28], pos_key$Value[28],      
                            ifelse(pos == pos_key$Code[29], pos_key$Value[29],      
                            ifelse(pos == pos_key$Code[30], pos_key$Value[30],     
                            ifelse(pos == pos_key$Code[31], pos_key$Value[31],     
                            ifelse(pos == pos_key$Code[32], pos_key$Value[32],      
                            ifelse(pos == pos_key$Code[33], pos_key$Value[33],
                            ifelse(pos == pos_key$Code[34], pos_key$Value[34],
                            ifelse(pos == pos_key$Code[35], pos_key$Value[35],
                            ifelse(pos == pos_key$Code[36], pos_key$Value[36],                            
                            ifelse(pos == pos_key$Code[37], pos_key$Value[37],
                            ifelse(pos == pos_key$Code[38], pos_key$Value[38],                            
                            ifelse(pos == pos_key$Code[39], pos_key$Value[39],                            
                            ifelse(pos == pos_key$Code[40], pos_key$Value[40],                            
                            ifelse(pos == pos_key$Code[41], pos_key$Value[41],                            
                            ifelse(pos == pos_key$Code[42], pos_key$Value[42],                            
                            ifelse(pos == pos_key$Code[43], pos_key$Value[43],                            
                            ifelse(pos == pos_key$Code[44], pos_key$Value[44],                          
                            ifelse(pos == pos_key$Code[45], pos_key$Value[45],                            
                            ifelse(pos == pos_key$Code[46], pos_key$Value[46], NA
                                   )))))))))))))))))))))))))))))))))))))))))))))))) %>%
  select(pos, Place_of_service, n, percent)

knitr::kable(pos2, align = "c", caption = "Place of Service")

```

## 11. from date (All data is in 2013)
```{r}
fromdate2 <- oregon_df %>%
  select(fromdate) %>%
  mutate(dates = as.Date(fromdate, "%Y-%m-%d"))

# filter the dates of 2013
fromdate_check <- oregon_df %>%
  filter((fromdate > "2013-12-31"| fromdate < "2013-01-01")) # All is 2013

summary(fromdate2)
hist(fromdate2$dates, "days", freq = TRUE, xlab="fromdate", main="Histogram of from dates")

```

## 12. to date (min: 1913-02-17; max: 2299-12-31) 
```{r}
todate2 <- oregon_df %>%
  select(todate) %>%
  mutate(dates = as.Date(todate, "%Y-%m-%d"))

# filter the appropriate to dates (2013-01-01 to 2014-06-30)
todate3 <- todate2 %>%
  filter(dates<=as.Date("2014-06-30")&dates>=as.Date("2013-01-01"))

hist(todate3$dates, "days", freq = TRUE, xlab="todate", main="Histogram of to dates")

```

## 13. Length of stay
```{r}
summary(as.factor(oregon_df$los))
summary(as.numeric(oregon_df$los)) # min: -2; max: 364

# check length of stay overall range
los2 <- oregon_df %>%
  select(los) %>%
  group_by(los) %>%
  summarise(n = n()) %>%
  mutate(total_n = sum(n),
         percent = round(n/total_n, 4)*100) %>%
  select(los, n, percent) 

knitr::kable(los2, align = "c", caption = "Length of Stay Range")

# density for length of stay
hist(as.numeric(los2$los), freq = T, xlab = "los", main = "Length of Stay")

# Check if fromdate and todate match los
los3 <- oregon_df %>%
  select(fromdate, todate, los) %>%
  mutate(dif = as.numeric(as.Date(todate)-as.Date(fromdate))) %>%
  select(los, dif) %>%
  mutate(los_cat = ifelse(los=="", "Missing",
                   ifelse(los==dif, "Right", 
                   ifelse(los!=dif, "Wrong", NA)))) %>%
  group_by(los_cat) %>%
  summarise(n=n()) %>%
  mutate(total_n = sum(n),
         percent = round(n/total_n, 4)*100) %>%
  select(los_cat, n, percent) 

knitr::kable(los3, align = "c", caption = "Length of Stay Range Match From date and To Date")

```

## dx1
```{r}
dx1_2 <- oregon_df %>% 
  select(dx1) %>%
  mutate(dx1_cat = ifelse(is.na(dx1), "Missing",
                     ifelse(!is.na(dx1), "Non-Missing", NA))) %>% 
  group_by(dx1_cat) %>%
  summarise(n = n()) %>%
  mutate(total_n =sum(n),
         percent = round(n/total_n, 4)*100) %>%
      select(dx1_cat, n, percent)

# table ----
knitr::kable(dx1_2, align = "c", caption = "dx1")

```

## dx2
```{r}
dx2_2 <- oregon_df %>% 
  select(dx2) %>%
  mutate(dx2_cat = ifelse(is.na(dx2), "Missing",
                     ifelse(!is.na(dx2), "Non-Missing", NA))) %>% 
  group_by(dx2_cat) %>%
  summarise(n = n()) %>%
  mutate(total_n =sum(n),
         percent = round(n/total_n, 4)*100) %>%
      select(dx2_cat, n, percent)

# table ----
knitr::kable(dx2_2, align = "c", caption = "dx2")

```

## dx3
```{r}
dx3_2 <- oregon_df %>% 
  select(dx3) %>%
  mutate(dx3_cat = ifelse(is.na(dx3), "Missing",
                     ifelse(!is.na(dx3), "Non-Missing", NA))) %>% 
  group_by(dx3_cat) %>%
  summarise(n = n()) %>%
  mutate(total_n =sum(n),
         percent = round(n/total_n, 4)*100) %>%
      select(dx3_cat, n, percent)

# table ----
knitr::kable(dx3_2, align = "c", caption = "dx3")

```

## dx4
```{r}
dx4_2 <- oregon_df %>% 
  select(dx4) %>%
  mutate(dx4_cat = ifelse(is.na(dx4), "Missing",
                     ifelse(!is.na(dx4), "Non-Missing", NA))) %>% 
  group_by(dx4_cat) %>%
  summarise(n = n()) %>%
  mutate(total_n =sum(n),
         percent = round(n/total_n, 4)*100) %>%
      select(dx4_cat, n, percent)

# table ----
knitr::kable(dx4_2, align = "c", caption = "dx4")

```

## dx5
```{r}
dx5_2 <- oregon_df %>% 
  select(dx5) %>%
  mutate(dx5_cat = ifelse(is.na(dx5), "Missing",
                     ifelse(!is.na(dx5), "Non-Missing", NA))) %>% 
  group_by(dx5_cat) %>%
  summarise(n = n()) %>%
  mutate(total_n =sum(n),
         percent = round(n/total_n, 4)*100) %>%
      select(dx5_cat, n, percent)

# table ----
knitr::kable(dx5_2, align = "c", caption = "dx5")

```


