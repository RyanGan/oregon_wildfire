# ------------------------------------------------------------------------------
# Title: Oregon 2013 Time-Series Analysis
# Author: Ryan Gan
# Date Created: 9/28/16
# ------------------------------------------------------------------------------

# Library
library(tidyverse)
library(splines) # needed to model asthma trend over time

# Load county time series data

setwd("C:/Users/jyliu/Desktop/local_git_repo/oregon_wildfire_new/r_scripts")
path <- paste0("../data_new/county_data/or_asthma_county_time_series.csv")

or_ts_df <- read_csv(path)
# check descriptives
summary(or_ts_df)

# missing values in sparsely populated counties. set to 0
check_missing <- filter(or_ts_df, is.na(n_obs))
# mutate each to fill in missing values
check2 <- check_missing %>% mutate_each(funs(new = ifelse(is.na(.), 0, .)), 
                                       n_obs:wrf_temp)
# summary(check2)

# plot asthma events for all counties
ggplot(or_ts_df, aes(y =asthma_n, x = date)) +
  geom_jitter() +
  facet_wrap(~county)

# Aggregated Asthma Events for all of Washington and Smoke PM ------------------
or_aggregate_df <- or_ts_df %>% 
  # set missing asthma values to 0, and also for smoke values, set to 0
  mutate_each(funs(wo_miss = ifelse(is.na(.), 0, .)), n_obs:wrf_temp) %>% 
  group_by(date) %>% 
  summarise(n_asthma = sum(asthma_n_wo_miss), 
            avg_wrf_smk = mean(wrf_smk_pm_wo_miss, na.rm = T),
            avg_geo_smk = mean(geo_smk_pm_wo_miss, na.rm = T), 
            avg_temp = mean(wrf_temp, na.rm = T))

summary(or_aggregate_df)

# asthma trend over the year for all of washington
asthma_trend_plot <- ggplot(or_aggregate_df, aes(x = date, y = n_asthma)) + 
  geom_jitter()
plot(asthma_trend_plot)

ave_geo_smk_ts <- ggplot(or_aggregate_df, aes(x = date, y = avg_geo_smk)) + 
  geom_jitter()
plot(ave_geo_smk_ts)

# fit spline model 
spl_yr <- bs(or_aggregate_df$date,degree=3, df=7) # spline for week/weekend trends

# model n asthma events with a spline with 7 df (knotches)
model1 <- glm(n_asthma ~ spl_yr, or_aggregate_df, family=quasipoisson) 
summary(model1)

# compute predicted number of asthma events from this model
pred1 <- predict(model1,type="response")

plot(or_aggregate_df$date, or_aggregate_df$n_asthma,
     ylim=c(0, 40),pch=19,cex=0.2,col=grey(0.6),
     main="Flexible cubic spline model",ylab="Daily Number of Asthma ER/Urgent",
     xlab="Date")
lines(or_aggregate_df$date, pred1, lwd=2)

# check residuals
res1 <- residuals(model1,type="response")

plot(or_aggregate_df$date, res1,ylim=c(-15,15),pch=19,cex=0.4,col=grey(0.6),
     main="Residuals over time",ylab="Residuals (observed-fitted)",xlab="Date")
abline(h=1,lty=2,lwd=2)

# models for association between smoke pm and daily asthma count 
# unadjusted model
model_unadj <- glm(n_asthma ~ avg_geo_smk, or_aggregate_df, family=quasipoisson)
summary(model_unadj)

# controlling for seasonality with spline
model_adj <- update(model_unadj,.~.+spl_yr+avg_temp)
summary(model_adj)
# appears to be an association with asthma and wildfire smoke on the aggregate

# Subset to May 1st to Sep 30th dates -------------------------------------
# Not need to subset because using the 

# fit spline to summer shape 
spl_summer <- bs(or_aggregate_df$date,degree=3, df=3) # spline for week/weekend trends

# model n asthma events with a spline with 7 df (knotches)
summer_model1 <- glm(n_asthma ~ spl_summer, or_aggregate_df, family=quasipoisson) 
summary(summer_model1)

# compute predicted number of asthma events from this model
pred1 <- predict(summer_model1,type="response")

plot(or_aggregate_df$date, or_aggregate_df$n_asthma,
     ylim=c(0, 40),pch=19,cex=0.2,col=grey(0.6),
     main="Flexible cubic spline model",ylab="Daily Number of Asthma ER/Urgent",
     xlab="Date")
lines(or_aggregate_df$date, pred1, lwd=2)

# check residuals
res1 <- residuals(summer_model1,type="response")

plot(or_aggregate_df$date, res1,ylim=c(-15,15),pch=19,cex=0.4,col=grey(0.6),
     main="Residuals over time",ylab="Residuals (observed-fitted)",xlab="Date")
abline(h=1,lty=2,lwd=2)

# models for association between smoke pm and daily asthma count 
# unadjusted model
summer_model_unadj <- glm(n_asthma ~ avg_geo_smk + avg_temp, or_aggregate_df, family=quasipoisson)
summary(summer_model_unadj)

# controlling for seasonality with spline
summer_model_adj <- update(summer_model_unadj,.~.+ spl_summer)
summary(summer_model_adj)

# going to try and limit the yearly spline to just jul 1 and oct 31
dates_2013 <- or_aggregate_df %>% select(date) %>% as.vector()
jul_oct_spline <- cbind(dates_2013, spl_yr)  
# subset spl_yr values 183 to 305  
jul_oct_spline2 <- spl_yr[1:153, ]

summer_model_adj_yr <- update(summer_model_unadj, .~ . + jul_oct_spline2)
summary(summer_model_adj_yr)

asthma_trend_plot <- ggplot(or_ts_df, aes(x = date, y = asthma_n)) + 
  geom_jitter() +
  facet_wrap(~ county)

print(asthma_trend_plot)

# try time-series in spokane county
douglas <- or_ts_df %>% filter(county == "Douglas" & !is.na(geo_smk_pm)) 

douglas_plot <- ggplot(douglas, aes(x = date, y = asthma_n)) + 
  geom_jitter() 

print(douglas_plot)

# not sure a spline would help because there doesn't look to be any seasonal trends
spl <- bs(douglas$date,degree=2,df=2) # spline for week/weekend trends

# model n asthma events with a spline with 7 df (knotches)
model1 <- glm(asthma_n ~ spl, douglas, family=quasipoisson) 
summary(model1)

# compute predicted number of asthma events from this model
pred1 <- predict(model1,type="response")

plot(douglas$date, douglas$asthma_n,
     ylim=c(0, 10),pch=19,cex=0.2,col=grey(0.6),
     main="Flexible cubic spline model",ylab="Daily Number of Admissions",
     xlab="Date")
lines(douglas$date, pred1, lwd=2)

# check residuals
res1 <- residuals(model1,type="response")

plot(douglas$date, res1,ylim=c(-15,15),pch=19,cex=0.4,col=grey(0.6),
     main="Residuals over time",ylab="Residuals (observed-fitted)",xlab="Date")
abline(h=1,lty=2,lwd=2)

# models for association between smoke pm and daily asthma count 
# unadjusted model
model_unadj <- glm(asthma_n ~ geo_smk_pm, douglas, family=quasipoisson)
summary(model_unadj)

# controlling for seasonality with spline
model_adj <- update(model_unadj,.~.+spl)
summary(model_adj)

