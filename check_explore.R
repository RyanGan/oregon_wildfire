#-------------------------------------------------------------------------------
# Title: Check and exploring the unreasonable plot
# Author: Jingyang Liu                                                                   
# Date Created: April 11, 2017                                                  
# R version: 3.3.3                                                       
#-------------------------------------------------------------------------------

library(tidyverse)
library(data.table)
library(survival)
library(htmlTable)

### Limit to only ER visits
## pos = 23
var_list <- c('respiratory', 'asthma', 'pneumonia',  'acute_bronch', 'copd', 
              'cvd', 'isch_heart_dis', 'arrhythmia', 'heart_failure', 
              'cerbrovas_dis', 'myocardial_infarc', 'broken_arm')

N <- NA
n <- 1

for (i in var_list){
  read_path <- paste('./data_new/update/ER/or', i, 'may_to_sep_time_strat_casecross.csv', sep = "_")
  or_disease <- read_csv(read_path)
  
  or_disease <- or_disease %>% 
    filter(pos=="23")
  
  # Create a permanent case-cross over dataset
  file_name <- paste('./data_new/update/ER/or', i, 'may_to_sep_time_strat_casecross_er.csv', sep = "_")
  
  # write permanent dataset
  write_csv(or_disease, paste0(file_name))
  N[n] = length(unique(or_disease$personkey))
  n = n+1
}


num_person <- data.frame(rbind(var_list, N))


## Analysis
path <- paste0("./data_new/update/ER")

resp_casecross <- read_csv(paste(path, 
                                 "or_respiratory_may_to_sep_time_strat_casecross_er.csv", 
                                 sep = "/"))

# asthma
asthma_casecross <- read_csv(paste(path, 
                                   "or_asthma_may_to_sep_time_strat_casecross_er.csv", 
                                   sep = "/")) 

# copd 
copd_casecross <- read_csv(paste(path, 
                                 "or_copd_may_to_sep_time_strat_casecross_er.csv", 
                                 sep = "/"))

# pneum or bronchitis
pneum_casecross <- read_csv(paste(path, 
                                  "or_pneumonia_may_to_sep_time_strat_casecross_er.csv",
                                  sep="/"))
 # acute bronchitis
acute_bronch_casecross <- read_csv(paste(path, 
                                         "or_acute_bronch_may_to_sep_time_strat_casecross_er.csv",
                                         sep = "/"))
# cvd
cvd_casecross <- read_csv(paste(path, 
                                "or_cvd_may_to_sep_time_strat_casecross_er.csv",
                                sep="/"))
# arrhythmia
arrhythmia_casecross <- read_csv(paste(path, 
                                       "or_arrhythmia_may_to_sep_time_strat_casecross_er.csv",
                                       sep="/"))
# cerebral vascular
cereb_vas_casecross <- read_csv(paste(path, 
                                      "or_cerbrovas_dis_may_to_sep_time_strat_casecross_er.csv",
                                      sep="/"))
# heart failure
hf_casecross <- read_csv(paste(path, 
                               "or_heart_failure_may_to_sep_time_strat_casecross_er.csv", 
                               sep="/"))
# ischemic heart disease
ihd_casecross <- read_csv(paste(path, 
                                "or_isch_heart_dis_may_to_sep_time_strat_casecross_er.csv",
                                sep="/"))
# myo infarc
mi_casecross <- read_csv(paste(path, 
                               "or_myocardial_infarc_may_to_sep_time_strat_casecross_er.csv", sep="/"))

# broken arm
broken_arm_casecross <- read_csv(paste(path, 
                                       "or_broken_arm_may_to_sep_time_strat_casecross_er.csv",
                                       sep="/"))


# read zip pm for counts by zip
smoke_path <- paste0("./data/Oregon/")
zip_pm <- read_csv(paste0(smoke_path, "zip_pm_to_merge_with_chars.csv"))




df_list <- list(resp_casecross, asthma_casecross, copd_casecross, 
                pneum_casecross, acute_bronch_casecross, cvd_casecross, arrhythmia_casecross,
                cereb_vas_casecross, hf_casecross, ihd_casecross, mi_casecross, 
                broken_arm_casecross)

outcome_list <- c('All Respiratory', 'Asthma', 'COPD', 
                  'Pneumonia', 'Acute Bronchitis', 'Cardiovascular Disease',
                  'Arrhythmia', 'Cerebrovascular Disease', 'Heart Failure',
                  'Ischemic Heart Disease', 'Myocardial Infarction',
                  'Broken Arm')

method_list <- c('WRF-Chem Smoke', 'Kriging Smoke', 'Geo-Weighted Smoke')

# create an empty list to row bind dataframes together
datalist <- list()

# data wrangling ----
# Producing conditional logit model estimates loop 
for(i in 1:length(df_list)){
  
  # dataframe to loop through
  df_to_loop <- data.frame(df_list[i])
  # indication of column
  outcome <- colnames(df_to_loop[76])
  # outcome name
  outcome_name <- outcome_list[i]
  
  # extract covariates from dataframe
  covariates_df <- df_to_loop[, c(1:26, 71, 74, 76:85)]
  
  # extract pm values and divide by 10 and ordered
  which(colnames(df_to_loop)=="background_pm_zip") # code to find column numbers, 91
  pm_estimates_df <- df_to_loop[, c(88, 93, 92, 94)]/10  # create 10 unit increases
  
  # dataframe for analysis creation
  # bind columns back together 
  df_analysis <- cbind(covariates_df, pm_estimates_df) %>% 
    # remove missing pm values
    filter(!is.na(wrf_smk_pm_zip)) %>% 
    
    # join in cdc met data
    # full_join(daily_zip_met, by = c("ZIPCODE", "date")) %>% 
    # the following code makes sure that the counterfactual values retained are 
    # symetric in that number of obs before = number of obs after
    mutate(obs_diff_admission = (fromdate - date)/7) 
  # dataframe is already for the entire fire season, so I don't need to subset anymore
  
  
  # empty df for table
  table_df <- data.frame()
  
  # empty matrix
  point_estimates <- matrix(nrow = 3, ncol = 9, byrow = T)
  
  colnames(point_estimates) <- c('outcome', 'pm_method', 'n', 'n_events', 'odds_ratio', 
                                 'lower95', 'upper95', 'se', 'p_val')
  
  # fill in the outcome name for the dataframe before the loop
  point_estimates[, 1] <- outcome_name
  
  
  # second loop to run a model for each pm estimation method
  for(j in 39:41){
    
    # variable to model 
    var_name <- colnames(df_analysis[j])
    
    # conditional logistic regression model
    mod <- clogit(outcome ~ df_analysis[[j]] + wrf_temp_zip + strata(personkey), df_analysis)
    
    # some model/DAG checks of data rish provided
    
    #glimpse(df_analysis)
    
    # adjusting for admit month is not possible as it's the same var for the subject.
    # adjusting for month smoke is a collider and should not be adjusted for.
    #summary(clogit(outcome ~ geo_smk_pm_zip + daily_meanRH + daily_meanT + daily_meanWS + daily_meanPrcp +
    #                as.factor(month_smk) + strata(PATIENTID), df_analysis))
    #outocme association: daily_meanRH, 
    
    # check for confoudning following DAG assumptions
    #summary(clogit(outcome ~ wrf_temp_zip + strata(PATIENTID), df_analysis))
    #summary(clogit(outcome ~ wrf_temp_zip + strata(PATIENTID), df_analysis))
    # smoke predictor association: daily_meanRH, 
    #summary(lmer(geo_smk_pm_zip ~ daily_meanWS + (1 | PATIENTID), df_analysis ))
    #summary(lmer(wrf_temp_zip ~ daily_meanT + (1 | PATIENTID), df_analysis ))
    #cor(df_analysis$wrf_temp_county, df_analysis$daily_meanT)
    
    
    # populate matrix
    row_n <- j-38
    
    point_estimates[row_n, 2] <- method_list[row_n]
    point_estimates[row_n, 3] <- mod$n
    point_estimates[row_n, 4] <- mod$nevent
    # odds ratio
    point_estimates[row_n, 5] <- round(exp(summary(mod)$coefficient[1,1]), 3)
    
    # 95% lower bound
    point_estimates[row_n, 6] <- round(exp((summary(mod)$coefficient[1,1]) -
                                             1.96*(summary(mod)$coefficient[1,3])), 3)
    # 95% upper bound
    point_estimates[row_n, 7] <- round(exp((summary(mod)$coefficient[1,1]) +
                                             1.96*(summary(mod)$coefficient[1,3])), 3)
    # standard error
    point_estimates[row_n, 8] <- round(summary(mod)$coefficient[1,3], 4)
    # p val
    point_estimates[row_n, 9] <- round(summary(mod)$coefficient[1,5], 4)
    
    # save point estimates as a dataframe
    point_est_df <- as_data_frame(point_estimates)
    
  }
  
  # combine previous values in dataframe that has all outcome/methods comparisons
  datalist[[i]] <- point_est_df
  
} # end of loop

# combine each outcome dataframe itteration in to a big dataset
combined_point_est_df <- bind_rows(datalist)


# subset columns I want to put in to the table
table_df <- combined_point_est_df %>% select(2, 3:7) 



tab <- htmlTable(txtRound(table_df, digits = 3, 1:3), 
                 caption = "Association between a 10 ug/m^3 in PM2.5 and Health Outcomes",
                 # row group by outcome
                 rgroup = outcome_list,
                 n.rgroup = c(rep(3, 12)), # 4 rows for each method for each outcome
                 # column headers
                 header = c("Method", "Obs.", "Events",
                            "OR&dagger;", "Lower", "Upper"),
                 # column spanner
                 cgroup = c("", "95% CI"), 
                 n.cgroup = c(4, 2),
                 padding.rgroup = "&nbsp;&nbsp;",
                 css.cell = "padding-left: 0.5em; padding-right: .5em;", # cell space
                 align = "llccccc", # column alignment,
                 tfoot="&dagger; Adjusted for CDC temperature, relatively humidity, wind speed, and precipitation; accounting for subject. Time-stratified: referent periods matched to events on same day of week within July to October fire season."
) # end table

print(tab)

# ggplot of odds ratios, facet wrapped by outcomes -----
# convert variables from character to either numeric or factor
# factor preserves the order of the variable
combined_point_est_df$outcome <- factor(combined_point_est_df$outcome, 
                                        levels = unique(combined_point_est_df$outcome))

combined_point_est_df$pm_method <- factor(combined_point_est_df$pm_method, 
                                          levels = unique(combined_point_est_df$pm_method))

combined_point_est_df$n <- as.numeric(combined_point_est_df$n)
combined_point_est_df$n_events <- as.numeric(combined_point_est_df$n_events)
combined_point_est_df$odds_ratio <- as.numeric(combined_point_est_df$odds_ratio)
combined_point_est_df$lower95 <- as.numeric(combined_point_est_df$lower95)
combined_point_est_df$upper95 <- as.numeric(combined_point_est_df$upper95)
combined_point_est_df$se <- as.numeric(combined_point_est_df$se)
combined_point_est_df$p_val <- as.numeric(combined_point_est_df$p_val)

# subset out global method
combined_point_est_df <- combined_point_est_df


## ggplot
print_plot <- ggplot(combined_point_est_df, 
                     aes(x = pm_method, y = odds_ratio, colour = pm_method)) +
  # custom color 
  scale_color_manual(name = "Smoke-Estimation Method", 
                     values = c("red", "blue", "#32115C"),
                     guide = guide_legend(title.position = "top", title.hjust = 0.5)) +
  geom_point() + #geom_text(vjust = 0, nudge_x = 0.3) +
  geom_errorbar(aes(ymin=lower95, ymax=upper95), width = 0.2) +
  facet_wrap(~outcome, nrow = 3) +
  geom_hline(yintercept = 1, linetype=2) +
  #    ggtitle('Association Between PM2.5 \n from Wildfire Smoke on Hospitalizations') +
  ylab(expression(paste("Odds Ratio for 10µg/m"^3, " Increase in PM"[2.5]))) +
  #xlab('Smoke Estimation Method') +
  # plot theme
  theme(panel.background = element_rect(fill = 'white', colour = 'black'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # strip element
        strip.background = element_rect(colour=NA, fill=NA),
        panel.border = element_rect(fill = NA, color = "black"),
        # facet text size
        #strip.text = element_text(size = 8),
        # axis element
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_text(angle = 90),
        # legend elements
        legend.position = "bottom")
#legend.text = element_text(size = 8))

print(print_plot)
# save figure
ggsave("./fig_only_er.pdf", plot = print_plot, 
       width = 12, height = 8, units = "in")


# figures for research day
# plot of only respiratory outcomes

