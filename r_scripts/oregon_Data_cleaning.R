# ------------------------------------------------------------------------------
# Title: Oregon 2013 claims data for MobaXterm
# Author: Jingyang Liu
# Date Created: Feb 9th, 2017
# R Version: 3.3.2
# ------------------------------------------------------------------------------

getwd() # "/home/jyliu/wildfire/r_scripts"

library(dplyr)
library(data.table)

read_path <- paste0("../data/gan_episodes_of_care.txt")
oregon_df <- fread(read_path, sep = "|", nrows = 2000000, showProgress = T)
fwrite(oregon_df, "../data/oregon_epis_care_reduced.csv")

# I wrote a reduced dataframe of 2000,000 rows to get an idea of data structure

# import the reduced dataframe
start_time <- Sys.time()

oregon_df <- fread("../data/oregon_epis_care_reduced.csv", sep = ",", 
                   colClasses = rep("character", 72))

stop_time <-  Sys.time() - start_time 
# time it took
stop_time # 15.83603 secs


start_time <- Sys.time()
oregon_full <- fread(read_path, sep = "|", showProgress = T) # not change class
stop_time <-  Sys.time() - start_time 
# time it took
stop_time # 7.771713 mins

summary(as.factor(oregon_full$year)) # all 2013, 77069320
summary(as.factor(oregon_full$personkey))
min(as.numeric(oregon_full$personkey))
max(as.numeric(oregon_full$personkey)) # seems length is 8
unique(as.numeric(oregon_full$line)) # 1362 levels, 0 - 9999
summary(as.factor(oregon_full$clmstatus))
#       D        E        P
# 8676643  3324496 65068181
summary(as.factor(oregon_full$cob)) 
#                 N        Y
# 23517978 44438688  9112654
summary(as.factor(oregon_full$paytype)) #C, D, G, P, T
summary(as.factor(oregon_full$prod))
summary(as.factor(oregon_full$payer))
summary(as.factor(oregon_full$medflag))
#       0        1     NA's
# 5697342 70573441   798537
summary(as.factor(oregon_full$rxflag))
#       0        1     NA's
# 7761348 68509435   798537
summary(as.factor(oregon_full$ohvmhflag))
#        N   *NULL*        Y
# 53291284 23589431   188605
summary(as.factor(oregon_full$pebb))
#        0        1
# 73949154  3120166
summary(as.factor(oregon_full$oebb))
#        0        1
# 74285962  2783358
summary(as.factor(oregon_full$patid))
summary(as.factor(oregon_full$gender))
#        F        M        U
# 46672307 30350771    46242
summary(as.factor(oregon_full$yob)) # old:1901 young:2016
summary(as.factor(oregon_full$race))
#        1        2        3        4        5        6        9     NA's
# 24132190  1611193   620854   203550    85465   248416 50141889    25763
summary(as.factor(oregon_full$ethn))
#        1        2        3     NA's
#  583948  7398053 69061589    25730
summary(as.factor(oregon_full$lang))
#        1        2        3        4        8        9     NA's
# 36761368  1031059      587   203152   501575 38002063   569516
summary(as.factor(oregon_full$MSA))
summary(as.factor(oregon_full$STATE)) # OR: 76555225; blank: 78646
summary(as.factor(oregon_full$ZIP)) # NA: 78646
summary(as.factor(oregon_full$fromdate))
unique(as.factor(oregon_full$fromdate))
summary(as.factor(oregon_full$todate))
summary(as.factor(oregon_full$paydate))
summary(as.factor(oregon_full$paid)) # NA: 279811
summary(as.factor(oregon_full$tob)) # blank: 40545960 NULL: 19358510 
summary(as.factor(oregon_full$pos)) # NULL: 7106719
summary(as.factor(oregon_full$dx1)) # NULL: 24513232

# ndc
summary(as.numeric(oregon_full$ndc)) # blank: 53599072
length(as.factor(oregon_full$dx1)!= NULL)

summary(as.factor(oregon_full$dx2))
summary(as.factor(oregon_full$dx3))
summary(as.factor(oregon_full$dx4))
summary(as.factor(oregon_full$dx5))
summary(as.factor(oregon_full$dx6))
summary(as.factor(oregon_full$dx7))
summary(as.factor(oregon_full$dx8))
summary(as.factor(oregon_full$dx9))
summary(as.factor(oregon_full$dx10))
summary(as.factor(oregon_full$dx11))
summary(as.factor(oregon_full$dx12))
summary(as.factor(oregon_full$dx13))

summary(as.factor(oregon_full$poa1))
#                 N        Y
# 72434455    58842  4576023

summary(as.factor(oregon_full$poa2))
#                 N        Y
# 74380115   261029  2428176

summary(as.factor(oregon_full$poa3))
#                 N        Y
# 74688528   264287  2116505

summary(as.factor(oregon_full$px1)) 
summary(as.factor(oregon_full$px2))
summary(as.factor(oregon_full$px3))

summary(as.factor(oregon_full$proccode))
summary(as.factor(oregon_full$mod1))
summary(as.factor(oregon_full$mod2))
summary(as.factor(oregon_full$megcode))
summary(as.factor(oregon_full$megdesc))
summary(as.factor(oregon_full$megbodysys))
summary(as.factor(oregon_full$megstage))
summary(as.factor(oregon_full$megtype))
#                   9 NOT-CATEGORIZED             Acute           Chronic
#              1751           6979372          34062467          31197648
#         Well Care
#           4828082

summary(as.factor(oregon_full$megcomplete))
#        0        1     NA's
# 10090656 66976913     1751

summary(as.factor(oregon_full$megnum))
summary(as.factor(oregon_full$megdays))
summary(as.factor(oregon_full$megprorate))
summary(as.factor(oregon_full$megoutlier))
#        0        1
# 54675387 22393933

summary(as.factor(oregon_full$meglow))
#    FALSE     TRUE
# 75145616  1923704

summary(as.factor(oregon_full$meghigh))
#    FALSE     TRUE
# 56599091 20470229

summary(as.factor(oregon_full$rxclass))
summary(as.factor(as.character(oregon_full$qtydisp)))
summary(as.factor(oregon_full$rxdays))
summary(as.factor(oregon_full$daw))
summary(as.factor(oregon_full$ptstatus))
summary(as.factor(oregon_full$los))
summary(as.factor(oregon_full$msdrg))
summary(as.factor(oregon_full$icd_ver))
#    FALSE     NA's
# 52556088 24513232


sum(oregon_full$dx1=='*NULL*'&oregon_full$ndc=="") # 1042984

