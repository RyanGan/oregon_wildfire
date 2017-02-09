# ------------------------------------------------------------------------------
# Title: Oregon 2013 claims data for MobaXterm
# Author: Jingyang Liu
# Date Created: Feb 9th, 2017
# R Version: 3.3.2
# ------------------------------------------------------------------------------

getwd() # "/home/jyliu/wildfire/r_scripts"

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
stop_time # 8.281118 mins

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




