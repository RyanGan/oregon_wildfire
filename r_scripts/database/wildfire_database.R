# ------------------------------------------------------------------------------
# Title:  Wildfire database
# Author: Jingyang Liu
# Date Created: May 1, 2017
# ------------------------------------------------------------------------------

library(RSQLite)
library(dplyr)

# getwd()  "/home/jyliu/wildfire/local_git_repo/oregon_wildfire/r_scripts"

read_path <- paste0("../../../../../magzamen/wildfire_database/wa_2012_county_er_time_series.csv")
wa_2012_county_ts <- read_csv(read_path)

read_path2 <- paste0("../../../../../magzamen/wildfire_database/wa_county_pop_wt_pm.csv")
wa_county_pop_wt_pm <- read_csv(read_path2)

wildfire_DB <- "../../../../../magzamen/wildfire_database/wildfire_db.db"
wf_Conn <- dbConnect(drv = SQLite(), dbname= wildfire_DB)

start_time <- Sys.time()
dbListTables(wf_Conn)
dbWriteTable(wf_Conn, name = "health_wa_2012_county_er_time_series",wa_2012_county_ts)

stop_time <-  Sys.time() - start_time 
# time it took
stop_time # 18.47879 mins

start_time <- Sys.time()
dbListTables(wf_Conn)
dbWriteTable(wf_Conn, name = "smoke_wa_county_pop_wt_pm",wa_county_pop_wt_pm)

stop_time <-  Sys.time() - start_time 
# time it took
stop_time # 18.47879 mins

