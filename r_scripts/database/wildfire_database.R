# ------------------------------------------------------------------------------
# Title:  Wildfire database
# Author: Jingyang Liu
# Date Created: May 1, 2017
# ------------------------------------------------------------------------------

library(RSQLite)
library(dplyr)
library(readr)

# getwd()  "/home/jyliu/wildfire/local_git_repo/oregon_wildfire/r_scripts"

### Import csv files
read_path <- paste0("../../../../../magzamen/ryan_data/wa_2012_county_er_time_series.csv")
wa_2012_county_er_ts <- read_csv(read_path)

read_path2 <- paste0("../../../../../magzamen/ryan_data/wa_county_pop_wt_pm.csv")
wa_county_pop_wt_pm <- read_csv(read_path2)

read_path3 <- paste0("../../../../../magzamen/ryan_data/or_2013_county_er_time_series.csv")
or_2013_county_er_ts <- read_csv(read_path3)

read_path4 <- paste0("../../../../../magzamen/ryan_data/or_county_pop_wt_pm.csv")
or_county_pop_wt_pm <- read_csv(read_path4)

read_path5 <- paste0("../../../../../magzamen/ryan_data/colorado_county_timeseries_2010_2015.csv")
co_2010_2015_county_ts <- read_csv(read_path5)

### Create wildfire database
wildfire_DB <- "../../../../../magzamen/wildfire_database/wildfire_db.db"
wf_Conn <- dbConnect(drv = SQLite(), dbname= wildfire_DB)

### Write tables in database
dbListTables(wf_Conn)
dbWriteTable(wf_Conn, name = "health_wa_2012_county_er_ts",or_2013_county_er_ts)
dbWriteTable(wf_Conn, name = "smoke_wa_county_pop_wt_pm",wa_county_pop_wt_pm)
dbWriteTable(wf_Conn, name = "health_or_2013_county_er_ts",wa_2012_county_er_ts)
dbWriteTable(wf_Conn, name = "smoke_or_county_pop_wt_pm",or_county_pop_wt_pm)
dbWriteTable(wf_Conn, name = "health_co_2010_2015_county_ts",co_2010_2015_county_ts)
dbListTables(wf_Conn)

q()


###-----------------------------------------------------------------------------
### close and revoke
library(RSQLite)
library(dplyr)
library(readr)

wf_DB <- "../../../../../magzamen/wildfire_database/wildfire_db.db"

newConn <- dbConnect(drv = SQLite(), dbname= wf_DB)
dbListTables(newConn)

co_try <- dbGetQuery(newConn, "SELECT * FROM health_co_2010_2015_county_ts")



### future analysis-------------------------------------------------------------
library(RSQLite)
library(dplyr)
library(readr)

wf_DB <- "../../../../../magzamen/wildfire_database/wildfire_db.db"
wf_Conn <- dbConnect(drv = SQLite(), dbname= wf_DB)
dbListTables(wf_Conn)

co_county_ts <- dbGetQuery(wf_Conn, "SELECT * FROM health_co_2010_2015_county_ts")
wa_county_ts <- dbGetQuery(wf_Conn, "SELECT * FROM health_wa_2012_county_er_ts")
or_county_ts <- dbGetQuery(wf_Conn, "SELECT * FROM health_or_2013_county_er_ts")
wa_county_pop_wt_pm <- dbGetQuery(wf_Conn, "SELECT * FROM smoke_wa_county_pop_wt_pm")
or_county_pop_wt_pm <- dbGetQuery(wf_Conn, "SELECT * FROM smoke_or_county_pop_wt_pm")















