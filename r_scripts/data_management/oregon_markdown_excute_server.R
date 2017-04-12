# ------------------------------------------------------------------------------
# Title: Script to excute descriptive summary R markdown file
# Author: Jingyang Liu
# Date: 3/2/2016
# ------------------------------------------------------------------------------


# call libraries
library(rmarkdown)

# call markdown doc.
rmarkdown::render('./oregon_data_explore_server.rmd')


