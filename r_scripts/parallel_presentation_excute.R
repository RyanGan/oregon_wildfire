# ------------------------------------------------------------------------------
# Title: Script to excute descriptive summary R markdown file
# Author: Jingyang Liu
# Date: Mar 29, 2017
# ------------------------------------------------------------------------------


# call libraries
library(rmarkdown)

# specify an option for incremental rendering
render("parallel_presentation.Rmd", slidy_presentation(incremental = TRUE))

