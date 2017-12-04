#!/bin/bash
#PBS -N ryan_rjob
#PBS -l nodes=1:ppn=1
#PBS -W group_list=pierce_group
#PBS -q batch

# set command directory
cd /home/ryangan/local_git_repo/oregon_wildfire/
# print directory
pwd

# export R library
export R_LIB=~/R/x86_64-pc-linux-gnu-library/3.3

# run rscript
Rscript --vanilla ./r_scripts/data_wrangle/asthma_cohort.R

