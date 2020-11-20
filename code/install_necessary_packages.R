#! /usr/bin/env R
#
#
# (c) Daniel Green and Erik Loualiche
# 
# Replication code for _State and Local Government Employment in the COVID-19 Crisis_
# Journal of Public Economics 
#
# This file installs necessary packages for running R code
#
# --------------------------------------------------------------------------------------



# --------------------------------------------------------------------------------------
install.packages("pacman", repos='http://cran.us.r-project.org')
library(pacman)


# list of necessary packages across
l_pak <- c("bit64", "brew", "crayon", "data.table", "devtools", 
  "fredr", "fst", "glue", "haven",
  "lfe", "ltxsparklines", "lubridate", "magrittr", "progress", "purrr",
  "readr", "readxl", "stargazer", "statar", "stringr", "tidyr")

for (i in seq(1,length(l_pak))){
  message("Checking/Installing ... ", l_pak[i])
  p_install(l_pak[i], character.only=TRUE, force=FALSE)
}
# --------------------------------------------------------------------------------------
