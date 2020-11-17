# ------------------------------------------------------------------------------------------
library(devtools)
library(crayon)

library(lubridate)
library(haven)
library(brew)
library(ltxsparklines)
library(ggplot2)
library(magrittr)
library(data.table)
library(statar)

options(width=200)
# ------------------------------------------------------------------------------------------


# ------------------------------------------------------------------------------------------
dt_cps <- read_dta("../derived/state_agg_cps_long_full.dta") %>% data.table
dt_cps_us <- unique(dt_cps[, .(date = as.Date(ISOdate(year, month, 1)), classwkr,
	 employed_national, unemployed_national, urate_national)])
dt_cps_us <- dt_cps_us[ date >= as.Date("2020-02-01") ]
dt_cps_us <- dt_cps_us[ classwkr %in% c(22, 25, 27, 28) ]
setorder(dt_cps_us, classwkr, date)
dt_cps_us[]

# 22: Private, for profit
# 25: Federal government employee
# 27: State government employee
# 28: Local government employee

brew("../data/tables_to_brew/ts_cps_national2.brew.tex", "../output/appendix/tables/ts_cps_national2.tex")
# ------------------------------------------------------------------------------------------

# prettyNum(round(as.numeric(dt_cps_us[ classwkr==22 & date == as.Date("2020-02-01") ][["unemployed_national"]] / 1E3), 3), big.mark="," , scientific=F)
# prettyNum(round(as.numeric(dt_cps_us[ classwkr==22 & date == as.Date("2020-03-01") ][["unemployed_national"]] / 1E3), 3), big.mark="," , scientific=F) 
# prettyNum(round(as.numeric(dt_cps_us[ classwkr==22 & date == as.Date("2020-04-01") ][["unemployed_national"]] / 1E3), 3), big.mark="," , scientific=F) 
#  prettyNum(round(as.numeric(dt_cps_us[ classwkr==22 & date == as.Date("2020-05-01") ][["unemployed_national"]] / 1E3), 3), big.mark="," , scientific=F)  
#  prettyNum(round(as.numeric(dt_cps_us[ classwkr==22 & date == as.Date("2020-06-01") ][["unemployed_national"]] / 1E3), 3), big.mark="," , scientific=F) 
# sparkline(dt_cps_us[classwkr==22 & date <=as.Date("2020-06-01")][["unemployed_national"]],  enddotcolor="red", bottomline=TRUE) 