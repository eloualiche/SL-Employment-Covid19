#! /usr/bin/env R
#
#
# (c) Daniel Green and Erik Loualiche
# 
# Replication code for _State and Local Government Employment in the COVID-19 Crisis_
# Journal of Public Economics 
#
# This file builds Appendix Table 1
# Breakdown of State and Local Government Employment Declines
#
# --------------------------------------------------------------------------------------


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
# OCCUPATION TREND IN EMPLOYMENT
dt_cps_occ <- read_dta("../derived/state_occ_cps_long.dta") %>% data.table

dt_cps_occ[, date := as.Date(ISOdate(year, month, 1)) ]
dt_cps_occ <- dt_cps_occ[ date >= as.Date("2020-02-01") ]
dt_cps_occ <- dt_cps_occ[ classwkr %in% c(22, 25, 27, 28) ]

# Aggregate at national level
dt_occ_agg <- dt_cps_occ[, lapply(.SD, sum), .SDcols=c("employed", "unemployed"), by = .(classwkr, occ_detail, date) ][ order(classwkr, date, -employed) ]
dt_occ_agg[, total_employed := sum(employed), by = .(classwkr, date) ]
dt_occ_agg[, total_unemployed := sum(unemployed), by = .(classwkr, date) ]
dt_occ_agg[, total_workers  := total_employed + total_unemployed, by = .(classwkr, date) ]
dt_occ_agg[, occ_frac_employed    := 100 * employed / total_employed ]
dt_occ_agg[, occ_frac_unemployed  := 100 * unemployed / total_unemployed ]

# View states
dt_occ_agg[ classwkr == 27 & date %in% c(as.Date("2020-02-01"), as.Date("2020-04-01"))][ order(date, occ_frac_unemployed)]
dt_occ_agg[ classwkr == 28 & date %in% c(as.Date("2020-02-01"), as.Date("2020-04-01"))][ order(date, occ_frac_unemployed)]

dt_occ_out <- dt_occ_agg[ occ_detail %in% c("Education", "Administrative", "Healthcare", "Protective service", "Management")]
dt_occ_out[ occ_detail %in% "Education" ]

# BUILD TABLE
dt_occ_table <- dt_occ_agg[ date %in% c(as.Date("2020-02-01"), as.Date("2020-04-01"))][ order(date, occ_frac_unemployed)]
dt_occ_table[, date := month(date) ]
dt_occ_table <- dcast(dt_occ_table[, .(classwkr, occ_detail, date, employed, unemployed, total_employed, total_workers) ], classwkr + occ_detail ~ date,
    value.var = c("employed", "unemployed", "total_employed", "total_workers"))
dt_occ_table[, occ_group := "other" ]
dt_occ_table[occ_detail %in% c("Education", "Administrative", "Healthcare", "Protective service", "Management"), occ_group := occ_detail ]
dt_occ_table <- merge(
  dt_occ_table[, lapply(.SD, function(x) sum(x, na.rm=T)),
    .SDcols = c("employed_2", "employed_4", "unemployed_2", "unemployed_4") , by = .(classwkr, occ_group) ],
dt_occ_table[, lapply(.SD, function(x) median(x, na.rm=T)),
    .SDcols = c("total_employed_2", "total_employed_4", "total_workers_2", "total_workers_4") , by = .(classwkr) ],
by = c("classwkr") )

dt_occ_table[, frac_emp_2 := 100*employed_2 / total_employed_2]
dt_occ_table[, `:=`(change_emp = employed_4 - employed_2, total_change_emp = total_employed_4 - total_employed_2) ]
dt_occ_table[, `:=`(change_a_emp_pct = change_emp / (employed_2 + unemployed_2) ) ]
dt_occ_table[, `:=`(change_b_emp_pct = change_emp / employed_2) ]
dt_occ_table <- dt_occ_table[, .(occ_group, classwkr, frac_emp_2,
  frac_change_emp = 100*change_emp/total_change_emp,
  change_a_emp_pct = 100*change_a_emp_pct, change_b_emp_pct = 100*change_b_emp_pct) ]
dt_occ_table <- dcast(dt_occ_table, occ_group ~ classwkr,
  value.var = c("frac_emp_2", "frac_change_emp", "change_a_emp_pct", "change_b_emp_pct") )[ order(-frac_emp_2_27)]
dt_occ_table <- dt_occ_table[, .(occ_group, change_a_emp_pct_27, change_b_emp_pct_27, frac_emp_2_27, frac_change_emp_27, change_a_emp_pct_28, change_b_emp_pct_28, frac_emp_2_28, frac_change_emp_28)]
dt_occ_table[, `:=`(change_emp_pct_27=change_b_emp_pct_27, change_emp_pct_28=change_b_emp_pct_28) ]

brew("../data/tables_to_brew/summary_occupation.brew.tex", "../output/appendix/tables/summary_occupation.tex")
# ------------------------------------------------------------------------------------------
