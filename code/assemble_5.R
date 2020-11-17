#! /usr/bin/env R
#
#
# (c) Daniel Green and Erik Loualiche
# 
# Replication code for _State and Local Government Employment in the COVID-19 Crisis_
# Journal of Public Economics 
#
# This file builds the supplementary dataset table for regression analysis
# It is mostly used for long run annual table A7
#
# --------------------------------------------------------------------------------------


# ------------------------------------------------------------------------------------------
library(devtools)
library(crayon)

# normal stuff
library(glue)
library(stringr)
library(lubridate)
library(readr)
library(Hmisc)
library(haven)
library(fredr)
library(fst)
library(readxl)
library(lfe)
library(brew)
library(texreg)
library(data.table)
library(statar)
# ------------------------------------------------------------------------------------------


# ------------------------------------------------------------------------------------------\
# AGGREGATE DATA: GDP DEFLATOR
# FRED_API_key <- '../data/fred.api.txt' # fill in your API key for FRED
# FRED_API_key <- readChar(FRED_API_key, file.info(FRED_API_key)$size)# 
# fredr_set_key(FRED_API_key)
# dt_gdpdef <- fredr(series_id="GDPDEF",
#       observation_start = as.Date(paste0(1980, "-01-01") ), 
#       observation_end = as.Date(paste0(2020, "-12-01") )  ) %>% data.table
# dt_gdpdef <- dt_gdpdef[, .(date, gdpdef=value)]
# # rebase for 2017 (last year in reg short)
# dt_gdpdef[ date=="2017-01-01"]
# dt_gdpdef <- dt_gdpdef[, .(dateq=as.quarterly(date), gdpdef=gdpdef / dt_gdpdef[ date=="2017-01-01"][["gdpdef"]]) ]
# fwrite(dt_gdpdef, "../data/gdpdef.csv")

dt_gdpdef <- fread("../data/gdpdef.csv")
dt_gdpdef[, dateq := as.quarterly(dateq) ]
# ------------------------------------------------------------------------------------------\


# ------------------------------------------------------------------------------------------
# CPS AND LOCAL POPULATION
dt_cps  <- read_dta("../derived/state_agg_cps_long_full.dta") %>% data.table
dt_cps[, date  := as.Date(ISOdate(year, month, 1)) ]
dt_cps[, datem := as.monthly(date) ]
dt_cps[, .(nobs=.N), by = .(datem) ][order(datem)]
dt_cps[]
# ------------------------------------------------------------------------------------------


# ------------------------------------------------------------------------------------------\
# BLS CES DATA, LABOR FORCE
dt_blf <- read_dta("../derived/BLS_laborforce_size.dta") %>% data.table
dt_blf <- dt_blf[ seasonality == "S", .(datem=as.monthly(date), state_name=state, civil_pop, lforce = laborforce_total) ]
# CES LAU DATA
dt_lau_agg <- read_fst("../derived/lau_aggregates.fst", as.data.table=TRUE)
dt_lau_agg[, datem := as.monthly(datem) ]
# ------------------------------------------------------------------------------------------


# ------------------------------------------------------------------------------------------
# PAYROLL DATA
dt_payroll_ASPEP <- read_dta("../derived/ASPEP_local_payroll_aggregates_1993_2018.dta") %>% data.table
dt_payroll_ASPEP[, state_code := NULL ]
dt_payroll_ASPEP <- dcast(dt_payroll_ASPEP, date_y + state ~ lvl_code, value.var=c("emp_full_time", "payroll_full_time"))
setnames(dt_payroll_ASPEP, gsub("_1", "_SL", colnames(dt_payroll_ASPEP) ) )
setnames(dt_payroll_ASPEP, gsub("_2", "_S", colnames(dt_payroll_ASPEP) ) )
setnames(dt_payroll_ASPEP, gsub("_3", "_L", colnames(dt_payroll_ASPEP) ) )
# pay per capita
dt_payroll_ASPEP[, pay_full_time_SL := payroll_full_time_SL / emp_full_time_SL ]
dt_payroll_ASPEP[, pay_full_time_S  := payroll_full_time_S  / emp_full_time_S  ]
dt_payroll_ASPEP[, pay_full_time_L  := payroll_full_time_L  / emp_full_time_L  ]
dt_payroll_ASPEP
# ------------------------------------------------------------------------------------------


# ------------------------------------------------------------------------------------------
# NASBO RAINY DAY FUNDS

# ------------------------------------------------------------------------------------------
# PROCESS 2011 to 2020 (one table)
dt_rdf1 <- data.table()

for (date_y in seq(1986, 1994)){
  # date_y = 1986
  dt_tmp <- read_excel("../data/NASBO/RainyDayFunds1986-1994.xlsx", sheet = paste0(date_y)) %>% data.table
  dt_tmp[, year := date_y ]
  dt_tmp[, State := str_trim(gsub("\\*", "", State)) ]  
  setnames(dt_tmp, c("State", "rdf", "Expenditures", "year"))
  dt_tmp[, rdf := as.numeric(rdf) ]
  dt_rdf1 <- rbind(dt_rdf1, dt_tmp)
}

for (date_y in seq(1995, 2008)){
  # date_y = 1998
  dt_tmp <- read_excel("../data/NASBO/RainyDayFunds1995-2008.xlsx", sheet = paste(date_y, "Actual")) %>% data.table
  setnames(dt_tmp, gsub(" ", "", colnames(dt_tmp)))
  dt_tmp <- dt_tmp[, .(State, Expenditures, BudgetStabilizationFund) ]
  dt_tmp[, year := date_y ]
  dt_tmp[, State := str_trim(gsub("\\*", "", State)) ]  
  setnames(dt_tmp, c("State", "rdf", "Expenditures", "year"))
  dt_tmp[, rdf := as.numeric(rdf) ]
  dt_tmp[, Expenditures := as.numeric(Expenditures) ]
  dt_rdf1 <- rbind(dt_rdf1, dt_tmp)
}

dt_rdf1

dt_rdf2 <- read_excel("../data/NASBO/RainyDayFunds2011-2020.xlsx", sheet = "Total Amount") %>% data.table
col_list2 <- glue("rdf_{seq(2011, 2020)}")
setnames(dt_rdf2, c("State", col_list2) )
dt_rdf2[, c(col_list2) := lapply(.SD, as.numeric), .SDcols = c(col_list2) ]
dt_rdf2[ State != "State" ]
dt_rdf2[, State := str_trim(gsub("\\*", "", State)) ]  
dt_rdf2 <- melt(dt_rdf2, "State", value.name = "rdf")
dt_rdf2[, year := as.integer(str_sub(variable, -4, -1)) ]
dt_tmp <- read_excel("../data/NASBO/RainyDayFunds2011-2020.xlsx", sheet = "Percent") %>% data.table
setnames(dt_tmp, c("State", col_list2) )
dt_tmp[, c(col_list2) := lapply(.SD, as.numeric), .SDcols = c(col_list2) ]
dt_tmp[ State != "State" ]
dt_tmp[, State := str_trim(gsub("\\*", "", State)) ]  
dt_tmp <- melt(dt_tmp, "State", value.name = "rdf_frac")
dt_tmp[, year := as.integer(str_sub(variable, -4, -1)) ]
dt_rdf2 <- merge(dt_rdf2[, -"variable"], dt_tmp[, -"variable"], by = c("State", "year"))

dt_rdf_full <- rbind(dt_rdf1, dt_rdf2, fill = T)
dt_rdf_full[rdf_frac > 0, Expenditures := rdf / rdf_frac * 100 ]
setcolorder(dt_rdf_full, c("State", "year"))
write_dta(dt_rdf_full, "../derived/rainy_day_funds_timeseries.dta")

dt_nasbo_rdf <- copy(dt_rdf_full)
setnames(dt_nasbo_rdf, c("state_name", "date_y", "rdf", "Expenditures", "rdf_frac"))

# ------------------------------------------------------------------------------------------


# ------------------------------------------------------------------------------------------
# STATE AND LOCAL TAXES
# --------------------
# QUARTERLY STATE TAXES
dt_qtax <- read_dta("../derived/QTAX_STATE_processed.dta") %>% data.table
dt_qtax[, datem := as.monthly(date) ]
dt_qtax[, dateq := as.quarterly(datem)]
l_tax_keep <- c("T01", "T09", "T40", "T41")
dt_qtax <- dcast(dt_qtax[item_code %in% l_tax_keep, .(datem, dateq, state, state_name, total_taxes, item_code, value)],
                 datem + dateq + state + state_name + total_taxes ~ item_code, value.var = c("value") )
dt_qtax[, c(paste0(paste0("S_", l_tax_keep, "_share"))) := lapply(.SD, function(x) x/total_taxes), .SDcols=l_tax_keep]
setnames(dt_qtax, l_tax_keep, paste0("S_", l_tax_keep))
# Build lag tax shares
for (i in seq(1, 4)){
  dt_qtax[, glue("l{i}_S_T01_share") := tlag(S_T01_share, n=3*i, time=datem), by = .(state) ]
  dt_qtax[, glue("l{i}_S_T09_share") := tlag(S_T09_share, n=3*i, time=datem), by = .(state) ]
  dt_qtax[, glue("l{i}_S_T40_share") := tlag(S_T40_share, n=3*i, time=datem), by = .(state) ]
  dt_qtax[, glue("l{i}_S_T41_share") := tlag(S_T41_share, n=3*i, time=datem), by = .(state) ]
}
dt_qtax[]


# # --------------- WORK IN PROGRESS -> checking what's wrong in this dataset
# # TAXES FOR STATE AND LOCAL GOVERNMENT

# ---------------
# STATE AND LOCAL AGGREGATE TABLES REVENUES AND TAXES
dt_sl_agg <- read_fst("../derived/SandL_aggregates.fst", as.data.table=T)
dt_sl_agg %>% tab(variable)
# - aggregate the data again in 3 categories: 1: state + local total / 2: state only /  3: local only
dt1 <- dt_sl_agg[, .(value = sum(value, na.rm=T)), by = .(date_y, state, variable) ]
dt2 <- dt_sl_agg[type==0, .(value = sum(value, na.rm=T)), by = .(date_y, state, variable) ]
dt3 <- dt_sl_agg[type!=0, .(value = sum(value, na.rm=T)), by = .(date_y, state, variable) ]
dt_sl_agg <- rbind(dt1[, lvl_code := 1], dt2[, lvl_code := 2], dt3[, lvl_code := 3] )
dt_sl_agg %>% tab(variable)

dt_sl_agg <- dt_sl_agg[ variable %in% c("GAL_revenue", "IG_fed_revenue", "TAX_revenue",
                                        "PROPTAX_revenue", "SALESTAX_revenue", "INCMTAX_revenue", "CORPTAX_revenue", "OTHTAX_revenue") ]
dt_sl_us <- dt_sl_agg[, .(value=sum(value, na.rm=T)), by = .(date_y, variable, lvl_code) ]
dt_sl_us[, state := "US" ]

dt_sl_agg_wide <- dcast(dt_sl_agg, date_y + state ~ variable + lvl_code, value.var = "value")
setnames(dt_sl_agg_wide, gsub("_1", "_SL", colnames(dt_sl_agg_wide)) )
setnames(dt_sl_agg_wide, gsub("_2", "_S",  colnames(dt_sl_agg_wide)) )
setnames(dt_sl_agg_wide, gsub("_3", "_L",  colnames(dt_sl_agg_wide)) )
# --- evaluate tax shares for instruments
dt_sl_agg_wide[, SALESTAX_revshare_SL := SALESTAX_revenue_SL / TAX_revenue_S ]
dt_sl_agg_wide[, SALESTAX_revshare_L  := SALESTAX_revenue_L  / TAX_revenue_L ]
dt_sl_agg_wide[, SALESTAX_revshare_S  := SALESTAX_revenue_S  / TAX_revenue_S ]
dt_sl_agg_wide[, INCMTAX_revshare_SL  := INCMTAX_revenue_SL / TAX_revenue_S ]
dt_sl_agg_wide[, INCMTAX_revshare_L   := INCMTAX_revenue_L  / TAX_revenue_L ]
dt_sl_agg_wide[, INCMTAX_revshare_S   := INCMTAX_revenue_S  / TAX_revenue_S ]
dt_sl_agg_wide[, CORPTAX_revshare_SL  := CORPTAX_revenue_SL / TAX_revenue_S ]
dt_sl_agg_wide[, CORPTAX_revshare_L   := CORPTAX_revenue_L  / TAX_revenue_L ]
dt_sl_agg_wide[, CORPTAX_revshare_S   := CORPTAX_revenue_S  / TAX_revenue_S ]
dt_sl_agg_wide[, PROPTAX_revshare_L   := PROPTAX_revenue_L  / TAX_revenue_L ]
dt_sl_agg_wide
# ------------------------------------------------------------------------------------------


# ------------------------------------------------------------------------------------------
# --- merge all of the datasets

# merge qtax
dt_reg <- merge(dt_cps[, .(datem, statefip=as.integer(statefip),  dateq=as.quarterly(datem), 
                           classwkr=as.character(classwkr), employed, unemployed, travel, laborforce, laidoff,
		                       total, uhrsworkt, ahrsworkt, part_time_reason, part_time_covid, urate, urate_bl, urate_wpt) ],
	            dt_lau_agg[, .(datem, statefip, state, state_name, classwkr=as.character(classwkr), emp_ces)],
	            all.x = T, all.y = T, by = c("datem", "statefip", "classwkr") )
dt_reg <- merge(dt_reg, dt_qtax[, -"datem"], all.x=T, by = c("state", "dateq", "state_name") )
dt_reg <- merge(dt_reg, dt_blf, by = c("datem", "state_name"), all.x = T)
dt_reg[, `:=`(date_y = year(datem) ) ]
# --- merge InterGovernmental Revenue
dt_reg <- merge(dt_reg, dt_sl_agg_wide, all.x = T, by = c("date_y", "state"))
# --- merge Rainy Day Funds
dt_reg <- merge(dt_reg, dt_nasbo_rdf[, .(date_y, state_name, rdf, rdf_frac)], all.x = T, 
  by = c("date_y", "state_name"))
# --- merge payroll
dt_reg <- merge(dt_reg, dt_payroll_ASPEP, all.x = T, by = c("date_y", "state"))
# --- merge GDP deflator
dt_reg <- merge(dt_reg, dt_gdpdef, all.x=T, by = c("dateq"))
dt_reg <- dt_reg[ !is.na(state) ]
# ------------------------------------------------------------------------------------------


# ------------------------------------------------------------------------------------------
# DEFINE REGRESSION VARIABLES

# create time trend and factors
dt_reg[, time_trend := as.integer(datem) ]
dt_reg[, classwkr   := as.factor(classwkr) ]
dt_reg[, time_fac   := as.factor(datem) ]

# ADJUST FOR INFLATION USING GDP DEFLATOR
gdpdef_adj = TRUE
if (gdpdef_adj == TRUE){
	dt_reg[, TAX_revenue_SL := TAX_revenue_SL * gdpdef ]
	dt_reg[, TAX_revenue_S  := TAX_revenue_S  * gdpdef ]
	dt_reg[, TAX_revenue_L  := TAX_revenue_L  * gdpdef ]

	dt_reg[, IG_fed_revenue_SL := IG_fed_revenue_SL * gdpdef ]
	dt_reg[, IG_fed_revenue_S  := IG_fed_revenue_S  * gdpdef ]
	dt_reg[, IG_fed_revenue_L  := IG_fed_revenue_L  * gdpdef ]

	dt_reg[, payroll_full_time_SL := payroll_full_time_SL * gdpdef ]
	dt_reg[, payroll_full_time_S  := payroll_full_time_S  * gdpdef ]
	dt_reg[, payroll_full_time_L  := payroll_full_time_L  * gdpdef ]

	dt_reg[, pay_full_time_SL := pay_full_time_SL * gdpdef ]
	dt_reg[, pay_full_time_S  := pay_full_time_S  * gdpdef ]
	dt_reg[, pay_full_time_L  := pay_full_time_L  * gdpdef ]
}
# ---

# dt_reg[, tax_pop_sa := total_taxes_sa / civil_pop ]
dt_reg[, tax_pop    := total_taxes    / civil_pop ]
# dt_reg[ total_taxes_sa>0, log_total_taxes_sa := log(total_taxes_sa) ]
dt_reg[ total_taxes>0,    log_total_taxes    := log(total_taxes) ]
dt_reg[, laidoff_frac := laidoff / (employed + unemployed) ]

# create some log variables
dt_reg[, `:=`(log_laidoff=log(1+laidoff), log_hours=log(ahrsworkt),
	log_ptime_reason=log(part_time_reason), log_ptime_covid=log(part_time_covid) ) ]
dt_reg[ urate>0,     log_urate1 := log(urate) ]
dt_reg[ urate_bl>0,  log_urate2 := log(urate_bl)  ]
dt_reg[ urate_wpt>0, log_urate3 := log(urate_wpt) ]
dt_reg[, emp_ces_rate   := 100 * emp_ces / lforce ]
dt_reg[, unemp_ces_rate := 100 * (1 - emp_ces / lforce) ]

# CREATE QUANTILES FOR SOME VARIABLES
dt_reg[, `:=`(q_rdf_frac = xtile(rdf_frac, 5)), by = .(datem, classwkr) ]

# New labor variable
dt_reg[, `:=`(log_emp=log(employed), log_unemp=log(unemployed), log_emp_ces=log(emp_ces) ) ]
dt_reg[, `:=`(log_emp_SL=log(emp_full_time_SL), log_emp_S=log(emp_full_time_S), log_emp_L=log(emp_full_time_L) ) ]
dt_reg[, `:=`(log_payroll_SL=log(payroll_full_time_SL), log_payroll_S=log(payroll_full_time_S), log_payroll_L=log(payroll_full_time_L) ) ]
dt_reg[, `:=`(log_pay_SL=log(pay_full_time_SL), log_pay_S=log(pay_full_time_S), log_pay_L=log(pay_full_time_L) ) ]
dt_reg[, `:=`(log_TAX_revenue_SL=log(TAX_revenue_SL), log_TAX_revenue_S=log(TAX_revenue_S), log_TAX_revenue_L=log(TAX_revenue_L)) ]
dt_reg[, `:=`(log_rdf=log(rdf), log_rdf_frac=log(rdf_frac) ) ]

# Introduce lags
dt_reg[, f1y_log_emp    := tlead(log_emp, 12L, time=datem), by = .(state, classwkr) ]
dt_reg[, f1y_log_unemp  := tlead(log_unemp, 12L, time=datem), by = .(state, classwkr) ]
dt_reg[, f1y_log_emp_SL := tlead(log_emp_SL, 12L, time=datem), by = .(state, classwkr) ]
dt_reg[, f1y_log_emp_S  := tlead(log_emp_S,  12L, time=datem), by = .(state, classwkr) ]
dt_reg[, f1y_log_emp_L  := tlead(log_emp_L,  12L, time=datem), by = .(state, classwkr) ]
dt_reg[, f1y_log_payroll_SL := tlead(log_payroll_SL, 12L, time=datem), by = .(state, classwkr) ]
dt_reg[, f1y_log_payroll_S  := tlead(log_payroll_S,  12L, time=datem), by = .(state, classwkr) ]
dt_reg[, f1y_log_payroll_L  := tlead(log_payroll_L,  12L, time=datem), by = .(state, classwkr) ]
dt_reg[, f1y_log_pay_SL := tlead(log_pay_SL, 12L, time=datem), by = .(state, classwkr) ]
dt_reg[, f1y_log_pay_S  := tlead(log_pay_S,  12L, time=datem), by = .(state, classwkr) ]
dt_reg[, f1y_log_pay_L  := tlead(log_pay_L,  12L, time=datem), by = .(state, classwkr) ]

# dt_reg[, `:=`(log_TAX_revenue_SL=log(TAX_revenue_SL), log_TAX_revenue_S=log(TAX_revenue_S), log_TAX_revenue_L=log(TAX_revenue_L)) ]

# --- create dummies by class of workers
dt_reg[, dumm_private := fifelse(classwkr == "22", 1, 0) ]
dt_reg[, dumm_fed     := fifelse(classwkr == "25", 1, 0) ]
dt_reg[, dumm_state   := fifelse(classwkr == "27", 1, 0) ]
dt_reg[, dumm_local   := fifelse(classwkr == "28", 1, 0) ]

# long with classwkr
dt_reg[, classwkr := as.factor(classwkr) ]
dt_reg[, classwkr := relevel(classwkr, ref = "22") ]  # reference level of factor is private -> 22
dt_reg[, date_fac := as.factor(datem) ]
dt_reg <- dt_reg[ classwkr %in% c(22, 25, 27, 28, 271, 281) ]
# ------------------------------------------------------------------------------------------


# ------------------------------------------------------------------------------------------
# ANNUAL REGRESSION

# --- make the dataset annual by removing superfluous variables
dt_reg_short <- dt_reg[, -c("log_emp", "log_unemp", "f1y_log_emp", "f1y_log_unemp", "classwkr", "employed", "unemployed", "travel",
	"laborforce", "laidoff", "total", "uhrsworkt", "ahrsworkt", "part_time_reason",  "part_time_covid",
	"urate", "urate_bl", "urate_wpt", "laidoff_frac", "log_laidoff", "log_hours", "log_ptime_reason", "log_ptime_covid",
	"log_urate1", "log_urate2", "log_urate3", "dumm_private", "dumm_fed", "dumm_state", "dumm_local", "tax_pop",
	"emp_ces", "emp_ces_rate", "unemp_ces_rate", "log_emp_ces", "log_total_taxes", "gdpdef")] %>% unique
# remove quarterly taxes
dt_reg_short[, c(colnames(dt_qtax)[5:length(colnames(dt_qtax))]) := NULL ]
dt_reg_short <- dt_reg_short[ !is.na(dateq) ] %>% unique
dt_reg_short <- dt_reg_short[ quarter(dateq) == 4 ]
dt_reg_short <- dt_reg_short[, -c("dateq", "datem", "time_trend", "date_fac", "time_fac", "lforce", "civil_pop") ] %>% unique
# make unique year / state observations
dt_reg_short[, n_obs := seq(1, .N), by = .(state, date_y) ]
dt_reg_short <- dt_reg_short[ n_obs == 1 ]
dt_reg_short[, .N, by = .(state, date_y)][ N > 1 ]

# --- merge back civil pop for weights
dt_reg_short <- merge(dt_reg_short,
    dt_blf[ month(datem)==12, .(date_y=year(datem), state_name, civil_pop, lforce) ], all.x=T, by = c("date_y", "state_name"))
dt_reg_short <- merge(dt_reg_short,
	dt_lau_agg[ month(date) == 12 & classwkr == 22, .(date_y=year(datem), state_name, log_emp_ces=log(emp_ces)) ], all.x=T, by = c("date_y", "state_name"))


# LEAD LHS VARIABALE
for (h in seq(1,5)){
	dt_reg_short[, c(glue("f{h}y_log_emp_ces")) := tlead(log_emp_ces, h, time=date_y), by = .(state) ]
	dt_reg_short[, c(glue("f{h}y_log_emp_SL"))  := tlead(log_emp_SL,  h, time=date_y), by = .(state) ]
	dt_reg_short[, c(glue("f{h}y_log_emp_S"))   := tlead(log_emp_S,   h, time=date_y), by = .(state) ]
	dt_reg_short[, c(glue("f{h}y_log_emp_L"))   := tlead(log_emp_L,   h, time=date_y), by = .(state) ]
}

# FIRST DIFFERENCE
for (h in seq(1,5)){
	dt_reg_short[, c(glue("df{h}y_log_emp_ces")) := get(glue("f{h}y_log_emp_ces")) - log_emp_ces, by = .(state) ]
	dt_reg_short[, c(glue("df{h}y_log_emp_SL"))  := get(glue("f{h}y_log_emp_SL")) - log_emp_SL, by = .(state) ]
	dt_reg_short[, c(glue("df{h}y_log_emp_S"))   := get(glue("f{h}y_log_emp_S")) - log_emp_S, by = .(state) ]
	dt_reg_short[, c(glue("df{h}y_log_emp_L"))   := get(glue("f{h}y_log_emp_L")) - log_emp_L, by = .(state) ]
}

# LAGG VARIABLES
dt_reg_short[, l1y_INCMTAX_revshare_S  := tlag(INCMTAX_revshare_S,  1L, time=date_y), by = .(state) ]
dt_reg_short[, l1y_CORPTAX_revshare_S  := tlag(CORPTAX_revshare_S,  1L, time=date_y), by = .(state) ]
dt_reg_short[, l1y_SALESTAX_revshare_S := tlag(SALESTAX_revshare_S, 1L, time=date_y), by = .(state) ]
dt_reg_short[, l1y_SALESTAX_revshare_L := tlag(SALESTAX_revshare_L, 1L, time=date_y), by = .(state) ]
dt_reg_short[, l1y_PROPTAX_revshare_L  := tlag(PROPTAX_revshare_L, 1L, time=date_y), by = .(state) ]

# LAG AND MEASURE OF DEFSHOCK
dt_reg_short[, l1y_log_TAX_revenue_SL   := tlag(log_TAX_revenue_SL,  1L, time=date_y), by = .(state) ]
dt_reg_short[, l1y_log_TAX_revenue_S    := tlag(log_TAX_revenue_S,   1L, time=date_y), by = .(state) ]
dt_reg_short[, l1y_log_TAX_revenue_L    := tlag(log_TAX_revenue_L,   1L, time=date_y), by = .(state) ]
dt_reg_short[, dl1y_log_TAX_revenue_SL  := log_TAX_revenue_SL - get(glue("l1y_log_TAX_revenue_SL")), by = .(state) ]
dt_reg_short[, dl1y_log_TAX_revenue_S   := log_TAX_revenue_S  - get(glue("l1y_log_TAX_revenue_SL")), by = .(state) ]
dt_reg_short[, dl1y_log_TAX_revenue_L   := log_TAX_revenue_L  - get(glue("l1y_log_TAX_revenue_SL")), by = .(state) ]

dt_reg_short[, f1y_log_TAX_revenue_SL   := tlead(log_TAX_revenue_SL, 1L, time=date_y), by = .(state) ]
dt_reg_short[, f1y_log_TAX_revenue_S    := tlead(log_TAX_revenue_S,  1L, time=date_y), by = .(state) ]
dt_reg_short[, f1y_log_TAX_revenue_L    := tlead(log_TAX_revenue_L,  1L, time=date_y), by = .(state) ]
dt_reg_short[, df1y_log_TAX_revenue_SL  := get(glue("f1y_log_TAX_revenue_SL")) - log_TAX_revenue_SL, by = .(state) ]
dt_reg_short[, df1y_log_TAX_revenue_S   := get(glue("f1y_log_TAX_revenue_SL")) - log_TAX_revenue_S, by = .(state) ]
dt_reg_short[, df1y_log_TAX_revenue_L   := get(glue("f1y_log_TAX_revenue_SL")) - log_TAX_revenue_L, by = .(state) ]


dt_reg_short[, sign_tax_shock_SL := as.factor(sign(log_TAX_revenue_SL-l1y_log_TAX_revenue_SL)) ]
dt_reg_short[, sign_tax_shock_S  := as.factor(sign(log_TAX_revenue_S-l1y_log_TAX_revenue_S)) ]
dt_reg_short[, sign_tax_shock_L  := as.factor(sign(log_TAX_revenue_L-l1y_log_TAX_revenue_L)) ]
dt_reg_short[, sign_tax_shock_SL := as.factor(sign(f1y_log_TAX_revenue_SL-log_TAX_revenue_SL)) ]
dt_reg_short[, sign_tax_shock_S  := as.factor(sign(f1y_log_TAX_revenue_S-log_TAX_revenue_S)) ]
dt_reg_short[, sign_tax_shock_L  := as.factor(sign(f1y_log_TAX_revenue_L-log_TAX_revenue_L)) ]

dt_reg_short[, time_trend := as.integer(date_y) ]
dt_reg_short[, `:=`(q_rdf_frac = xtile(rdf_frac, 3)) ]
# dt_reg_short[, `:=`(q_rdf_frac = xtile(rdf_frac, 3)), by = .(date_y) ]
dt_reg_short[, q_rdf_frac := as.factor(q_rdf_frac) ]
dt_reg_short[, q_rdf_frac := relevel(q_rdf_frac, ref = "3") ]

# DEALING WITH DEF SHOCK
# dt_reg_short[, log_def_shock := fifelse(def_shock>=0, 0, log(1-def_shock))]

# SAVE IT FOR DAN
dt_reg_short <- dt_reg_short[, .(state, date_y, time_trend, rdf_frac, q_rdf_frac,
	log_emp_ces, log_emp_SL, log_emp_S, log_emp_L, f1y_log_emp_ces, f1y_log_emp_SL, f1y_log_emp_S, f1y_log_emp_L,
	SALESTAX_revenue_SL, SALESTAX_revenue_S, SALESTAX_revenue_L, INCMTAX_revenue_SL, INCMTAX_revenue_S,
    CORPTAX_revenue_SL, CORPTAX_revenue_S, PROPTAX_revenue_SL, PROPTAX_revenue_S,
	log_TAX_revenue_SL, log_TAX_revenue_S, log_TAX_revenue_L,
	IG_fed_revenue_SL, IG_fed_revenue_S, IG_fed_revenue_L,
	INCMTAX_revshare_S, CORPTAX_revshare_S, SALESTAX_revshare_S, SALESTAX_revshare_L, PROPTAX_revshare_L,
	l1y_INCMTAX_revshare_S, l1y_CORPTAX_revshare_S, l1y_SALESTAX_revshare_S, l1y_SALESTAX_revshare_L, l1y_PROPTAX_revshare_L) ]
dt_reg_short <- dt_reg_short[ is.finite(f1y_log_emp_SL) ]
dt_reg_short[]

# NOW ESTIMATE AGGREGATE TAX CHANGES
dt_tax_us <- dt_sl_us[ state == "US" ]
dt_tax_us <- dcast(dt_tax_us, date_y + state ~ variable + lvl_code, value.var = "value")
setnames(dt_tax_us, gsub("_1", "_SL", colnames(dt_tax_us)) )
setnames(dt_tax_us, gsub("_2", "_S",  colnames(dt_tax_us)) )
setnames(dt_tax_us, gsub("_3", "_L",  colnames(dt_tax_us)) )
dt_tax_us <- dt_tax_us[, .(date_y, INCMTAX_revenue_S, CORPTAX_revenue_S, SALESTAX_revenue_S, SALESTAX_revenue_L, PROPTAX_revenue_L)]
tax_names <- colnames(dt_tax_us)[2:(ncol(dt_tax_us))]
# dt_tax_us[, c(glue("l1y_{tax_names}")) := lapply(.SD, function(x) tlag(x, 1L, date_y)), .SDcols=tax_names ]
# dt_tax_us[, c(glue("d1y_{tax_names}")) := lapply(.SD, function(x) log(x / tlag(x, 1L, date_y)) ), .SDcols=tax_names ]
# dt_tax_us <- dt_tax_us[, c("date_y", glue("d1y_{tax_names}")), with=F ]
setnames(dt_tax_us, c("date_y", glue("{tax_names}_US_AGG")) )
dt_tax_us[]

dt_reg_short <- merge(dt_reg_short, dt_tax_us, by = c("date_y"), all.x = TRUE)
setnames(dt_reg_short, "date_y", "year")

write_dta(dt_reg_short, "../derived/reg_data_annual.dta")
# ------------------------------------------------------------------------------------------




