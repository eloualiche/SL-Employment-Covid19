#! /usr/bin/env R
#
#
# (c) Daniel Green and Erik Loualiche
# 
# Replication code for _State and Local Government Employment in the COVID-19 Crisis_
# Journal of Public Economics 
#
# This file builds Appendix Table 8
#
# --------------------------------------------------------------------------------------


# ------------------------------------------------------------------------------------------
library(devtools)
library(crayon)
library(glue)
library(stringr)
library(lubridate)
library(haven)
library(fredr)
library(fst)
library(lfe)
library(brew)
library(stargazer)
library(data.table)
library(statar)
# ------------------------------------------------------------------------------------------


# ------------------------------------------------------------------------------------------
# FIPS CODES
fips <- fread("../data/fips_state.csv")
setnames(fips, c("state_name", "statefip", "state"))

# BLS LABOR FORCE
dt_blf <- read_dta("../derived/BLS_laborforce_size.dta") %>% data.table
dt_blf <- dt_blf[ seasonality == "S", .(datem=as.monthly(date), state_name=state, civil_pop, lforce = laborforce_total) ]
dt_blf <- merge(dt_blf, fips[, .(statefip, state_name)], by = c("state_name"), all.x = T)

# CES LAU DATA
dt_lau_agg <- read_fst("../derived/lau_aggregates.fst", as.data.table=T)

# GDP DEFLATOR
dt_gdpdef <- fread("../data/gdpdef.csv")
dt_gdpdef[, dateq := as.quarterly(dateq) ]
# ------------------------------------------------------------------------------------------


# ------------------------------------------------------------------------------------------
# PAYROLL DATA
dt_payroll_ASPEP <- read_dta("../derived/ASPEP_local_payroll_aggregates_1993_2018.dta") %>% data.table
dt_payroll_ASPEP[, state_code := NULL ]
dt_payroll_ASPEP <- dcast(dt_payroll_ASPEP, date_y + state ~ lvl_code, value.var=c("emp_full_time", "payroll_full_time"))
setnames(dt_payroll_ASPEP, gsub("_1", "_SL", colnames(dt_payroll_ASPEP) ) )
setnames(dt_payroll_ASPEP, gsub("_2", "_S", colnames(dt_payroll_ASPEP) ) )
setnames(dt_payroll_ASPEP, gsub("_3", "_L", colnames(dt_payroll_ASPEP) ) )
# ------------------------------------------------------------------------------------------


# ------------------------------------------------------------------------------------------
# ---------------
# STATE AND LOCAL AGGREGATE TABLES REVENUES AND TAXES
dt_sl_agg <- read_fst("../derived/SandL_aggregates.fst", as.data.table=T)
dt_sl_agg %>% tab(variable)
# - aggregate the data again in 3 categories: 1: state + local total / 2: state only /  3: local only
dt1 <- dt_sl_agg[, .(value = sum(value, na.rm=T)), by = .(date_y, state, variable) ]
dt2 <- dt_sl_agg[type==0, .(value = sum(value, na.rm=T)), by = .(date_y, state, variable) ]
dt3 <- dt_sl_agg[type!=0, .(value = sum(value, na.rm=T)), by = .(date_y, state, variable) ]
dt_sl_agg <- rbind(dt1[, lvl_code := 1], dt2[, lvl_code := 2], dt3[, lvl_code := 3] )
dt_sl_agg <- dt_sl_agg[ variable %in% c("GAL_revenue", "IG_fed_revenue", "TAX_revenue",
                                        "PROPTAX_revenue", "SALESTAX_revenue", "INCMTAX_revenue", "CORPTAX_revenue", "OTHTAX_revenue") ]
# dt_sl_agg %>% tab(variable)

dt_sl_agg_wide <- dcast(dt_sl_agg, date_y + state ~ variable + lvl_code, value.var = "value")
setnames(dt_sl_agg_wide, gsub("_1", "_SL", colnames(dt_sl_agg_wide)) )
setnames(dt_sl_agg_wide, gsub("_2", "_S",  colnames(dt_sl_agg_wide)) )
setnames(dt_sl_agg_wide, gsub("_3", "_L",  colnames(dt_sl_agg_wide)) )
# ------------------------------------------------------------------------------------------



# ------------------------------------------------------------------------------------------
# --- merge all of the datasets

dt_reg <- dt_lau_agg[, .(datem=as.monthly(datem), statefip, classwkr=as.character(classwkr), emp_ces)]
dt_reg[, dateq := as.quarterly(datem)]
dt_reg[, `:=`(date_y = year(datem) ) ]
# --- merge names
dt_reg <- merge(dt_reg, fips, by = c("statefip"), all.x=T)
# --- merge InterGovernmental Revenue
dt_reg <- merge(dt_reg, dt_sl_agg_wide, all.x = T, by = c("date_y", "state"))
# --- merge Rainy Day Funds

# --- merge payroll
dt_reg <- merge(dt_reg, dt_payroll_ASPEP, all.x = T, by = c("date_y", "state"))
# --- merge GDP deflator
dt_reg <- merge(dt_reg, dt_gdpdef, all.x=T, by = c("dateq"))
# clean up
dt_reg <- dt_reg[ !is.na(state) ]
# ------------------------------------------------------------------------------------------


# ------------------------------------------------------------------------------------------
# COMPUTE REGRESSION VARIABLES

# create time trend and factors
dt_reg[, classwkr := as.factor(classwkr) ]
dt_reg[, classwkr := relevel(classwkr, ref = "22") ]  # reference level of factor is private -> 22
dt_reg <- dt_reg[ classwkr %in% c(22, 25, 27, 28, 271, 281) ]

# ---
# ADJUST FOR INFLATION USING GDP DEFLATOR
gdpdef_adj = TRUE
if (gdpdef_adj == TRUE){
	dt_reg[, TAX_revenue_SL := TAX_revenue_SL * gdpdef ]
	dt_reg[, TAX_revenue_S  := TAX_revenue_S  * gdpdef ]
	dt_reg[, TAX_revenue_L  := TAX_revenue_L  * gdpdef ]
}
# ---

# New labor variable
dt_reg[, `:=`(log_emp_ces=log(emp_ces) ) ]
dt_reg[, `:=`(log_emp_SL=log(emp_full_time_SL), log_emp_S=log(emp_full_time_S), log_emp_L=log(emp_full_time_L) ) ]
dt_reg[, `:=`(log_TAX_revenue_SL=log(TAX_revenue_SL), log_TAX_revenue_S=log(TAX_revenue_S), log_TAX_revenue_L=log(TAX_revenue_L)) ]
 # ------------------------------------------------------------------------------------------



# ------------------------------------------------------------------------------------------
# ANNUAL REGRESSION
# --- make the dataset annual by removing superfluous variables
dt_reg_short <- dt_reg[, -c("classwkr", "emp_ces", "log_emp_ces", "gdpdef")] %>% unique

# remove quarterly taxes
dt_reg_short <- dt_reg_short[ !is.na(dateq) ] %>% unique
dt_reg_short <- dt_reg_short[ quarter(dateq) == 4 ]
dt_reg_short <- dt_reg_short[, -c("dateq", "datem") ] %>% unique
# dt_reg_short <- dt_reg_short[ is.finite(log_emp_SL) ]
dt_reg_short[]

# --- merge back civil pop for weights
dt_reg_short <- merge(dt_reg_short,
    dt_blf[ month(datem)==12, .(date_y=year(datem), state_name, civil_pop, lforce) ], all.x=T, by = c("date_y", "state_name"))
dt_reg_short <- merge(dt_reg_short,
  dt_lau_agg[ month(as.Date(date)) == 12 & classwkr == 22 , 
              .(date_y=year(as.monthly(datem)), state_name, log_emp_ces=log(emp_ces)) ],
  all.x=T, by = c("date_y", "state_name"))

# --- merge defshock
# LEAD LHS VARIABALE
for (h in seq(1,5)){
	dt_reg_short[, c(glue("f{h}y_log_emp_ces")) := tlead(log_emp_ces, h, time=date_y), by = .(state) ]
	dt_reg_short[, c(glue("f{h}y_log_emp_SL"))  := tlead(log_emp_SL,  h, time=date_y), by = .(state) ]
	dt_reg_short[, c(glue("f{h}y_log_emp_S"))   := tlead(log_emp_S,   h, time=date_y), by = .(state) ]
	dt_reg_short[, c(glue("f{h}y_log_emp_L"))   := tlead(log_emp_L,   h, time=date_y), by = .(state) ]
}

dt_reg_short[, time_trend := as.integer(date_y) ]
# ------------------------------------------------------------------------------------------


# ------------------------------------------------------------------------------------------
# ---------------------- MAIN TABLE
rh1_privemp_SL_T <- felm(f1y_log_emp_ces ~ log_TAX_revenue_SL + state:time_trend | state + date_y | 0 | date_y + state, dt_reg_short)
rh1_emp_SL_T     <- felm(f1y_log_emp_SL  ~ log_TAX_revenue_SL + state:time_trend | state + date_y | 0 | date_y + state, dt_reg_short)
rh1_emp_S_T      <- felm(f1y_log_emp_S   ~ log_TAX_revenue_S + state:time_trend | state + date_y | 0 | date_y + state, dt_reg_short)
rh1_emp_L_T      <- felm(f1y_log_emp_L   ~ log_TAX_revenue_L + state:time_trend | state + date_y | 0 | date_y + state, dt_reg_short)

# different horizons
rh2_privemp_SL_T <- felm(f2y_log_emp_ces ~ log_TAX_revenue_SL + state:time_trend | state + date_y | 0 | date_y + state, dt_reg_short)
rh2_emp_SL_T     <- felm(f2y_log_emp_SL  ~ log_TAX_revenue_SL + state:time_trend | state + date_y | 0 | date_y + state, dt_reg_short)
rh2_emp_S_T      <- felm(f2y_log_emp_S   ~ log_TAX_revenue_S + state:time_trend | state + date_y | 0 | date_y + state, dt_reg_short)
rh2_emp_L_T      <- felm(f2y_log_emp_L   ~ log_TAX_revenue_L + state:time_trend | state + date_y | 0 | date_y + state, dt_reg_short)

rh3_privemp_SL_T <- felm(f3y_log_emp_ces ~ log_TAX_revenue_SL + state:time_trend | state + date_y | 0 | date_y + state, dt_reg_short)
rh3_emp_SL_T     <- felm(f3y_log_emp_SL  ~ log_TAX_revenue_SL + state:time_trend | state + date_y | 0 | date_y + state, dt_reg_short)
rh3_emp_S_T      <- felm(f3y_log_emp_S   ~ log_TAX_revenue_S + state:time_trend | state + date_y | 0 | date_y + state, dt_reg_short)
rh3_emp_L_T      <- felm(f3y_log_emp_L   ~ log_TAX_revenue_L + state:time_trend | state + date_y | 0 | date_y + state, dt_reg_short)

rh4_privemp_SL_T <- felm(f4y_log_emp_ces ~ log_TAX_revenue_SL + state:time_trend | state + date_y | 0 | date_y + state, dt_reg_short)
rh4_emp_SL_T     <- felm(f4y_log_emp_SL  ~ log_TAX_revenue_SL + state:time_trend | state + date_y | 0 | date_y + state, dt_reg_short)
rh4_emp_S_T      <- felm(f4y_log_emp_S   ~ log_TAX_revenue_S + state:time_trend | state + date_y | 0 | date_y + state, dt_reg_short)
rh4_emp_L_T      <- felm(f4y_log_emp_L   ~ log_TAX_revenue_L + state:time_trend | state + date_y | 0 | date_y + state, dt_reg_short)

l_rh_private <- list(rh1_privemp_SL_T, rh2_privemp_SL_T, rh3_privemp_SL_T, rh4_privemp_SL_T)
l_rh_SL_emp  <- list(rh1_emp_SL_T, rh2_emp_SL_T, rh3_emp_SL_T, rh4_emp_SL_T)
l_rh_S_emp   <- list(rh1_emp_S_T, rh2_emp_S_T, rh3_emp_S_T, rh4_emp_S_T)
l_rh_L_emp   <- list(rh1_emp_L_T, rh2_emp_L_T, rh3_emp_L_T, rh4_emp_L_T)


stargazer(l_rh_private, omit="time_trend", omit.stat="ser", type="text")
stargazer(l_rh_SL_emp, omit="time_trend", omit.stat="ser", type="text")
stargazer(l_rh_S_emp, omit="time_trend", omit.stat="ser", type="text")
stargazer(l_rh_L_emp, omit="time_trend", omit.stat="ser", type="text")
# --------------------------------------------------------------------------------


# --------------------------------------------------------------------------------
star_builder <- function(b, se){

# values for 100 degrees of freedom two sided t
# 0.80    0.9     0.95    0.99   
# 1.29    1.66    1.98    2.63   
z <- abs(b/se)

  if ( z < 1.66 ){
    star <- ""
  } else if ( z < 1.98 ){
    star <- "*"
  } else if ( z < 2.63 ){
    star <- "**"
  } else if ( z >= 2.63){
    star <- "***"
  }

  return(star)

}


brew("../data/tables_to_brew/table_ols_publicemp_full_horizon.brew.tex", 
     "../output/appendix/tables/table_ols_publicemp_full_horizon.tex")
# ------------------------------------------------------------------------------------------

