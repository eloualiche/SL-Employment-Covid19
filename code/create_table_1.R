#! /usr/bin/env R
#
#
# (c) Daniel Green and Erik Loualiche
# 
# Replication code for _State and Local Government Employment in the COVID-19 Crisis_
# Journal of Public Economics 
#
# This file creates our main summary statistics table
#
# --------------------------------------------------------------------------------------



# ------------------------------------------------------------------------------------------
library(devtools)
library(crayon)

library(brew)
library(readxl)
library(haven)
library(fst)
library(glue)
library(stringr)
library(data.table)
library(statar)
# ------------------------------------------------------------------------------------------


# define summary function
# ------------------------------------------------------------------------------------------
my_summary <- function(x) list(mean = mean(x, na.rm=T), min = min(x, na.rm=T),
                               max = max(x, na.rm=T), sd(x, na.rm=T), p25 = quantile(x, 0.25, na.rm=T), p75 = quantile(x, 0.75, na.rm=T))
# ------------------------------------------------------------------------------------------


# import state ids
# ------------------------------------------------------------------------------------------
dt_fips_state <- fread("../data/fips_state.csv")
setnames(dt_fips_state, c("name", "fips", "state"))
dt_pop_raw <- haven::read_dta("../data/state_pop.dta") %>% data.table
dt_pop_raw <- merge(dt_pop_raw[, .(year, fips=as.integer(fips), pop)], dt_fips_state, by = "fips")
dt_pop <- dt_pop_raw[year==2019, .(state, pop, fips, state_name=name) ]
# ------------------------------------------------------------------------------------------



# ------------------------------------------------------------------------------------------
## STATE and LOCAL GOVERNMENTS: TAXES
dt_munis <- read_fst("../derived/CensusFin_statelocalgov_taxes.fst", as.data.table=T)
dt_munis <- dt_munis[ type %in% c(0, 1, 2, 3) ] # 0 STATE;  1 county; 2 municipal, and 3 township governments
setorder(dt_munis, date_y, type, state, fips, fips_county, name, gov_id)
dt_munis[, gov_type := fifelse( type == 0, "state", "local") ]
dt_munis <- dt_munis[, .(amount = sum(amount, na.rm=T), TOT_agg_rev, TOT_agg_exp),
  by = .(date_y, gov_type, state, fips, type, item_code) ] %>% unique

# aggregate expenditure etc... local or not:
dt_agg_tmp <- dt_munis[, .(date_y, gov_type, state, type, TOT_agg_rev, TOT_agg_exp) ] %>% unique
dt_agg_tmp <- dt_agg_tmp[, .(TOT_agg_rev=sum(TOT_agg_rev, na.rm=T), TOT_agg_exp=sum(TOT_agg_exp, na.rm=T) ),
  by = .(date_y, gov_type, state) ]
dt_agg_tmp <- rbind(dt_agg_tmp,
                    dt_agg_tmp[, .(gov_type="statelocal", TOT_agg_rev=sum(TOT_agg_rev, na.rm=T),
                                   TOT_agg_exp=sum(TOT_agg_exp, na.rm=T)), by = .(date_y, state) ] )
dt_munis <- dt_munis[, .(amount = sum(amount, na.rm=T) ),
    by = .(date_y, gov_type, state, fips, item_code) ] %>% unique

# TOTAL TAX
dt_agg_tax <- dt_munis[, .(amount = sum(amount, na.rm=T), item_code="TOT"), by = .(date_y, fips, gov_type, state) ]
dt_munis <- rbind(dt_munis, dt_agg_tax)
dt_munis <- merge(dt_munis, dt_agg_tax[, .(total_tax=amount, date_y, gov_type, state) ], by = c("date_y", "gov_type", "state") )

# AGGREGATE STATE AND LOCAL
dt_agg_sl <- dt_munis[, .(amount=sum(amount, na.rm=T), total_tax=sum(total_tax, na.rm=T), gov_type = "statelocal"),
         by = .(date_y, state, fips, item_code) ]
dt_munis <- rbind(dt_munis, dt_agg_sl)
dt_munis <- merge(dt_munis, dt_agg_tmp, by = c("date_y", "gov_type", "state"))

# TAX SHARE ...
dt_munis[, tax_share := amount / total_tax ]
dt_munis[, tax_rev_share := amount / TOT_agg_rev ]

# SUMMARY STATS
dt_st_tax1 <- copy(dt_munis)
# definition of Sales and gross receipts
dt_st_tax1[item_code %in% c("T09", "T10", "T12", "T13", "T14", "T15", "T16", "T19"), item_code := "SAL" ]
dt_st_tax1 <- dt_st_tax1[ item_code %in% c("T01", "SAL") & date_y >= 2017 ]
# AGGREGATE SALES
dt_st_tax1 <- dt_st_tax1[, .(amount=sum(amount, na.rm=T),
  tax_share=sum(tax_share, na.rm=T), tax_rev_share=sum(tax_rev_share, na.rm=T) ),
  by = .(date_y, state, fips, item_code, gov_type) ] %>% unique
dt_st_tax1 <- dt_st_tax1[, .(tax_rev_share = mean(tax_rev_share)), by = .(state, gov_type, item_code) ]
dt_st_tax1 <- dcast(dt_st_tax1, state + gov_type ~ item_code, value.var=c("tax_rev_share"))
dt_st_tax1[, `:=`(T01 = fifelse(is.na(T01), 0, T01), SAL = fifelse(is.na(SAL), 0, SAL) ) ]
dt_st_tax1 <- dt_st_tax1[, .(unlist(lapply(.SD, my_summary)), rep(c("avg", "min", "max", "cs", "p25", "p75"), 2) ),
  .SDcols=c("T01", "SAL"), by = .(gov_type) ]
dt_st_tax1[, var_name := rep(CJ(c("T01", "SAL"), seq(1,6), sorted=F)[["V1"]], 3) ]
dt_st_tax1 <- dcast(dt_st_tax1, var_name + gov_type ~ V2, value.var = "V1")
dt_st_tax1[]

dt_st_tax2 <- dt_munis[ date_y >= 2017, .(date_y, gov_type, state, TOT_agg_rev=TOT_agg_rev/1E6) ] %>% unique  # revenue
dt_st_tax2 <- dt_st_tax2[, .(unlist(lapply(.SD, my_summary)), rep(c("avg", "min", "max", "cs", "p25", "p75"),1) ),
  .SDcols=c("TOT_agg_rev"), by = .(gov_type) ]
dt_st_tax2 <- dcast(dt_st_tax2, gov_type ~ V2, value.var = "V1")
dt_st_tax2[]

dt_st_tax <- rbind(dt_st_tax1[, var_name:= CJ(c("prop_tax_share", "sales_tax_share"), seq(1,3))[["V1"]]],
                   dt_st_tax2[, var_name:=c("tot_tax")], fill=T)
dt_st_tax[]
# ------------------------------------------------------------------------------------------


# import derived combined data
# ------------------------------------------------------------------------------------------
# hopefully this is temporary and will be rearrange for next version of the data.
dt_sl_tax <- read_dta("../derived/muni_covid_combined_data_cps.dta") %>% data.table
dt_sl_tax[, date := as.Date(ISOdate(year, month, 1)) ]
dt_sl_tax <- dt_sl_tax[ date == as.Date("2020-04-01") ]

dt_sl_tax <- dt_sl_tax[, .(state=state_abbrv, state_name=state,
                           TotalRevenue_State=TotalRevenue_State/1E6, TotalRevenue_StateLocal=TotalRevenue_StateLocal/1E6,
                           sales_tax_share_rev_State=100*sales_tax_share_rev_State, sales_tax_share_rev_StateLocal=100*sales_tax_share_rev_StateLocal) ]
dt_sl_tax <- unique(dt_sl_tax)

var_summary <- c("TotalRevenue_State", "TotalRevenue_StateLocal", "sales_tax_share_rev_State", "sales_tax_share_rev_StateLocal")
dt_sl_tax <- dt_sl_tax[, .(unlist(lapply(.SD, my_summary)), c("avg", "min", "max", "cs", "p25", "p75") ),
                       .SDcols=var_summary ]
dt_sl_tax[, var_name := CJ(var_summary, seq(1,6), sorted=F)[["var_summary"]] ]
dt_sl_tax <- dcast(dt_sl_tax, var_name ~ V2, value.var = "V1")
dt_sl_tax[]

dt1_st <- dt_sl_tax[ grep("_State$", var_name) ]
dt1_st[, var_name := c("tot_rev", "sales_tax_share") ]
dt1_sl <- dt_sl_tax[ grep("Local$", var_name) ]
dt1_sl[, var_name := c("tot_rev", "sales_tax_share") ]
# ------------------------------------------------------------------------------------------


# import rainyday data
# ------------------------------------------------------------------------------------------
## STATE GOVERNMENTS: RAINY DAY FUNDS

dt_rdf1 <- read_excel(glue("../data/NASBO/RainyDayFunds2009-2021.xlsx"), skip=1, sheet="Total Amount") %>% data.table
dt_rdf2 <- read_excel(glue("../data/NASBO/RainyDayFunds2009-2021.xlsx"), skip=1, sheet="Percent") %>% data.table
col_list1 <- glue("rdf_balance_{seq(2009, 2021)}"); col_list2 <- glue("rdf_frac_{seq(2009, 2021)}")
setnames(dt_rdf1, c("state", col_list1))
setnames(dt_rdf2, c("state", col_list2))
dt_rdf1[, c(col_list1) := lapply(.SD, as.numeric), .SDcols = col_list1 ]
dt_rdf1 <- dt_rdf1[ 1:50 ]
dt_rdf2[, c(col_list2) := lapply(.SD, as.numeric), .SDcols = col_list2 ]
dt_rdf2 <- dt_rdf2[ 1:50 ]

dt_rdf1 <- melt(dt_rdf1, id = "state")
dt_rdf1 <- dt_rdf1[, .(state=gsub("\\*", "", state), date_y=as.integer(str_sub(variable, -4,-1)),
  var_type = tstrsplit(variable, "_")[2], rdf_value=value), by = seq(1, nrow(dt_rdf1))]
dt_rdf2 <- melt(dt_rdf2, id = "state")
dt_rdf2 <- dt_rdf2[, .(state=gsub("\\*", "", state), date_y=as.integer(str_sub(variable, -4,-1)),
  var_type = tstrsplit(variable, "_")[2], rdf_value=value), by = seq(1, nrow(dt_rdf1))]
dt_rdf2[, state := gsub(",", " ", state) ]
dt_rdf <- rbind(dt_rdf1, dt_rdf2)[, seq := NULL]
dt_rdf[, var_name := as.character(var_type) ]
dt_rdf <- dt_rdf[ date_y == 2019 ]

dt_st_rdf <- dt_rdf[, .(unlist(lapply(.SD, my_summary)), c("avg", "min", "max", "cs", "p25", "p75") ),
  .SDcols=c("rdf_value"), by = .(var_name) ]
dt_st_rdf <- dcast(dt_st_rdf, var_name ~ V2, value.var = "V1")
dt_st_rdf[]
# ------------------------------------------------------------------------------------------


# import derived combined data
# ------------------------------------------------------------------------------------------
dt_laidoff <- read_dta("../derived/muni_covid_combined_data_cps.dta") %>% data.table
dt_laidoff <- dt_laidoff[ year >= 2019 ]
names(dt_laidoff)
dt_laidoff <- dt_laidoff[, .(state=state_abbrv, state_name=state, 
    year, month, datem,
    laidoff_statelocal, employed_statelocal, unemployed_statelocal,
    laidoff_state, employed_state, unemployed_state,
    laidoff_federal, employed_federal, unemployed_federal) ]
dt_laidoff[, `:=`(laidoff_frac_statelocal = (laidoff_statelocal)/(employed_statelocal+unemployed_statelocal),
                  laidoff_frac_state      = (laidoff_state)/(employed_state+unemployed_state),
                  laidoff_frac_federal    = (laidoff_federal)/(employed_federal+unemployed_federal) ) ]
dt_laidoff[]

# you can look in analysis_munisCovid.do for the regressions
dt_reg <- read_dta("../derived/muni_covid_combined_data_cps.dta") %>% data.table
dt_reg <- dt_reg[ year >= 2019 ]
dt_reg[, date := as.Date(ISOdate(year, month, 1)) ]

dt_reg1 <- dt_reg[ date == as.Date("2020-04-01") , .(state,
    d_muni_laidoff=100*s2_laidoff_frac_statelocal, d_muni_parttime=100*s2_part_time_covid_statelocal,
    d_muni_urate=100*s2_urate_statelocal, d_muni_hlth_laidoff=100*s2_laidoff_frac_statelocal_hlth,
    d_state_laidoff=100*s2_laidoff_frac_state, d_state_hlth_laidoff=100*s2_laidoff_frac_state_hlth,
    d_federal_laidoff=100*s2_laidoff_frac_federal,
    cares_frac_rev_sl=covid_relief_treasury/TotalRevenue_StateLocal*100,
    cares_frac_rev_st=covid_relief_treasury/TotalRevenue_State*100 ) ]

dt_reg2 <- dt_reg[ date == as.Date("2020-05-01") ,  .(state,
  d_may_muni_laidoff=100*s3_laidoff_frac_statelocal, d_may_state_laidoff=100*s3_laidoff_frac_state) ]
dt_reg3 <- dt_reg[ date == as.Date("2020-06-01") ,  .(state,
  d_june_muni_laidoff=100*s4_laidoff_frac_statelocal, d_june_state_laidoff=100*s4_laidoff_frac_state) ]

dt_st_reg <- merge(dt_reg1, dt_reg2, by = c("state"))
dt_st_reg <- merge(dt_st_reg, dt_reg3, by = c("state"))
dt_st_reg[]

# April 2020
var_summary <- c("d_muni_laidoff", "d_may_muni_laidoff", "d_june_muni_laidoff", "d_muni_hlth_laidoff",
  "d_muni_parttime", "d_muni_urate",
  "d_state_laidoff", "d_may_state_laidoff", "d_june_state_laidoff", "d_state_hlth_laidoff",
  "d_federal_laidoff", "cares_frac_rev_sl", "cares_frac_rev_st")
dt_st_reg <- dt_st_reg[, .(unlist(lapply(.SD, my_summary)), c("avg", "min", "max", "cs", "p25", "p75") ),
  .SDcols=var_summary ]
dt_st_reg[, var_name := CJ(var_summary, seq(1,6), sorted=F)[["var_summary"]] ]
dt_st_reg <- dcast(dt_st_reg, var_name ~ V2, value.var = "V1")
dt_st_reg[]
# ------------------------------------------------------------------------------------------


# ------------------------------------------------------------------------------------------
brew("../data/tables_to_brew/summary_SL_covid.brew.tex", "../output/tables/summary_SL_covid.tex")
# ------------------------------------------------------------------------------------------
