#! /usr/bin/env R
#
#
# (c) Daniel Green and Erik Loualiche
# 
# Replication code for _State and Local Government Employment in the COVID-19 Crisis_
# Journal of Public Economics 
#
# This file builds Appendix Table 2
# Tax Revenues by Population across States
#
# --------------------------------------------------------------------------------------


# ------------------------------------------------------------------------------------------
library(devtools)
library(crayon)

# normal stuff
library(glue)
library(stargazer)
library(lubridate)
library(stringr)
library(readxl)
library(haven)
library(brew)
library(fst)
library(Hmisc)
library(mFilter) # hp filters
library(readr)
library(dplyr)
library(data.table)
library(statar)
# ------------------------------------------------------------------------------------------


# --------------------------------------------------------------------------------
# Other data
dt_fips_state <- fread("../data/fips_state.csv")
setnames(dt_fips_state, c("name", "fips", "state"))
dt_fips_state[]

dt_pop_raw <- haven::read_dta("../data/state_pop.dta") %>% data.table
dt_pop_raw <- merge(dt_pop_raw[, .(year, fips=as.integer(fips), pop)], dt_fips_state, by = "fips")
dt_pop <- dt_pop_raw[year==2019, .(state, pop, fips, state_name=name) ]
dt_pop[]  # population is in #

# --------------------------------------------------------------------------------

my_summary <- function(x) list(mean = mean(x, na.rm=T), min = min(x, na.rm=T),
                               max = max(x, na.rm=T), sd(x, na.rm=T), p25 = quantile(x, 0.25, na.rm=T), p75 = quantile(x, 0.75, na.rm=T))

# --------------------------------
# SECOND TABLE: FIRST PANEL STATE GOVERNMENTS: QTAX
dt_munis <- read_fst("../derived/CensusFin_statelocalgov_taxes.fst", as.data.table=T)
dt_munis <- dt_munis[ type %in% c(0) ] # 0 - state
setorder(dt_munis, date_y, type, state, fips, fips_county, name, gov_id)
dt_munis[, total_tax := sum(amount, na.rm=T), by = .(date_y, type, state, fips, fips_county, name, gov_id) ]
dt_munis <- rbind(dt_munis, fill = TRUE,
                  unique(dt_munis[, .(date_y, state, fips, fips_county, type, name, gov_id, amount=total_tax, total_tax, item_code="TOT")])  )
dt_munis[, tax_share := amount / total_tax ]
dt_munis[]

# do the time series .... this might be a little sketchy because of sampling so we aggregate at state level first
dt1 <- dt_munis[, .(tax=sum(amount, na.rm=T)), by = .(state, item_code, date_y) ]
dt1 <- merge(dt1, dt_pop_raw[, .(date_y=year, pop, state)], all.x = T, by = c("date_y", "state"))
dt1[, tax_pop := tax / pop ]
setorder(dt1, state, item_code, date_y)
dt1[, d1_tax_pop := (tax_pop - tlag(tax_pop, time=date_y))/tlag(tax_pop, time=date_y), by=.(state, item_code)]
dt1 <- dt1[, lapply(.SD, function(x) sd(x, na.rm=T)), .SDcols=c("d1_tax_pop"), by=.(state, item_code) ]
dt1 <- dcast(dt1, state ~ item_code, value.var=c("d1_tax_pop"))
dt1[, c("T01", "T09", "T40", "T41", "T40-41", "TOT") := lapply(.SD, winsorize), .SDcols=c("T01", "T09", "T40", "T41", "T40-41", "TOT") ]
dt1 <- dt1[, .(unlist(lapply(.SD, my_summary)), c("avg", "min", "max", "cs", "p25", "p75") ), .SDcols=c("T01", "T09", "T40", "T41", "T40-41", "TOT") ]
dt1[, tax_name := CJ(c("T01", "T09", "T40", "T41", "T40-41", "TOT"), seq(1,6), ordered=F)[["V1"]] ]
dt1 <- dcast(dt1, tax_name ~ V2, value.var = "V1")
dt1[]


# do share
dt2 <- dt_munis[, .(tax=sum(amount, na.rm=T), total_tax=sum(total_tax, na.rm=T)), by = .(state, item_code, date_y) ]
dt2 <- dt2[ item_code %in% c("T01", "T09", "T40", "T41", "T40-41") & date_y >= 1980 ]
dt2[, tax_share := tax / total_tax ]
dt2 <- dt2[, .(tax_share = mean(tax_share)), by = .(state, item_code) ]
dt2 <- dcast(dt2, state ~ item_code, value.var=c("tax_share"))
dt2 <- dt2[, .(unlist(lapply(.SD, my_summary)), c("avg", "min", "max", "cs", "p25", "p75") ), .SDcols=c("T01", "T09", "T40", "T41", "T40-41") ]
dt2[, tax_name := CJ(c("T01", "T09", "T40", "T41", "T40-41"), seq(1,6), ordered=T)[["V1"]] ]
dt2 <- dcast(dt2, tax_name ~ V2, value.var = "V1")
dt2[]

#  --------------------------------
# SECOND TABLE: SECOND PANEL LOCAL GOVERNMENTS
dt_munis <- read_fst("../derived/CensusFin_statelocalgov_taxes.fst", as.data.table=T)
dt_munis <- dt_munis[ type %in% c(1,2,3) ] # 1 county; 2 municipal, and 3 township governments
setorder(dt_munis, date_y, type, state, fips, fips_county, name, gov_id)
dt_munis[, total_tax := sum(amount, na.rm=T), by = .(date_y, type, state, fips, fips_county, name, gov_id) ]
dt_munis <- rbind(dt_munis, fill = TRUE,
                  unique(dt_munis[, .(date_y, state, fips, fips_county, type, name, gov_id, amount=total_tax, total_tax, item_code="TOT")])  )
dt_munis[, tax_share := amount / total_tax ]
dt_munis[]

# do the time series .... this might be a little sketchy because of sampling so we aggregate at state level first
dt3 <- dt_munis[, .(tax=sum(amount, na.rm=T)), by = .(state, item_code, date_y) ]
dt3 <- merge(dt3, dt_pop_raw[, .(date_y=year, pop, state)], all.x = T, by = c("date_y", "state"))
dt3[, tax_pop := tax / pop ]
setorder(dt3, state, item_code, date_y)
dt3[, d1_tax_pop := (tax_pop - tlag(tax_pop, time=date_y))/tlag(tax_pop, time=date_y), by=.(state, item_code)]
dt3 <- dt3[, lapply(.SD, function(x) sd(x, na.rm=T)), .SDcols=c("d1_tax_pop"), by=.(state, item_code) ]
dt3 <- dcast(dt3, state ~ item_code, value.var=c("d1_tax_pop"))
dt3[, c("T01", "T09", "TOT") := lapply(.SD, winsorize), .SDcols=c("T01", "T09", "TOT") ]
dt3 <- dt3[, .(unlist(lapply(.SD, my_summary)), c("avg", "min", "max", "cs", "p25", "p75") ), .SDcols=c("T01", "T09", "TOT") ]
dt3[, tax_name := CJ(c("T01", "T09", "TOT"), seq(1,6))[["V1"]] ]
dt3 <- dcast(dt3, tax_name ~ V2, value.var = "V1")
dt3[]

# do share
dt4 <- dt_munis[, .(tax=sum(amount, na.rm=T), total_tax=sum(total_tax, na.rm=T)), by = .(state, item_code, date_y) ]
dt4 <- dt4[ item_code %in% c("T01", "T09") & date_y >= 1980 ]
dt4[, tax_share := tax / total_tax ]
dt4 <- dt4[, .(tax_share = mean(tax_share)), by = .(state, item_code) ]
dt4 <- dcast(dt4, state ~ item_code, value.var=c("tax_share"))
dt4 <- dt4[, .(unlist(lapply(.SD, my_summary)), c("avg", "min", "max", "cs", "p25", "p75") ), .SDcols=c("T01", "T09") ]
dt4[, tax_name := CJ(c("T01", "T09"), seq(1,6))[["V1"]] ]
dt4 <- dcast(dt4, tax_name ~ V2, value.var = "V1")
dt4[]

#  --------------------------------
# SECOND TABLE: THIRD PANEL STATE & LOCAL GOVERNMENTS
dt_sl <- read_fst("../derived/CensusFin_statelocalgov_taxes.fst", as.data.table=T)
dt_sl <- dt_sl[ type %in% c(0,1,2,3,4,5) ]
setorder(dt_sl, date_y, type, state, fips, fips_county, name, gov_id)
dt_sl[, total_tax := sum(amount, na.rm=T), by = .(date_y, type, state, fips, fips_county, name, gov_id) ]
dt_sl <- rbind(dt_sl, fill = TRUE,
               unique(dt_sl[, .(date_y, state, fips, fips_county, type, name, gov_id, amount=total_tax, total_tax, item_code="TOT")])  )
dt_sl[, tax_share := amount / total_tax ]
dt_sl[]

# do the time series .... this might be a little sketchy because of sampling so we aggregate at state level first
dt5 <- dt_sl[, .(tax=sum(amount, na.rm=T)), by = .(state, item_code, date_y) ]
dt5 <- merge(dt5, dt_pop_raw[, .(date_y=year, pop, state)], all.x = T, by = c("date_y", "state"))
dt5[, tax_pop := tax / pop ]
setorder(dt5, state, item_code, date_y)
dt5[, d1_tax_pop := (tax_pop - tlag(tax_pop, time=date_y))/tlag(tax_pop, time=date_y), by=.(state, item_code)]
dt5 <- dt5[, lapply(.SD, function(x) sd(x, na.rm=T)), .SDcols=c("d1_tax_pop"), by=.(state, item_code) ]
dt5 <- dcast(dt5, state ~ item_code, value.var=c("d1_tax_pop"))
dt5[, c("T01", "T09", "T40", "T41", "T40-41", "TOT") := lapply(.SD, winsorize), .SDcols=c("T01", "T09", "T40", "T41", "T40-41", "TOT") ]
dt5 <- dt5[, .(unlist(lapply(.SD, my_summary)), c("avg", "min", "max", "cs", "p25", "p75") ), .SDcols=c("T01", "T09", "T40", "T41", "T40-41", "TOT") ]
dt5[, tax_name := CJ(c("T01", "T09", "T40", "T41", "T40-41", "TOT"), seq(1,6))[["V1"]] ]
dt5 <- dcast(dt5, tax_name ~ V2, value.var = "V1")
dt5[]

# do share
dt6 <- dt_sl[, .(tax=sum(amount, na.rm=T), total_tax=sum(total_tax, na.rm=T)), by = .(state, item_code, date_y) ]
dt6 <- dt6[ item_code %in% c("T01", "T09", "T40", "T41", "T40-41", "TOT") & date_y >= 1980 ]
dt6[, tax_share := tax / total_tax ]
dt6 <- dt6[, .(tax_share = mean(tax_share)), by = .(state, item_code) ]
dt6 <- dcast(dt6, state ~ item_code, value.var=c("tax_share"))
dt6 <- dt6[, .(unlist(lapply(.SD, my_summary)), c("avg", "min", "max", "cs", "p25", "p75") ), .SDcols=c("T01", "T09", "T40", "T41", "T40-41", "TOT") ]
dt6[, tax_name := CJ(c("T01", "T09", "T40", "T41", "T40-41", "TOT"), seq(1,6))[["V1"]] ]
dt6 <- dcast(dt6, tax_name ~ V2, value.var = "V1")
dt6[]

brew("../data/tables_to_brew/summary_qtax3.brew.tex", "../output/appendix/tables/summary_qtax3.tex")
# --------------------------------------------------------------------------------
