# ------------------------------------------------------------------------------------------
library(devtools)
library(crayon)

# normal stuff
library(glue)
library(fst)
library(lubridate)
library(stringr)
library(readxl)
library(readr)
library(purrr)
library(haven)
library(data.table)
library(statar)
# ------------------------------------------------------------------------------------------


# ------------------------------------------------------------------------------------------
# create BLS_laborforce_size.dta
# ------------------------------------------------------------------------------------------
# Seasonally adjusted data
dt_sa <- read_fwf("../data/LAU/ststdsadata.txt",
	skip=19,
	fwf_cols(state     = c(1, 30),
		     civil_pop = c(32, 44),
	         laborforce_total = c(45, 58),
	         laborforce_frac  = c(63, 68),
			 employed_total   = c(70, 83),
	         employed_frac    = c(87, 92),
	         unemployed_total = c(96, 105),
	         unemployed_frac  = c(108, 114) ),
    col_types="cccccccc") %>% data.table


# Clean up
dt_sa <- dt_sa[ !is.na(state) ]
dt_sa[, state := gsub("\\.", "", state) ]

# figure out the dates
in_date_y <- rep(seq(1976, 2020), 12*53)
in_date_y <- in_date_y[order(in_date_y)][ 1:nrow(dt_sa) ]
dt_sa[, date_y := in_date_y ]

y_span <- max(in_date_y) - min(in_date_y) + 1
in_month <- seq(1,12) %>% map(~ rep(as.numeric(.x), 53)) %>% unlist
in_month <- rep(in_month, y_span)
in_month <- in_month[ 1:nrow(dt_sa) ]
dt_sa[, date_month := in_month ]
dt_sa[, date := as.Date(ISOdate(date_y, date_month, 1)) ]
dt_sa[, date_month := NULL ]

# Non seasonally adjusted
dt_nsa <- read_fwf("../data/LAU/ststdnsadata.txt",
	skip=19,
	fwf_cols(state     = c(1, 30),
		     civil_pop = c(32, 44),
	         laborforce_total = c(45, 58),
	         laborforce_frac  = c(63, 68),
			 employed_total   = c(70, 83),
	         employed_frac    = c(87, 92),
	         unemployed_total = c(96, 105),
	         unemployed_frac  = c(108, 114) ),
    col_types="cccccccc") %>% data.table

# Clean up
dt_nsa <- dt_nsa[ !is.na(state) ]
dt_nsa[, state := gsub("\\.", "", state) ]

# figure out the dates
in_date_y <- rep(seq(1976, 2020), 12*53)
in_date_y <- in_date_y[order(in_date_y)][ 1:nrow(dt_nsa) ]
dt_nsa[, date_y := in_date_y ]

y_span <- max(in_date_y) - min(in_date_y) + 1
in_month <- seq(1,12) %>% map(~ rep(as.numeric(.x), 53)) %>% unlist
in_month <- rep(in_month, y_span)
in_month <- in_month[ 1:nrow(dt_nsa) ]
dt_nsa[, date_month := in_month ]
dt_nsa[, date := as.Date(ISOdate(date_y, date_month, 1)) ]
dt_nsa[, date_month := NULL ]

# SAVE AS ONE FILE
dt_bls_laborforce <- rbind(dt_sa[, seasonality := "S" ], dt_nsa[, seasonality := "U" ])
dt_bls_laborforce[, civil_pop        := as.integer(gsub(",", "", civil_pop))  ]
dt_bls_laborforce[, laborforce_total := as.integer(gsub(",", "", laborforce_total))  ]
dt_bls_laborforce[, employed_total   := as.integer(gsub(",", "", employed_total))  ]
dt_bls_laborforce[, unemployed_total := as.integer(gsub(",", "", unemployed_total))  ]
dt_bls_laborforce

write_dta(dt_bls_laborforce, "../derived/BLS_laborforce_size.dta")
# ------------------------------------------------------------------------------------------


# ------------------------------------------------------------------------------------------
# create LAU_industry_state.dta
# ------------------------------------------------------------------------------------------
# Import all employment statistics
dt_lau_state_industry <- fread("../data/LAU/lau_all.txt")
dt_lau_state_industry[]

# dt_lau_state_industry[, date := as.Date(ISOdate(year, as.integer(str_sub(period, 2, 3)), 1)) ]
setnames(dt_lau_state_industry, "series_id", "id")
dt_lau_state_industry[, `:=`(survey = str_sub(id,1,2), seasonal = str_sub(id,3,3) ) ]
dt_lau_state_industry[, `:=`(state_code = str_sub(id, 4, 5), area_code = str_sub(id, 6, 10) ) ]
dt_lau_state_industry[, `:=`(supersector = str_sub(id, 11, 12), industry_code=str_sub(id, 11, 18),
  	          data_type=str_sub(id, 19,20) )]

# ---
# export state x industries
dt_lau_state_industry <- dt_lau_state_industry[ str_sub(industry_code, 3, 3) != "0" & area_code == "00000" ]
dt_lau_state_industry[, industry_code := as.integer(industry_code) ]
dt_lau_state_industry <- merge(dt_lau_state_industry,
	fread("../data/LAU/sm.industry.txt"), by = c("industry_code"), all.x=T)
dt_lau_state_industry <- merge(dt_lau_state_industry,
	fread("../data/LAU/sm.state.txt", colClasses=c("character") ), by = c("state_code"), all.x=T)
# dt_lau_state_industry %>% tab(seasonal)
# dt_lau_state_industry[, seasonal := "U" ]
dt_lau_state_industry[, date := as.Date(ISOdate(year, as.integer(str_sub(period, 2, 3)), 1)) ]
dt_lau_state_industry <- dt_lau_state_industry[ !is.na(date) ]
dt_lau_state_industry <- dt_lau_state_industry[, .(date, state_code, state_name, seasonal,
	industry=industry_code, industry_name, value) ]
# --- merge super sector code
dt_lau_state_industry[, supersector_code := as.integer(str_sub(industry, 1, 2)) ]
dt_lau_state_industry <- merge(dt_lau_state_industry,
  fread("../data/LAU/sm.supersector.txt"), by = c("supersector_code"), all.x=T)
setcolorder(dt_lau_state_industry, c(2,3,4,9,7))
setnames(dt_lau_state_industry, "value", "emp")
dt_lau_state_industry[, emp := as.numeric(emp) ]

# --- save in dta (and zip)
write_dta(dt_lau_state_industry, "../derived/LAU_industry_state.dta")
# --------------------------------------------------------------------------------------


# ------------------------------------------------------------------------------------------\
# CES LAU DATA: aggregates

# Split private by supersector and government
dt_lau_state_industry <- read_dta("../derived/LAU_industry_state.dta") %>% data.table

dt_lau_private <- dt_lau_state_industry[, .(emp = sum(emp, na.rm=T)), 
	by = .(date, state_name, supersector_name, supersector_code) ]
dt_lau_private <- dt_lau_private[ supersector_code != 90 ]
dt_lau_gov     <- dt_lau_state_industry[supersector_code==90,
                             .(emp = sum(emp, na.rm=T)), by = .(date, state_name, supersector_name, supersector_code, industry_name, industry) ]
dt_lau_gov <- dt_lau_gov[ industry %in% c(90910000, 90920000, 90930000, 90940000, 90921611, 90931611) ]
dt_lau_agg <- rbind(
  dt_lau_private[, .(emp = sum(emp, na.rm=T)), by = .(date, state_name) ],
  dt_lau_gov[, #industry %in% c(90910000, 90920000, 90930000, 90940000),
             .(date, state_name, industry_name, emp) ], fill = T )
dt_lau_agg[, classwkr := fcase(industry_name=="Federal Government", 25, industry_name=="State Government", 27,
                               industry_name=="Local Government", 28, industry_name=="Total State and Local Government", 278,
                               industry_name=="State Government Educational Services", 271, industry_name=="Local Government Educational Services", 281,
                               default=22) ]
dt_lau_agg[, datem := as.monthly(date) ]
setnames(dt_lau_agg, "emp", "emp_ces")
dt_lau_agg <- merge(dt_lau_agg, data.table(state_name=state.name, state=state.abb), all.x=T, by=c("state_name"))
dt_lau_agg


dt_lau_agg <- merge(dt_lau_agg, fread("../data/fips_state.csv")[, .(statefip=V2, state=V3) ], by = c("state") )

write_fst(dt_lau_agg, "../derived/lau_aggregates.fst")
# ------------------------------------------------------------------------------------------


