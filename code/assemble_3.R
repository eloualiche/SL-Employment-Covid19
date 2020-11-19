#! /usr/bin/env R
#
#
# (c) Daniel Green and Erik Loualiche
# 
# Replication code for _State and Local Government Employment in the COVID-19 Crisis_
# Journal of Public Economics 
#
# 
# 
# --------------------------------------------------------------------------------------



# --------------------------------------------------------------------------------------
library(crayon)
library(devtools)

library(tidyr);
library(magrittr)
library(stringr);
library(glue);
library(purrr)
library(readr)
library(haven);
library(readxl);
library(lubridate)
library(progress);
library(fst);
library(data.table); library(bit64)
library(statar)
# --------------------------------------------------------------------------------------

# --------------------------------------------------------------------------------------
# create micro_publicuse_emp_1992_2018.dta
# --------------------------------------------------------------------------------------
# PREAMBLE
fips_state <- fread("../data/fips_state.csv")
setnames(fips_state, c("state_name", "fips", "state"))

state_census_codes <- read_excel("../data/CoG/census_state_codes.xlsx", skip=2) %>% data.table
setnames(state_census_codes, c("state_name", "state", "tmp", "state_code"))
state_census_codes[, tmp := NULL ]
state_census_codes <- merge(state_census_codes, fips_state[, .(fips, state)], by = c("state"))
state_census_codes[]
# --------------------------------------------------------------------------------------

# --------------------------------------------------------------------------------------
# 1.1 Individual Unit Data File (Public Use Format)
# This is an ASCII fixed length text file containing a standard 94-character Public Use Format layout. It contains values for all variables by Item Code (Functional Category) for each Individual Unit ID (see Section 1.2). All payroll and part-time hours in the data file are 31-day monthly equivalent values for the month of March. There may be multiple Item Codes (Functional Category) for the same Individual Unit ID (see Section 1.2). The first 14 characters comprise the Individual Unit ID, which is a concatenation of State Code, Unit Type Code, County Code, Unit Identification Number, Supplement Code, and Sub Code.
# `yycempst` for census years
# `yyempst` for non census years
read_micro <- function(year){
  message(glue("# Processing Census Year ... {year} ") )
  files <- list.files(glue("../data/CoG/ASPEP/"), pattern = glue("{str_sub(year, 3, 4)}.*empst") )
  if ( (year < 1992) | (year %in% c(1996, 2003)) ){
    dt1 <- data.table()
    return(dt1)
  } else if (year >= 2007){
    dt1 <- read_fwf(glue("../data/CoG/ASPEP/{files[1]}"),
      fwf_cols(state_code = c(1, 2), type = c(3, 3), county = c(4, 6), unit_id = c(7, 9),
               supplement = c(10, 12), sub_code = c(13, 14), item_code = c(18, 20),
               emp_full_time = c(21, 30), emp_full_time_flag = c(32, 32),
               payroll_full_time = c(33, 44), payroll_full_time_flag = c(46, 46),
               emp_part_time = c(47, 56), emp_part_time_flag = c(58, 58),
               payroll_part_time = c(59, 70), payroll_part_time_flag = c(72, 72),
               hours_part_time = c(73, 82), hours_part_time_flag = c(84, 84),
               emp_full_time_equivalent = c(85, 94)) ) %>% data.table
  } else if (year <= 2006){
    dt1 <- read_fwf(glue("../data/CoG/ASPEP/{files[1]}"),
      fwf_cols(state_code = c(1, 2), type = c(3, 3), filler = c(4, 17),
               item_code = c(18, 20),
               emp_full_time = c(21, 30), payroll_full_time = c(31, 42),
               emp_part_time = c(43, 52), payroll_part_time = c(53, 64),
               hours_part_time = c(65, 74), emp_full_time_equivalent = c(75, 84)) ) %>% data.table
  }

  # Tag census years
  dt1[, date_y := year ]
  dt1[, census_year := 0 ]
  if (year %in% c(1992, 1997, 2002, 2007, 2012)){ dt1[, census_year := 1 ]  }

  dt1[, state_code := as.integer(state_code) ]
  dt1 <- merge(dt1, state_census_codes, all.x = T, by = c("state_code"))
  return(dt1)
}

dt_micro <- data.table()
for (i_year in seq(1992, 2018)){
  dt_tmp <- read_micro(i_year)
  dt_micro <- rbind(dt_micro, dt_tmp, fill = T)
}
dt_micro[]

# --- adjustment
dt_micro[ emp_full_time == "9999999999", `:=`(emp_full_time=NA, payroll_full_time=NA,
  emp_part_time=NA, payroll_part_time=NA, hours_part_time=NA, emp_full_time_equivalent=NA) ]

# --- SAVE
# write_dta(dt_micro, "../derived/micro_publicuse_emp_1992_2018.dta")
# --------------------------------------------------------------------------------------



# --------------------------------------------------------------------------------------
# create ASPEP_local_payroll_aggregates_1993_2018.dta

# keep states and aggregates
dt_pay_micro <- dt_micro[ state != "US" ][ item_code == "000" ]

# cleaning up NAs
dt_pay_micro[ emp_full_time     == 999999999, emp_full_time     := NA ]
dt_pay_micro[ payroll_full_time == 999999999, payroll_full_time := NA ]
dt_pay_micro[ emp_part_time     == 999999999, emp_part_time     := NA ]
dt_pay_micro[ payroll_part_time == 999999999, payroll_part_time := NA ]
dt_pay_micro[ hours_part_time   == 999999999, hours_part_time := NA ]
dt_pay_micro[ emp_full_time_equivalent == 999999999, emp_full_time_equivalent := NA ]

# --- aggregation
dt_pay_micro[, .(emp_full_time = sum(emp_full_time, na.rm=T), payroll_full_time = sum(payroll_full_time, na.rm=T)),
	by = .(date_y, state_code, state, type) ]

dt_pay_agg <- rbind(
	dt_pay_micro[type %in% c(0,1,2,3,4,5), .(lvl_code=1, emp_full_time = sum(emp_full_time, na.rm=T), payroll_full_time = sum(payroll_full_time, na.rm=T)),
		by = .(date_y, state_code, state) ],
	dt_pay_micro[type==0, .(lvl_code=2, emp_full_time = sum(emp_full_time, na.rm=T), payroll_full_time = sum(payroll_full_time, na.rm=T)),
	    by = .(date_y, state_code, state) ],
	dt_pay_micro[type %in% c(1,2,3,4,5), .(lvl_code=3, emp_full_time = sum(emp_full_time, na.rm=T), payroll_full_time = sum(payroll_full_time, na.rm=T)),
		by = .(date_y, state_code, state) ])
dt_pay_agg[]

write_dta(dt_pay_agg, "../derived/ASPEP_local_payroll_aggregates_1993_2018.dta")
# --------------------------------------------------------------------------------------
