#! /usr/bin/env R
#
#
# (c) Daniel Green and Erik Loualiche
# 
# Replication code for _State and Local Government Employment in the COVID-19 Crisis_
# Journal of Public Economics 
#
# This file builds the main Census of Governments datasets
#
# --------------------------------------------------------------------------------------


# --------------------------------------------------------------------------------------
#
# EXECUTE THE CODE FROM WITHIN THE CODE FOLDER
#
# --------------------------------------------------------------------------------------


# --------------------------------------------------------------------------------------
library(crayon)
library(devtools)

library(tidyr); 
library(magrittr)
library(haven); 
library(readxl); 
library(lubridate)
library(stringr);
library(glue);
library(readr);
library(purrr)
library(fst);
library(bit64)
library(data.table); 
library(statar)
# --------------------------------------------------------------------------------------


# --------------------------------------------------------------------------------------
# --- create IndFin_2012_census.fst
# PREP THE IMPORT
base_col <- c("SortCode", "SurveyYr", "Year4", "ID", "IDChanged", "State Code", "Type Code", "County", "Name", "Census Region",
              "FIPS Code-State", "Weight", "FYEndDate", "YearofData", "YearPop", "YearDepSch", "YearRetire", "SchLevCode", "Version",
              "ReviseDate", "Data_Flag", "JacketUnit", "ZeroData", "Imputed Record", "Population")
other_col <- c("Total Revenue", "Total Rev-Own Sources", "General Revenue", "Gen Rev-Own Sources", "Total Taxes")

years_available <-
  c(1967, 1970, 1971, 1972, 1973, 1974, 1975, 1976, 1977, 1978, 1979, 1980,
    1981, 1982, 1983, 1984, 1985, 1986, 1987, 1988, 1989, 1990, 1991, 1992,
    1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
    2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012)

# RUN THE IMPORT
dt_gov_fin <- data.table()
for (year_iter in years_available){
  ## year_iter = 2001
  year_str = str_sub(year_iter, 3, 4)
  message("\nProcessing year ", year_str)

  dt_gov_a <- fread(paste0("../data/CoG/IndFin/IndFin", year_str, "a.Txt"))
  dt_gov_b <- fread(paste0("../data/CoG/IndFin/IndFin", year_str, "b.Txt"))
  dt_gov_c <- fread(paste0("../data/CoG/IndFin/IndFin", year_str, "c.Txt"))
  dt_tmp <- merge(dt_gov_a,
                  dt_gov_b,
                  by = c("SortCode", "Year4", "ID"))
  dt_tmp <- merge(dt_tmp,
                  dt_gov_c,
                  by = c("SortCode", "Year4", "ID"))

  setnames(dt_tmp, c("State Code", "Type Code", "Population"), c("state_code", "type_code", "population"))
  setnames(dt_tmp, c("Total Debt Outstanding"), c("total_debt_out"))

  dt_tmp[, total_debt_out := as.numeric(total_debt_out) ]
  dt_tmp[, year_census := year_iter ]
  dt_gov_fin <- rbind(dt_gov_fin, dt_tmp)
}

# CLEAN UP SOME COLUMN NAMES AND THE LIKE
col_final <- colnames(dt_gov_fin)
var_keep = c(col_final[ grep("expenditure", col_final, ignore.case = T) ],
             col_final[ grep("debt",        col_final, ignore.case = T) ])
var_labels <- col_final

# CLEAN VARIABLE NAMES 
setnames(dt_gov_fin, c("Year4", "FIPS Code-State", "County"), c("year", "fips_state", "fips_county") )
var_names <- colnames(dt_gov_fin) %>% tolower %>%
  sapply(. , function(x) gsub(" ", "_", x)) %>%
  sapply(. , function(x) gsub("&", "and", x)) %>%
  sapply(. , function(x) gsub("/", "_", x) )  %>%
  sapply(. , function(x) gsub("-", "_", x) )
setnames(dt_gov_fin, var_names)
# ------------------------------------------------------------------------------------------


# --------------------------------------------------------------------------------------
# Auxiliary datasets
state_census_codes <- read_excel("../data/CoG/census_state_codes.xlsx", skip=2) %>% data.table
setnames(state_census_codes, c("state_name", "state", "tmp", "state_code"))
state_census_codes[, tmp := NULL ]
fips_state <- fread("../data/fips_state.csv")[, .(fips=V2, state=V3)]
state_census_codes <- merge(state_census_codes, fips_state, by = c("state"))
state_census_codes[]
# --------------------------------------------------------------------------------------


# ------------------------------------------------------------------------------------------
# Match the old and the new names in the Census
# --- Note that we have not match all categories, only the ones we needed
l_bridge_old_new <- list(
	# --- TAXES
	  c("property_tax",           "T01"),
	  c("total_gen_sales_tax",    "T09"),
    c("alcoholic_beverage_tax", "T10"),
    c("amusement_tax",          "T11"),
    c("insurance_premium_tax",  "T12"),
    c("motor_fuels_tax",        "T13"),
    c("pari_mutuels_tax",       "T14"),
    c("public_utility_tax",     "T15"),
    c("tobacco_tax",            "T16"),
    c("other_select_sales_tax", "T19"),
    c("total_license_taxes",    "T20-29", "m10"),  # multiple matches (here 10)
    c("other_license_taxes",    "T29"),
    c("total_income_taxes",     "T40-41", "m2"),
    c("individual_income_tax",  "T40"),
    c("corp_net_income_tax",    "T41"),
    c("death_and_gift_tax",     "T50"),
    c("docum_and_stock_tr_tax", "T51"),
    c("severance_tax",          "T53"),
    c("taxes_nec",              "T99")
    )

# --- Intergovernmental Fed Revenues
l_bridge_old_new <- c(l_bridge_old_new,
	list(
    c("total_ig_revenue",              "BXX", "missing"),  # not in modern files
    c("total_fed_ig_revenue",          "BXX", "sum_of_B"),  # not in modern files sum of B's
    c("fed_igr_air_transport",         "B01"),  #
    c("fed_igr_education",             "B21"),  #
    c("fed_igr_emp_sec_adm",           "B22"),  #
    c("fed_igr_gen_rev_shar",          "BYY", "missing"),  # not in modern files but always 0 in 2012
    c("fed_igr_gen_support",           "B30"),  #
    c("fed_igr_health_and_hos",        "B42"),  #
    c("fed_igr_highways",              "B46"),  #
    c("fed_igr_transit_sub",           "B94"),  #
    c("fed_igr_hous_com_dev",          "B50"),  #
    c("fed_igr_natural_res",           "B59"),  #
    c("fed_igr_public_welf",           "B79"),  #
    c("fed_igr_sewerage",              "B80"),  #
    c("fed_igr_other",                 "B89")  #
    )     )

# -----------
# EXPENDITURES
# --- Current Operations
l_bridge_old_new <- c(l_bridge_old_new,
	list(
    c("air_trans_direct_expend",    "E01"),  #
    c("misc_com_activ_tot_exp",     "E03" ),
    c("elem_educ_direct_exp",       "E12" )
    )     )

# --- Capital outlay and Construction
l_bridge_old_new <- c(l_bridge_old_new,
	list(
    c("correct_construct",    "F04-F05", "m2")
    )     )

# -------------------
# --- OTHER variables
l_bridge_old_new <- c(l_bridge_old_new,
	list(
    c("total_salaries_and_wages",    "Z00")
    )     )

# ------------------
# CREATE DT for merge (l_bridge_old_new???)
dt_bridge_old_new <- l_bridge_old_new %>% map(~ data.table(t(.x))) %>% rbindlist(., fill = T)
setnames(dt_bridge_old_new, c("variable", "item_code", "item_match_note"))
dt_bridge_old_new
# ------------------------------------------------------------------------------------------


# ------------------------------------------------------------------------------------------
read_micro <- function(year){
  files <- list.files("../data/CoG/ASSLGF/", pattern = glue("{year}FinEstDAT"))
  dt1 <- read_fwf(glue("../data/CoG/ASSLGF/{files[1]}"),
                  fwf_cols(gov_id = c(1,14),
                           item_code = c(15, 17), amount = c(18,29), survey_year = c(30, 33),
                           imputation_flag = c(34, 34)) ) %>% data.table
  dt1[, state_code       := as.integer(str_sub(gov_id, 1, 2)) ]
  dt1[, type             := as.integer(str_sub(gov_id, 3, 3)) ]
  dt1[, county           := as.integer(str_sub(gov_id, 4, 6)) ]
  dt1[, unit_id          := as.integer(str_sub(gov_id, 7, 9)) ]
  dt1[, belong_other_gov := as.integer(str_sub(gov_id, 10, 14)) ]
  dt1 <- merge(dt1, state_census_codes, all.x = T, by = c("state_code"))
  return(dt1)
}

# --- READ THE NEW FILES
dt_gov_new <- data.table()
for (i_year in seq(2012, 2017)){
  dt_tmp <- read_micro(i_year)
  dt_tmp[, date_y := i_year ]
  dt_gov_new <- rbind(dt_gov_new, dt_tmp, fill = T)
}
# ------------------------------------------------------------------------------------------



# ------------------------------------------------------------------------------------------
# JOINING BEFORE AFTER

# Clean up the old file to prepare the merge
c1 <- colnames(dt_gov_fin)[1:25]
c2 <- dt_bridge_old_new[["variable"]]
dt_gov_fin    <- dt_gov_fin[, c(c1, c2), with = FALSE ]
dt_gov_modern <- melt(dt_gov_fin, id = c1)
dt_gov_modern <- dt_gov_modern[ year < 2012 ]

# merge the data with the bridge defined above
dt_gov_modern <- merge(dt_gov_modern, dt_bridge_old_new, by = c("variable"), all.x = T)
dt_gov_modern <- dt_gov_modern[, .(date_y=year, state_code, type=type_code, fips_county, name,
                                   amount=value, item_code, variable, item_match_note)]
dt_gov_modern <- merge(dt_gov_modern, state_census_codes, all.x=T, by=c("state_code") )
dt_gov_modern[]

# BIND OLD AND NEW TOGETHER
dt_cog <- rbind(
  dt_gov_new[, .(date_y, state_code, type, fips_county=county, unit_id, item_code, amount, state_name, state, fips, gov_id)],
  dt_gov_modern[ date_y < 2012 ], fill = TRUE)
dt_cog[, item_cat := str_sub(item_code, 1, 1) ]
setorder(dt_cog, date_y, state_code)
# ------------------------------------------------------------------------------------------


# ------------------------------------------------------------------------------------------
# GENERAL AGGREGATION
setorder(dt_cog, date_y, state)


# --- TOTAL EXPENDITURES AND REVENUES
# TOTAL REVENUE
l_TOT_revenue <- c("B01", "B21", "B22", "B30", "B42", "B46", "B50", "B59", "B79", "B80", "B89", "B91", "B92", "B93", "B94", "C21",
  "C30", "C42", "C46", "C50", "C79", "C80", "C89", "C91", "C92", "C93", "C94", "D21", "D30", "D42", "D46", "D50", "D79", "D80",
  "D89", "D91", "D92", "D93", "D94", "T01", "T09", "T10", "T11", "T12", "T13", "T14", "T15", "T16", "T19", "T20", "T21", "T22",
  "T23", "T24", "T25", "T27", "T28", "T29", "T40", "T41", "T50", "T51", "T53", "T99", "A01", "A03", "A09", "A10", "A12", "A16",
  "A18", "A21", "A36", "A44", "A45", "A50", "A56", "A59", "A60", "A61", "A80", "A81", "A87", "A89", "U01", "U11", "U20", "U21",
  "U30", "U40", "U41", "U50", "U95", "U99", "A90", "A91", "A92", "A93", "A94", "X01", "X02", "X05", "X08", "Y01", "Y02", "Y04",
  "Y11", "Y12", "Y51", "Y52")
# GENERAL REVENUE
l_GAL_revenue <- c("B01", "B21", "B22", "B30", "B42", "B46", "B50", "B59", "B79", "B80", "B89", "B91", "B92", "B93", "B94", "C21",
  "C30", "C42", "C46", "C50", "C79", "C80", "C89", "C91", "C92", "C93", "C94", "D21", "D30", "D42", "D46", "D50", "D79", "D80",
  "D89", "D91", "D92", "D93", "D94", "T01", "T09", "T10", "T11", "T12", "T13", "T14", "T15", "T16", "T19", "T20", "T21", "T22",
  "T23", "T24", "T25", "T27", "T28", "T29", "T40", "T41", "T50", "T51", "T53", "T99", "A01", "A03", "A09", "A10", "A12", "A16",
  "A18", "A21", "A36", "A44", "A45", "A50", "A56", "A59", "A60", "A61", "A80", "A81", "A87", "A89", "U01", "U11", "U20", "U21",
  "U30", "U40", "U41", "U50", "U95", "U99")
# TOTAL EXPENDITURE
l_TOT_expenditure <- c("E01", "E03", "E04", "E05", "E12", "E16", "E18", "E21", "E22", "E23", "E24", "E25", "E26", "E29", "E31",
  "E32", "E36", "E44", "E44", "E45", "E50", "E52", "E55", "E56", "E59", "E60", "E61", "E62", "E66", "E74", "E75", "E77", "E79",
  "E80", "E81", "E85", "E87", "E89", "E90", "E91", "E92", "E93", "E94", "I89", "I91", "I92", "I93", "I94", "J19", "L67", "J68",
  "J85", "X11", "X12", "Y05", "Y06", "Y14", "Y53", "F01", "F03", "F04", "F05", "F12", "F16", "F18", "F21", "F22", "F23", "F24",
  "F25", "F26", "F29", "F31", "F32", "F36", "F44", "F45", "F50", "F52", "F55", "F56", "F59", "F60", "F61", "F62", "F66", "F77",
  "F79", "F80", "F81", "F85", "F87", "F89", "F90", "F91", "F92", "F93", "F94", "G01", "G03", "G04", "G05", "G12", "G16", "G18",
  "G21", "G22", "G23", "G24", "G25", "G26", "G29", "G31", "G32", "G36", "G44", "G45", "G50", "G52", "G55", "G56", "G59", "G60",
  "G61", "G62", "G66", "G77", "G79", "G79", "G80", "G81", "G85", "G87", "G89", "G90", "G91", "G92", "G93", "G94", "L01", "L04",
  "L05", "L12", "L18", "L23", "L25", "L29", "L32", "L36", "L44", "L52", "L59", "L60", "L61", "L62", "L66", "L67", "L79", "L80",
  "L81", "L87", "L89", "L91", "L92", "L93", "L94", "M01", "M04", "M05", "M12", "M18", "M21", "M23", "M24", "M25", "M29", "M30",
  "M32", "M36", "M44", "M50", "M52", "M55", "M56", "M59", "M60", "M61", "M62", "M66", "M67", "M68", "M79", "M80", "M81", "M87",
  "M89", "M91", "M92", "M93", "M94", "Q12", "Q18", "S67", "S74", "S89")
# DIRECT EXPENDITURE
l_DIR_expenditure <- c("E01", "E03", "E04", "E05", "E12", "E16", "E18", "E21", "E22", "E23", "E24", "E25", "E26", "E29", "E31",
  "E32", "E36", "E44", "E45", "E50", "E52", "E55", "E56", "E59", "E60", "E61", "E62", "E66", "E74", "E75", "E77", "E79", "E80",
  "E81", "E85", "E87", "E89", "E90", "E91", "E92", "E93", "E94", "F01", "F03", "F04", "F05", "F12", "F16", "F18", "F18", "F21",
  "F22", "F23", "F24", "F25", "F26", "F29", "F31", "F32", "F36", "F44", "F45", "F50", "F52", "F55", "F56", "F59", "F60", "F61",
  "F62", "F66", "F77", "F79", "F80", "F81", "F85", "F87", "F89", "F90", "F91", "F92", "F93", "F94", "G01", "G03", "G04", "G05",
  "G12", "G16", "G18", "G21", "G22", "G23", "G24", "G25", "G26", "G29", "G31", "G32", "G36", "G44", "G45", "G50", "G52", "G55",
  "G56", "G59", "G60", "G61", "G62", "G66", "G77", "G79", "G80", "G81", "G85", "G87", "G89", "G90", "G91", "G92", "G93", "G94",
  "X11", "X12", "Y05", "Y06", "Y14", "Y53", "J19", "J67", "J68", "J85", "I89", "I91", "I92", "I93", "I94")

# initialize
dt_tmp_agg <- dt_cog[ item_code %in% l_TOT_revenue, .(TOT_revenue = sum(amount, na.rm=TRUE)), by = .(date_y, state, type) ]
dt_cog_agg <- copy(dt_tmp_agg)

dt_tmp_agg <- dt_cog[ item_code %in% l_GAL_revenue, .(GAL_revenue = sum(amount, na.rm=TRUE)), by = .(date_y, state, type) ]
dt_cog_agg <- merge(dt_cog_agg, dt_tmp_agg, all.x=T, all.y=T, by = c("date_y", "state", "type"))
dt_tmp_agg <- dt_cog[ item_code %in% l_TOT_expenditure, .(TOT_expenditure = sum(amount, na.rm=TRUE)), by = .(date_y, state, type) ]
dt_cog_agg <- merge(dt_cog_agg, dt_tmp_agg, all.x=T, all.y=T, by = c("date_y", "state", "type"))
dt_tmp_agg <- dt_cog[ item_code %in% l_DIR_expenditure, .(DIR_expenditure = sum(amount, na.rm=TRUE)), by = .(date_y, state, type) ]
dt_cog_agg <- merge(dt_cog_agg, dt_tmp_agg, all.x=T, all.y=T, by = c("date_y", "state", "type"))
dt_cog_agg[]

# --- IG EXPENDITURE AND REVENUES
l_IG_expenditure <- c("L01", "L04", "L05", "L12", "L18", "L23", "L25", "L29", "L32", "L36", "L44",
  "L50", "L52", "L59", "L60", "L61", "L62", "L66", "L67", "L79", "L80", "L81", "L87", "L89", "L91", "L92",
  "L93", "L94", "M01", "M04", "M05", "M12", "M18", "M21", "M23", "M24", "M25", "M29", "M30", "M32", "M36",
  "M44", "M50", "M52", "M52", "M55", "M56", "M59", "M60", "M61", "M62", "M66", "M67", "M68", "M79", "M80",
  "M81", "M87", "M89", "M91", "M92", "M93", "M94", "Q12", "Q18", "S67", "S89")
l_IG_revenue <- c("B01", "B21", "B22", "B30", "B42", "B46", "B50", "B59", "B79", "B80", "B89", "B91", "B92",
  "B93", "B94", "C21", "C30", "C42", "C46", "C50", "C79", "C80", "C89", "C91", "C92", "C93", "C94", "D21",
  "D30", "D42", "D46", "D50", "D79", "D80", "D89", "D91", "D92", "D93", "D94")
l_IG_fed_revenue   <- c("B01", "B21", "B22", "B30", "B42", "B46", "B50", "B59", "B79", "B80", "B89", "B91", "B92", "B93", "B94")
l_IG_state_revenue <- c("C21", "C30", "C42", "C46", "C50", "C79", "C80", "C89", "C91", "C92", "C93", "C94")
l_IG_local_revenue <- c("D21", "D30", "D42", "D46", "D50", "D79", "D80", "D89", "D91", "D92", "D93", "D94")

dt_tmp_agg <- dt_cog[ item_code %in% l_IG_expenditure, .(IG_expenditure = sum(amount, na.rm=TRUE)), by = .(date_y, state, type) ]
dt_cog_agg <- merge(dt_cog_agg, dt_tmp_agg, all.x=T, all.y=T, by = c("date_y", "state", "type"))
dt_tmp_agg <- dt_cog[ item_code %in% l_IG_revenue, .(IG_revenue = sum(amount, na.rm=TRUE)), by = .(date_y, state, type) ]
dt_cog_agg <- merge(dt_cog_agg, dt_tmp_agg, all.x=T, all.y=T, by = c("date_y", "state", "type"))
dt_tmp_agg <- dt_cog[ item_code %in% l_IG_fed_revenue, .(IG_fed_revenue = sum(amount, na.rm=TRUE)), by = .(date_y, state, type) ]
dt_cog_agg <- merge(dt_cog_agg, dt_tmp_agg, all.x=T, all.y=T, by = c("date_y", "state", "type"))
dt_tmp_agg <- dt_cog[ item_code %in% l_IG_state_revenue, .(IG_state_revenue = sum(amount, na.rm=TRUE)), by = .(date_y, state, type) ]
dt_cog_agg <- merge(dt_cog_agg, dt_tmp_agg, all.x=T, all.y=T, by = c("date_y", "state", "type"))
dt_tmp_agg <- dt_cog[ item_code %in% l_IG_local_revenue, .(IG_local_revenue = sum(amount, na.rm=TRUE)), by = .(date_y, state, type) ]
dt_cog_agg <- merge(dt_cog_agg, dt_tmp_agg, all.x=T, all.y=T, by = c("date_y", "state", "type"))

# --- SOME DETAILS OF EXPENDITURES
# Current Operations
l_CUR_expenditure <- c("E01", "E03", "E04", "E05", "E12", "E16", "E18", "E21", "E22", "E23", "E24", "E25", "E26", "E29", "E31", "E32", "E36", "E44", "E45", "E50", "E52", "E55", "E56", "E59", "E60", "E61", "E62", "E66", "E74", "E75", "E77", "E79", "E80", "E81", "E85", "E87", "E89", "E90", "E91", "E92", "E93", "E94")
# Capital Outlay
l_CAP_expenditure <- c("F01", "F03", "F04", "F05", "F12", "F16", "F18", "F21", "F22", "F23", "F24", "F25", "F26", "F29", "F31", "F32", "F36", "F44", "F45", "F50", "F52", "F55", "F56", "F59", "F91", "F92", "F93", "F94", "G01", "G03", "G04", "G05", "G12", "G16", "G18", "G21", "G22", "G23", "G24", "G25", "G26", "G29", "G31", "G32", "G36", "G44", "G45", "G50", "G52", "G55", "G56", "G59", "G60", "G61", "G62", "G66", "G77", "G79", "G80", "G81", "G85", "G87", "G89", "G90", "G91", "G92", "G93", "G94")
# Construction
l_CONST_expenditure <- c("F01", "F03", "F04", "F05", "F12", "F16", "F18", "F21", "F22", "F23", "F24", "F25", "F26", "F29", "F31", "F32", "F36", "F44", "F45", "F50", "F52", "F55", "F56", "F59", "F91", "F92", "F93", "F94")
# Other Capital Outlay
l_OTCAP_expenditure <- c("G01", "G03", "G04", "G05", "G12", "G16", "G18", "G21", "G22", "G23", "G24", "G25", "G26", "G29", "G31", "G32", "G36", "G44", "G45", "G50", "G52", "G55", "G56", "G59", "G60", "G61", "G62", "G66", "G77", "G79", "G80", "G81", "G85", "G87", "G89", "G90", "G91", "G92", "G93", "G94")
# Assistance and Subsidies
l_ASST_expenditure <- c("J19", "J67", "J68", "J85")
# Interest on debt
l_DEBT_expenditure <- c("I89", "I91", "I92", "I93", "I94")
# Insurance benefits and repayments
l_INS_expenditure <- c("X11", "X12", "Y05", "Y06", "Y14", "Y53")
# Salaries and wages
l_WAG_expenditure <- c("Z00")

dt_tmp_agg <- dt_cog[ item_code %in% l_CUR_expenditure, .(CURRENTOP_expenditure = sum(amount, na.rm=TRUE)), by = .(date_y, state, type) ]
dt_cog_agg <- merge(dt_cog_agg, dt_tmp_agg, all.x=T, all.y=T, by = c("date_y", "state", "type"))
dt_tmp_agg <- dt_cog[ item_code %in% l_CAP_expenditure, .(CAP_expenditure = sum(amount, na.rm=TRUE)), by = .(date_y, state, type) ]
dt_cog_agg <- merge(dt_cog_agg, dt_tmp_agg, all.x=T, all.y=T, by = c("date_y", "state", "type"))
dt_tmp_agg <- dt_cog[ item_code %in% l_CONST_expenditure, .(CONSTCAP_expenditure = sum(amount, na.rm=TRUE)), by = .(date_y, state, type) ]
dt_cog_agg <- merge(dt_cog_agg, dt_tmp_agg, all.x=T, all.y=T, by = c("date_y", "state", "type"))
dt_tmp_agg <- dt_cog[ item_code %in% l_OTCAP_expenditure, .(OTHCAP_expenditure = sum(amount, na.rm=TRUE)), by = .(date_y, state, type) ]
dt_cog_agg <- merge(dt_cog_agg, dt_tmp_agg, all.x=T, all.y=T, by = c("date_y", "state", "type"))

dt_tmp_agg <- dt_cog[ item_code %in% l_ASST_expenditure, .(ASST_expenditure = sum(amount, na.rm=TRUE)), by = .(date_y, state, type) ]
dt_cog_agg <- merge(dt_cog_agg, dt_tmp_agg, all.x=T, all.y=T, by = c("date_y", "state", "type"))
dt_tmp_agg <- dt_cog[ item_code %in% l_DEBT_expenditure, .(INTDEBT_expenditure = sum(amount, na.rm=TRUE)), by = .(date_y, state, type) ]
dt_cog_agg <- merge(dt_cog_agg, dt_tmp_agg, all.x=T, all.y=T, by = c("date_y", "state", "type"))
dt_tmp_agg <- dt_cog[ item_code %in% l_INS_expenditure, .(INSUR_expenditure = sum(amount, na.rm=TRUE)), by = .(date_y, state, type) ]
dt_cog_agg <- merge(dt_cog_agg, dt_tmp_agg, all.x=T, all.y=T, by = c("date_y", "state", "type"))
dt_tmp_agg <- dt_cog[ item_code %in% l_WAG_expenditure, .(WAGE_expenditure = sum(amount, na.rm=TRUE)), by = .(date_y, state, type) ]
dt_cog_agg <- merge(dt_cog_agg, dt_tmp_agg, all.x=T, all.y=T, by = c("date_y", "state", "type"))

# --- SOME DETAILS OF REVENUES
# Revenue own sources
l_OWN_revenue <- c("A01", "A03", "A09", "A10", "A12", "A16", "A18", "A21", "A36", "A44", "A45", "A50", "A56", "A59", "A60",
  "A61", "A80", "A81", "A87", "A89", "T01", "T09", "T10", "T11", "T12", "T13", "T14", "T15", "T16", "T19", "T20", "T21",
  "T22", "T23", "T24", "T25", "T27", "T28", "T29", "T40", "T41", "T50", "T51", "T53", "T99", "U01", "U11", "U20", "U21",
  "U30", "U40", "U41", "U50", "U95", "U99")
# Total Taxes
l_TAX_revenue <- c("T01", "T09", "T10", "T11", "T12", "T13", "T14", "T15", "T16", "T19", "T20", "T21", "T22", "T23", "T24",
  "T25", "T27", "T28", "T29", "T40", "T41", "T50", "T51", "T53", "T99")
# Property Taxes
l_PROPTAX_revenue <- c("T01")
# Sales and Gross Receips
l_SALESTAX_revenue <- c("T09", "T10", "T11", "T12", "T13", "T14", "T15", "T16", "T19")
# Individual Income Tax
l_INCMTAX_revenue <- c("T40")
# Corporate Income Tax
l_CORPTAX_revenue <- c("T41")
# Other Taxes
l_OTHTAX_revenue  <- c("T20", "T21", "T22", "T23", "T25", "T27", "T28", "T29", "T50", "T51", "T53", "T99")
# Charges and Miscellaneous General Revenue
l_CHGMISC_revenue <- c("A01, A03, A09, A10, A12, A16, A18, A21, A36, A44, A45, A50, A56, A59, A60, A61, A80, A81, A87, A89, U01, U11, U20, U21, U30, U40, U41, U50, U95, U99")
# Current Charges
l_CHG_revenue <- c("A01, A03, A09, A10, A12, A16, A18, A21, A36, A44, A45, A50, A56, A59, A60, A61, A80, A81, A87, A89")

dt_tmp_agg <- dt_cog[ item_code %in% l_OWN_revenue, .(OWN_revenue = sum(amount, na.rm=TRUE)), by = .(date_y, state, type) ]
dt_cog_agg <- merge(dt_cog_agg, dt_tmp_agg, all.x=T, all.y=T, by = c("date_y", "state", "type"))
dt_tmp_agg <- dt_cog[ item_code %in% l_TAX_revenue, .(TAX_revenue = sum(amount, na.rm=TRUE)), by = .(date_y, state, type) ]
dt_cog_agg <- merge(dt_cog_agg, dt_tmp_agg, all.x=T, all.y=T, by = c("date_y", "state", "type"))
dt_tmp_agg <- dt_cog[ item_code %in% l_PROPTAX_revenue, .(PROPTAX_revenue = sum(amount, na.rm=TRUE)), by = .(date_y, state, type) ]
dt_cog_agg <- merge(dt_cog_agg, dt_tmp_agg, all.x=T, all.y=T, by = c("date_y", "state", "type"))
dt_tmp_agg <- dt_cog[ item_code %in% l_SALESTAX_revenue, .(SALESTAX_revenue = sum(amount, na.rm=TRUE)), by = .(date_y, state, type) ]
dt_cog_agg <- merge(dt_cog_agg, dt_tmp_agg, all.x=T, all.y=T, by = c("date_y", "state", "type"))
dt_tmp_agg <- dt_cog[ item_code %in% l_INCMTAX_revenue, .(INCMTAX_revenue = sum(amount, na.rm=TRUE)), by = .(date_y, state, type) ]
dt_cog_agg <- merge(dt_cog_agg, dt_tmp_agg, all.x=T, all.y=T, by = c("date_y", "state", "type"))
dt_tmp_agg <- dt_cog[ item_code %in% l_CORPTAX_revenue, .(CORPTAX_revenue = sum(amount, na.rm=TRUE)), by = .(date_y, state, type) ]
dt_cog_agg <- merge(dt_cog_agg, dt_tmp_agg, all.x=T, all.y=T, by = c("date_y", "state", "type"))
dt_tmp_agg <- dt_cog[ item_code %in% l_OTHTAX_revenue, .(OTHTAX_revenue = sum(amount, na.rm=TRUE)), by = .(date_y, state, type) ]
dt_cog_agg <- merge(dt_cog_agg, dt_tmp_agg, all.x=T, all.y=T, by = c("date_y", "state", "type"))

dt_tmp_agg <- dt_cog[ item_code %in% l_CHGMISC_revenue, .(CHRGMISC_revenue = sum(amount, na.rm=TRUE)), by = .(date_y, state, type) ]
dt_cog_agg <- merge(dt_cog_agg, dt_tmp_agg, all.x=T, all.y=T, by = c("date_y", "state", "type"))
dt_tmp_agg <- dt_cog[ item_code %in% l_CHG_revenue, .(CHRGE_revenue = sum(amount, na.rm=TRUE)), by = .(date_y, state, type) ]
dt_cog_agg <- merge(dt_cog_agg, dt_tmp_agg, all.x=T, all.y=T, by = c("date_y", "state", "type"))


# --- MISCELLANEOUS
# Debt
l_DBT_fin <- c("44T", "49U", "64V")
# Long term debt
l_LTDBT_fin <- c("44T", "49U")
# Short Term Debt
l_STDBT_fin <- c("64V")

dt_tmp_agg <- dt_cog[ item_code %in% l_DBT_fin, .(DBT_fin = sum(amount, na.rm=TRUE)), by = .(date_y, state, type) ]
dt_cog_agg <- merge(dt_cog_agg, dt_tmp_agg, all.x=T, all.y=T, by = c("date_y", "state", "type"))
dt_tmp_agg <- dt_cog[ item_code %in% l_LTDBT_fin, .(LTDBT_fin = sum(amount, na.rm=TRUE)), by = .(date_y, state, type) ]
dt_cog_agg <- merge(dt_cog_agg, dt_tmp_agg, all.x=T, all.y=T, by = c("date_y", "state", "type"))
dt_tmp_agg <- dt_cog[ item_code %in% l_STDBT_fin, .(STDBT_fin = sum(amount, na.rm=TRUE)), by = .(date_y, state, type) ]
dt_cog_agg <- merge(dt_cog_agg, dt_tmp_agg, all.x=T, all.y=T, by = c("date_y", "state", "type"))
# ------------------------------------------------------------------------------------------


# ------------------------------------------------------------------------------------------
# SAVING 
dt_cog_agg_long <- melt(dt_cog_agg, id = c("date_y", "state", "type")) # --- wide to long
write_fst(dt_cog_agg_long, "../derived/SandL_aggregates.fst", compress=100)

# --- aggregate the data again in 3 categories: state + local total / state only /  local only
# dt1 <- dt_cog_agg_long[, .(value = sum(value, na.rm=T)), by = .(date_y, state, variable) ]
# dt2 <- dt_cog_agg_long[type==0, .(value = sum(value, na.rm=T)), by = .(date_y, state, variable) ]
# dt3 <- dt_cog_agg_long[type!=0, .(value = sum(value, na.rm=T)), by = .(date_y, state, variable) ]
# dt_sl_agg <- rbind(dt1[, lvl_code := 1], dt2[, lvl_code := 2], dt3[, lvl_code := 3] )
# write_dta(dt_sl_agg, "../derived/state_local_aggregate_expenditure.dta")
# ------------------------------------------------------------------------------------------



# ------------------------------------------------------------------------------------------
# --- create CensusFin_statelocalgov_taxes.fst
dt_munis <- dt_cog[, .(date_y, state_code, type, fips, fips_county, unit_id, item_code, amount, state, name, gov_id) ]
dt_munis[, item_cat := str_sub(item_code, 1, 1) ] # to filter out taxes
dt_munis <- dt_munis[item_cat=="T" & date_y >= 1980 &  type %in% c(0,1,2,3,4,5) ]

dt_CoG_agg <- dcast(dt_cog_agg_long[ grep("TOT", variable) ], date_y + state + type ~ variable)

dt_munis <- merge(dt_munis[], dt_CoG_agg[], by = c("state", "type", "date_y"), all.x=TRUE)
setnames(dt_munis, c("TOT_revenue", "TOT_expenditure"), c("TOT_agg_rev", "TOT_agg_exp"))

# Check this matches with the 2017 excel file:
# dt_munis[ date_y == 2017 & state == "CA" ]
# dt_munis[ date_y == 2017 & type == 0 & state == "CA" ]

write_fst(dt_munis[, .(date_y, state, fips, fips_county, type, item_code, amount, name, gov_id, TOT_agg_rev, TOT_agg_exp) ],
          "../derived/CensusFin_statelocalgov_taxes.fst", compress=100)
# ------------------------------------------------------------------------------------------


# ------------------------------------------------------------------------------------------
# ---  create CensusFin_localgov_taxes.fst
# dt_munis <- dt_cog[, .(date_y, state_code, type, fips, fips_county, unit_id, item_code, amount, state, name, gov_id) ]
# dt_munis <- dt_munis[ type %in% c(1,2,3) ]
# dt_munis[, item_cat := str_sub(item_code, 1, 1) ] # to filter out taxes
# write_fst(dt_munis[item_cat=="T" & date_y >= 1980, .(date_y, state, fips_county, unit_id, 
#   name, gov_id, type, item_code, amount) ],
#   "../derived/CensusFin_localgov_taxes.fst", compress=100)
# ------------------------------------------------------------------------------------------


# --------------------------------------------------------------------------------------
# create QTAX_STATE_processed.dta
# This corresponds to the Quarterly tax file, a revenue source at higher frequency for states
dt_qtax <- fread("../data/CoG/QTAX/QTAX-data.csv")
dt_qtax <- merge(dt_qtax, fread("../data/CoG/QTAX/QTAX-timeperiods.csv"), by = c("per_idx")) # time
dt_qtax <- merge(dt_qtax, fread("../data/CoG/QTAX/QTAX-categories.csv")[, c(1,2,3) ], by = c("cat_idx"))  # categories
dt_qtax <- merge(dt_qtax, fread("../data/CoG/QTAX/QTAX-datatypes.csv")[, c(1,2,3) ], by = c("dt_idx"))   # data (in millions of dollars)
dt_qtax <- merge(dt_qtax, fread("../data/CoG/QTAX/QTAX-geolevels.csv"), by = c("geo_idx"))   # data (in millions of dollars)
dt_qtax %>% tab(is_adj)

# The one that interest us is all taxes by state: # QTAXCAT3
dt_state_qtax <- dt_qtax[ cat_code == "QTAXCAT3" ]
dt_state_qtax[, date := as.Date(ISOdate(as.integer(str_sub(per_name, -4, -1)), 3*as.integer(str_sub(per_name, 2, 2)), 1) ) ]
dt_state_qtax <- dt_state_qtax[, .(date, state=geo_code, state_name=geo_desc, item_code=dt_code, item_desc=dt_desc, value=val) ]

# remove seasonality test
# l_item_tax_sa <- c("T09", "T13", "T40", "T41", "TOTAL")
# dt_qtax_sa <- copy(dt_state_qtax[ item_code %in% l_item_tax_sa ] )
# setorder(dt_qtax_sa, item_code, state, date)
# dt_qtax_sa[, mean_val := mean(value), by = .(state, item_code) ] # remove the 0s -- some have nostate taxes
# setorder(dt_qtax_sa, item_code, state, date)
# dt_qtax_sa[mean_val > 0, value_sa := try(
#   final(seas(ts(value, start=c(year(min(date)), quarter(min(date))), frequency=4)))
#   ),
#   by = .(item_code, state) ]
# dt_qtax_sa[, value_sa := NA ]
# dt_qtax_sa[, value_sa := as.numeric(value_sa) ]
# dt_qtax_sa[, sa_pass  := fifelse(is.na(value_sa), FALSE, TRUE) ]
# dt_qtax_sa[  sa_pass == F, value_sa := value ]
# dt_qtax_sa[ sa_pass == F, .(item_code, state) ] %>% unique # inspection which state items have failed
# dt_state_qtax <- merge(dt_state_qtax, dt_qtax_sa[, .(state, date, item_code, value_sa, sa_pass)],
#     by = c("state", "date", "item_code"), all.x = T) # merge it back in master file

dt_state_qtax <- merge(dt_state_qtax[ item_code != "TOTAL" ],
    dt_state_qtax[ item_code == "TOTAL", .(date, state, total_taxes=value) ],
    by = c("date", "state") )

write_dta(dt_state_qtax, "../derived/QTAX_STATE_processed.dta")
# --------------------------------------------------------------------------------------
