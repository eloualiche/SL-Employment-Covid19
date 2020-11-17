***********************************************************************
// import state id
***********************************************************************

import delimited using "../data/fips_state.csv", clear
rename v1 state
rename v2 fips
rename v3 state_abbrv
tempfile stateid
save `stateid'

***********************************************************************
// import rainy day funds
***********************************************************************

import excel using "../data/NASBO/RainyDayFunds2011-2020.xlsx", clear firstrow cellrange(A2) sheet("Total Amount")
replace State = subinstr(State,"*","",.)
destring y2020, replace force
destring y2019, replace force
reshape long y, i(State) j(datey)
rename y state_rainyday_level
tempfile rainyday_level
save `rainyday_level'

import excel using "../data/NASBO/RainyDayFunds2011-2020.xlsx", clear firstrow cellrange(A2) sheet("Percent")
replace State = subinstr(State,"*","",.)
replace State = subinstr(State,","," ",.)
destring y2020, replace force
destring y2019, replace force
reshape long y, i(State) j(datey)
rename y state_rainyday_pctExp
gen state_rainyday_fracExp = state_rainyday_pctExp / 100

// merge
merge 1:1 State datey using `rainyday_level', keep(3) nogen

// drop national or non-state
drop if inlist(State,"Total","US")

// merge with stateid
rename State state
merge m:1 state using `stateid', keep(1 3) nogen

rename datey year
order state year

tempfile rainyday
save `rainyday'

***********************************************************************
// import revenue sensitivity data
***********************************************************************

import excel using "../data/CoG/ASSLGF/2017_Combined.xlsx", sheet("transposed") firstrow clear

gen sales_tax_share_rev = SalesAndGrossReceiptsTax / TotalRevenue
gen prop_tax_share_rev = PropertyTax / TotalRevenue
gen income_tax_share_rev = (CorporateTax + IncomeTax) / TotalRevenue
gen fs_tnf_share_rev = (FromFed + FromState) / TotalRevenue
replace Level = "_"+Level
keep State Level *share* TotalRevenue

// reshape
reshape wide *share_rev TotalRevenue, i(State) j(Level) string

// drop national or non-state
drop if inlist(State,"US","District of Columbia")

// merge with stateid
rename State state
merge 1:1 state using `stateid', keep(1 3) nogen

order state

tempfile revenueSources
save `revenueSources'

***********************************************************************
// import ffis covid allocations by state
***********************************************************************

import excel using "../data/FFIS_COVID_19_State_by_State_Allocations.20.xlsx", sheet(Allocations) cellrange(A10:CQ65) clear
rename A state
rename C covid_relief_treasury
rename CQ covid_relief_total
keep state covid_relief_treasury covid_relief_total

// drop national or non-state
drop if inlist(state,"American Samoa","Guam","Northern Mariana Islands","Virgin Islands","District of Columbia","Puerto Rico")

// merge with stateid
merge 1:1 state using `stateid', keep(1 3) nogen

order state

tempfile state_ffis
save `state_ffis'


***********************************************************************
// import covid cases by population
***********************************************************************

// import state populations
use "../data/state_pop.dta", clear
keep if year == 2019
rename pop pop2019
destring fips, replace
drop year
tempfile pop2019
save `pop2019'

// For most recent data, run: wget --no-use-server-timestamps http://covidtracking.com/api/states/daily.csv -O  $(covid)/output/state_daily_covid.csv
import delimited "../data/state_daily_covid.csv", stringcols(1) clear
merge m:1 fips using `pop2019', keep(3) nogen

// drop national or non-state
drop if state == "DC"

// clean date
gen date2 = date(date,"YMD")
drop date
rename date2 date
format date %td
order date

// create month date
gen datem = mofd(date)
format datem %tm

// keep last date of month
keep if inlist(date,td(31jan2020),td(29feb2020),td(31mar2020),td(30apr2020),td(31may2020),td(30jun2020),td(31jul2020),td(31aug2020),td(30sep2020))

// merge with stateid
rename state state_abbrv
merge m:1 state_abbrv using `stateid', keep(1 3) nogen

order state datem

tempfile covid_ts
save `covid_ts'

***********************************************************************
// import cps data
***********************************************************************

use "../data/CPS/cps_1980_2019.dta", clear
append using "../data/CPS/cps_post2020.dta"

// keep if state government employee or local government employee (27 and 28); keep others too (22= private, for profit , 25 = federal govt employee)
keep if inlist(classwkr,22,25,27,28)

// drop national or non-state
drop if statefip == 11 // DC

// create new if local or state government employee
expand 2 if inlist(classwkr,27,28), gen(newclass)
replace classwkr = 30 if newclass == 1
drop newclass

// create date
gen datem = ym(year,month)
format datem %tm

// create variables
replace ahrsworkt = . if ahrsworkt == 999 // replace missing, hours worked last week
gen employed = empstat == 10 // at work
gen unemployed = inlist(empstat,21,22) // Unemployed, experienced worker / Unemployed, new worker
gen laborforce = inlist(empstat,10,12,21,22,34) // at work / Has job, not at work last week /Unemployed, experienced worker /Unemployed, new worker / NILF, other
gen laidoff = inlist(absent,2) // Yes, laid off
gen absent_other = inlist(absent,3) // Yes, other reason (vacation, illness, labor dispute)
gen part_time_covid = inlist(wkstat,12,13,20,21,42,50,60) // Part-time for non-economic reasons, usually full-time / Not at work, usually full-time / Part-time for economic reasons / Part-time for economic reasons, usually full-time / Not at work, usually part-time / Unemployed, seeking full-time work / Unemployed, seeking part-time work
gen part_time_reason = inlist(whyptlwk,10,60,52,111,130) // Slack work, business conditions / Could only find part-time / Job terminated / Vacation/personal day / Other
gen travel = ind == 8660  
gen total = 1

// Replace missing coded value in earnweek
gen badearn = earnweek > 9999
replace earnweek = . if earnweek > 9999
replace hourwage = . if hourwage > 999

// save for occupation data
save "../derived/temp_ipums_full.dta", replace
// tempfile ipums_full
// save `ipums_full'

//////////////////////////

// collapse by WTFINL
use "../derived/temp_ipums_full.dta", clear
gcollapse (sum) employed unemployed travel laborforce laidoff absent_other total part_time_covid_sum = part_time_covid  part_time_reason_sum = part_time_reason (mean) uhrsworkt ahrsworkt part_time_reason part_time_covid [pweight=wtfinl], by(statefip datem classwkr)
tempfile collapsed_wtfinl
save `collapsed_wtfinl'

// collapse by EARNWT
use "../derived/temp_ipums_full.dta", clear
gcollapse (mean) earnweek hourwage [pweight=earnwt], by(statefip datem classwkr)
tempfile collapsed_earnwt
save `collapsed_earnwt'

// merge with collapsed data
merge 1:1 statefip datem classwkr using `collapsed_wtfinl', keep(1 2 3) nogen

// create variables
egen employed_national = total(employed), by(classwkr datem)
egen unemployed_national = total(unemployed), by(classwkr datem)
gen urate_national = unemployed_national / (employed_national+unemployed_national)

gen urate = unemployed / (employed + unemployed)
gen urate_bl = unemployed / (laborforce)
gen urate_wpt = (unemployed + part_time_covid) / laborforce

egen stateclass = group(statefip classwkr)
gen travel_frac = travel / total
replace travel_frac = . if classwkr ~= 22  //only private sector
egen tf = max(travel_frac), by(statefip datem)
drop travel_frac
rename tf travel_frac

gen year = yofd(dofm(datem))
gen month = month(dofm(datem))
egen emp_yearstart = max(employed*(month(dofm(datem))==1)), by(statefip classwkr year)
replace emp_yearstart = . if emp_yearstart == 0
gen ytd_employed = employed-emp_yearstart

xtset stateclass datem
gen d1_employed = d.employed
gen s12_employed = s12.employed
gen g12_employed = s12.employed/l12.employed
gen g_employed = d.employed/l.employed
gen g_ytd_employed = ytd_employed/emp_yearstart


****************************************
// save long format
save "../derived/state_agg_cps_long_full.dta", replace

****************************************

// reshape to wide
drop stateclass emp_yearstart travel total
reshape wide laborforce laidoff absent_other earnweek hourwage employed_national unemployed_national urate_national uhrsworkt ahrsworkt part_time_reason part_time_covid part_time_reason_sum part_time_covid_sum urate urate_bl urate_wpt ytd_employed d1_employed s12_employed g12_employed g_ytd_employed g_employed employed unemployed, i(statefip datem travel_frac) j(classwkr)

rename *22 *_private
rename *25 *_federal
rename *27 *_state
rename *28 *_local
rename *30 *_statelocal

// drop if dc
drop if statefip == 11

// merge with stateid
rename statefip fips
merge m:1 fips using `stateid', keep(1 3) nogen

order state datem year

tempfile state_agg_cps_wide_full
save `state_agg_cps_wide_full'


***********************************************************************
// import occupation data (from cps data)
***********************************************************************

use "../derived/temp_ipums_full.dta", clear

replace occ2010 = occ if datem >= m(2020m1)

// create variables
gen occ_detail = "Education" if inrange(occ2010,2200,2550)
replace occ_detail = "Healthcare" if inrange(occ2010,3000,3655)
replace occ_detail = "Community serive" if inrange(occ2010,2000,2060)
replace occ_detail = "Protective service" if inrange(occ2010,3700,4160)

replace occ_detail = "Management" if inrange(occ2010,10,430)
replace occ_detail = "Business and finance" if inrange(occ2010,500,950)
replace occ_detail = "Computer and math science" if inrange(occ2010,1000,1240)
replace occ_detail = "Architcure and engineering" if inrange(occ2010,1300,1560)
replace occ_detail = "Life, physical, social science" if inrange(occ2010,1600,1965)
replace occ_detail = "Legal" if inrange(occ2010,2100,2160)
replace occ_detail = "Arts and entertainment" if inrange(occ2010,2600,2960)
replace occ_detail = "Food service" if inrange(occ2010,4000,4160)
replace occ_detail = "Plant" if inrange(occ2010,4200,4250)
replace occ_detail = "Personal care" if inrange(occ2010,4300,4650)
replace occ_detail = "sales" if inrange(occ2010,4700,4965)
replace occ_detail = "Administrative" if inrange(occ2010,5000,5940)
replace occ_detail = "Farming fishing forestry" if inrange(occ2010,6000,6130)
replace occ_detail = "Construction" if inrange(occ2010,6200,6940)
replace occ_detail = "Install maintain repair" if inrange(occ2010,7000,7630)
replace occ_detail = "Production" if inrange(occ2010,7700,7630)
replace occ_detail = "Transportation" if inrange(occ2010,9000,9750)
replace occ_detail = "Armed services" if inrange(occ2010,9840,9850)

// collapses relevant variables by wtfinl by occ_detail by workerclass
gcollapse (sum) employed unemployed travel laborforce laidoff absent_other total part_time_covid_sum = part_time_covid  part_time_reason_sum = part_time_reason (mean) uhrsworkt ahrsworkt part_time_reason part_time_covid [pweight=wtfinl], by(statefip datem year occ_detail classwkr)

gen month = month(dofm(datem))

save "../derived/state_occ_cps_long.dta", replace

// keep if in education, healthcare, community serive, or protective servies
keep if occ_detail == "Education" | occ_detail == "Healthcare" | occ_detail == "Community serive" |  occ_detail == "Protective service"
replace occ_detail = "_educ" if occ_detail == "Education"
replace occ_detail = "_hlth" if occ_detail == "Healthcare"
replace occ_detail = "_cmty" if occ_detail == "Community serive"
replace occ_detail = "_sfty" if occ_detail == "Protective service"

//////////////////////////

gen laidoff_frac = laidoff / (employed+unemployed)

keep datem year statefip occ_detail classwkr laidoff_frac
reshape wide laidoff_frac, i(datem year statefip occ_detail) j(classwkr)

rename *22 *_private
rename *25 *_federal
rename *27 *_state
rename *28 *_local
rename *30 *_statelocal

keep datem year statefip occ_detail laidoff*
reshape wide laidoff* , i(datem year statefip) j(occ_detail) string

// merge with stateid
rename statefip fips
merge m:1 fips using `stateid', keep(1 3) nogen

order state datem year

tempfile occupations
save `occupations'

***********************************************************************
// merge all data
***********************************************************************

use `state_agg_cps_wide_full', clear
merge 1:1 state datem using `covid_ts', keep(1 3) nogen
merge 1:1 state datem using `occupations', keep(1 2 3) nogen
merge m:1 state year using `rainyday', keep(1 3) nogen
merge m:1 state using `revenueSources', keep(1 3) nogen
merge m:1 state using `state_ffis', keep(1 3) nogen
* merge m:1 state using `state_forecasts', keep(1 3) nogen

save "../derived/muni_covid_combined_data_cps.dta", replace


***********************************************************************
// new variables
***********************************************************************
use "../derived/muni_covid_combined_data_cps.dta", clear

keep if inrange(datem,tm(1983m1),tm(2020m8))

drop if datem == m(2020m4) & missing(covid_relief_treasury)
sort datem pop

gen laidoff_frac_statelocal = (laidoff_statelocal) / (employed_statelocal+unemployed_statelocal)
gen laidoff_frac_private = laidoff_private / (employed_private+unemployed_private)
gen laidoff_frac_state = (laidoff_state) / (employed_state+unemployed_state)
gen laidoff_frac_federal = (laidoff_federal) / (employed_federal+unemployed_federal)
gen laidoff_frac_local = (laidoff_local)/(employed_local+unemployed_local)

xtset fip datem

gen s2_urate_statelocal = s2.urate_statelocal
gen s2_part_time_covid_statelocal = s2.part_time_covid_statelocal
gen s2_laidoff_frac_federal = s2.laidoff_frac_federal

gen s2_laidoff_frac_statelocal = s2.laidoff_frac_statelocal
gen s2_laidoff_frac_private = s2.laidoff_frac_private
gen s2_laidoff_frac_state = s2.laidoff_frac_state
gen s2_laidoff_frac_local = s2.laidoff_frac_local

gen s3_laidoff_frac_statelocal = s3.laidoff_frac_statelocal
gen s3_laidoff_frac_private = s3.laidoff_frac_private
gen s3_laidoff_frac_state = s3.laidoff_frac_state

gen s4_laidoff_frac_statelocal = s4.laidoff_frac_statelocal
gen s4_laidoff_frac_private = s4.laidoff_frac_private
gen s4_laidoff_frac_state = s4.laidoff_frac_state

gen s_laidoff_frac_private = s2_laidoff_frac_private if datem == m(2020m4)
replace s_laidoff_frac_private = s3_laidoff_frac_private if datem == m(2020m5)
replace s_laidoff_frac_private = s4_laidoff_frac_private if datem == m(2020m6)
replace s_laidoff_frac_private = s5.laidoff_frac_private if datem == m(2020m7)
replace s_laidoff_frac_private = s6.laidoff_frac_private if datem == m(2020m8)

gen s2_laidoff_frac_statelocal_hlth = s2.laidoff_frac_statelocal_hlth
gen s2_laidoff_frac_state_hlth = s2.laidoff_frac_state_hlth
replace s2_laidoff_frac_state_hlth = 0 if missing(s2_laidoff_frac_state_hlth) & datem == m(2020m4)

gen s_laidoff_frac_statelocal = s2_laidoff_frac_statelocal if datem == m(2020m4)
replace s_laidoff_frac_statelocal = s3_laidoff_frac_statelocal if datem == m(2020m5)
replace s_laidoff_frac_statelocal = s4_laidoff_frac_statelocal if datem == m(2020m6)
replace s_laidoff_frac_statelocal = s5.laidoff_frac_statelocal if datem == m(2020m7)
replace s_laidoff_frac_statelocal = s6.laidoff_frac_statelocal if datem == m(2020m8)
replace s_laidoff_frac_statelocal = s1.laidoff_frac_statelocal if datem == m(2020m3)
replace s_laidoff_frac_statelocal = 0 if datem == m(2020m2)

gen s_laidoff_frac_state = s2_laidoff_frac_state if datem == m(2020m4)
replace s_laidoff_frac_state = s3_laidoff_frac_state if datem == m(2020m5)
replace s_laidoff_frac_state = s4_laidoff_frac_state if datem == m(2020m6)
replace s_laidoff_frac_state = s5.laidoff_frac_state if datem == m(2020m7)
replace s_laidoff_frac_state = s6.laidoff_frac_state if datem == m(2020m8)

gen pos_pop_ratio_ts = 1000 * (positive / pop)
gen death_pop_ratio_ts = 1000 * (death / pop)
replace death_pop_ratio_ts = 0 if datem == m(2020m3) & state == "Hawaii"

gen l1_pos_pop_ratio_ts = l1.pos_pop_ratio_ts
gen l1_death_pop_ratio_ts = l1.death_pop_ratio_ts

gen logpop = log(pop)
gen invpop = 1 / pop

gen cares_frac_rev_sl = covid_relief_treasury*1e3/(TotalRevenue_StateLocal*1e3)
gen cares_treasury_pc = covid_relief_treasury*1e3 / pop
gen covid_relief_treasury_bn = covid_relief_treasury / 1e6

gen linear_state = cares_treasury_pc < 388
gen flat_state = cares_treasury_pc > 388

gen april = datem == m(2020m4)
gen may = datem == m(2020m5)
gen june = datem == m(2020m6)
gen july = datem == m(2020m7)
gen august = datem == m(2020m8)

gen one = 1

gen state_rainyday_fracExp3 = state_rainyday_fracExp
bys state (datem): replace state_rainyday_fracExp3 = state_rainyday_fracExp3[_n-1] if missing(state_rainyday_fracExp3) & ~missing(state_rainyday_fracExp3[_n-1])

xtile rainy_tri = state_rainyday_fracExp3 if datem == m(2020m4), n(3)
bys state (rainy_tri): replace rainy_tri = rainy_tri[1]

gen rainy_1_salesshare = (rainy_tri==1)*sales_tax_share_rev_State
gen rainy_2_salesshare = (rainy_tri==2)*sales_tax_share_rev_State
gen rainy_3_salesshare = (rainy_tri==3)*sales_tax_share_rev_State

gen rainy_1_cares = (rainy_tri==1)*cares_frac_rev_sl
gen rainy_2_cares = (rainy_tri==2)*cares_frac_rev_sl
gen rainy_3_cares = (rainy_tri==3)*cares_frac_rev_sl

bys datem (pop2019): gen pop_even = floor(_n/2) == _n/2
replace pop_even = 1 if inlist(state_abbrv,"FL","NY","TX","CA")

gen travel_frac_feb = travel_frac if datem == m(2020m2)
bys state (travel_frac_feb): replace travel_frac_feb = travel_frac_feb[1]

// labels
label var s2_laidoff_frac_statelocal "$\Delta$ Muni Laid Off"
label var s2_laidoff_frac_local "$\Delta$ Local Laid Off"
label var s2_laidoff_frac_private "$\Delta$ Private Laid Off"

label var s2_urate_statelocal "$\Delta$ Muni Laid Off"
label var s2_part_time_covid_statelocal "$\Delta$ Muni U/R"
label var s2_laidoff_frac_federal "$\Delta$ Part Time"

label var s2_laidoff_frac_state "$\Delta$ State Laid Off"
label var s3_laidoff_frac_state "$\Delta$ State Laid Off"
label var s4_laidoff_frac_state "$\Delta$ State Laid Off"

label var s2_laidoff_frac_statelocal_hlth "$\Delta$ Muni Healthcare Laid Off"
label var s2_laidoff_frac_state_hlth "$\Delta$ State Healthcare Laid Off"

label var s_laidoff_frac_statelocal "$\Delta$ Muni Laid Off"
label var s_laidoff_frac_state "$\Delta$ State Laid Off"
label var s_laidoff_frac_private "$\Delta$ Private Laid Off"

label var sales_tax_share_rev_State "Sales Tax Exposure"
label var sales_tax_share_rev_StateLocal "Sales Tax Exposure"

label var pos_pop_ratio_ts "COVID Infection Rate"
label var death_pop_ratio_ts "COVID Death Rate"

label var l1_pos_pop_ratio_ts "COVID Infection Rate"
label var l1_death_pop_ratio_ts "COVID Death Rate"

label var logpop "Log Population"
label var invpop "\$Population^{-1}\$"

label var cares_frac_rev_sl "CARES Act Exposure"

label var linear_state "Large State"
label var flat_state "Small State"

label var april "April"
label var may "May"
label var june "June"
label var july "July"
label var august "August"

label var prop_tax_share_rev_StateLocal "Property Tax Exposure"
label var fs_tnf_share_rev_StateLocal "Intergov Exposure"
label var income_tax_share_rev_StateLocal "Income Tax Exposure"

label var state_rainyday_fracExp3 "Rainy Day Fund Exposure"

label var rainy_1_salesshare "Low Rainy \# Sales Tax Exp."
label var rainy_2_salesshare "Med Rainy \# Sales Tax Exp."
label var rainy_3_salesshare "High Rainy \# Sales Tax Exp."

label var rainy_1_cares  "Low Rainy Day \# CARES Act Exposure"
label var rainy_2_cares  "Med Rainy Day \# CARES Act Exposure"
label var rainy_3_cares "High Rainy Day \# CARES Act Exposure"

label var travel_frac_feb "Tourism Employment"

save "../derived/muni_covid_combined_data_cps.dta", replace
