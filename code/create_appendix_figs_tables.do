***********************************************************************
// FIGURE A1
***********************************************************************

set linesize 200

// import derived data
use "../derived/muni_covid_combined_data_cps.dta", clear

// settings
graph set ps fontface "Times New Roman"
graph set eps fontface "Times New Roman"
graph set window fontface "Times New Roman"

twoway (scatter laidoff_frac_statelocal sales_tax_share_rev_StateLocal if datem == m(2020m4), mlabel(state_abbrv)) (lfit urate_statelocal sales_tax_share_rev_StateLocal if datem == m(2020m4)), ///
scheme(s1mono) legend(off) ytitle("Public Worker Layoffs") name(aprilLaidoff, replace)
graph export "../output/appendix/figures/muni_laidofffrac_salestax_april2020-eps-converted-to.pdf", as(pdf) replace

***********************************************************************
// FIGURE A2
***********************************************************************

// import derived data
use "../derived/muni_covid_combined_data_cps.dta", clear

// settings
graph set ps fontface "Times New Roman"
graph set eps fontface "Times New Roman"
graph set window fontface "Times New Roman"

local labels = ""
forval x = 6(1)7 {
	local y = 10^`x'
	local labels = "`labels'`y' "
}
local vallabel = ""
forval x = 5(1)7{
  local y = 10^`x'
  local vallabel =  `"`vallabel'`y' "10^`x'" "'
}
label define expon `vallabel',replace
label val pop2019 expon

local ticks = ""
local x = 5
forval z = 5(1)9{
    local y = 10^(`x')*`z'
    local ticks = "`ticks'`y' "
}
forval x = 6(1)7{
  forval z = 1(1)9{
    local y = 10^(`x')*`z'
    local ticks = "`ticks'`y' "
  }
}

sort datem pop2019

twoway (scatter covid_relief_treasury_bn pop2019 if pop_even == 0 & datem == m(2020m4), mlab(state_abbrv) mlabpos(11)  mlabangle(0) mstyle(p1) mlabsize(vsmall)) ///
	   (scatter covid_relief_treasury_bn pop2019 if pop_even == 1 & datem == m(2020m4), mlab(state_abbrv) mlabpos(5) mlabangle(0) mstyle(p1) mlabsize(vsmall)), ///
	xtitle("State Population") ///
	ytitle("CARES Act Funding ({c $|}bn)") legend(off) ///
	scheme(s1mono) ylabel(1.25 5 10 15) yscale(range(0 15))
graph export "../output/appendix/figures/cares_formula.pdf", as(pdf) fontface("Times New Roman") replace

***********************************************************************
// TABLE A4
***********************************************************************

// import derived data
use "../derived/muni_covid_combined_data_cps.dta", clear

//R2 wants to see columns 1 and 2 for state only and local only
qui reg s2_laidoff_frac_statelocal sales_tax_share_rev_StateLocal  if datem == m(2020m4), robust
est store TR2a
qui reg s2_laidoff_frac_statelocal sales_tax_share_rev_StateLocal l1_pos_pop_ratio_ts l1_death_pop_ratio_ts logpop s_laidoff_frac_private if datem == m(2020m4), robust
est store TR2b
qui reg s2_laidoff_frac_state sales_tax_share_rev_StateLocal  if datem == m(2020m4), robust
est store TR2c
qui reg s2_laidoff_frac_state sales_tax_share_rev_StateLocal l1_pos_pop_ratio_ts l1_death_pop_ratio_ts logpop s_laidoff_frac_private if datem == m(2020m4), robust
est store TR2d
qui reg s2_laidoff_frac_local sales_tax_share_rev_StateLocal  if datem == m(2020m4), robust
est store TR2e
qui reg s2_laidoff_frac_local sales_tax_share_rev_StateLocal l1_pos_pop_ratio_ts l1_death_pop_ratio_ts logpop s_laidoff_frac_private if datem == m(2020m4), robust
est store TR2f
esttab TR2*, star(* 0.1 ** 0.05 *** 0.01) stat(N r2) label order(sales_tax_share_rev_StateLocal)
esttab TR2* using "../output/appendix/tables/muniLaidoffCovidDiff_state_and_local.tex", t(a2)  b(a2)  booktabs alignment(D{.}{.}{-1})  star(* 0.1 ** 0.05 *** 0.01) nogaps style(tex) replace stat(N r2, labels("N" "$ R^2$")) label order(sales_tax_share_rev_StateLocal)

***********************************************************************
// TABLE A5 (CHECK ISSUE STILL!!!)
***********************************************************************

// import derived data
use "../derived/muni_covid_combined_data_cps.dta", clear

// difference in travel_frac_feb (new mexico for some reason has a different value than originial data??)
sum travel_frac_feb if state_ab=="NM"
replace travel_frac_feb = .01049425 if state_ab=="NM"

// issue with January (chnaged l1 to march specific variable for output)
gen march_pos_pop_ratio_ts = pos_pop_ratio_ts if datem == m(2020m3)
bys state (march_pos_pop_ratio_ts): replace march_pos_pop_ratio_ts = march_pos_pop_ratio_ts[1]

gen march_death_pop_ratio_ts = death_pop_ratio_ts if datem == m(2020m3)
bys state (march_death_pop_ratio_ts): replace march_death_pop_ratio_ts = march_death_pop_ratio_ts[1]

gen march_logpop = logpop if datem == m(2020m3)
bys state (march_logpop): replace march_logpop = march_logpop[1]

label var march_pos_pop_ratio_ts "COVID Infection Rate"
label var march_death_pop_ratio_ts "COVID Death Rate"
label var march_logpop "Log Population"

qui reg s2_laidoff_frac_statelocal sales_tax_share_rev_StateLocal prop_tax_share_rev_StateLocal income_tax_share_rev_StateLocal fs_tnf_share_rev_StateLocal march_pos_pop_ratio_ts march_death_pop_ratio_ts march_logpop s2_laidoff_frac_private travel_frac_feb if datem == m(2020m4), robust
estadd local Date "April"
est sto TRa
qui reg s2_urate_statelocal sales_tax_share_rev_StateLocal prop_tax_share_rev_StateLocal income_tax_share_rev_StateLocal fs_tnf_share_rev_StateLocal march_pos_pop_ratio_ts march_death_pop_ratio_ts march_logpop s2_laidoff_frac_private travel_frac_feb if datem == m(2020m4), robust
estadd local Date "April"
est sto TRb
qui reg s2_part_time_covid_statelocal sales_tax_share_rev_StateLocal prop_tax_share_rev_StateLocal income_tax_share_rev_StateLocal fs_tnf_share_rev_StateLocal march_pos_pop_ratio_ts march_death_pop_ratio_ts march_logpop s2_laidoff_frac_private travel_frac_feb if datem == m(2020m4), robust
estadd local Date "April"
est sto TRc
qui reg s2_laidoff_frac_statelocal sales_tax_share_rev_StateLocal prop_tax_share_rev_StateLocal income_tax_share_rev_StateLocal fs_tnf_share_rev_StateLocal march_pos_pop_ratio_ts march_death_pop_ratio_ts march_logpop s2_laidoff_frac_private travel_frac_feb if datem == m(2020m1), robust
estadd local Date "January"
est sto TRd
qui reg s2_laidoff_frac_federal sales_tax_share_rev_StateLocal prop_tax_share_rev_StateLocal income_tax_share_rev_StateLocal fs_tnf_share_rev_StateLocal march_pos_pop_ratio_ts march_death_pop_ratio_ts march_logpop s2_laidoff_frac_private travel_frac_feb  if datem == m(2020m4), robust
estadd local Date "April"
est sto TRe
esttab TRa TRb TRc TRd TRe, star(* 0.1 ** 0.05 *** 0.01) stat(Date N r2) label order(sales_tax_share_rev_StateLocal prop_tax_share_rev_StateLocal fs_tnf_share_rev_StateLocal income_tax_share_rev_StateLocal)
esttab TRa TRb TRc TRd TRe using "../output/appendix/tables/muniLaidoffRobustness.tex", t(a2)  b(a2)  booktabs alignment(D{.}{.}{-1})  star(* 0.1 ** 0.05 *** 0.01) nogaps style(tex) replace stat(Date N r2, labels("Date" "N" "$ R^2$")) label order(sales_tax_share_rev_StateLocal prop_tax_share_rev_StateLocal fs_tnf_share_rev_StateLocal income_tax_share_rev_StateLocal)


***********************************************************************
// TABLE A6
***********************************************************************

// import derived data
use "../derived/muni_covid_combined_data_cps.dta", clear


qui reg s_laidoff_frac_statelocal c.(april may june july august)#c.(sales_tax_share_rev_StateLocal prop_tax_share_rev_StateLocal income_tax_share_rev_StateLocal fs_tnf_share_rev_StateLocal l1_pos_pop_ratio_ts l1_death_pop_ratio_ts logpop s_laidoff_frac_private one) if inlist(datem,m(2020m4),m(2020m5),m(2020m6),m(2020m7),m(2020m8)), cluster(state)
est store ts_a
// cares
qui reg s_laidoff_frac_statelocal c.(april may june july august)#c.(cares_frac_rev_sl l1_pos_pop_ratio_ts l1_death_pop_ratio_ts one) if inlist(datem,m(2020m4),m(2020m5),m(2020m6),m(2020m7),m(2020m8)), cluster(state)
est store ts_b
//rainy day
qui reg s_laidoff_frac_state c.(april may june july august)#c.(state_rainyday_fracExp3 l1_pos_pop_ratio_ts l1_death_pop_ratio_ts logpop one) if inlist(datem,m(2020m4),m(2020m5),m(2020m6),m(2020m7),m(2020m8)) & ~inlist(state_abbrv,"AK","WY"), cluster(state)
est store ts_c

esttab ts_a ts_b ts_c, label order(c.april#c.sales_tax_share_rev_StateLocal c.may#c.sales_tax_share_rev_StateLocal c.june#c.sales_tax_share_rev_StateLocal c.july#c.sales_tax_share_rev_StateLocal c.august#c.sales_tax_share_rev_StateLocal ///
								   c.april#c.cares_frac_rev_sl c.may#c.cares_frac_rev_sl c.june#c.cares_frac_rev_sl       c.july#c.cares_frac_rev_sl       c.august#c.cares_frac_rev_sl               ///
								   c.april#c.state_rainyday_fracExp3 c.may#c.state_rainyday_fracExp3 c.june#c.state_rainyday_fracExp3 c.july#c.state_rainyday_fracExp3 c.august#c.state_rainyday_fracExp3 ///
	 							   ) ///
							keep(c.april#c.sales_tax_share_rev_StateLocal c.may#c.sales_tax_share_rev_StateLocal c.june#c.sales_tax_share_rev_StateLocal c.july#c.sales_tax_share_rev_StateLocal c.august#c.sales_tax_share_rev_StateLocal ///
								   c.april#c.state_rainyday_fracExp3 c.may#c.state_rainyday_fracExp3 c.june#c.state_rainyday_fracExp3 c.july#c.state_rainyday_fracExp3 c.august#c.state_rainyday_fracExp3 ///
								   c.april#c.cares_frac_rev_sl c.may#c.cares_frac_rev_sl c.june#c.cares_frac_rev_sl       c.july#c.cares_frac_rev_sl       c.august#c.cares_frac_rev_sl              ///
	 							   ) ///
							stats(N r2, labels("N" "$\\R^2$")) numbers star(* 0.1 ** 0.05 *** 0.01)

esttab ts_a ts_b ts_c using "../output/appendix/tables/april_thru_august_combined.tex", replace label order(c.april#c.sales_tax_share_rev_StateLocal c.may#c.sales_tax_share_rev_StateLocal c.june#c.sales_tax_share_rev_StateLocal c.july#c.sales_tax_share_rev_StateLocal c.august#c.sales_tax_share_rev_StateLocal ///
								   c.april#c.cares_frac_rev_sl c.may#c.cares_frac_rev_sl c.june#c.cares_frac_rev_sl       c.july#c.cares_frac_rev_sl       c.august#c.cares_frac_rev_sl                ///
								   c.april#c.state_rainyday_fracExp3 c.may#c.state_rainyday_fracExp3 c.june#c.state_rainyday_fracExp3 c.july#c.state_rainyday_fracExp3 c.august#c.state_rainyday_fracExp3 ///
								    ) ///
							keep(c.april#c.sales_tax_share_rev_StateLocal c.may#c.sales_tax_share_rev_StateLocal c.june#c.sales_tax_share_rev_StateLocal c.july#c.sales_tax_share_rev_StateLocal c.august#c.sales_tax_share_rev_StateLocal ///
								   c.april#c.state_rainyday_fracExp3 c.may#c.state_rainyday_fracExp3 c.june#c.state_rainyday_fracExp3 c.july#c.state_rainyday_fracExp3 c.august#c.state_rainyday_fracExp3 ///
								   c.april#c.cares_frac_rev_sl c.may#c.cares_frac_rev_sl c.june#c.cares_frac_rev_sl       c.july#c.cares_frac_rev_sl       c.august#c.cares_frac_rev_sl                     ///
								   ) ///
							stats(N r2, labels("N" "$ R^2$")) numbers star(* 0.1 ** 0.05 *** 0.01)


***********************************************************************
// TABLE A7
***********************************************************************

use "../derived/reg_data_annual.dta", clear

keep year state q_rdf_frac f1y_log_emp_ces f1y_log_emp_SL f1y_log_emp_S f1y_log_emp_L log_TAX_revenue_SL log_TAX_revenue_S log_TAX_revenue_L

encode state, gen(statei)

label var f1y_log_emp_ces "Private Emp."
label var f1y_log_emp_SL "State and Local Emp."
label var f1y_log_emp_S "State Emp."
label var f1y_log_emp_L "Local Emp."
label var log_TAX_revenue_SL "log State and Local Tax Revenue"
rename log_TAX_revenue_S log_TAX_state_revenue
label var log_TAX_state_revenue "log State Tax Revenue"
label var log_TAX_revenue_L "log Local Tax Revenue"

recode q_rdf_frac (1 = 3) (2 = 1) (3 = 2), gen(q_rdf_frac2)
label define tri 1 "Low Rainy Day" 2 "Med Rainy Day" 3 "High Rainy Day"
label values q_rdf_frac2 tri

qui reghdfe f1y_log_emp_ces log_TAX_revenue_SL, absorb(i.statei#c.year statei year) cluster(year statei)
est sto r1_privemp_SL_T

qui reghdfe f1y_log_emp_SL log_TAX_revenue_SL, absorb(i.statei#c.year statei year) cluster(year statei)
est sto r1_emp_SL_T

qui reghdfe f1y_log_emp_S log_TAX_state_revenue, absorb(i.statei#c.year statei year) cluster(year statei)
est sto r1_emp_S_T

qui reghdfe f1y_log_emp_S c.log_TAX_state_revenue##ib3.q_rdf_frac2, absorb(i.statei#c.year statei year) cluster(year statei)
est sto r1_emp_S_T_rdf

qui reghdfe f1y_log_emp_L log_TAX_revenue_L, absorb(i.statei#c.year statei year) cluster(year statei)
est sto r1_emp_L_T

esttab r1_privemp_SL_T r1_emp_SL_T r1_emp_S_T r1_emp_S_T_rdf r1_emp_L_T, star(* 0.1 ** 0.05 *** 0.01) stat(N r2_within) label ///
		 drop(_cons 1.q_rdf_frac2 2.q_rdf_frac2 3.q_rdf_frac2* ) order(log_TAX_revenue_SL log_TAX_state_revenue log_TAX_revenue_L)
		 
esttab r1_privemp_SL_T r1_emp_SL_T r1_emp_S_T r1_emp_S_T_rdf r1_emp_L_T using "../output/appendix/tables/muniEmployPanel.tex", replace  t(a2) b(a2) booktabs alignment(D{.}{.}{-1}) star(* 0.1 ** 0.05 *** 0.01) ///
		nogaps style(tex) stat(N r2_within) label drop(1.q_rdf_frac2 2.q_rdf_frac2 3.q_rdf_frac2*) order(log_TAX_revenue_SL log_TAX_state_revenue log_TAX_revenue_L)
