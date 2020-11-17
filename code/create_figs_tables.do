
set linesize 200

***********************************************************************
// FIGURE 1
***********************************************************************

// import derived data
use "../derived/muni_covid_combined_data_cps.dta", clear

// settings
graph set ps fontface "Times New Roman"
graph set eps fontface "Times New Roman"
graph set window fontface "Times New Roman"

local ticks = ""
local x = 5
forval z = 5(1)9{
    local y = 10^(`x')*`z'
    local ticks = "`ticks'`y' "
}
forval x = 6(1)7{
  forval z = 1(1)9{
    local y = 10^(`x')*`z'
    local logy = log(`y')
    local ticks = "`ticks'`y' "
    local logticks = "`logticks'`logy' "
  }
}

//CARES ACT AS FRACTION OF STATE AND LOCAL REVENUE
twoway (scatter cares_frac_rev_sl pop2019 if datem == m(2020m4) & linear_state== 1, mlab(state_abbrv) msymbol(d) mcolor(maroon) mlabcolor(gs3))  ///
	   (scatter cares_frac_rev_sl pop2019 if datem == m(2020m4) & flat_state== 1, mlab(state_abbrv) msymbol(o) mcolor(navy) mlabcolor(gs3)), ///
	ytitle("CARES Funding as Fraction of Revenue") ///
	xtitle("State Population") legend(order(2 "Small States" 1 "Large States") position(5) ring(0)) ///
	xscale(log) xlabel(1000000 "1,000,000" 10000000 "10,000,000") xmticks(`ticks') ///
	scheme(s1color) name(logpop_fs, replace)
graph export "../output/figures/cares_logpop_fs.pdf", replace fontface("Times New Roman")

***********************************************************************
// FIGURE 2
***********************************************************************

// import derived data
use "../derived/muni_covid_combined_data_cps.dta", clear

local pop_cutoff = 3221649
gen high_pop = pop2019 > `pop_cutoff'
gen low_pop = pop2019 < `pop_cutoff'

gen logpop_highpop = logpop*high_pop
gen logpop_lowpop = logpop*low_pop

reg s2_laidoff_frac_statelocal logpop_lowpop high_pop logpop_highpop  if datem == m(2020m4), robust

local slope_highpop: display %-4.3f _b[logpop_highpop]
local slope_lowpop: display %-4.3f _b[logpop_lowpop]
local alpha_all = _b[_cons]
local alpha_high = _b[high_pop]
local se_highpop: display %-4.3f _se[logpop_highpop]
local se_lowpop: display %-4.3f _se[logpop_lowpop]
//test that the lines intersect at the population cutoff point
nlcom _b[high_pop] + (_b[logpop_highpop]-_b[logpop_lowpop])*log(`pop_cutoff')
//p-value = 0.951

//test for difference in slopes
nlcom _b[logpop_highpop]-_b[logpop_lowpop]
//p-value = 0.029

capture drop graph_x graph_y
gen graph_x = 13.26 + (_n/_N)*(17.49-13.26)
gen graph_y = `alpha_all' + _b[logpop_lowpop]*graph_x*(graph_x<log(`pop_cutoff'))  + (`alpha_high'+_b[logpop_highpop]*graph_x)*(graph_x>log(`pop_cutoff'))


//REDUCED FORM LOG WITH BEST FIT LINES
local xtickslog  "`=6*log(10)' `=7*log(10)'"

twoway (scatter s2_laidoff_frac_statelocal logpop if datem == m(2020m4) & linear_state == 1, mlab(state_abbrv) msymbol(d) mcolor(maroon) mlabcolor(gs3))  ///
	   (scatter s2_laidoff_frac_statelocal logpop if datem == m(2020m4) & flat_state == 1, mlab(state_abbrv) msymbol(o) mcolor(navy) mlabcolor(gs3)) ///
	   (line graph_y graph_x if graph_x < 14.98, lcolor(gs8))  ///
	   (line graph_y graph_x if graph_x > log(`pop_cutoff'), lcolor(gs8)), ///
	 	xtitle("State Population") xlabel(`=6*log(10)' "1,000,000" `=7*log(10)' "10,000,000"  ) xmticks(`logticks') xticks(`xtickslog') ///
	 	legend(order(2 "Small States" 1 "Large States") position(5) ring(0)) ///
	 	text(0.09 14.25 "{bf:`slope_lowpop'}", size(small) color(navy)) ///
	 	text(0.083 14.25 "(`se_lowpop')", size(small) color(navy)) ///
	 	text(0.113 16.70 "{bf:`slope_highpop'}", size(small) color(maroon)) ///
	 	text(0.106 16.70 "(`se_highpop')", size(small) color(maroon)) ///
	    ytitle("State and Local Gov Unemployment") scheme(s1color) name(logpop_rf, replace)

graph export "../output/figures/cares_logpop_rf_no_controls.pdf", replace fontface("Times New Roman")

***********************************************************************
// TABLE 2
***********************************************************************

// import derived data
use "../derived/muni_covid_combined_data_cps.dta", clear

tsset

qui reg s2_laidoff_frac_statelocal sales_tax_share_rev_StateLocal  if datem == m(2020m4), robust
estadd local Date "April"
est store T1ca
qui reg s2_laidoff_frac_statelocal sales_tax_share_rev_StateLocal l1_pos_pop_ratio_ts l1_death_pop_ratio_ts logpop s_laidoff_frac_private if datem == m(2020m4), robust
estadd local Date "April"
est store T1cb
qui reg s2_laidoff_frac_statelocal_hlth sales_tax_share_rev_StateLocal l1_pos_pop_ratio_ts l1_death_pop_ratio_ts logpop s_laidoff_frac_private if datem == m(2020m4), robust
estadd local Date "April"
est store T1cc
qui reg s2_laidoff_frac_statelocal sales_tax_share_rev_StateLocal prop_tax_share_rev_StateLocal income_tax_share_rev_StateLocal fs_tnf_share_rev_StateLocal l1_pos_pop_ratio_ts l1_death_pop_ratio_ts logpop s_laidoff_frac_private if datem == m(2020m4), robust
estadd local Date "April"
est store T1cd

esttab T1c*, star(* 0.1 ** 0.05 *** 0.01) stat(Date N r2) label order(sales_tax_share_rev_StateLocal prop_tax_share_rev_StateLocal fs_tnf_share_rev_StateLocal income_tax_share_rev_StateLocal)
esttab T1c* using "../output/tables/muniLaidoffCovidDiff.tex", t(a2)  b(a2)  booktabs alignment(D{.}{.}{-1})  star(* 0.1 ** 0.05 *** 0.01) nogaps style(tex) replace stat(Date N r2, labels("Date" "N" "$ R^2$")) label order(sales_tax_share_rev_StateLocal prop_tax_share_rev_StateLocal fs_tnf_share_rev_StateLocal income_tax_share_rev_StateLocal)

***********************************************************************
// TABLE 3
***********************************************************************

// import derived data
use "../derived/muni_covid_combined_data_cps.dta", clear

capture estimates clear

//REDO OF CARES TABLE
qui reg s2_laidoff_frac_statelocal cares_frac_rev_sl l1_pos_pop_ratio_ts l1_death_pop_ratio_ts if datem == m(2020m4), robust
est store caresIV_ols
qui reg s2_laidoff_frac_statelocal flat_state c.logpop#c.flat_state c.logpop#c.linear_state l1_pos_pop_ratio_ts l1_death_pop_ratio_ts  if datem == m(2020m4), robust
est store caresIV_rf
qui reg cares_frac_rev_sl flat_state c.logpop#c.flat_state c.logpop#c.linear_state l1_pos_pop_ratio_ts l1_death_pop_ratio_ts  if datem == m(2020m4), robust
est store caresIV_fs
qui ivreghdfe s2_laidoff_frac_statelocal l1_pos_pop_ratio_ts l1_death_pop_ratio_ts logpop (cares_frac_rev_sl =  flat_state c.logpop#c.flat_state c.logpop#c.linear_state) if datem == m(2020m4), robust
est store caresIV_iv
qui ivreghdfe s2_laidoff_frac_statelocal_hlth l1_pos_pop_ratio_ts l1_death_pop_ratio_ts logpop (cares_frac_rev_sl =  flat_state c.logpop#c.flat_state c.logpop#c.linear_state) if datem == m(2020m4), robust
est store caresIV_ivHealth
qui reg s2_laidoff_frac_statelocal ibn.rainy_tri l1_pos_pop_ratio_ts l1_death_pop_ratio_ts c.rainy_?_cares if datem == m(2020m4), robust
est store caresIV_IVtriRainy

esttab caresIV_*, label order(cares_frac_rev_sl rainy_?_cares flat_state c.logpop#c.flat_state c.logpop#c.linear_state) stats(N r2 widstat, labels("N" "$\\R^2$" "First-Stage F")) drop(_cons 1.rainy_tri 2.rainy_tri 3.rainy_tri) numbers mtitles("OLS" "RF" "FS" "IV" "IV (Health)" "OLS")
esttab caresIV_* using "../output/tables/cares_laidoff_IV.tex", t(a2)  b(a2) booktabs alignment(D{.}{.}{-1}) star(* 0.1 ** 0.05 *** 0.01) nogaps style(tex) replace stats(N r2 widstat, labels("N" "$ R^2$" "First-Stage F")) label order(cares_frac_rev_sl rainy_?_cares flat_state c.logpop#c.flat_state c.logpop#c.linear_state) drop(_cons 1.rainy_tri 2.rainy_tri 3.rainy_tri) mtitles("OLS" "RF" "FS" "IV" "IV (Health)" "OLS")

***********************************************************************
// TABLE 4 (stateLaidOff_rainyday.tex)
***********************************************************************

// import derived data
use "../derived/muni_covid_combined_data_cps.dta", clear

tsset

//with the nornmal controls
qui reg s2_laidoff_frac_state state_rainyday_fracExp3 l1_pos_pop_ratio_ts l1_death_pop_ratio_ts logpop if datem == m(2020m4) & ~inlist(state_abbrv,"AK","WY"), robust
estadd local Date "April"
est store TS_a
qui reg s2_laidoff_frac_state_hlth state_rainyday_fracExp3  l1_pos_pop_ratio_ts l1_death_pop_ratio_ts logpop if datem == m(2020m4) & ~inlist(state_abbrv,"AK","WY"), robust
estadd local Date "April"
est store TS_b
qui reg s2_laidoff_frac_state state_rainyday_fracExp3 sales_tax_share_rev_State  l1_pos_pop_ratio_ts l1_death_pop_ratio_ts logpop if datem == m(2020m4) & ~inlist(state_abbrv,"AK","WY"), robust
estadd local Date "April"
est store TS_c
qui reg s2_laidoff_frac_state rainy_?_salesshare  l1_pos_pop_ratio_ts l1_death_pop_ratio_ts logpop if datem == m(2020m4) & ~inlist(state_abbrv,"AK","WY"), robust
estadd local Date "April"
est store TS_d

esttab TS_*, stat(N r2) star(* 0.1 ** 0.05 *** 0.01) t(a2)  b(a2) r2(4) label order(state_rainyday_fracExp3 sales_tax_share_rev_State rainy_?_salesshare ) keep(state_rainyday_fracExp3 rainy_?_salesshare sales_tax_share_rev_State)
esttab TS_* using "../output/tables/stateLaidOff_rainyday.tex", t(a2)  b(a2) booktabs alignment(D{.}{.}{-1}) star(* 0.1 ** 0.05 *** 0.01) nogaps style(tex) replace stat(N r2, labels("Date" "N" "$ R^2$")) label order(state_rainyday_fracExp3 sales_tax_share_rev_State rainy_?_salesshare ) keep(state_rainyday_fracExp3 rainy_?_salesshare sales_tax_share_rev_State)
