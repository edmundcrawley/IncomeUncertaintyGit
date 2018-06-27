* do file that creates household income and consumption residuals
// clear workspace
clear 
cap log close
est clear
clear matrix 
clear mata 
graph drop _all
macro drop _all
set more off, permanently

set matsize 11000
set maxvar  32767
// label this run for logging etc
global run = "first_try"

global five_percent_sample = 0

global level = 1  // Choose 0 for logs and 1 for levels
global labor_inc_only = 1	

if $labor_inc_only==0 {
	global inc_type = "inc"
}
else {
	global inc_type = "linc"
}

//Set Paths
global rawdata = "E:\ProjektDB\706172\Workdata\706172\Husholdningsprojekt\Precautionary saving with time varying risk"
global savedirectory = "E:\ProjektDB\706172\Workdata\706172\Husholdningsprojekt\Precautionary saving with time varying risk\edmund\BPP\save"
global dofiles = "E:\ProjektDB\706172\Workdata\706172\Husholdningsprojekt\Precautionary saving with time varying risk\edmund\BPP\dofiles"
global logfiles = "E:\ProjektDB\706172\Workdata\706172\Husholdningsprojekt\Precautionary saving with time varying risk\edmund\BPP\logfiles"

// log and load family level data
log using "${logfiles}\${run}3", text replace
if $five_percent_sample == 1 {
	use  "${savedirectory}\family_firm_5percent_sample.dta"
}
else{
	use  "${rawdata}\family_firm.dta"
	/*
	//make a 5% sample
	gen random = runiform()
	bys familie_id: replace random = random[1]
	drop if random>0.05
	drop random
	save "${savedirectory}\family_firm_5percent_sample.dta", replace
	*/
}


* merge in inflation index (PRISS112)
merge m:1 year  using "${rawdata}\PRISS112.dta", nogenerate

//  Now start cleaning and creating residuals

* drop entries for families that appear more than once for a year (not many of these)
bysort familie_id year: gen foo = _N
drop if foo>1
drop foo
* Define head of household as the person who earns the most
bysort familie_id: egen tot_inc_1 = total(perindkialt_13_1/PRISS112*100)
bysort familie_id: egen tot_inc_2 = total(perindkialt_13_2/PRISS112*100)
gen byte head = 1
replace head=2 if tot_inc_2>tot_inc_1 & tot_inc_2!=.
drop tot_inc_1 tot_inc_2
* Set as panel data
xtset familie_id year

// make 'head of household' variables for selected variables
global head_list_numeric = "socio13 koen pstill alder persbrc indkefterskat" //"alder arledgr hfaudd cvrnr koen perindkialt_13  pnr pstill socio13"
foreach var in $head_list_numeric {
	gen double `var'_head = .
	replace `var'_head = `var'_1 if head==1
	replace `var'_head = `var'_2 if head==2
}

/// Calculate URE, NNP //////////////////////////////////////////////////////////
// URE = Y-C+A-L where C includes interest payments and A and L are the maturing parts of assets
// Do this all as means accross observed period
bys familie_id: egen mean_inc_after_tax = mean(famindkefterskat/PRISS112*100) if year>=2009
bys familie_id: egen mean_cons_with_interest = mean((famforbrug1+ famrentudgpr)/PRISS112*100) if year>=2009
// assume mortgages held as assets have average maturity of 5y (fampantakt)
// assume bonds held have average maturity of 5y (famoblakt) 
// assume bank deposits are floating (fambankakt)
bys familie_id: egen mean_maturing_assets = mean((famoblakt/5 +   fampantakt/5 + fambankakt)/PRISS112*100) if year>=2009
// assume bank debt is floating (fambankgaeld)
// mortgage refinance already calculated
// assume other debt has maturity 5 years (famqpassivn -fambankgaeld -famoblgaeld -fampantgaeld )
bys familie_id: egen mean_maturing_liabiliies = mean((fambankgaeld + mortgage_refinance_1y +(famqpassivn -fambankgaeld -famoblgaeld -fampantgaeld )/5)/PRISS112*100) if year>=2009
gen URE = mean_inc_after_tax - mean_cons_with_interest + mean_maturing_assets - mean_maturing_liabiliies
bys familie_id: egen mean_nominal_assets = mean((famoblakt + fampantakt + fambankakt)/PRISS112*100) if year>=2009
bys familie_id: egen mean_nominal_liabilities = mean(famqpassivn/PRISS112*100) if year>=2009
gen NNP = mean_nominal_assets-mean_nominal_liabilities

* Descriptive statistics *
**************************

gen alder_head_08 = alder_head if year==2008
replace alder_head_08 = alder_head + 6 if year == 2002
replace alder_head_08 = alder_head + 5 if year == 2003
replace alder_head_08 = alder_head + 4 if year == 2004
replace alder_head_08 = alder_head + 3 if year == 2005
replace alder_head_08 = alder_head + 2 if year == 2006
replace alder_head_08 = alder_head + 1 if year == 2007
replace alder_head_08 = alder_head - 1 if year == 2009
replace alder_head_08 = alder_head - 2 if year == 2010
replace alder_head_08 = alder_head - 3 if year == 2011
replace alder_head_08 = alder_head - 4 if year == 2012
replace alder_head_08 = alder_head - 5 if year == 2013
replace alder_head_08 = alder_head - 6 if year == 2014
replace alder_head_08 = alder_head - 7 if year == 2015

gen est_sample = alder_head_08 >= 30 & alder_head_08 <= 55 

///Calculate percentiles of URE, NNP and Income for age group between 30 and 55



///////////////////////////////////////////////////////////////////////////////////////

bysort familie_id: egen inc_perm = mean(famindkefterskat/PRISS112*100)

* Specifically for labor income, a spouse variable is included
gen indkefterskat_head_adj = (indkefterskat_head/PRISS112*100)/inc_perm
gen double indkefterskat_spouse_adj = .
replace indkefterskat_spouse_adj = (indkefterskat_2/PRISS112*100)/inc_perm if head == 1 & alder_2!=.
replace indkefterskat_spouse_adj = (indkefterskat_1/PRISS112*100)/inc_perm if head == 2


global head_list_string = "civst funk industry_agg industry_level4 work_function_agg work_function_level4"
foreach var in $head_list_string {
	gen `var'_head = ""
	replace `var'_head = `var'_1 if head==1
	replace `var'_head = `var'_2 if head==2
}


gen int family_size = 1+(alder_2!=.)+famantboernf
/* We don't use these categories anymore. Can remove code if everything works
gen agegroup = .
replace agegroup = 29 if alder_head < 30
replace agegroup = 30 if alder_head >= 30 & alder_head < 40
replace agegroup = 40 if alder_head >= 40 & alder_head < 50
replace agegroup = 50 if alder_head >= 50 & alder_head < 60
replace agegroup = 60 if alder_head >= 60 & alder_head < 70
replace agegroup = 70 if alder_head >= 70 & alder_head !=.

gen emp_status_head = ""
replace emp_status_head = "7 Unemployed" if pstill_head == 40
replace emp_status_head = "8 Outside labor force" if (pstill_head >=41 & pstill_head <=57 ) | (pstill_head >= 91 & pstill_head <= 98)
replace emp_status_head = "6 Self employed etc" if pstill_head >=1 & pstill_head <=20
replace emp_status_head = "1 Top level manager" if pstill_head == 31 
replace emp_status_head = "2 Upper level wage earners" if pstill_head == 32
replace emp_status_head = "3 Mid level wage earners" if pstill_head == 34
replace emp_status_head = "4 Lower level wage earners" if pstill_head == 35 
replace emp_status_head = "5 Other wage earners" if pstill_head == 36 | pstill_head == 37 
*/
cap drop highest_educ
gen highest_educ = ""
replace highest_educ = "Primary" if famhoejstudda / 1000 < 2 & famhoejstudda / 1000 > 0 
replace highest_educ = "Upper secondary" if famhoejstudda / 1000 < 3 & famhoejstudda / 1000 >= 2 
replace highest_educ = "Vocational" if famhoejstudda / 1000 < 4 & famhoejstudda / 1000 >= 3
replace highest_educ = "Short cycle" if famhoejstudda / 1000 < 5 & famhoejstudda / 1000 >= 4
replace highest_educ = "BA" if famhoejstudda / 1000 <= 6.1 & famhoejstudda / 1000 >= 5
replace highest_educ = "Masters +" if famhoejstudda / 1000 < 7.1 & famhoejstudda / 1000 >= 6.5

// Do we have race dummies? Do we need them?

// Is there a less fine region dummy than 'kom'

// Employment - use pstill (is there a less fine version?)

egen marital_status = group(civst_head)
// gender of single households. Couples are '3'
gen gender = koen_head
replace gender = 3 if alder_2!=.
// need dummy for homeowner as consumption measure will vary with this
gen homeowner = famboligvaerdi>0 & famboligvaerdi!=.

gen fam_lincome = indkefterskat_1
replace fam_lincome = indkefterskat_1+indkefterskat_2 if alder_head<.

if $level == 0 {
// log family income adjusted for inflation
	if $labor_inc_only==0 {
		gen log_cons_adj = log(famforbrug1) - log(PRISS112)
		gen log_fam_inc_adj = log(famindkefterskat) - log(PRISS112)
	}
	else {
		gen log_cons_adj = log(famforbrug1) - log(PRISS112)
		gen log_fam_inc_adj = log(fam_lincome) - log(PRISS112)
	}
}
else {
	if $labor_inc_only==0 {
		gen log_cons_adj = (famforbrug1/PRISS112*100)/inc_perm
		gen log_fam_inc_adj = (famindkefterskat/PRISS112*100)/inc_perm
	}
	else {
		gen log_cons_adj = (famforbrug1/PRISS112*100)/inc_perm
		gen log_fam_inc_adj = (fam_lincome/PRISS112*100)/inc_perm
	}
}

//Here is where we throw out problem observations and outliers
keep if selvst != 1	//self employed in family (2,029,483 observations deleted)
keep if ikkeskattepligtig != 1 //not full Danish tax resident (680,972 observations deleted)
keep if ejerskift!=1	// no real estate transaction in year (messes up consumption measure) (1,245,332 observations deleted)
keep if famforbrug1>0.01	//has positive consumption (959,522 observations deleted)
keep if famindkefterskat>0.01	//has positive income (85,136 observations deleted)
//drop top and bottom percentiles in CHANGES of income and consumption
global drop_top = "99.0"
global drop_bottom = "1.0"
gen d_log_fam_inc_adj = d.log_fam_inc_adj
gen d_log_cons_adj = d.log_cons_adj
bysort year: egen pincome_top = pctile(log_fam_inc_adj), p(${drop_top})
bysort year: egen pincome_bottom = pctile(log_fam_inc_adj), p(${drop_bottom})
bysort year: egen pconsumption_top = pctile(log_cons_adj), p(${drop_top})
bysort year: egen pconsumption_bottom = pctile(log_cons_adj), p(${drop_bottom})
gen non_extreme_income = log_fam_inc_adj<pincome_top & log_fam_inc_adj>pincome_bottom
gen non_extreme_consumption = log_cons_adj<pconsumption_top & log_cons_adj>pconsumption_bottom
drop if log_cons_adj==.
keep if non_extreme_income
keep if non_extreme_consumption

//egen emp_status_head_factor = group(emp_status_head)
//replace emp_status_head_factor = 10000 if emp_status_head_factor==.
egen highest_educ_factor = group(highest_educ)
replace highest_educ_factor = 10000 if highest_educ_factor==.
egen region_factor = group(region)
replace region_factor = 10000 if region_factor==.
//egen industry_agg_head_factor = group(industry_agg_head)
//replace industry_agg_head_factor = 10000 if industry_agg_head_factor==.
//egen industry_level4_head_factor =group(industry_level4_head)
//replace industry_level4_head_factor = 10000 if industry_level4_head_factor==.
//egen work_function_level4_head_factor=group(work_function_level4_head)
//replace work_function_level4_head_factor = 10000 if work_function_level4_head_factor==.
//replace pstill_head = 10000 if pstill_head==.

// file "${savedirectory}\inc_cons_residuals.dta" saved at this point

//reduce number of factors in some variables
//**************Could reduce work_function_level4_head_factor and industry_level4_head_factor in this way too (but they are not intereacted with time)
//cap drop group_count
//bysort industry_agg_head_factor: gen group_count = _N
//replace industry_agg_head_factor=11000 if group_count<50000  & industry_agg_head_factor!=10000
//cap drop group_count
bysort region_factor: gen group_count = _N
replace region_factor=11000 if group_count<50000  & region_factor!=10000

//Now do regressions to get income and consumption residuals
//global regressors = "i.year i.alder_head i.pstill_head i.gender#i.year i.marital_status#i.year i.emp_status_head_factor#i.year i.highest_educ_factor#i.year i.kom i.region_factor#i.year i.family_size i.homeowner#i.year i.industry_agg_head_factor#i.year i.industry_level4_head_factor i.work_function_level4_head_factor"
//global regressors_w_lag = "i.year i.alder_head i.pstill_head i.gender#i.year i.marital_status#i.year i.emp_status_head_factor#i.year i.highest_educ_factor#i.year i.kom i.region_factor#i.year i.family_size i.homeowner#i.year i.industry_agg_head_factor#i.year i.industry_level4_head_factor i.work_function_level4_head_factor i.L.pstill_head i.L.marital_status#i.year i.L.emp_status_head_factor#i.year i.L.kom i.L.family_size i.L.industry_agg_head_factor#i.year i.L.industry_level4_head_factor i.L.work_function_level4_head_factor"

global regressors = "i.year i.alder_head i.gender#i.year i.marital_status#i.year i.highest_educ_factor#i.year i.kom i.region_factor#i.year i.family_size i.homeowner#i.year  "

//Do we want to regress levels or growth here?
xtset
reg log_fam_inc_adj $regressors
predict unpredictable_income if e(sample), residuals
reg log_cons_adj $regressors
predict unpredictable_consumption if e(sample), residuals
gen delta_log_y = D.unpredictable_income
gen delta_log_c = D.unpredictable_consumption

if $level == 1 {
reg indkefterskat_head_adj $regressors 
predict unpredictable_lincome_head if e(sample), residuals
reg indkefterskat_spouse_adj $regressors 
predict unpredictable_lincome_spouse if e(sample), residuals
gen delta_lincome_head = D.unpredictable_lincome_head
gen delta_lincome_spouse = D.unpredictable_lincome_spouse
// level of car wealth is bilvaerdi. The change in car wealth (car_spending) includes depreciation
replace bilvaerdi = 0 if bilvaerdi==.
gen car_spending = (d.bilvaerdi/PRISS112*100)/inc_perm
reg car_spending $regressors 
predict unpredictable_car_spending if e(sample), residuals
gen delta_car_spending = D.unpredictable_car_spending
replace delta_car_spending =. if abs(delta_car_spending)>2 //eliminate 2x income spending on cars
gen delta_c_0nocar = delta_log_c - 0.0*delta_car_spending	//this is so that the sample is comparable with the car sample
gen delta_c_nocar = delta_log_c - delta_car_spending
gen delta_c_nodurableproxy = delta_log_c - delta_car_spending/0.421 //this assumes cars as42.1% of durable spending. From national accounts
}

/*
reg d_log_fam_inc_adj $regressors
predict delta_log_y if e(sample), residuals
reg d_log_cons_adj $regressors
predict delta_log_c if e(sample), residuals
*/

//drop top and bottom percentiles of CHNAGES in income and consumption
global drop_top_change = "99.0"
global drop_bottom_change = "1.0"
bysort year: egen pincome_top_change = pctile(delta_log_y), p(${drop_top_change})
bysort year: egen pincome_bottom_change = pctile(delta_log_y), p(${drop_bottom_change})
bysort year: egen pconsumption_top_change = pctile(delta_log_c), p(${drop_top_change})
bysort year: egen pconsumption_bottom_change = pctile(delta_log_c), p(${drop_bottom_change})
gen non_extreme_income_change = delta_log_y<pincome_top_change & delta_log_y>pincome_bottom_change
gen non_extreme_consumption_change = delta_log_c<pconsumption_top_change & delta_log_c>pconsumption_bottom_change
drop if delta_log_c==. //Need to check why we drop so many (11million) observations here
keep if non_extreme_income_change
keep if non_extreme_consumption_change

xtset

// Save the residuals
if $level == 0 {
	if $five_percent_sample == 1 {
	save "${savedirectory}\\${inc_type}_cons_residuals_5percent_sample.dta", replace
	}
	else{
	save "${savedirectory}\\${inc_type}_cons_residuals.dta", replace
	}
}

else {
rename log_cons_adj cons_adj
rename log_fam_inc_adj fam_inc_adj
rename delta_log_y delta_y
rename delta_log_c delta_c
rename d_log_fam_inc_adj d_fam_inc_adj
rename d_log_cons_adj d_cons_adj

	if $five_percent_sample == 1 {
	save "${savedirectory}\\${inc_type}_cons_residuals_levels_5percent_sample.dta", replace
	}
	else{
	save "${savedirectory}\\${inc_type}_cons_residuals_levels.dta", replace
	}
}


log close

