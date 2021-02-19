global savedirectory = "E:\ProjektDB\706172\Workdata\706172\Husholdningsprojekt\Precautionary saving with time varying risk\edmund\BPP\save"

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
*log using "${logfiles}\nostocks", text replace
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


* Correct missing regions in 2006
preserve
keep if year == 2005
keep familie_id region
rename region region2005
save "${rawdata}\region2005.dta", replace
restore

preserve
keep if year == 2007
keep familie_id region
rename region region2007
save "${rawdata}\region2007.dta", replace
restore

merge m:1 familie_id  using "${rawdata}\region2005.dta", nogenerate
merge m:1 familie_id  using "${rawdata}\region2007.dta", nogenerate
replace region = region2005 if region == "." & year == 2006
replace region = region2007 if region == "." & year == 2006
drop region2005 region2007


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
bys familie_id: egen mean_inc_after_tax_2009 = mean(famindkefterskat/PRISS112*100) if year>=2009	//for URE calculation need numbers from 2009 onward
gen famforbrug1_a = famforbrug1 if ejerskift!=1 & selvst != 1 & ikkeskattepligtig != 1 & famforbrug1>=0.01
bys familie_id: egen mean_cons_with_interest = mean((famforbrug1_a+ famrentudgpr)/PRISS112*100) if year>=2009
// assume mortgages held as assets have average maturity of 5y (fampantakt)
// assume bonds held have average maturity of 5y (famoblakt) 
// assume bank deposits are floating (fambankakt)
bys familie_id: egen mean_maturing_assets = mean((famoblakt/5 +   fampantakt/5 + fambankakt)/PRISS112*100) if year>=2009
// assume bank debt is floating (fambankgaeld)
// mortgage refinance already calculated, divide by 2 as rates change once every 6 months
// assume other debt has maturity 5 years (famqpassivn -fambankgaeld -famoblgaeld -fampantgaeld )
bys familie_id: egen mean_maturing_liabiliies = mean((fambankgaeld + mortgage_refinance_1y/2 +(famqpassivn -fambankgaeld -famoblgaeld -fampantgaeld )/5)/PRISS112*100) if year>=2009
gen URE = mean_inc_after_tax_2009 - mean_cons_with_interest + mean_maturing_assets - mean_maturing_liabiliies
bys familie_id: egen mean_nominal_assets = mean((famoblakt + fampantakt + fambankakt)/PRISS112*100) if year>=2009
bys familie_id: egen mean_nominal_liabilities = mean(famqpassivn/PRISS112*100) if year>=2009
gen NNP = mean_nominal_assets-mean_nominal_liabilities

bys familie_id: egen mean_inc_after_tax = mean(famindkefterskat/PRISS112*100)
bys familie_id: egen mean_consumption = mean(famforbrug1/PRISS112*100) if ejerskift!=1 & selvst != 1 & ikkeskattepligtig != 1 & famforbrug1>=0
bys familie_id: egen mean_consumption_2009 = mean(famforbrug1/PRISS112*100) if ejerskift!=1 & selvst != 1 & ikkeskattepligtig != 1 & famforbrug1>=0 & year >=2009
bys familie_id: egen sd_inc_after_tax = sd(famindkefterskat/PRISS112*100)


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

gen homeowner = famboligvaerdi>0 & famboligvaerdi!=.
gen liquidassets_adj = fambankakt/PRISS112*100

xtile dec_URE = URE if est_sample==1, nq(10) 
xtile dec_NNP = NNP if est_sample==1, nq(10) 
xtile dec_cons = mean_consumption_2009 if est_sample==1, nq(10) 
xtile dec_inc = mean_inc_after_tax_2009 if est_sample==1, nq(10) 
egen cat_age = cut(alder_head_08)  , at(29, 35, 40, 45, 50, 56) icodes
replace cat_age = cat_age + 1

bysort familie_id: egen inc_perm = mean(famindkefterskat/PRISS112*100)
gen inc_adj = famindkefterskat/PRISS112*100

gen liquid_to_income = (fambankakt/PRISS112)/inc_perm*100
gen netwealth_adj = famnyformue_net/PRISS112*100 

xtile dec_netwealth_adj = netwealth_adj if est_sample==1, nq(10) 
xtile dec_liquid = liquidassets_adj if est_sample==1, nq(10) 
xtile dec_liquid_to_income = liquid_to_income if est_sample==1, nq(10) 

gen highest_educ = ""
replace highest_educ = "Primary" if famhoejstudda / 1000 < 2 & famhoejstudda / 1000 > 0 
replace highest_educ = "Upper secondary" if famhoejstudda / 1000 < 3 & famhoejstudda / 1000 >= 2 
replace highest_educ = "Vocational" if famhoejstudda / 1000 < 4 & famhoejstudda / 1000 >= 3
replace highest_educ = "Short cycle" if famhoejstudda / 1000 < 5 & famhoejstudda / 1000 >= 4
replace highest_educ = "BA" if famhoejstudda / 1000 <= 6.1 & famhoejstudda / 1000 >= 5
replace highest_educ = "Masters +" if famhoejstudda / 1000 < 7.1 & famhoejstudda / 1000 >= 6.5

gen higher_educ = 0 if highest_educ != ""
replace higher_educ = 1 if highest_educ == "BA" | highest_educ == "Masters +"
gen carowner = bilvaerdi > 0 & bilvaerdi !=.



/*egen dec_URE = cut(ure)  , at(-1000000000, -1039710.8, -643721.94, -408146.38, -248926.75, -133397.73, -49292.805, 5872.418, 75810.563, 269715.03,10000000000) icodes // Cutoffs defined in "create_residuals.do" (based on full population aged 30-55)
egen dec_NNP = cut(nnp)  , at(-1000000000, -2496762.8, -1763238.8, -1314953.9, -936498.06, -586963, -292605, -105299.98, -2986.3457, 98530.328,10000000000) icodes
egen dec_cons = cut(mean_consumption)  , at(-1000000000, 158837.2, 198269.78, 240233.03, 291810, 350603.28, 406463.88, 459949.5, 521918.44, 630560,10000000000) icodes

replace dec_URE = dec_URE+1
replace dec_NNP = dec_NNP+1
replace dec_cons = dec_cons+1
*/
	matrix URE_decile_stats = J(10,6,.)
	matrix NNP_decile_stats = J(10,6,.)
	matrix con_decile_stats = J(10,6,.)
	matrix inc_decile_stats = J(10,6,.)
	matrix netwealth_decile_stats = J(10,6,.)
	matrix liquid_decile_stats = J(10,6,.)
	matrix liquidtoinc_decile_stats = J(10,6,.)
	matrix liquid_decile_stats_1 = J(10,12,.)
	
	
	
	forvalues i= 1(1)10 {
	sum liquidassets_adj if dec_URE==`i', d
	matrix URE_decile_stats[`i',1] = r(p50)
	matrix URE_decile_stats[`i',4] = r(mean)
	sum netwealth_adj if dec_URE==`i', d
	matrix URE_decile_stats[`i',2] = r(p50)
	matrix URE_decile_stats[`i',5] = r(mean)
	sum liquidassets_adj if dec_NNP==`i', d
	matrix NNP_decile_stats[`i',1] = r(p50)
	matrix NNP_decile_stats[`i',4] = r(mean)	
	sum netwealth_adj if dec_NNP==`i', d
	matrix NNP_decile_stats[`i',2] = r(p50)
	matrix NNP_decile_stats[`i',5] = r(mean)
	sum liquidassets_adj if dec_cons==`i', d
	matrix con_decile_stats[`i',1] = r(p50)
	matrix con_decile_stats[`i',4] = r(mean)
	sum netwealth_adj if dec_cons==`i', d
	matrix con_decile_stats[`i',2] = r(p50)
	matrix con_decile_stats[`i',5] = r(mean)
	sum liquid_to_income if dec_URE==`i', d
	matrix URE_decile_stats[`i',3] = r(p50)
	matrix URE_decile_stats[`i',6] = r(mean)
	sum liquid_to_income if dec_NNP==`i', d
	matrix NNP_decile_stats[`i',3] = r(p50)
	matrix NNP_decile_stats[`i',6] = r(mean)
	sum liquid_to_income if dec_cons==`i', d
	matrix con_decile_stats[`i',3] = r(p50)
	matrix con_decile_stats[`i',6] = r(mean)
	
	sum liquidassets_adj if dec_inc==`i', d
	matrix inc_decile_stats[`i',1] = r(p50)
	matrix inc_decile_stats[`i',4] = r(mean)
	sum netwealth_adj if dec_inc==`i', d
	matrix inc_decile_stats[`i',2] = r(p50)
	matrix inc_decile_stats[`i',5] = r(mean)
	sum liquid_to_income if dec_inc==`i', d
	matrix inc_decile_stats[`i',3] = r(p50)
	matrix inc_decile_stats[`i',6] = r(mean)
	
	sum liquidassets_adj if dec_netwealth_adj==`i', d
	matrix netwealth_decile_stats[`i',1] = r(p50)
	matrix netwealth_decile_stats[`i',4] = r(mean)
	sum netwealth_adj if dec_netwealth_adj==`i', d
	matrix netwealth_decile_stats[`i',2] = r(p50)
	matrix netwealth_decile_stats[`i',5] = r(mean)
	sum liquid_to_income if dec_netwealth_adj==`i', d
	matrix netwealth_decile_stats[`i',3] = r(p50)
	matrix netwealth_decile_stats[`i',6] = r(mean)

	sum liquidassets_adj if dec_liquid==`i', d
	matrix liquid_decile_stats[`i',1] = r(p50)
	matrix liquid_decile_stats[`i',4] = r(mean)
	sum netwealth_adj if dec_liquid==`i', d
	matrix liquid_decile_stats[`i',2] = r(p50)
	matrix liquid_decile_stats[`i',5] = r(mean)
	sum liquid_to_income if dec_liquid==`i', d
	matrix liquid_decile_stats[`i',3] = r(p50)
	matrix liquid_decile_stats[`i',6] = r(mean)
	
	sum liquidassets_adj if dec_liquid_to_income==`i', d
	matrix liquidtoinc_decile_stats[`i',1] = r(p50)
	matrix liquidtoinc_decile_stats[`i',4] = r(mean)
	sum netwealth_adj if dec_liquid_to_income==`i', d
	matrix liquidtoinc_decile_stats[`i',2] = r(p50)
	matrix liquidtoinc_decile_stats[`i',5] = r(mean)
	sum liquid_to_income if dec_liquid_to_income==`i', d
	matrix liquidtoinc_decile_stats[`i',3] = r(p50)
	matrix liquidtoinc_decile_stats[`i',6] = r(mean)
	
	* Additional stats for external validity analysis
	sum liquidassets_adj if dec_liquid==`i', d
	matrix liquid_decile_stats_1[`i',1] = r(mean)
	sum netwealth_adj if dec_liquid==`i', d
	matrix liquid_decile_stats_1[`i',2] = r(mean)
	sum liquid_to_income if dec_liquid==`i', d
	matrix liquid_decile_stats_1[`i',3] = r(mean)
	sum mean_inc_after_tax if dec_liquid==`i', d
	matrix liquid_decile_stats_1[`i',4] = r(mean) 
	sum sd_inc_after_tax if dec_liquid==`i', d
	matrix liquid_decile_stats_1[`i',5] = r(mean)
	sum alder_head_08 if dec_liquid==`i', d
	matrix liquid_decile_stats_1[`i',6] = r(mean)
	sum homeowner if dec_liquid==`i', d
	matrix liquid_decile_stats_1[`i',7] = r(mean)
	sum carowner if dec_liquid==`i', d
	matrix liquid_decile_stats_1[`i',8] = r(mean)
	sum URE if dec_liquid==`i', d
	matrix liquid_decile_stats_1[`i',9] = r(mean)
	sum NNP if dec_liquid==`i', d
	matrix liquid_decile_stats_1[`i',10] = r(mean)
	sum mean_nominal_liabilities if dec_liquid==`i', d
	matrix liquid_decile_stats_1[`i',11] = r(mean) 
	sum higher_educ if dec_liquid==`i', d
	matrix liquid_decile_stats_1[`i',12] = r(mean)
	}
		
	matrix cat_age_stats = J(5,6,.)
	forvalues i= 1(1)5 {
	sum liquidassets_adj if cat_age==`i', d
	matrix cat_age_stats[`i',1] = r(p50)
	matrix cat_age_stats[`i',4] = r(mean)
	sum netwealth_adj if cat_age==`i', d
	matrix cat_age_stats[`i',2] = r(p50)
	matrix cat_age_stats[`i',5] = r(mean)
	sum liquid_to_income if cat_age==`i', d
	matrix cat_age_stats[`i',3] = r(p50)
	matrix cat_age_stats[`i',6] = r(mean)
	
	}
		



foreach var in URE_decile_stats NNP_decile_stats con_decile_stats inc_decile_stats netwealth_decile_stats liquid_decile_stats liquidtoinc_decile_stats cat_age_stats {
	svmat double `var'
	rename `var'1  liquidassets_adj_p50
	rename `var'2  netwealth_adj_p50
	rename `var'3  liquid_to_perm_p50
	rename `var'4  liquidassets_adj_mean
	rename `var'5  netwealth_adj_mean
	rename `var'6  liquid_to_perm_mean
	
	outsheet liquidassets_adj_p50 netwealth_adj_p50 liquid_to_perm_p50 liquidassets_adj_mean netwealth_adj_mean liquid_to_perm_mean using "${savedirectory}\\`var'1.txt" if liquidassets_adj_p50<., replace comma
	
	drop liquidassets_adj_p50 netwealth_adj_p50 liquid_to_perm_p50 liquidassets_adj_mean netwealth_adj_mean liquid_to_perm_mean
	
}
		/*
	svmat double URE_decile_stats
	outsheet URE_decile_stats* using "${savedirectory}\\URE_decile_stats1.txt" if URE_decile_stats1<., replace comma nonames
	svmat double NNP_decile_stats
	outsheet NNP_decile_stats* using "${savedirectory}\\NNP_decile_stats1.txt" if NNP_decile_stats1<., replace comma nonames
	svmat double con_decile_stats
	outsheet con_decile_stats* using "${savedirectory}\\con_decile_stats1.txt" if con_decile_stats1<., replace comma nonames
	svmat double inc_decile_stats
	outsheet inc_decile_stats* using "${savedirectory}\\inc_decile_stats1.txt" if inc_decile_stats1<., replace comma nonames
	svmat double netwealth_decile_stats
	outsheet netwealth_decile_stats* using "${savedirectory}\\netwealth_decile_stats1.txt" if netwealth_decile_stats1<., replace comma nonames
	svmat double liquid_decile_stats
	outsheet liquid_decile_stats* using "${savedirectory}\\liquid_decile_stats1.txt" if liquid_decile_stats1<., replace comma nonames
	svmat double liquidtoinc_decile_stats
	outsheet liquidtoinc_decile_stats* using "${savedirectory}\\liquidtoinc_decile_stats1.txt" if liquidtoinc_decile_stats1<., replace comma nonames
	svmat double cat_age_stats
	outsheet cat_age_stats* using "${savedirectory}\\cat_age_stats1.txt" if cat_age_stats1<., replace comma nonames
	*/
	
	svmat double liquid_decile_stats_1
	outsheet liquid_decile_stats_1* using "${savedirectory}\\liquid_decile_stats_ext_val.txt" if liquid_decile_stats_11<., replace comma nonames
	
preserve

//Here is where we throw out problem observations and outliers
keep if selvst != 1	//self employed in family (2,029,483 observations deleted)
keep if ikkeskattepligtig != 1 //not full Danish tax resident (680,972 observations deleted)
keep if ejerskift!=1	// no real estate transaction in year (messes up consumption measure) (1,245,332 observations deleted)
keep if famforbrug1>0.01	//has positive consumption (959,522 observations deleted)
keep if famindkefterskat>0.01	//has positive income (85,136 observations deleted)
//drop top and bottom percentiles in income and consumption
global drop_top = "99.0"
global drop_bottom = "1.0"
sort familie_id year
*gen d_log_fam_inc_adj = d.log_fam_inc_adj
*gen d_log_cons_adj = d.log_cons_adj
*gen fam_lincome = indkefterskat_1
*replace fam_lincome = indkefterskat_1+indkefterskat_2 if alder_head<.
*gen inc_adj = fam_lincome/PRISS112*100
*bys familie_id: egen sd_inc = sd(inc_adj)
*gen log_cons_adj = (famforbrug1/PRISS112*100)/inc_perm
*gen log_fam_inc_adj = (fam_lincome/PRISS112*100)/inc_perm
bysort year: egen pincome_top = pctile(log_fam_inc_adj), p(${drop_top})
bysort year: egen pincome_bottom = pctile(log_fam_inc_adj), p(${drop_bottom})
bysort year: egen pconsumption_top = pctile(log_cons_adj), p(${drop_top})
bysort year: egen pconsumption_bottom = pctile(log_cons_adj), p(${drop_bottom})
gen non_extreme_income = log_fam_inc_adj<pincome_top & log_fam_inc_adj>pincome_bottom
gen non_extreme_consumption = log_cons_adj<pconsumption_top & log_cons_adj>pconsumption_bottom
drop if log_cons_adj==.
keep if non_extreme_income
keep if non_extreme_consumption


* Check whether households switch quantiles
local vars liquidassets_adj inc_adj netwealth_adj 
foreach var of varlist `vars' {
bys familie_id: egen mean_`var' = mean(`var')
xtile q_`var'_13 = mean_`var' if est_sample==1 & year == 2013, n(5) 
bys familie_id: egen q_mean_`var' = mean(q_`var'_13)
	
	gen q_`var'_y = .
	forvalues i=2004/2015 {
	xtile q_`var'_`i' = `var' if est_sample==1 & year==`i', n(5)
	replace q_`var'_y = q_`var'_`i' if year==`i'
	drop q_`var'_`i'
	}
	
tab q_mean_`var' q_`var'_y
	
}

* Check whether households switch quantiles
local vars URE NNP
foreach var of varlist `vars' {
bys familie_id: egen mean_`var' = mean(`var')
xtile q_`var'_13 = mean_`var' if est_sample==1 & year == 2013, n(5) 
bys familie_id: egen q_mean_`var' = mean(q_`var'_13)
	
	gen q_`var'_y = .
	forvalues i=2009/2015 {
	xtile q_`var'_`i' = `var' if est_sample==1 & year==`i', n(5)
	replace q_`var'_y = q_`var'_`i' if year==`i'
	drop q_`var'_`i'
	}
	
tab q_mean_`var' q_`var'_y
	
}

restore

	
	