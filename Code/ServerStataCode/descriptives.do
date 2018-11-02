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
global savedirectory = "E:\ProjektDB\706172\Workdata\706172\Husholdningsprojekt\Precautionary saving with time varying risk\edmund\BPP\save\"
global dofiles = "E:\ProjektDB\706172\Workdata\706172\Husholdningsprojekt\Precautionary saving with time varying risk\edmund\BPP\dofiles"
global logfiles = "E:\ProjektDB\706172\Workdata\706172\Husholdningsprojekt\Precautionary saving with time varying risk\edmund\BPP\logfiles"


use  "${savedirectory}\\${inc_type}_cons_residuals_levels.dta"

cap drop est_sample
gen est_sample = alder_head_08 >= 30 & alder_head_08 <= 55 

bys familie_id: egen coun=count(famforbrug1)
replace est_sample = 0 if coun < 5

gen l1 = delta_c ==. | l.delta_c==. | l2.delta_c==. | l3.delta_c==.
gen l2 = f.delta_c ==. | delta_c==. | l.delta_c==. | l2.delta_c==.
gen l3 = f2.delta_c ==. | f.delta_c==. | delta_c==. | l.delta_c==.
gen l4 = f3.delta_c ==. | f2.delta_c==. | f.delta_c==. | delta_c==.
gen l_sum = l1 + l2 + l3 + l4

replace est_sample = 0 if l_sum == 4 

gen liquidassets_adj = fambankakt/PRISS112*100 / 6.87
gen netwealth_adj = famnyformue_net/PRISS112*100 / 6.87
gen cons = famforbrug1/PRISS112*100 / 6.87
gen inc = famindkefterskat/PRISS112*100 / 6.87
gen carowner = bilvaerdi > 0 & bilvaerdi !=.
gen higher_educ = 0 if highest_educ != ""
replace higher_educ = 1 if highest_educ == "BA" | highest_educ == "Masters +"
replace URE = URE / 6.87 // Conversion to USD
replace NNP = NNP / 6.87

log using "${savedirectory}\descriptives", text replace
tabstat inc cons liquidassets_adj netwealth_adj homeowner carowner higher_educ alder_head_08 URE NNP if est_sample , s(n mean sd p10 q p90) c(s)
log close

sort familie_id year
gen d_inc = d.inc

bysort year est_sample: egen sd_delta_inc = sd(d_inc)
bysort year est_sample: egen sd_delta_inc_unexpl = sd(delta_lincome_head)

egen tag=tag(year) if est_sample

label var sd_delta_inc "Sd. of delta income"
label var sd_delta_inc_unexpl "Sd. of delta inc. (unexpl.)"

graph twoway line sd_delta_inc  year if tag || line sd_delta_inc_unexpl year if tag , yaxis(2) 
graph export "${savedirectory}\\inc_std.png", replace

keep if tag 
keep year sd_delta_inc sd_delta_inc_unexpl
outsheet using "${savedirectory}\descriptives_stddev.csv",comma replace

*******************
* POPULATION DATA *
*******************

clear
use  "${rawdata}\family_firm.dta"


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



gen liquidassets_adj = fambankakt/PRISS112*100 / 6.87
gen netwealth_adj = famnyformue_net/PRISS112*100 / 6.87
bys year: egen cons_max = pctile(famforbrug1), p(99)
bys year: egen cons_min = pctile(famforbrug1), p(1)
gen cons = famforbrug1/PRISS112*100 / 6.87 if ejerskift!=1 & selvst != 1 & ikkeskattepligtig != 1 & famforbrug1>=0 & famforbrug1 < cons_max & ejerskift != 1
gen inc = famindkefterskat/PRISS112*100 / 6.87
gen carowner = bilvaerdi > 0 & bilvaerdi !=.
cap drop highest_educ
gen highest_educ = ""
replace highest_educ = "Primary" if famhoejstudda / 1000 < 2 & famhoejstudda / 1000 > 0 
replace highest_educ = "Upper secondary" if famhoejstudda / 1000 < 3 & famhoejstudda / 1000 >= 2 
replace highest_educ = "Vocational" if famhoejstudda / 1000 < 4 & famhoejstudda / 1000 >= 3
replace highest_educ = "Short cycle" if famhoejstudda / 1000 < 5 & famhoejstudda / 1000 >= 4
replace highest_educ = "BA" if famhoejstudda / 1000 <= 6.1 & famhoejstudda / 1000 >= 5
replace highest_educ = "Masters +" if famhoejstudda / 1000 < 7.1 & famhoejstudda / 1000 >= 6.5

gen higher_educ = 0 if highest_educ != ""
replace higher_educ = 1 if highest_educ == "BA" | highest_educ == "Masters +"
replace NNP = NNP / 6.87 // Conversion to USD
replace URE = URE / 6.87

gen homeowner = famboligvaerdi>0 & famboligvaerdi!=.


log using "${savedirectory}\descriptives", text append
tabstat inc cons liquidassets_adj netwealth_adj homeowner carowner higher_educ alder_head_08 URE NNP if est_sample , s(n mean sd median) c(s)
tabstat inc cons liquidassets_adj netwealth_adj homeowner carowner higher_educ alder_head_08 URE NNP , s(n mean sd median) c(s)
log close
