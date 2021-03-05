* Calculates URE, NNP etc from SCF data as best as possible
clear all
set maxvar 6000
global savedirectory = "\\srv9dnbfil002\userhome\aku\Desktop\Cons hetero AEJ macro"

use "${savedirectory}\rscfp2016.dta", clear

gen NNP = liq +cds+gbmutf+obmutf+bond ///
			- mrthel - resdbt - othloc - ccbal - install - odebt	
xtile NNP_decile = NNP [pweight=wgt], nq(10)

gen URE = (liq-prepaid) +cds/5 + gbmutf/5 + obmutf/5 + bond/5 ///
			- mrthel/7 - resdbt/5 - othloc - ccbal - install/5 - odebt	
xtile URE_decile = URE [pweight=wgt], nq(10)

xtile liq_decile = liq [pweight=wgt], nq(10)
xtile liq_pctile = liq [pweight=wgt], nq(100)

xtile inc_decile = income [pweight=wgt], nq(10)
xtile inc_pctile = income [pweight=wgt], nq(100)


gen liquid_to_income = liq / income


gen est_sample = age >= 30 & age <= 55 
gen homeowner = housecl == 1
replace homeowner = . if housecl == .
gen carowner = vehic > 0 & vehic !=.
gen higher_educ = inrange(edcl,3,4)

xtile liq_decile_a = liq if est_sample [pweight=wgt], nq(10)

svyset [pweight=wgt]


local vars "liq networth liquid_to_income income age homeowner carowner URE NNP debt higher_educ"

unab count1: `vars'  
scalar count = `: word count `count1''

matrix stats = J(10,count,.)
matrix stats_m = J(10,count,.)

local j=1
foreach var of varlist `vars' {
	forvalues i=1/10 {
	svy: mean `var' if liq_decile_a ==`i'  
	matrix stats[`i',`j'] = e(b)
	_pctile `var' [pweight = wgt] if liq_decile_a ==`i' == 1, p(50)   
	matrix stats_m[`i',`j'] = `r(r1)'
	}
	local j = `j' + 1
}

* Means
svmat double stats
local j=1
foreach var of varlist `vars' {
rename stats`j' m_`var'
local j = `j' + 1
}
outsheet m_* using "${savedirectory}\descriptives_SCF.csv" if _n <= 10 ,comma replace

* Medians
svmat double stats_m
local j=1
foreach var of varlist `vars' {
rename stats_m`j' md_`var'
local j = `j' + 1
}
outsheet md_* using "${savedirectory}\descriptives_SCF_medians.csv" if _n <= 10 ,comma replace

