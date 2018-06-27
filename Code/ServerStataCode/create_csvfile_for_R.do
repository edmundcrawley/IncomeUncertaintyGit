* This do file creates a csv file of the income and consumption residuals suitable
* for use in R
*global level = 1  // Choose 0 for logs and 1 for levels

if $level == 0 {
if $five_percent_sample == 1 {
	use "${savedirectory}\\${inc_type}_cons_residuals_5percent_sample.dta", clear
}
else{
	use "${savedirectory}\\${inc_type}_cons_residuals.dta", clear
}
}

else {
if $five_percent_sample == 1 {
	use "${savedirectory}\\${inc_type}_cons_residuals_levels_5percent_sample.dta", clear
}
else{
	use "${savedirectory}\\${inc_type}_cons_residuals_levels.dta", clear
}
}

if $level == 0 {
keep familie_id year delta_log_y delta_log_c alder_head fambankakt famnyformue_net PRISS112 ///
		agegroup emp_status_head famhoejstudda work_function_agg_head region part_of_country ///
		highest_educ industry_agg_head industry_level4_head work_function_agg_head ///
		work_function_level4_head homeowner ///
		
}
else {
keep familie_id year delta_y delta_c delta_lincome_head delta_lincome_spouse delta_car_spending ///
		delta_c_0nocar delta_c_nocar delta_c_nodurableproxy alder_head fambankakt famnyformue_net PRISS112  ///
		region part_of_country highest_educ  homeowner famrentudgpr famkursakt famoblakt fampantakt fambankakt ///
		fambankgaeld famoblgaeld fampantgaeld  famqpassivn  famafdrag_for mortgage_refinance_1y famindkefterskat famforbrug1 PRISS112 ///
		mean_inc_after_tax mean_cons_with_interest mean_maturing_assets mean_maturing_liabiliies mean_nominal_assets mean_nominal_liabilities URE NNP
//These are legacy pieces of data needed to column numbering to be correct in R code
gen industry_agg_head =.
gen industry_level4_head =.
gen work_function_level4_head =.
gen emp_status_head  =.
gen work_function_agg_head =.
gen agegroup=.
}

fillin familie_id year

if $labor_inc_only==0 {
	global lab_inc_tag = ""
}
else {
	global lab_inc_tag = "_lincome"
}
gen fambankakt_adj = fambankakt/PRISS112*100
gen famqaktivf_adj = famnyformue_net/PRISS112*100
gen cons_adj = famforbrug1/PRISS112*100

if $level == 0 {
gen log_liquidassets = log(fambankakt/PRISS112)
gen log_netwealth = log(famnyformue_net/PRISS112)  // To be substituted with "famnyformue_net" when available (+ perhaps car value as well?)

// following code is a little superfluous at the moment - if one is missing both are, but that might change...
gen y_included = delta_log_y!=.
replace delta_log_y=0 if delta_log_y==.

gen c_included = delta_log_c!=.
replace delta_log_c=0 if delta_log_c==.

egen maxy = max(year)
egen miny = min(year)
scalar nyears = maxy-miny+1
drop maxy miny
scalar nind = _N/nyears
sort familie_id year
gen id = group(nind)

keep id year delta_log_y y_included delta_log_c c_included agegroup log_liquidassets log_netwealth alder_head emp_status_head region part_of_country highest_educ industry_agg_head industry_level4_head work_function_agg_head work_function_level4_head homeowner
order id year delta_log_y y_included delta_log_c c_included agegroup log_liquidassets log_netwealth alder_head emp_status_head region part_of_country highest_educ industry_agg_head industry_level4_head work_function_agg_head work_function_level4_head homeowner

xtset id year

	if $five_percent_sample == 1 {
	outsheet using "${savedirectory}\input_for_R_5percentsample${lab_inc_tag}.csv",comma replace
	}
	else{
	outsheet using "${savedirectory}\input_for_R_fullsample${lab_inc_tag}.csv",comma replace
	}

}

else {
gen liquidassets_adj = fambankakt/PRISS112*100
gen netwealth_adj = famnyformue_net/PRISS112*100 

gen y_included = delta_y!=.
replace delta_y=0 if delta_y==.

gen lincome_head_included = delta_lincome_head!=.
replace delta_lincome_head=0 if delta_lincome_head==.

gen lincome_spouse_included = delta_lincome_spouse!=.
replace delta_lincome_spouse=0 if delta_lincome_spouse==.

gen c_included = delta_c!=.
replace delta_c=0 if delta_c==.

gen car_included = delta_c_nocar!=.
replace delta_c_0nocar=0 if delta_c_0nocar==.
replace delta_c_nocar=0 if delta_c_nocar==.
replace delta_c_nodurableproxy=0 if delta_c_nodurableproxy==.

egen maxy = max(year)
egen miny = min(year)
scalar nyears = maxy-miny+1
drop maxy miny
scalar nind = _N/nyears
sort familie_id year
gen id = group(nind)




preserve
keep id year delta_lincome_head lincome_head_included delta_c c_included agegroup liquidassets_adj netwealth_adj alder_head emp_status_head region part_of_country highest_educ industry_agg_head industry_level4_head work_function_agg_head work_function_level4_head homeowner delta_c_0nocar delta_c_nocar delta_c_nodurableproxy car_included mean_inc_after_tax mean_cons_with_interest mean_maturing_assets mean_maturing_liabiliies URE mean_nominal_assets mean_nominal_liabilities NNP
order id year delta_lincome_head lincome_head_included delta_c c_included agegroup liquidassets_adj netwealth_adj alder_head emp_status_head region part_of_country highest_educ industry_agg_head industry_level4_head work_function_agg_head work_function_level4_head homeowner delta_c_0nocar delta_c_nocar delta_c_nodurableproxy car_included mean_inc_after_tax mean_cons_with_interest mean_maturing_assets mean_maturing_liabiliies URE mean_nominal_assets mean_nominal_liabilities NNP
if $five_percent_sample == 1 {
	outsheet using "${savedirectory}\input_for_R_5percentsample_level_lincome_head.csv",comma replace
	}
	else{
	outsheet using "${savedirectory}\input_for_R_fullsample_level_lincome_head.csv",comma replace
	}
restore

preserve
keep id year delta_lincome_spouse lincome_spouse_included delta_c c_included agegroup liquidassets_adj netwealth_adj alder_head emp_status_head region part_of_country highest_educ industry_agg_head industry_level4_head work_function_agg_head work_function_level4_head homeowner delta_c_0nocar delta_c_nocar delta_c_nodurableproxy car_included
order id year delta_lincome_spouse lincome_spouse_included delta_c c_included agegroup liquidassets_adj netwealth_adj alder_head emp_status_head region part_of_country highest_educ industry_agg_head industry_level4_head work_function_agg_head work_function_level4_head homeowner delta_c_0nocar delta_c_nocar delta_c_nodurableproxy car_included
if $five_percent_sample == 1 {
	outsheet using "${savedirectory}\input_for_R_5percentsample_level_lincome_spouse.csv",comma replace
	}
	else{
	outsheet using "${savedirectory}\input_for_R_fullsample_level_lincome_spouse.csv",comma replace
	}
restore

preserve
keep id year delta_y y_included delta_c c_included agegroup liquidassets_adj netwealth_adj alder_head emp_status_head region part_of_country highest_educ industry_agg_head industry_level4_head work_function_agg_head work_function_level4_head homeowner delta_c_0nocar delta_c_nocar delta_c_nodurableproxy car_included mean_inc_after_tax mean_cons_with_interest mean_maturing_assets mean_maturing_liabiliies URE mean_nominal_assets mean_nominal_liabilities NNP
order id year delta_y y_included delta_c c_included agegroup liquidassets_adj netwealth_adj alder_head emp_status_head region part_of_country highest_educ industry_agg_head industry_level4_head work_function_agg_head work_function_level4_head homeowner delta_c_0nocar delta_c_nocar delta_c_nodurableproxy car_included mean_inc_after_tax mean_cons_with_interest mean_maturing_assets mean_maturing_liabiliies URE mean_nominal_assets mean_nominal_liabilities NNP

xtset id year

	if $five_percent_sample == 1 {
	outsheet using "${savedirectory}\input_for_R_5percentsample_level${lab_inc_tag}.csv",comma replace
	}
	else{
	outsheet using "${savedirectory}\input_for_R_fullsample_level${lab_inc_tag}.csv",comma replace
	}
restore
	
	}






/*
//make a 5% sample
gen random = runiform()
bys id: replace random = random[1]
drop if random>0.05
drop random
*/

//save as a csv file
