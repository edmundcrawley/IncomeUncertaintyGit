# Calculates table like Andreas' for different groups of households

# input_dir = "E:/ProjektDB/706172/Workdata/706172/Husholdningsprojekt/Precautionary saving with time varying risk/Edmund/BPP/save/"
# this_file_name = "input_for_R_fullsample.csv"
# #this_file_name = "input_for_R_5percentsample.csv"
# empirical_input_file =paste(input_dir,this_file_name,sep="")
# 
# raw_data = read.csv(empirical_input_file, sep=",")
# all_data<- as.matrix(raw_data[,1:9]) 
year_col = 2
T  <- max(all_data[,year_col])-min(all_data[,year_col])+1 
y      <-nrow(all_data) 

#create index for those who are between 35 and 60 in 2013
age_col=10
age_2013 = array("", dim=c(y,1))
for (k in 0:((y/T)-1)){
  i <- k*T
  age_2013[(i+1):(i+T)] = as.character(raw_data[(i+10),age_col])
}
age_2013[is.na(age_2013)] = 0
age_range = age_2013>=35 & age_2013<=60

#Function to calculate graphs of variance and MPX by different categories
params_by_subset<- function(all_data, category_col, category_set, age_range) {
  year_col = 2
  T  <- max(all_data[,year_col])-min(all_data[,year_col])+1 
  y      <-nrow(all_data) 
  
  #category_col can either be the vector itself or a column number of raw_data
  category_2013 = category_col
  if (length(category_col)==1) {
    category_2013 = array("", dim=c(y,1))
    for (k in 0:((y/T)-1)){
    	i <- k*T
    	category_2013[(i+1):(i+T)] = as.character(raw_data[(i+10),category_col])
    }
  }
  category_2013[is.na(category_2013)] = 0
  
  category_params = array(0, dim=c(length(category_set),4))
  category_se = array(0, dim=c(length(category_set),4))
  category_obs = array(0, dim=c(length(category_set)))
  category_total_var = array(0, dim=c(length(category_set)))
  for (i in 1:length(category_set)){
  	this_category = as.character(category_set[i])
  	this_data = all_data[category_2013==this_category & age_range,]
  	

  	
  	this_moments <- create_moments_CS(this_data)
  	this_c_vector <- this_moments[["c_vector"]]
  	this_omega <- this_moments[["omega"]]
  	this_CS_output = CS_parameter_estimation(this_c_vector, this_omega,T) 
  	category_params[i,1] = this_CS_output$var_perm
  	category_params[i,2] = this_CS_output$var_tran
  	category_params[i,3] = this_CS_output$ins_perm
  	category_params[i,4] = this_CS_output$ins_tran
  	category_se[i,1] = this_CS_output$var_perm_se
  	category_se[i,2] = this_CS_output$var_tran_se
  	category_se[i,3] = this_CS_output$ins_perm_se
  	category_se[i,4] = this_CS_output$ins_tran_se
  	category_obs[i] = nrow(this_data)/T
  	category_total_var[i] = this_moments$delta_y_var
  }
  output = list("category_params"=category_params,"category_se"=category_se,"category_obs"=category_obs,"category_total_var"=category_total_var)
  return (output)
}

# 1) Do by emp_status_head 
emp_status_head_col = 11
emp_status_head_set = c("1 Top level manager","2 Upper level wage earners","3 Mid level wage earners","4 Lower level wage earners","5 Other wage earners","6 Self employed etc","7 Unemployed","8 Outside labor force")
output =params_by_subset(all_data, emp_status_head_col, emp_status_head_set,age_range)
emp_status_head_output=output
emp_status_head_params = output$category_params
emp_status_head_se = output$category_se
emp_status_head_obs = output$category_obs
emp_status_head_total_var = output$category_total_var

# # 2) Do by agegroup 
# agegroup_col = 7
# agegroup_set = c("29","30","40","50","60","70")
# output =params_by_subset(all_data, agegroup_col, agegroup_set)
# agegroup_output=output
# agegroup_params = output$category_params
# agegroup_se = output$category_se
# agegroup_obs = output$category_obs
# agegroup_total_var = output$category_total_var

# 3) Do by region
region_col = 12
region_set = c("81 Nordjylland","82 Midtjylland","83 Syddanmark","84 Hovedstaden","85 Sjælland")
output =params_by_subset(all_data, region_col, region_set,age_range)
region_output=output
region_params = output$category_params
region_se = output$category_se
region_obs = output$category_obs
region_total_var = output$category_total_var

# 5) Do by highest_educ
highest_educ_col = 14
highest_educ_set = c("Primary","Upper secondary","Vocational","Short cycle","BA","Masters +")
output =params_by_subset(all_data,highest_educ_col, highest_educ_set,age_range)
highest_educ_output=output
highest_educ_params = output$category_params
highest_educ_se = output$category_se
highest_educ_obs = output$category_obs
highest_educ_total_var = output$category_total_var

# 6) Do by industry_agg_head
industry_agg_head_col = 15
# removed "11 Uoplyst aktivitet" from the list below - give crazy results, or doesn't solve
industry_agg_head_set = c("1 Landbrug, skovbrug og fiskeri","10 Kultur, fritid og anden service","2 Industri, råstofindvinding og forsyningsvirksomhed","3 Bygge og anlæg","4 Handel og transport mv.","5 Information og kommunikation","6 Finansiering og forsikring","7 Ejendomshandel og udlejning","8 Erhvervsservice","9 Offentlig administration, undervisning og sundhed")
output =params_by_subset(all_data, industry_agg_head_col, industry_agg_head_set,age_range)
industry_agg_head_output=output
industry_agg_head_params = output$category_params
industry_agg_head_se = output$category_se
industry_agg_head_obs = output$category_obs
industry_agg_head_total_var = output$category_total_var

# 8) Do by work_function_agg_head
work_function_agg_head_col = 17
work_function_agg_head_set = c("0 Militært arbejde","1 Ledelsesarbejde","2 Arbejde, der forudsætter viden på højeste niveau inden for pågældende område","3 Arbejde, der forudsætter viden på mellemniveau","4 Almindeligt kontor- og kundeservicearbejde","5 Service- og salgsarbejde","6 Arbejde inden for landbrug, skovbrug og fiskeri ekskl. medhjælp","7 Håndværkspræget arbejde","8 Operatør- og monteringsarbejde samt transportarbejde","9 Andet manuelt arbejde")
output =params_by_subset(all_data, work_function_agg_head_col, work_function_agg_head_set,age_range)
work_function_agg_head_output=output
work_function_agg_head_params = output$category_params
work_function_agg_head_se = output$category_se
work_function_agg_head_obs = output$category_obs
work_function_agg_head_total_var = output$category_total_var

# 10) Do by homeowner
homeowner_col = 19
homeowner_set = c("0","1")
output =params_by_subset(all_data, homeowner_col, homeowner_set,age_range)
homeowner_output=output
homeowner_params = output$category_params
homeowner_se = output$category_se
homeowner_obs = output$category_obs
homeowner_total_var = output$category_total_var

# ############################ Categories have not been defined for the following yet
# # 4) Do by part_of_country 
# part_of_country_col = 13
# part_of_country_set = c(********)
# output =params_by_subset(all_data, part_of_country_col, part_of_country_set,age_range)
# part_of_country_output=output
# part_of_country_params = output$category_params
# part_of_country_se = output$category_se
# part_of_country_obs = output$category_obs
# part_of_country_total_var = output$category_total_var
# 
# # 7) Do by industry_level4_head
# industry_level4_head_col = 16
# industry_level4_head_set = c(********)
# output =params_by_subset(all_data, industry_level4_head_col, industry_level4_head_set,age_range)
# industry_level4_head_output=output
# industry_level4_head_params = output$category_params
# industry_level4_head_se = output$category_se
# industry_level4_head_obs = output$category_obs
# industry_level4_head_total_var = output$category_total_var
# 
# # 9) Do by work_function_level4_head
# work_function_level4_head_col = 18
# work_function_level4_head_set = c(********)
# output =params_by_subset(all_data, work_function_level4_head_col, work_function_level4_head_set,age_range)
# work_function_level4_head_output=output
# work_function_level4_head_params = output$category_params
# work_function_level4_head_se = output$category_se
# work_function_level4_head_obs = output$category_obs
# work_function_level4_head_total_var = output$category_total_var
# ############################


plot_output<- function(params, se, labels, category_for_title, category_for_save) {
figures_dir = "E:/ProjektDB/706172/Workdata/706172/Husholdningsprojekt/Precautionary saving with time varying risk/Edmund/BPP/figures/"
# Now create table of output
# First plot the variances
dev.new()
par(mar=c(8,7,4,5)+0.1)
plotTop = max(params[,1:2])*1.2
barCenters <- barplot(height=t(params[,1:2]),
			names.arg=labels,
			cex.names=0.75,
			beside=TRUE,col=c("green","red"),
			las=2,ylim=c(0,plotTop), xaxt="n",
			main=paste("Permanent and Transitory Variance by ",category_for_title),
			ylab = "Shock Variance", border="black", axes=TRUE)
text(x=barCenters[1,]+1, y =-plotTop*0.02,srt=45, adj=1, labels=labels,xpd=TRUE)
segments(barCenters, t(params[,1:2]-se[,1:2]*1.96),
			barCenters,
			t(params[,1:2]+se[,1:2]*1.96), lwd=1.5)
arrows(barCenters, t(params[,1:2]-se[,1:2]*1.96),
			barCenters,
			t(params[,1:2]+se[,1:2]*1.96), lwd=1.5,
			angle=90,code=3, length=0.05)
legend(2, plotTop, legend=c("Permanent Var", "Transitory Var"), fill=c("green","red"),bty="n")
dev.copy(png, paste(figures_dir, "VarianceBy",category_for_save,".png",sep=""))
dev.off()

# Now plot the MPXs
dev.new()
barCenters <- barplot(t(params[,3:4]),names.arg=labels,cex.names=0.8,beside=TRUE,col=c("green","red"))
par(mar=c(8,7,4,5)+0.1)
plotTop = max(params[,3:4])*1.2
barCenters <- barplot(height=t(params[,3:4]),
			names.arg=labels,
			cex.names=0.75,
			beside=TRUE,col=c("green","red"),
			las=2,ylim=c(0,plotTop), xaxt="n",
			main=paste("Permanent and Transitory MPX by ",category_for_title),
			ylab = "MPX", border="black", axes=TRUE)
text(x=barCenters[1,]+1, y =-plotTop*0.02,srt=45, adj=1, labels=labels,xpd=TRUE)
segments(barCenters, t(params[,3:4]-se[,3:4]*1.96),
			barCenters,
			t(params[,3:4]+se[,3:4]*1.96), lwd=1.5)
arrows(barCenters, t(params[,3:4]-se[,3:4]*1.96),
			barCenters,
			t(params[,3:4]+se[,3:4]*1.96), lwd=1.5,
			angle=90,code=3, length=0.05)
legend(2, plotTop, legend=c("Permanent MPX", "Transitory MPX"), fill=c("green","red"),bty="n")
dev.copy(png, paste(figures_dir, "MPXBy",category_for_save,".png",sep=""))
dev.off()
}


plot_output(emp_status_head_params,emp_status_head_se,emp_status_head_set ,"Employment Status","EmploymentStatus")
#plot_output(agegroup_params,agegroup_se,agegroup_set ,"Age","AgeGroup")
plot_output(region_params,region_se,region_set ,"Region","Region")
plot_output(highest_educ_params,highest_educ_se,highest_educ_set ,"Education" ,"Education")
plot_output(industry_agg_head_params,industry_agg_head_se,industry_agg_head_set ,"Industry","Industry")
plot_output(work_function_agg_head_params,work_function_agg_head_se,work_function_agg_head_set ,"Work Function","WorkFunction")
plot_output(homeowner_params,homeowner_se,c("Non-Homeowner","Homeowner") ,"Home Owner","HomeOwner")

