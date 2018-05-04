###############################################################################
# 
# This file calculates all the moments used in the paper
#
# 1) Loads all data
# 2) Runs create_moments_CS (from file BPPLikeCarrollSamwick.r) for different subsets of the data
# 3) Checks to make sure individuals cannot be identified from moments
# 4) Save the moments
# 
# Moment generation is very slow to run
# The output moments contain no indentifiable information about individuals
# 
###############################################################################

# Choose which input data to use
five_percent_sample = FALSE

if (five_percent_sample) {
  empirical_input_file="input_for_R_5percentsample.csv"
  tag = "_fivepercent"
} else {
  empirical_input_file="Input_for_R_fullsample.csv"
  tag = ""
}

# Set folders and load code
Rcode_folder = "E:/ProjektDB/706172/Workdata/706172/Husholdningsprojekt/Precautionary saving with time varying risk/Edmund/BPP/Rcode/BPPLikeCarrollSamwick/"
empirical_input_folder = "E:/ProjektDB/706172/Workdata/706172/Husholdningsprojekt/Precautionary saving with time varying risk/Edmund/BPP/save/"
moments_dir = "E:/ProjektDB/706172/Workdata/706172/Husholdningsprojekt/Precautionary saving with time varying risk/Edmund/BPP/save/moments/"
log_dir = "E:/ProjektDB/706172/Workdata/706172/Husholdningsprojekt/Precautionary saving with time varying risk/Edmund/BPP/logfiles/"
library(zoo)
source(paste(Rcode_folder,"BPPLikeCarrollSamwick.r",sep=""))

###############################################################################

sink(paste(log_dir,"create_and_save_moments_log.txt",sep=""))   # Start logging results

###############################################################################
# Load the data
raw_data = read.csv(paste(empirical_input_folder,empirical_input_file,sep=""), sep=",")
all_data<- as.matrix(raw_data[,1:9])  # Format input data as a martrix

# Create some variable definitions that will be useful later
year_col = 2
age_col = 10
liquidasset_col = 8
netwealth_col = 9
T  <- max(all_data[,year_col])-min(all_data[,year_col])+1  # Length of panel
y      <-nrow(all_data)                                    # Total number of person, year observations
# Create a variable equal to a household head's age in 2013
age_2013 = array("", dim=c(y,1))
for (k in 0:((y/T)-1)){
  i <- k*T
  age_2013[(i+1):(i+T)] = as.character(raw_data[(i+10),age_col])
}
# Fill in the age_2013 from other years if it is missing in 2013
for (t in 1:T){
  for (k in 0:((y/T)-1)){
    i <- k*T
    if (is.na(age_2013[i+10])){
      age_2013[(i+1):(i+T)] = as.character(raw_data[(i+t),age_col]+t-10)
    }
  }
}
age_2013[is.na(age_2013)] = 0
age_range = age_2013>=35 & age_2013<=60  # Boolean that specifies if a household head is in the age range

# Define function to calculate moments by categories
# Note, the function makes use of some of the variable defintions above
moments_by_category<- function(category_col, category_set, age_range=TRUE) {
  # category_col is either the column number, or a vector containing categories
  # catgeory_set is the .....................
  # age_range specifies that moments should only be calculated for those in this range
  
  # Need categories to be constant over the panel, so set equal to value in 2013
  category_2013 = category_col
  if (length(category_col)==1) {
    category_2013 = array("", dim=c(y,1))
    for (k in 0:((y/T)-1)){
      i <- k*T
      category_2013[(i+1):(i+T)] = as.character(raw_data[(i+10),category_col])
    }
  }
  category_2013[is.na(category_2013)] = 0
  
  moments_by_category = list()
  for (i in 1:length(category_set)){
    this_category = as.character(category_set[i])
    this_data = all_data[category_2013==this_category & age_range,]
    this_moments = make.names(this_category)
    moments_by_category[[this_moments]]=create_moments_CS(this_data)
    print(paste("Minimum number of individuals in a cell of ", this_moments, " = ", min(moments_by_category[[this_moments]]$d_dif)))
  }
  return (moments_by_category)
}
###############################################################################

###############################################################################
# First create moments on all the data
moments_all <- create_moments_CS(all_data)
# Check to see if individuals can be identified
print(paste("Minimum number of individuals in a cell of moments_all = ", min(moments_all$d_dif)))
save(moments_all,file=paste(moments_dir,'moments_all',tag,'.RData',sep=''))
###############################################################################

###############################################################################
# Now calculate moments by age
moments_by_age = list()

age_set = 28:80
age_params = array(0, dim=c(length(age_set),4))
age_se = array(0, dim=c(length(age_set),4))
age_obs = array(0, dim=c(length(age_set)))
age_total_var = array(0, dim=c(length(age_set)))
for (i in 1:length(age_set)){
  this_age = age_set[i]
  this_data = all_data[age_2013==this_age,]
  this_moments = paste('age',this_age,sep='')
  moments_by_age[[this_moments]]=create_moments_CS(this_data)
  print(paste("Minimum number of individuals in a cell of ", this_moments, " = ", min(moments_by_age[[this_moments]]$d_dif)))
}
save(moments_by_age,file=paste(moments_dir,'moments_by_age',tag,'.RData',sep=''))
###############################################################################

###############################################################################
# Now calculate moments by other categories

# Do by emp_status_head 
emp_status_head_col = 11
emp_status_head_set = c("1 Top level manager","2 Upper level wage earners","3 Mid level wage earners","4 Lower level wage earners","5 Other wage earners","6 Self employed etc","7 Unemployed","8 Outside labor force")
moments_by_emp_status = moments_by_category(emp_status_head_col, emp_status_head_set,age_range)
save(moments_by_emp_status,file=paste(moments_dir,'moments_by_emp_status',tag,'.RData',sep=''))

# Do by agegroup
agegroup_col = 7
agegroup_set = c("29","30","40","50","60","70")
moments_by_agegroup = moments_by_category(agegroup_col, agegroup_set, TRUE)
save(moments_by_agegroup,file=paste(moments_dir,'moments_by_agegroup',tag,'.RData',sep=''))

# Do by region
region_col = 12
region_set = c("81 Nordjylland","82 Midtjylland","83 Syddanmark","84 Hovedstaden","85 Sjælland")
moment_by_region =moments_by_category(region_col, region_set, age_range)
save(moment_by_region,file=paste(moments_dir,'moment_by_region',tag,'.RData',sep=''))

# Do by highest_educ
highest_educ_col = 14
highest_educ_set = c("Primary","Upper secondary","Vocational","Short cycle","BA","Masters +")
moments_by_educ =moments_by_category(highest_educ_col, highest_educ_set,age_range)
save(moments_by_educ,file=paste(moments_dir,'moments_by_educ',tag,'.RData',sep=''))

# Do by industry_agg_head
industry_agg_head_col = 15
# removed "11 Uoplyst aktivitet" from the list below - gives crazy results
industry_agg_head_set = c("1 Landbrug, skovbrug og fiskeri","10 Kultur, fritid og anden service","2 Industri, råstofindvinding og forsyningsvirksomhed","3 Bygge og anlæg","4 Handel og transport mv.","5 Information og kommunikation","6 Finansiering og forsikring","7 Ejendomshandel og udlejning","8 Erhvervsservice","9 Offentlig administration, undervisning og sundhed")
moments_by_industry =moments_by_category(industry_agg_head_col, industry_agg_head_set,age_range)
save(moments_by_industry,file=paste(moments_dir,'moments_by_industry',tag,'.RData',sep=''))

# Do by work_function_agg_head
work_function_agg_head_col = 17
work_function_agg_head_set = c("0 Militært arbejde","1 Ledelsesarbejde","2 Arbejde, der forudsætter viden på højeste niveau inden for pågældende område","3 Arbejde, der forudsætter viden på mellemniveau","4 Almindeligt kontor- og kundeservicearbejde","5 Service- og salgsarbejde","6 Arbejde inden for landbrug, skovbrug og fiskeri ekskl. medhjælp","7 Håndværkspræget arbejde","8 Operatør- og monteringsarbejde samt transportarbejde","9 Andet manuelt arbejde")
moments_by_work_function =moments_by_category(work_function_agg_head_col, work_function_agg_head_set,age_range)
save(moments_by_work_function,file=paste(moments_dir,'moments_by_work_function',tag,'.RData',sep=''))

# Do by homeowner
homeowner_col = 19
homeowner_set = c("0","1")
moments_by_home_owner =moments_by_category(homeowner_col, homeowner_set,age_range)
save(moments_by_home_owner,file=paste(moments_dir,'moments_by_home_owner',tag,'.RData',sep=''))
###############################################################################

###############################################################################
# Sort into liquid wealth quintiles and calculate related moments
num_quantiles =5
mean_log_liquidwealth = array(0.0, dim=c(y,1))
for (k in 0:((y/T)-1)){
  i <- k*T
  mean_log_liquidwealth[(i+1):(i+T),] = mean(all_data[(i+1):(i+T),liquidasset_col],na.rm=TRUE)
}
quantiles = seq(0,1.0,length=(num_quantiles+1))
quantile_cutoffs = quantile(mean_log_liquidwealth[age_range][seq(1,(y/T),by=T)],quantiles, na.rm=TRUE)
wealth_quantile = as.numeric(cut(mean_log_liquidwealth,breaks=quantile_cutoffs,include.lowest=TRUE, labels=1:num_quantiles))
wealth_quantile_set = as.character(1:num_quantiles)
moments_by_liquid_wealth_quantile =moments_by_category(wealth_quantile, wealth_quantile_set,age_range)
# Calculate quantile ranges in terms of dollars, take off 0 and 100% to avoid identifying individuals
quantiles = quantile_cutoffs[2:(length(quantile_cutoffs)-1)]
quantiles = exp(quantiles)*100.0/6.87 #convert to 2015 USD
moments_by_liquid_wealth_quantile$quantiles = quantiles
save(moments_by_liquid_wealth_quantile,file=paste(moments_dir,'moments_by_liquid_wealth_quantile',tag,'.RData',sep=''))
###############################################################################

###############################################################################
# Look at data growth over periods other than 3 to 5 years
# Increasing the maximum growth period reduces the sample (with possible sample selection)
# Therefore we calculate the moments consistently based on the sample that exists for each 'max_diff' 
this_data = all_data[age_range,]
moments_loop = list()
for (max_diff in 4:10){
  this_moments = paste('moments_',max_diff,sep='')
  moments_loop[[this_moments]]=create_moments_CS(this_data,1:max_diff)
  print(paste("Minimum number of individuals in a cell of ", this_moments, " = ", min(moments_loop[[this_moments]]$d_dif)))
}
save(moments_loop,file=paste(moments_dir,'moments_loop',tag,'.RData',sep=''))
###############################################################################

###############################################################################
# Sort into net wealth quintiles and calculate related moments
num_quantiles =5
mean_log_netwealth = array(0.0, dim=c(y,1))
for (k in 0:((y/T)-1)){
  i <- k*T
  mean_log_netwealth[(i+1):(i+T),] = mean(all_data[(i+1):(i+T),netwealth_col],na.rm=TRUE)
}
quantiles = seq(0,1.0,length=(num_quantiles+1))
quantile_cutoffs = quantile(mean_log_netwealth[age_range][seq(1,(y/T),by=T)],quantiles, na.rm=TRUE)
netwealth_quantile = as.numeric(cut(mean_log_netwealth,breaks=quantile_cutoffs,include.lowest=TRUE, labels=1:num_quantiles))
netwealth_quantile_set = as.character(1:num_quantiles)
moments_by_net_wealth_quantile =moments_by_category(netwealth_quantile, netwealth_quantile_set,age_range)
# Calculate quantile ranges in terms of dollars, take off 0 and 100% to avoid identifying individuals
quantiles = quantile_cutoffs[2:(length(quantile_cutoffs)-1)]
quantiles = exp(quantiles)*100.0/6.87 #convert to 2015 USD
moments_by_net_wealth_quantile$quantiles = quantiles
save(moments_by_net_wealth_quantile,file=paste(moments_dir,'moments_by_net_wealth_quantile',tag,'.RData',sep=''))
###############################################################################

sink() # End log
