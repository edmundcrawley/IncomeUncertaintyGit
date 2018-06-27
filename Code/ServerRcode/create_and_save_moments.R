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
levels = TRUE
lincome_head = TRUE       # Labor income hh head 
lincome_spouse = FALSE     # Labor income spouse

if (levels) {
	if (five_percent_sample) {
	  empirical_input_file="input_for_R_5percentsample_level.csv"
	  tag = "_level_fivepercent"
	} else {
		if (lincome_head) {
		  empirical_input_file="input_for_R_fullsample_level_lincome_head.csv"
		  tag = "_level_lincome_head"
		} else if (lincome_spouse) {
		  empirical_input_file="input_for_R_fullsample_level_lincome_spouse.csv"
		  tag = "_level_lincome_spouse"
		} else {
		empirical_input_file="input_for_R_fullsample_level_lincome.csv"
		  tag = "_level_lincome"
		}
	}
} else {
	if (five_percent_sample) {
  	empirical_input_file="input_for_R_5percentsample.csv"
  	tag = "_fivepercent"
	} else {
  	empirical_input_file="input_for_R_fullsample.csv"
  	tag = ""
	}
}

# Set folders and load code
Rcode_folder = "E:/ProjektDB/706172/Workdata/706172/Husholdningsprojekt/Precautionary saving with time varying risk/Edmund/BPP/Rcode/BPPLikeCarrollSamwick/"
empirical_input_folder = "E:/ProjektDB/706172/Workdata/706172/Husholdningsprojekt/Precautionary saving with time varying risk/Edmund/BPP/save/"
moments_dir = "E:/ProjektDB/706172/Workdata/706172/Husholdningsprojekt/Precautionary saving with time varying risk/Edmund/BPP/save/moments/"
log_dir = "E:/ProjektDB/706172/Workdata/706172/Husholdningsprojekt/Precautionary saving with time varying risk/Edmund/BPP/logfiles/"
library(zoo)
source(paste(Rcode_folder,"BPPLikeCarrollSamwick.r",sep=""))

###############################################################################

#sink(paste(log_dir,"create_and_save_moments_log.txt",sep=""))   # Start logging results

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
moments_by_category<- function(category_col, category_set, age_range=TRUE,max_diff=3:5) {
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
    moments_by_category[[this_moments]]=create_moments_CS(this_data,max_diff)
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
if (levels==TRUE) {
  quantiles = quantiles/6.87 #convert to 2015 USD
} else {
  quantiles = exp(quantiles)*100.0/6.87 #convert to 2015 USD
}
moments_by_liquid_wealth_quantile$quantiles = quantiles
save(moments_by_liquid_wealth_quantile,file=paste(moments_dir,'moments_by_liquid_wealth_quantile',tag,'.RData',sep=''))
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
if (levels==TRUE) {
  quantiles = quantiles/6.87 #convert to 2015 USD
} else {
  quantiles = exp(quantiles)*100.0/6.87 #convert to 2015 USD
}
moments_by_net_wealth_quantile$quantiles = quantiles
save(moments_by_net_wealth_quantile,file=paste(moments_dir,'moments_by_net_wealth_quantile',tag,'.RData',sep=''))
###############################################################################


###############################################################################
#create moments excluding car spending and durables
if (lincome_head & levels){
  all_data[,6]=raw_data[,23] #this is c_included for the nocar data
  first_year = all_data[,year_col]!=2004
  for (i in 1:3){
    if (i==1){
      durable_tag = "_head_0nocar"
    } else if (i==2) {
      durable_tag = "_head_nocar"
    } else if (i==3){
      durable_tag = "_head_nodurableproxy"
    }
    all_data[,5]=raw_data[,(19+i)] #this is the relevant c data
    # ###############################################################################
    # # First create moments on all the data
    # moments_all <- create_moments_CS(all_data[(first_year & age_range),])
    # # Check to see if individuals can be identified
    # print(paste("Minimum number of individuals in a cell of moments_all = ", min(moments_all$d_dif)))
    # save(moments_all,file=paste(moments_dir,'moments_all',durable_tag,'.RData',sep=''))
    ###############################################################################
    # Sort into liquid wealth quintiles and calculate related moments
    num_quantiles =5
    mean_liquidwealth = array(0.0, dim=c(y,1))
    # convert to 2015 USD
    if (levels==TRUE) {
      for (k in 0:((y/T)-1)){
        i <- k*T
        mean_liquidwealth[(i+1):(i+T),] = mean(all_data[(i+1):(i+T),liquidasset_col]/6.87,na.rm=TRUE)
      }
    } else {
      for (k in 0:((y/T)-1)){
        i <- k*T
        mean_liquidwealth[(i+1):(i+T),] = mean(100/6.97*exp(all_data[(i+1):(i+T),liquidasset_col]),na.rm=TRUE)
      }
    }
    quantiles = seq(0,1.0,length=(num_quantiles+1))
    quantile_cutoffs = quantile(mean_liquidwealth[age_range][seq(1,(y/T),by=T)],quantiles, na.rm=TRUE)
    wealth_quantile = as.numeric(cut(mean_liquidwealth,breaks=quantile_cutoffs,include.lowest=TRUE, labels=1:num_quantiles))
    wealth_quantile_set = as.character(1:num_quantiles)
    moments_by_liquid_wealth_quantile =moments_by_category(wealth_quantile, wealth_quantile_set,(first_year & age_range))
    # Calculate quantile ranges in terms of dollars, take off 0 and 100% to avoid identifying individuals
    quantiles = quantile_cutoffs[2:(length(quantile_cutoffs)-1)]
    moments_by_liquid_wealth_quantile$quantiles = quantiles
    save(moments_by_liquid_wealth_quantile,file=paste(moments_dir,'moments_by_liquid_wealth_quantile',durable_tag,'.RData',sep=''))
    ##############################################################################
  }
  #reverse changes to all_data
  all_data[,5]=raw_data[,5]
  all_data[,6]=raw_data[,6]
}

# #########################################################################################
# #Now do URE,NNP and Income
# do_nondurableproxy=FALSE
# after_2009 = all_data[,year_col]>=2009
# if (do_nondurableproxy==TRUE) {
#   durable_tag = "_head_nodurableproxy"
#   all_data[,6]=raw_data[,23] #this is c_included for the nocar data
#   all_data[,5]=raw_data[,22] #this is the relevant c data
#   first_year = all_data[,year_col]!=2004
# } else {
#   durable_tag = tag
#   #all_data[,6]=raw_data[,23] #this is c_included for the nocar data
#   #all_data[,5]=raw_data[,20] #this is the relevant c data
#   first_year = all_data[,year_col]!=5900 #(this just returns a true vector)
# }
# mean_cons_with_interest = mean(raw_data[,25][after_2009 & age_range],na.rm=TRUE)
# 
# # ###############################################################################
# # Sort into URE quintiles and calculate related moments
# URE_col =28
# URE = array(0.0, dim=c(y,1))
# for (k in 0:((y/T)-1)){
#   i <- k*T
#   URE[(i+1):(i+T),] = mean(raw_data[(i+1):(i+T),URE_col],na.rm=TRUE)/mean_cons_with_interest
# }
# num_quantiles =10
# 
# quantiles = seq(0,1.0,length=(num_quantiles+1))
# quantile_cutoffs = quantile(URE[age_range][seq(1,(y/T),by=T)],quantiles, na.rm=TRUE)
# URE_quantile = as.numeric(cut(URE,breaks=quantile_cutoffs,include.lowest=TRUE, labels=1:num_quantiles))
# URE_quantile_set = as.character(1:num_quantiles)
# moments_by_URE_quantile =moments_by_category(URE_quantile, URE_quantile_set,(first_year & age_range))
# # Calculate quantile ranges in terms of dollars, take off 0 and 100% to avoid identifying individuals
# quantiles = quantile_cutoffs[2:(length(quantile_cutoffs)-1)]
# moments_by_URE_quantile$quantiles = quantiles
# 
# quantile_means = array(0.0, dim=c(num_quantiles,1))
# quantile_means[1] = mean(URE[(URE<=(quantiles[1])) & age_range & all_data[,4]],na.rm=TRUE)
# for (i in 1:(num_quantiles-2)) {
#   quantile_means[i+1] = mean(URE[(URE>=(quantiles[i])) & (URE<=(quantiles[i+1])) & age_range & all_data[,4]],na.rm=TRUE)
# }
# quantile_means[num_quantiles] = mean(URE[(URE>=(quantiles[num_quantiles-1])) & age_range & all_data[,4]],na.rm=TRUE)
# moments_by_URE_quantile$quantile_means = quantile_means
# 
# save(moments_by_URE_quantile,file=paste(moments_dir,'moments_by_URE_quantile',durable_tag,'.RData',sep=''))
# ###############################################################################
# 
# # ###############################################################################
# # Sort into NNP quintiles and calculate related moments
# NNP_col =31
# NNP = array(0.0, dim=c(y,1))
# for (k in 0:((y/T)-1)){
#   i <- k*T
#   NNP[(i+1):(i+T),] = mean(raw_data[(i+1):(i+T),NNP_col],na.rm=TRUE)/mean_cons_with_interest
# }
# num_quantiles =10
# 
# quantiles = seq(0,1.0,length=(num_quantiles+1))
# quantile_cutoffs = quantile(NNP[age_range][seq(1,(y/T),by=T)],quantiles, na.rm=TRUE)
# NNP_quantile = as.numeric(cut(NNP,breaks=quantile_cutoffs,include.lowest=TRUE, labels=1:num_quantiles))
# NNP_quantile_set = as.character(1:num_quantiles)
# moments_by_NNP_quantile =moments_by_category(NNP_quantile, NNP_quantile_set,(first_year & age_range))
# # Calculate quantile ranges in terms of dollars, take off 0 and 100% to avoid identifying individuals
# quantiles = quantile_cutoffs[2:(length(quantile_cutoffs)-1)]
# moments_by_NNP_quantile$quantiles = quantiles
# 
# quantile_means = array(0.0, dim=c(num_quantiles,1))
# quantile_means[1] = mean(NNP[(NNP<=(quantiles[1])) & age_range & all_data[,4]],na.rm=TRUE)
# for (i in 1:(num_quantiles-2)) {
#   quantile_means[i+1] = mean(NNP[(NNP>=(quantiles[i])) & (NNP<=(quantiles[i+1])) & age_range & all_data[,4]],na.rm=TRUE)
# }
# quantile_means[num_quantiles] = mean(NNP[(NNP>=(quantiles[num_quantiles-1])) & age_range & all_data[,4]],na.rm=TRUE)
# moments_by_NNP_quantile$quantile_means = quantile_means
# 
# save(moments_by_NNP_quantile,file=paste(moments_dir,'moments_by_NNP_quantile',durable_tag,'.RData',sep=''))
# ###############################################################################
# # ###############################################################################
# # Sort into income quintiles and calculate related moments
# Income_col =24
# Income = array(0.0, dim=c(y,1))
# for (k in 0:((y/T)-1)){
#   i <- k*T
#   Income[(i+1):(i+T),] = mean(raw_data[(i+1):(i+T),Income_col],na.rm=TRUE)/mean_cons_with_interest
# }
# num_quantiles =10
# 
# quantiles = seq(0,1.0,length=(num_quantiles+1))
# quantile_cutoffs = quantile(Income[age_range][seq(1,(y/T),by=T)],quantiles, na.rm=TRUE)
# Income_quantile = as.numeric(cut(Income,breaks=quantile_cutoffs,include.lowest=TRUE, labels=1:num_quantiles))
# Income_quantile_set = as.character(1:num_quantiles)
# moments_by_Income_quantile =moments_by_category(Income_quantile, Income_quantile_set,(first_year & age_range))
# # Calculate quantile ranges in terms of dollars, take off 0 and 100% to avoid identifying individuals
# quantiles = quantile_cutoffs[2:(length(quantile_cutoffs)-1)]
# moments_by_Income_quantile$quantiles = quantiles
# 
# quantile_means = array(0.0, dim=c(num_quantiles,1))
# quantile_means[1] = mean(Income[(Income<=(quantiles[1])) & age_range & all_data[,4]],na.rm=TRUE)
# for (i in 1:(num_quantiles-2)) {
#   quantile_means[i+1] = mean(Income[(Income>=(quantiles[i])) & (Income<=(quantiles[i+1])) & age_range & all_data[,4]],na.rm=TRUE)
# }
# quantile_means[num_quantiles] = mean(Income[(Income>=(quantiles[num_quantiles-1])) & age_range & all_data[,4]],na.rm=TRUE)
# moments_by_Income_quantile$quantile_means = quantile_means
# 
# 
# save(moments_by_Income_quantile,file=paste(moments_dir,'moments_by_Income_quantile',durable_tag,'.RData',sep=''))
###############################################################################

#########################################################################################
#Now do URE,NNP and Income
do_nondurableproxy=FALSE
after_2009 = all_data[,year_col]>=2009
if (do_nondurableproxy==TRUE) {
  durable_tag = "_head_nodurableproxy"
  all_data[,6]=raw_data[,23] #this is c_included for the nocar data
  all_data[,5]=raw_data[,22] #this is the relevant c data
  first_year = all_data[,year_col]!=2004
} else {
  durable_tag = tag
  #all_data[,6]=raw_data[,23] #this is c_included for the nocar data
  #all_data[,5]=raw_data[,20] #this is the relevant c data
  first_year = all_data[,year_col]!=5900 #(this just returns a true vector)
}
# ###############################################################################
# Sort into URE quintiles and calculate related moments
URE_col =28
URE = array(0.0, dim=c(y,1))
for (k in 0:((y/T)-1)){
  i <- k*T
  URE[(i+1):(i+T),] = mean(raw_data[(i+1):(i+T),URE_col],na.rm=TRUE)
}
num_quantiles =10

quantile_cutoffs = t(read.csv(paste(empirical_input_folder,"URE_decile_cutoffs.txt",sep=""), sep=",",header=FALSE))
quantile_cutoffs = c(min(URE,na.rm=TRUE)-1000, quantile_cutoffs[1:num_quantiles-1], max(URE,na.rm=TRUE)+1000)
URE_quantile = as.numeric(cut(URE,breaks=quantile_cutoffs,include.lowest=TRUE, labels=1:num_quantiles))
URE_quantile_set = as.character(1:num_quantiles)
moments_by_URE_quantile =moments_by_category(URE_quantile, URE_quantile_set,(first_year & age_range))
# Calculate quantile ranges in terms of dollars, take off 0 and 100% to avoid identifying individuals
quantiles = quantile_cutoffs[2:(length(quantile_cutoffs)-1)]
moments_by_URE_quantile$quantiles = quantiles

quantile_means = read.csv(paste(empirical_input_folder,"URE_decile_means.txt",sep=""), sep=",",header=FALSE)
moments_by_URE_quantile$quantile_means = quantile_means

save(moments_by_URE_quantile,file=paste(moments_dir,'moments_by_URE_quantile',durable_tag,'.RData',sep=''))
###############################################################################


# ###############################################################################
# Sort into NNP quintiles and calculate related moments
NNP_col =31
NNP = array(0.0, dim=c(y,1))
for (k in 0:((y/T)-1)){
  i <- k*T
  NNP[(i+1):(i+T),] = mean(raw_data[(i+1):(i+T),NNP_col],na.rm=TRUE)
}
num_quantiles =10

quantile_cutoffs = t(read.csv(paste(empirical_input_folder,"NNP_decile_cutoffs.txt",sep=""), sep=",",header=FALSE))
quantile_cutoffs = c(min(NNP,na.rm=TRUE)-1000, quantile_cutoffs[1:num_quantiles-1], max(NNP,na.rm=TRUE)+1000)
NNP_quantile = as.numeric(cut(NNP,breaks=quantile_cutoffs,include.lowest=TRUE, labels=1:num_quantiles))
NNP_quantile_set = as.character(1:num_quantiles)
moments_by_NNP_quantile =moments_by_category(NNP_quantile, NNP_quantile_set,(first_year & age_range))
# Calculate quantile ranges in terms of dollars, take off 0 and 100% to avoid identifying individuals
quantiles = quantile_cutoffs[2:(length(quantile_cutoffs)-1)]
moments_by_NNP_quantile$quantiles = quantiles

quantile_means = read.csv(paste(empirical_input_folder,"NNP_decile_means.txt",sep=""), sep=",",header=FALSE)
moments_by_NNP_quantile$quantile_means = quantile_means

save(moments_by_NNP_quantile,file=paste(moments_dir,'moments_by_NNP_quantile',durable_tag,'.RData',sep=''))
###############################################################################



# ###############################################################################
# Sort into Income quintiles and calculate related moments
Income_col =24
Income = array(0.0, dim=c(y,1))
for (k in 0:((y/T)-1)){
  i <- k*T
  Income[(i+1):(i+T),] = mean(raw_data[(i+1):(i+T),Income_col],na.rm=TRUE)
}
num_quantiles =10

quantile_cutoffs = t(read.csv(paste(empirical_input_folder,"Income_decile_cutoffs.txt",sep=""), sep=",",header=FALSE))
quantile_cutoffs = c(min(Income,na.rm=TRUE)-1000, quantile_cutoffs[1:num_quantiles-1], max(Income,na.rm=TRUE)+1000)
Income_quantile = as.numeric(cut(Income,breaks=quantile_cutoffs,include.lowest=TRUE, labels=1:num_quantiles))
Income_quantile_set = as.character(1:num_quantiles)
moments_by_Income_quantile =moments_by_category(Income_quantile, Income_quantile_set,(first_year & age_range))
# Calculate quantile ranges in terms of dollars, take off 0 and 100% to avoid identifying individuals
quantiles = quantile_cutoffs[2:(length(quantile_cutoffs)-1)]
moments_by_Income_quantile$quantiles = quantiles

quantile_means = read.csv(paste(empirical_input_folder,"Income_decile_means.txt",sep=""), sep=",",header=FALSE)
moments_by_Income_quantile$quantile_means = quantile_means

save(moments_by_Income_quantile,file=paste(moments_dir,'moments_by_Income_quantile',durable_tag,'.RData',sep=''))
###############################################################################
#reverse changes to all_data
all_data[,5]=raw_data[,5]
all_data[,6]=raw_data[,6]


# ###############################################################################
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

# break up into two smaller files
moments_by_age_28to55 = list()
for (this_age in 28:55){
  this_moments = paste('age',this_age,sep='')
  moments_by_age_28to55[[this_moments]]=moments_by_age[[this_moments]]
}
save(moments_by_age_28to55,file=paste(moments_dir,'moments_by_age_28to55',tag,'.RData',sep=''))
moments_by_age_56to80 = list()
for (this_age in 56:80){
  this_moments = paste('age',this_age,sep='')
  moments_by_age_56to80[[this_moments]]=moments_by_age[[this_moments]]
}
save(moments_by_age_56to80,file=paste(moments_dir,'moments_by_age_56to80',tag,'.RData',sep=''))
# ###############################################################################

# ###############################################################################
# # Now calculate moments by other categories
# 
# # Do by emp_status_head 
# emp_status_head_col = 11
# emp_status_head_set = c("1 Top level manager","2 Upper level wage earners","3 Mid level wage earners","4 Lower level wage earners","5 Other wage earners","6 Self employed etc","7 Unemployed","8 Outside labor force")
# moments_by_emp_status = moments_by_category(emp_status_head_col, emp_status_head_set,age_range)
# save(moments_by_emp_status,file=paste(moments_dir,'moments_by_emp_status',tag,'.RData',sep=''))
# 
# # Do by agegroup
# agegroup_col = 7
# agegroup_set = c("29","30","40","50","60","70")
# moments_by_agegroup = moments_by_category(agegroup_col, agegroup_set, TRUE)
# save(moments_by_agegroup,file=paste(moments_dir,'moments_by_agegroup',tag,'.RData',sep=''))
# 
# # Do by region
# region_col = 12
# region_set = c("81 Nordjylland","82 Midtjylland","83 Syddanmark","84 Hovedstaden","85 Sjælland")
# moment_by_region =moments_by_category(region_col, region_set, age_range)
# save(moment_by_region,file=paste(moments_dir,'moment_by_region',tag,'.RData',sep=''))
# 
# # Do by highest_educ
# highest_educ_col = 14
# highest_educ_set = c("Primary","Upper secondary","Vocational","Short cycle","BA","Masters +")
# moments_by_educ =moments_by_category(highest_educ_col, highest_educ_set,age_range)
# save(moments_by_educ,file=paste(moments_dir,'moments_by_educ',tag,'.RData',sep=''))
# 
# # Do by industry_agg_head
# industry_agg_head_col = 15
# # removed "11 Uoplyst aktivitet" from the list below - gives crazy results
# industry_agg_head_set = c("1 Landbrug, skovbrug og fiskeri","10 Kultur, fritid og anden service","2 Industri, råstofindvinding og forsyningsvirksomhed","3 Bygge og anlæg","4 Handel og transport mv.","5 Information og kommunikation","6 Finansiering og forsikring","7 Ejendomshandel og udlejning","8 Erhvervsservice","9 Offentlig administration, undervisning og sundhed")
# moments_by_industry =moments_by_category(industry_agg_head_col, industry_agg_head_set,age_range)
# save(moments_by_industry,file=paste(moments_dir,'moments_by_industry',tag,'.RData',sep=''))
# 
# # Do by work_function_agg_head
# work_function_agg_head_col = 17
# work_function_agg_head_set = c("0 Militært arbejde","1 Ledelsesarbejde","2 Arbejde, der forudsætter viden på højeste niveau inden for pågældende område","3 Arbejde, der forudsætter viden på mellemniveau","4 Almindeligt kontor- og kundeservicearbejde","5 Service- og salgsarbejde","6 Arbejde inden for landbrug, skovbrug og fiskeri ekskl. medhjælp","7 Håndværkspræget arbejde","8 Operatør- og monteringsarbejde samt transportarbejde","9 Andet manuelt arbejde")
# moments_by_work_function =moments_by_category(work_function_agg_head_col, work_function_agg_head_set,age_range)
# save(moments_by_work_function,file=paste(moments_dir,'moments_by_work_function',tag,'.RData',sep=''))
# 
# # Do by homeowner
# homeowner_col = 19
# homeowner_set = c("0","1")
# moments_by_home_owner =moments_by_category(homeowner_col, homeowner_set,age_range)
# save(moments_by_home_owner,file=paste(moments_dir,'moments_by_home_owner',tag,'.RData',sep=''))
###############################################################################

###############################################################################
# Look at data growth over periods other than 3 to 5 years
# Increasing the maximum growth period reduces the sample (with possible sample selection)
# Therefore we calculate the moments consistently based on the sample that exists for each 'max_diff' 
this_data = all_data[age_range,]
moments_loop = list()
for (max_diff in 10:10){
  this_moments = paste('moments_',max_diff,sep='')
  moments_loop[[this_moments]]=create_moments_CS(this_data,1:max_diff)
  print(paste("Minimum number of individuals in a cell of ", this_moments, " = ", min(moments_loop[[this_moments]]$d_dif)))
}
save(moments_loop,file=paste(moments_dir,'moments_loop_10',tag,'.RData',sep=''))
# ###############################################################################
# # Look at data growth over periods other than 3 to 5 years
# # Increasing the maximum growth period reduces the sample (with possible sample selection)
# # Therefore we calculate the moments consistently based on the sample that exists for each 'max_diff' 
# this_data = all_data[age_range,]
# moments_loop = list()
# for (max_diff in 10:10){
#   this_moments = paste('moments_',max_diff,sep='')
#   moments_loop[[this_moments]]=create_moments_CS(this_data,1:max_diff)
#   print(paste("Minimum number of individuals in a cell of ", this_moments, " = ", min(moments_loop[[this_moments]]$d_dif)))
# }
# save(moments_loop,file=paste(moments_dir,'moments_loop',tag,'.RData',sep=''))
# # break up into two smaller files
# moments_loop_4to7 = list()
# for (max_diff in 4:7){
#   this_moments = paste('moments_',max_diff,sep='')
#   moments_loop_4to7[[this_moments]]=moments_loop[[this_moments]]
# }
# save(moments_loop_4to7,file=paste(moments_dir,'moments_loop_4to7',tag,'.RData',sep=''))
# moments_loop_8to10 = list()
# for (max_diff in 8:10){
#   this_moments = paste('moments_',max_diff,sep='')
#   moments_loop_8to10[[this_moments]]=moments_loop[[this_moments]]
# }
# save(moments_loop_8to10,file=paste(moments_dir,'moments_loop_8to10',tag,'.RData',sep=''))
###############################################################################


###############################################################################
# Sort into liquid wealth quintiles and calculate using diff 10 moments
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
moments_by_liquid_wealth_quantile_10 =moments_by_category(wealth_quantile, wealth_quantile_set,age_range,max_diff=1:10)
# Calculate quantile ranges in terms of dollars, take off 0 and 100% to avoid identifying individuals
quantiles = quantile_cutoffs[2:(length(quantile_cutoffs)-1)]
if (levels==TRUE) {
  quantiles = quantiles/6.87 #convert to 2015 USD
} else {
  quantiles = exp(quantiles)*100.0/6.87 #convert to 2015 USD
}
moments_by_liquid_wealth_quantile_10$quantiles = quantiles
save(moments_by_liquid_wealth_quantile_10,file=paste(moments_dir,'moments_by_liquid_wealth_quantile_10',tag,'.RData',sep=''))
#just quantiles 1 and 5
moments_by_liquid_wealth_quantile_10_1and5 = list()
moments_by_liquid_wealth_quantile_10_1and5[['X1']] = moments_by_liquid_wealth_quantile_10[['X1']]
moments_by_liquid_wealth_quantile_10_1and5[['X5']] = moments_by_liquid_wealth_quantile_10[['X5']]
save(moments_by_liquid_wealth_quantile_10_1and5,file=paste(moments_dir,'moments_by_liquid_wealth_quantile_10_1and5',tag,'.RData',sep=''))

###############################################################################
