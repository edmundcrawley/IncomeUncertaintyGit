###########################################################################
# 
# Split by income volatility
# 
# Original BPP estimates can be obtained by running the last part of the code
###############################################################################


# Set folders and load code
Rcode_folder = "E:/ProjektDB/706172/Workdata/706172/Husholdningsprojekt/Precautionary saving with time varying risk/Edmund/BPP/Rcode/BPPLikeCarrollSamwick/"
empirical_input_folder = "E:/ProjektDB/706172/Workdata/706172/Husholdningsprojekt/Precautionary saving with time varying risk/Edmund/BPP/save/"
moments_dir = "E:/ProjektDB/706172/Workdata/706172/Husholdningsprojekt/Precautionary saving with time varying risk/Edmund/BPP/save/moments/"
log_dir = "E:/ProjektDB/706172/Workdata/706172/Husholdningsprojekt/Precautionary saving with time varying risk/Edmund/BPP/logfiles/"
library(zoo)
source(paste(Rcode_folder,"BPPLikeCarrollSamwick.r",sep=""))
source(paste(Rcode_folder,"BPP_original.r",sep=""))



###############################################################################
# 
# Low volatility
# 
###############################################################################

empirical_input_file="input_for_R_fullsample_level_lincome_low_inc_vol.csv"
tag = "_level_lincome_low_inc_vol"


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
# Sort into liquid wealth quintiles and calculate related moments
num_quantiles =5
mean_log_liquidwealth = array(0.0, dim=c(y,1))
for (k in 0:((y/T)-1)){
  i <- k*T
  mean_log_liquidwealth[(i+1):(i+T),] = mean(all_data[(i+1):(i+T),liquidasset_col],na.rm=TRUE)
}
#quantiles = seq(0,1.0,length=(num_quantiles+1))
#quantile_cutoffs = quantile(mean_log_liquidwealth[age_range][seq(1,(y/T),by=T)],quantiles, na.rm=TRUE)
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


#moments by quantiles
for(quantile_type in c("liquid_wealth_quantile")){
  if (quantile_type=="liquid_wealth_quantile" || quantile_type=="net_wealth_quantile") {
    num_quantiles = 5
  }
  else {
    num_quantiles = 10
  }
  
  
  this_moment = paste('moments_by_',quantile_type,'$quantiles',sep ="")
  this_file = paste(moments_dir,'moments_low_inc_vol_by_',quantile_type,'_quantiles','.txt',sep ='')
  write.table(eval(parse(text = this_moment)),file = this_file,row.names = FALSE, col.names = FALSE, na ="",sep =',')
  
  for(i in 1:num_quantiles){
    this_moment = paste('moments_by_',quantile_type,'$X',i,'$','c_vector',sep ="")
    this_file = paste(moments_dir,'moments_low_inc_vol_by_',quantile_type,i,'c_vector','.txt',sep ='')
    write.table(eval(parse(text = this_moment)),file = this_file,row.names = FALSE, col.names = FALSE, na ="",sep =',')
    
    this_moment = paste('moments_by_',quantile_type,'$X',i,'$','omega',sep ="")
    this_file = paste(moments_dir,'moments_low_inc_vol_by_',quantile_type,i,'_omega','.txt',sep ='')
    write.table(eval(parse(text = this_moment)),file = this_file,row.names = FALSE, col.names = FALSE, na ="",sep =',')
    
    this_moment = paste('moments_by_',quantile_type,'$X',i,'$','delta_y_var',sep ="")
    this_file = paste(moments_dir,'moments_low_inc_vol_by_',quantile_type,i,'_delta_y_var','.txt',sep ='')
    write.table(eval(parse(text = this_moment)),file = this_file,row.names = FALSE, col.names = FALSE, na ="",sep =',')
    
    
  }
}





# High volatility
empirical_input_file="input_for_R_fullsample_level_lincome_high_inc_vol.csv"
tag = "_level_lincome_high_inc_vol"


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
# Sort into liquid wealth quintiles and calculate related moments
num_quantiles =5
mean_log_liquidwealth = array(0.0, dim=c(y,1))
for (k in 0:((y/T)-1)){
  i <- k*T
  mean_log_liquidwealth[(i+1):(i+T),] = mean(all_data[(i+1):(i+T),liquidasset_col],na.rm=TRUE)
}
#quantiles = seq(0,1.0,length=(num_quantiles+1))
#quantile_cutoffs = quantile(mean_log_liquidwealth[age_range][seq(1,(y/T),by=T)],quantiles, na.rm=TRUE)
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



#moments by quantiles
for(quantile_type in c("liquid_wealth_quantile")){
  if (quantile_type=="liquid_wealth_quantile" || quantile_type=="net_wealth_quantile") {
    num_quantiles = 5
  }
  else {
    num_quantiles = 10
  }
  
  this_moment = paste('moments_by_',quantile_type,'$quantiles',sep ="")
  this_file = paste(moments_dir,'moments_high_inc_vol_by_',quantile_type,'_quantiles','.txt',sep ='')
  write.table(eval(parse(text = this_moment)),file = this_file,row.names = FALSE, col.names = FALSE, na ="",sep =',')
  
  
  for(i in 1:num_quantiles){
    this_moment = paste('moments_by_',quantile_type,'$X',i,'$','c_vector',sep ="")
    this_file = paste(moments_dir,'moments_high_inc_vol_by_',quantile_type,i,'c_vector','.txt',sep ='')
    write.table(eval(parse(text = this_moment)),file = this_file,row.names = FALSE, col.names = FALSE, na ="",sep =',')
    
    this_moment = paste('moments_by_',quantile_type,'$X',i,'$','omega',sep ="")
    this_file = paste(moments_dir,'moments_high_inc_vol_by_',quantile_type,i,'_omega','.txt',sep ='')
    write.table(eval(parse(text = this_moment)),file = this_file,row.names = FALSE, col.names = FALSE, na ="",sep =',')
    
    this_moment = paste('moments_by_',quantile_type,'$X',i,'$','delta_y_var',sep ="")
    this_file = paste(moments_dir,'moments_high_inc_vol_by_',quantile_type,i,'_delta_y_var','.txt',sep ='')
    write.table(eval(parse(text = this_moment)),file = this_file,row.names = FALSE, col.names = FALSE, na ="",sep =',')
    
      }
}









###########################################################################
# BPP low volatility
###########################################################################

empirical_input_file="input_for_R_fullsample_level_lincome_low_inc_vol.csv"
tag = "_level_lincome_low_inc_vol"
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
moments_by_category_BPP<- function(category_col, category_set, age_range=TRUE) {
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
  
  moments_by_category_BPP = list()
  for (i in 1:length(category_set)){
    this_category = as.character(category_set[i])
    this_data = all_data[category_2013==this_category & age_range,]
    this_moments = make.names(this_category)
    moments_by_category_BPP[[this_moments]]=create_moments_BPP(this_data)
    print(paste("Minimum number of individuals in a cell of ", this_moments, " = ", min(moments_by_category_BPP[[this_moments]]$d_dif)))
  }
  return (moments_by_category_BPP)
}

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
moments_by_liquid_wealth_quantile =moments_by_category_BPP(wealth_quantile, wealth_quantile_set,age_range)
# Calculate quantile ranges in terms of dollars, take off 0 and 100% to avoid identifying individuals
quantiles = quantile_cutoffs[2:(length(quantile_cutoffs)-1)]
if (levels==TRUE) {
  quantiles = quantiles/6.87 #convert to 2015 USD
} else {
  quantiles = exp(quantiles)*100.0/6.87 #convert to 2015 USD
}
moments_by_liquid_wealth_quantile$quantiles = quantiles
save(moments_by_liquid_wealth_quantile,file=paste(moments_dir,'BPP_moments_by_liquid_wealth_quantile',tag,'.RData',sep=''))
###############################################################################


#moments by quantiles
for(quantile_type in c("liquid_wealth_quantile")){
  if (quantile_type=="liquid_wealth_quantile" || quantile_type=="net_wealth_quantile") {
    num_quantiles = 5
  }
  else {
    num_quantiles = 10
  }
  
  this_moment = paste('moments_by_',quantile_type,'$quantiles',sep ="")
  this_file = paste(moments_dir,'BPP_moments_low_inc_vol_by_',quantile_type,'_quantiles','.txt',sep ='')
  write.table(eval(parse(text = this_moment)),file = this_file,row.names = FALSE, col.names = FALSE, na ="",sep =',')
  
  
  for(i in 1:num_quantiles){
    this_moment = paste('moments_by_',quantile_type,'$X',i,'$','c_vector',sep ="")
    this_file = paste(moments_dir,'BPP_moments_low_inc_vol_by_',quantile_type,i,'c_vector','.txt',sep ='')
    write.table(eval(parse(text = this_moment)),file = this_file,row.names = FALSE, col.names = FALSE, na ="",sep =',')
    
    this_moment = paste('moments_by_',quantile_type,'$X',i,'$','omega',sep ="")
    this_file = paste(moments_dir,'BPP_moments_low_inc_vol_by_',quantile_type,i,'_omega','.txt',sep ='')
    write.table(eval(parse(text = this_moment)),file = this_file,row.names = FALSE, col.names = FALSE, na ="",sep =',')
    
#    this_moment = paste('moments_by_',quantile_type,'$X',i,'$','delta_y_var',sep ="")
#    this_file = paste(moments_dir,'BPP_moments_low_inc_vol_by_',quantile_type,i,'_delta_y_var','.txt',sep ='')
#    write.table(eval(parse(text = this_moment)),file = this_file,row.names = FALSE, col.names = FALSE, na ="",sep =',')
    
  }
}





###########################################################################
# BPP high volatility
###########################################################################

empirical_input_file="input_for_R_fullsample_level_lincome_high_inc_vol.csv"
tag = "_level_lincome_high_inc_vol"
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
moments_by_category_BPP<- function(category_col, category_set, age_range=TRUE) {
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
  
  moments_by_category_BPP = list()
  for (i in 1:length(category_set)){
    this_category = as.character(category_set[i])
    this_data = all_data[category_2013==this_category & age_range,]
    this_moments = make.names(this_category)
    moments_by_category_BPP[[this_moments]]=create_moments_BPP(this_data)
    print(paste("Minimum number of individuals in a cell of ", this_moments, " = ", min(moments_by_category_BPP[[this_moments]]$d_dif)))
  }
  return (moments_by_category_BPP)
}

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
moments_by_liquid_wealth_quantile =moments_by_category_BPP(wealth_quantile, wealth_quantile_set,age_range)
# Calculate quantile ranges in terms of dollars, take off 0 and 100% to avoid identifying individuals
quantiles = quantile_cutoffs[2:(length(quantile_cutoffs)-1)]
if (levels==TRUE) {
  quantiles = quantiles/6.87 #convert to 2015 USD
} else {
  quantiles = exp(quantiles)*100.0/6.87 #convert to 2015 USD
}
moments_by_liquid_wealth_quantile$quantiles = quantiles
save(moments_by_liquid_wealth_quantile,file=paste(moments_dir,'BPP_moments_by_liquid_wealth_quantile',tag,'.RData',sep=''))
###############################################################################


#moments by quantiles
for(quantile_type in c("liquid_wealth_quantile")){
  if (quantile_type=="liquid_wealth_quantile" || quantile_type=="net_wealth_quantile") {
    num_quantiles = 5
  }
  else {
    num_quantiles = 10
  }
  
  this_moment = paste('moments_by_',quantile_type,'$quantiles',sep ="")
  this_file = paste(moments_dir,'BPP_moments_high_inc_vol_by_',quantile_type,'_quantiles','.txt',sep ='')
  write.table(eval(parse(text = this_moment)),file = this_file,row.names = FALSE, col.names = FALSE, na ="",sep =',')
  
  
  for(i in 1:num_quantiles){
    this_moment = paste('moments_by_',quantile_type,'$X',i,'$','c_vector',sep ="")
    this_file = paste(moments_dir,'BPP_moments_high_inc_vol_by_',quantile_type,i,'c_vector','.txt',sep ='')
    write.table(eval(parse(text = this_moment)),file = this_file,row.names = FALSE, col.names = FALSE, na ="",sep =',')
    
    this_moment = paste('moments_by_',quantile_type,'$X',i,'$','omega',sep ="")
    this_file = paste(moments_dir,'BPP_moments_high_inc_vol_by_',quantile_type,i,'_omega','.txt',sep ='')
    write.table(eval(parse(text = this_moment)),file = this_file,row.names = FALSE, col.names = FALSE, na ="",sep =',')
    
#    this_moment = paste('moments_by_',quantile_type,'$X',i,'$','delta_y_var',sep ="")
#    this_file = paste(moments_dir,'BPP_moments_high_inc_vol_by_',quantile_type,i,'_delta_y_var','.txt',sep ='')
#    write.table(eval(parse(text = this_moment)),file = this_file,row.names = FALSE, col.names = FALSE, na ="",sep =',')
    
  }
}






