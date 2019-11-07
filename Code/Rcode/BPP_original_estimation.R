# This file does the minimum distance optimization for BPP to create Table 6 in their paper

R_code_folder =  "C:/Users/edmun/OneDrive/Documents/Research/Denmark/IncomeUncertaintyGit/Code/Rcode"
moments_BPP_dir = "C:/Users/edmun/OneDrive/Documents/Research/Denmark/IncomeUncertaintyGit/Code/ServerRcode/ServerOutput/BPP_original"

source(paste(R_code_folder,"/min_distance_BPP.r", sep=""))

###############################################################################
#First load the moments
c_vector = as.vector(t(read.csv(file=paste(moments_BPP_dir,"/moments_all_c_vector.txt", sep=""), header=FALSE, sep=",")))
omega =    as.matrix(read.csv(file=paste(moments_BPP_dir,"/moments_all_omega.txt",    sep=""), header=FALSE, sep=","))
T=12

#Next replicate BPP
BPP_output = BPP_parameter_estimation(c_vector, omega, T, ma=1, taste=1) 


###############################################################################
# Function to estimate parameters for each category for which we have moments
estimation_by_category_BPP<- function(moments_BPP_dir,moments_stub,category_set, T=12) {
  category_params = array(0, dim=c(length(category_set),4))
  category_se = array(0, dim=c(length(category_set),4))
  for (i in 1:length(category_set)){
    this_category = as.character(category_set[i])
    this_c_vector = as.vector(t(read.csv(file=paste(moments_BPP_dir,"/",moments_stub,i,"c_vector.txt", sep=""), header=FALSE, sep=",")))
    this_omega = as.matrix(read.csv(file=paste(moments_BPP_dir,"/",moments_stub,i,"_omega.txt", sep=""), header=FALSE, sep=","))
    this_BPP_output = BPP_parameter_estimation(this_c_vector, this_omega,T) 
    category_params[i,1] = mean(this_BPP_output$var_perm)
    category_params[i,2] = mean(this_BPP_output$var_tran)
    category_params[i,3] = this_BPP_output$ins_perm
    category_params[i,4] = this_BPP_output$ins_tran
    category_se[i,1] = mean(this_BPP_output$var_perm_se)
    category_se[i,2] = mean(this_BPP_output$var_tran_se)
    category_se[i,3] = this_BPP_output$ins_perm_se
    category_se[i,4] = this_BPP_output$ins_tran_se
  }
  output = list("category_params"=category_params,"category_se"=category_se)
  return (output)
}



###############################################################################
# load liquid weath quintile data and create graph
moments_stub = "moments_by_liquid_wealth_quantile"
num_quantiles = 5
round_digits = -3
wealth_quantile_set = as.character(1:num_quantiles)
output =estimation_by_category_BPP(moments_BPP_dir,moments_stub, make.names(wealth_quantile_set), T=12)
wealth_quantile_output=output
wealth_quantile_params = output$category_params
wealth_quantile_se = output$category_se
wealth_quantile_set = c(paste('$0-',format(round(moments_by_liquid_wealth_quantile$quantiles[[1]],round_digits),big.mark=",", trim=TRUE),sep=''))
wealth_quantile_set = c(wealth_quantile_set,paste('$',format(round(moments_by_liquid_wealth_quantile$quantiles[[1]],round_digits),big.mark=",", trim=TRUE),'-',format(round(moments_by_liquid_wealth_quantile$quantiles[[2]],round_digits),big.mark=",", trim=TRUE),sep=''))
wealth_quantile_set = c(wealth_quantile_set,paste('$',format(round(moments_by_liquid_wealth_quantile$quantiles[[2]],round_digits),big.mark=",", trim=TRUE),'-',format(round(moments_by_liquid_wealth_quantile$quantiles[[3]],round_digits),big.mark=",", trim=TRUE),sep=''))
wealth_quantile_set = c(wealth_quantile_set,paste('$',format(round(moments_by_liquid_wealth_quantile$quantiles[[3]],round_digits),big.mark=",", trim=TRUE),'-',format(round(moments_by_liquid_wealth_quantile$quantiles[[4]],round_digits),big.mark=",", trim=TRUE),sep=''))
wealth_quantile_set = c(wealth_quantile_set,paste('> $',format(round(moments_by_liquid_wealth_quantile$quantiles[[4]],round_digits),big.mark=",", trim=TRUE),sep=''))
plot_estimataion_output(wealth_quantile_params,wealth_quantile_se,wealth_quantile_set ,"Liquid Wealth Quantile","LiquidWealth")
###############################################################################