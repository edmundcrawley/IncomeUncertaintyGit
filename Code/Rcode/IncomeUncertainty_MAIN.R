###############################################################################
# 
# This file takes the moment data from the servers, 
# performs GMM estimation on it, and creates graphs
#
# 
###############################################################################

# Set folders
Rcode_folder = "C:/Users/edmun/OneDrive/Documents/Research/Denmark/IncomeUncertaintyGit/Code/Rcode/"
moments_dir = "C:/Users/edmun/OneDrive/Documents/Research/Denmark/IncomeUncertaintyGit/Code/ServerRcode/ServerOutput/"
figures_dir = "C:/Users/edmun/OneDrive/Documents/Research/Denmark/IncomeUncertaintyGit/Code/Rcode/Figures/"
# if running for production store figures here:
#figures_dir = "C:/Users/edmun/OneDrive/Documents/Research/Denmark/IncomeUncertaintyGit/Paper/Figures"
require(zoo)
source(paste(Rcode_folder,"min_distance_CS.r",sep=""))
###############################################################################


###############################################################################
# Define some useful functions

# Function to estimate parameters for each category for which we have moments
estimation_by_category<- function(category_moments,category_set) {
  category_params = array(0, dim=c(length(category_set),4))
  category_se = array(0, dim=c(length(category_set),4))
  category_obs = array(0, dim=c(length(category_set)))
  category_total_var = array(0, dim=c(length(category_set)))
  for (i in 1:length(category_set)){
    this_category = as.character(category_set[i])
    this_moments <- category_moments[[this_category]]
    this_c_vector <- this_moments[["c_vector"]]
    this_omega <- this_moments[["omega"]]
    T <- this_moments[["T"]]
    this_CS_output = CS_parameter_estimation(this_c_vector, this_omega,T) 
    category_params[i,1] = this_CS_output$var_perm
    category_params[i,2] = this_CS_output$var_tran
    category_params[i,3] = this_CS_output$ins_perm
    category_params[i,4] = this_CS_output$ins_tran
    category_se[i,1] = this_CS_output$var_perm_se
    category_se[i,2] = this_CS_output$var_tran_se
    category_se[i,3] = this_CS_output$ins_perm_se
    category_se[i,4] = this_CS_output$ins_tran_se
    category_total_var[i] = this_moments$delta_y_var
  }
  output = list("category_params"=category_params,"category_se"=category_se,"category_obs"=category_obs,"category_total_var"=category_total_var)
  return (output)
}

# Function to plot shock variances and consumption elasticities
plot_estimataion_output<- function(params, se, labels, category_for_title, category_for_save) {
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
  
  # Now plot the Expenditure Elasticities
  dev.new()
  barCenters <- barplot(t(params[,3:4]),names.arg=labels,cex.names=0.8,beside=TRUE,col=c("green","red"))
  par(mar=c(8,7,4,5)+0.1)
  plotTop = max(params[,3:4])*1.2
  barCenters <- barplot(height=t(params[,3:4]),
                        names.arg=labels,
                        cex.names=0.75,
                        beside=TRUE,col=c("green","red"),
                        las=2,ylim=c(0,plotTop), xaxt="n",
                        main=paste("Expenditure Elasticities by ",category_for_title),
                        ylab = "Elasticity", border="black", axes=TRUE)
  text(x=barCenters[1,]+1, y =-plotTop*0.02,srt=45, adj=1, labels=labels,xpd=TRUE)
  segments(barCenters, t(params[,3:4]-se[,3:4]*1.96),
           barCenters,
           t(params[,3:4]+se[,3:4]*1.96), lwd=1.5)
  arrows(barCenters, t(params[,3:4]-se[,3:4]*1.96),
         barCenters,
         t(params[,3:4]+se[,3:4]*1.96), lwd=1.5,
         angle=90,code=3, length=0.05)
  legend(2, plotTop, legend=c("Permanent", "Transitory"), fill=c("green","red"),bty="n")
  dev.copy(png, paste(figures_dir, "MPXBy",category_for_save,".png",sep=""))
  dev.off()
}
###############################################################################

###############################################################################
# load liquid weath quintile data and create graph
load(paste(moments_dir,'moments_by_liquid_wealth_quantile','.RData',sep=''))
num_quantiles = 5
wealth_quantile_set = as.character(1:num_quantiles)
output =estimation_by_category(moments_by_liquid_wealth_quantile, make.names(wealth_quantile_set))
wealth_quantile_output=output
wealth_quantile_params = output$category_params
wealth_quantile_se = output$category_se
wealth_quantile_obs = output$category_obs
wealth_quantile_total_var = output$category_total_var
wealth_quantile_set = c(paste('$0-',round(moments_by_liquid_wealth_quantile$quantiles[[1]]),sep=''))
wealth_quantile_set = c(wealth_quantile_set,paste('$',round(moments_by_liquid_wealth_quantile$quantiles[[1]]),'-',round(moments_by_liquid_wealth_quantile$quantiles[[2]]),sep=''))
wealth_quantile_set = c(wealth_quantile_set,paste('$',round(moments_by_liquid_wealth_quantile$quantiles[[2]]),'-',round(moments_by_liquid_wealth_quantile$quantiles[[3]]),sep=''))
wealth_quantile_set = c(wealth_quantile_set,paste('$',round(moments_by_liquid_wealth_quantile$quantiles[[3]]),'-',round(moments_by_liquid_wealth_quantile$quantiles[[4]]),sep=''))
wealth_quantile_set = c(wealth_quantile_set,paste('$',round(moments_by_liquid_wealth_quantile$quantiles[[4]]),'+',sep=''))
plot_estimataion_output(wealth_quantile_params,wealth_quantile_se,wealth_quantile_set ,"Liquid Wealth Quantile","LiquidWealth")
###############################################################################

###############################################################################
# load net weath quintile data and create graph
load(paste(moments_dir,'moments_by_net_wealth_quantile','.RData',sep=''))
num_quantiles = 5
wealth_quantile_set = as.character(1:num_quantiles)
output =estimation_by_category(moments_by_net_wealth_quantile, make.names(wealth_quantile_set))
wealth_quantile_output=output
wealth_quantile_params = output$category_params
wealth_quantile_se = output$category_se
wealth_quantile_obs = output$category_obs
wealth_quantile_total_var = output$category_total_var
wealth_quantile_set = c(paste('$...-',round(moments_by_net_wealth_quantile$quantiles[[1]]),sep=''))
wealth_quantile_set = c(wealth_quantile_set,paste('$',round(moments_by_net_wealth_quantile$quantiles[[1]]),'-',round(moments_by_net_wealth_quantile$quantiles[[2]]),sep=''))
wealth_quantile_set = c(wealth_quantile_set,paste('$',round(moments_by_net_wealth_quantile$quantiles[[2]]),'-',round(moments_by_net_wealth_quantile$quantiles[[3]]),sep=''))
wealth_quantile_set = c(wealth_quantile_set,paste('$',round(moments_by_net_wealth_quantile$quantiles[[3]]),'-',round(moments_by_net_wealth_quantile$quantiles[[4]]),sep=''))
wealth_quantile_set = c(wealth_quantile_set,paste('$',round(moments_by_net_wealth_quantile$quantiles[[4]]),'+',sep=''))
plot_estimataion_output(wealth_quantile_params,wealth_quantile_se,wealth_quantile_set ,"Net Wealth Quantile","NetWealth")
###############################################################################

###############################################################################
# Moments by age
# saved data is divided into two files to reduce file size, put them together first
moments_by_age=list()
load(paste(moments_dir,'moments_by_age_28to55','.RData',sep=''))
load(paste(moments_dir,'moments_by_age_56to80','.RData',sep=''))
for (this_age in 28:55){
  this_moment = paste('age',this_age,sep='')
  moments_by_age[[this_moment]] = moments_by_age_28to55[[this_moment]]
}
for (this_age in 56:80){
  this_moment = paste('age',this_age,sep='')
  moments_by_age[[this_moment]] = moments_by_age_56to80[[this_moment]]
}
age_set = 28:80
age_params = array(0, dim=c(length(age_set),4))
age_se = array(0, dim=c(length(age_set),4))
age_obs = array(0, dim=c(length(age_set)))
age_total_var = array(0, dim=c(length(age_set)))
for (i in 1:length(age_set)){
  this_age = age_set[i]
  this_moments <- moments_by_age[[paste('age',this_age,sep='')]]
  this_c_vector <- this_moments[["c_vector"]]
  this_omega <- this_moments[["omega"]]
  T <- this_moments[["T"]]
  this_CS_output = CS_parameter_estimation(this_c_vector, this_omega, T) 
  age_params[i,1] = this_CS_output$var_perm
  age_params[i,2] = this_CS_output$var_tran
  age_params[i,3] = this_CS_output$ins_perm
  age_params[i,4] = this_CS_output$ins_tran
  age_se[i,1] = this_CS_output$var_perm_se
  age_se[i,2] = this_CS_output$var_tran_se
  age_se[i,3] = this_CS_output$ins_perm_se
  age_se[i,4] = this_CS_output$ins_tran_se
  age_total_var[i] = this_moments$delta_y_var
}

png(filename=paste(figures_dir,'VarianceByAge.png',sep=''))
plot(age_set, age_params[,1],col="green",main="Permanent and Transitory Variance by Age",xlab="Age",ylab="Shock Variance",ylim=c(0,0.04))
points(age_set, age_params[,2],col="red")
points(age_set, age_total_var,col="black")
lines(age_set, rollmean(age_params[,1],5,fill=NA), col="green")
lines(age_set, rollmean(age_params[,2],5,fill=NA), col="red")
lines(age_set, rollmean(age_total_var,5,fill=NA), col="black")
lines(age_set, rollmean(2.0/3.0*age_params[,1]+2.0*age_params[,2],5,fill=NA), col="black",lty="dashed")
legend(40, 0.035, legend=c("Permanent Var", "Transitory Var", expression(paste("var(",Delta,"y)")),expression(paste("Model implied var(",Delta,"y)"))), col=c("green","red","black","black"),lty=c("solid","solid","solid","dashed"))
dev.off()

png(filename=paste(figures_dir,'MPXByAge.png',sep=''))
plot(age_set, age_params[,3],col="green",main="Expenditure Elasticity by Age",xlab="Age",ylab="Elasticity",ylim=c(0,1))
points(age_set, age_params[,4],col="red")
lines(age_set, rollmean(age_params[,3],5,fill=NA), col="green")
lines(age_set, rollmean(age_params[,4],5,fill=NA), col="red")
legend(38, 0.24, legend=c("Permanent", "Transitory"), col=c("green","red"),lty=c("solid","solid"))
dev.off()
###############################################################################

###############################################################################
# Moments by different growth period
# saved data is divided into two files to reduce file size, put them together first
moments_loop=list()
load(paste(moments_dir,'moments_loop_4to7','.RData',sep=''))
load(paste(moments_dir,'moments_loop_8to10','.RData',sep=''))
for (i in 4:7){
  this_moment = paste('moments_',i,sep='')
  moments_loop[[this_moment]] = moments_loop_4to7[[this_moment]]
}
for (i in 8:10){
  this_moment = paste('moments_',i,sep='')
  moments_loop[[this_moment]] = moments_loop_8to10[[this_moment]]
}

# Plot regression coefficient of expenditure growth vs income growth for different growth periods
reg_coefs = array(0,dim=c(10,1))
start_col=1
for (n in 1:10){
  reg_coefs[n] = mean(moments_loop$moments_10$moment_cy[,start_col:(start_col+10-n)])/mean(moments_loop$moments_10$moment_y2[,start_col:(start_col+10-n)])
  start_col = start_col+n
}

