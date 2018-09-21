
#################################################################################
# 
# This file takes the moment data from the servers, 
# performs GMM estimation on it, and creates graphs
#
# 
###############################################################################

tag = "_level_lincome_head"
if (substr(tag,1,6)=="_level") {
  title_string = "MPX"
  axis_string = "MPX"
} else {
  title_string = "Expenditure Elasticity"
  axis_string = "Elasticity"
}

# Set folders
Rcode_folder = "C:/Users/edmun/OneDrive/Documents/Research/Denmark/IncomeUncertaintyGit/Code/Rcode/"
moments_dir = "C:/Users/edmun/OneDrive/Documents/Research/Denmark/IncomeUncertaintyGit/Code/ServerRcode/ServerOutput/"
figures_dir = "C:/Users/edmun/OneDrive/Documents/Research/Denmark/IncomeUncertaintyGit/Code/Rcode/Figures/"
tables_dir = "C:/Users/edmun/OneDrive/Documents/Research/Denmark/IncomeUncertaintyGit/Code/Rcode/Tables/"
PythonResults_folder = "C:/Users/edmun/OneDrive/Documents/Research/Denmark/IncomeUncertaintyGit/Code/PrefShockModel/Results/"

# if running for production store figures here:
#figures_dir = "C:/Users/edmun/OneDrive/Documents/Research/Denmark/IncomeUncertaintyGit/Paper/Figures"
require(zoo)
require(latex2exp)
source(paste(Rcode_folder,"min_distance_CS.r",sep=""))
###############################################################################
colors = c("#fc8d59","#91bfdb","#ffffbf")


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
plot_estimataion_output<- function(params, se, labels, category_for_title, category_for_save,transitory_only=FALSE) {
  # First plot the variances
  dev.new()
  if (transitory_only){
    param_cols=2
    this_colors=c(colors[2])
    this_legend =c( expression(paste(sigma[q]^2," Transitory Var")))
    xlabel_pos = 0
  } else {
    param_cols=1:2
    this_colors=c(colors[1],colors[2])
    this_legend=c(expression(paste(sigma[p]^2," Permanent Var")), expression(paste(sigma[q]^2," Transitory Var")))
    xlabel_pos = 1
  }
  par(mar=c(8,7,4,5)+0.1,cex.axis=1.2,cex.lab=1.5)
  plotTop = max(params[,param_cols])*1.2
  barCenters <- barplot(height=t(params[,param_cols]),
                        names.arg=labels,
                        cex.names=0.75,
                        beside=TRUE,col=this_colors,
                        las=2,ylim=c(0,plotTop), xaxt="n",
                        main=paste("Permanent and Transitory Variance by ",category_for_title),
                        ylab = "Shock Variance\n", border="black", axes=TRUE)
  text(x=barCenters[1,]+xlabel_pos, y =-plotTop*0.02,srt=45, adj=1, labels=labels,xpd=TRUE)
  segments(barCenters, t(params[,param_cols]-se[,param_cols]*1.96),
           barCenters,
           t(params[,param_cols]+se[,param_cols]*1.96), lwd=1.5)
  arrows(barCenters, t(params[,param_cols]-se[,param_cols]*1.96),
         barCenters,
         t(params[,param_cols]+se[,param_cols]*1.96), lwd=1.5,
         angle=90,code=3, length=0.05)
  legend(2, plotTop, legend=this_legend, fill=this_colors,bty="n")
  #dev.copy(png, paste(figures_dir, "VarianceBy",category_for_save,tag,".png",sep=""))
  dev.copy(pdf, paste(figures_dir, "VarianceBy",category_for_save,tag,".pdf",sep=""))
  #dev.copy(svg, paste(figures_dir, "VarianceBy",category_for_save,tag,".svg",sep=""))
  dev.off()
  
  # Now plot the Expenditure Elasticities
  dev.new()
  if (transitory_only){
    param_cols=4
    this_legend=c(expression(paste(psi," Transitory MPX")))
  } else {
    param_cols=3:4
    this_legend=c(expression(paste(phi," Permanent MPX")),expression(paste(psi," Transitory MPX")))
  }
  par(mar=c(8,7,4,5)+0.1,cex.axis=1.2,cex.lab=1.5)
  barCenters <- barplot(t(params[,param_cols]),names.arg=labels,cex.names=0.8,beside=TRUE,col=this_colors)
  plotTop = max(max(params[,param_cols]),1.0)
  barCenters <- barplot(height=t(params[,param_cols]),
                        names.arg=labels,
                        cex.names=0.75,
                        beside=TRUE,col=this_colors,
                        las=2,ylim=c(0,plotTop), xaxt="n",
                        main=paste(title_string, " by ",category_for_title),
                        ylab = axis_string, border="black", axes=TRUE)
  text(x=barCenters[1,]+xlabel_pos, y =-plotTop*0.02,srt=45, adj=1, labels=labels,xpd=TRUE)
  segments(barCenters, t(params[,param_cols]-se[,param_cols]*1.96),
           barCenters,
           t(params[,param_cols]+se[,param_cols]*1.96), lwd=1.5)
  arrows(barCenters, t(params[,param_cols]-se[,param_cols]*1.96),
         barCenters,
         t(params[,param_cols]+se[,param_cols]*1.96), lwd=1.5,
         angle=90,code=3, length=0.05)
  legend(2, plotTop, legend=this_legend, fill=this_colors,bty="n")
  #dev.copy(png, paste(figures_dir, "MPXBy",category_for_save,tag,".png",sep=""))
  dev.copy(pdf, paste(figures_dir, "MPXBy",category_for_save,tag,".pdf",sep=""))
  #dev.copy(svg, paste(figures_dir, "MPXBy",category_for_save,tag,".svg",sep=""))
  dev.off()
}
###############################################################################

###############################################################################
# load liquid weath quintile data and create graph
load(paste(moments_dir,'moments_by_liquid_wealth_quantile',tag,'.RData',sep=''))
num_quantiles = 5
round_digits = -3
wealth_quantile_set = as.character(1:num_quantiles)
output =estimation_by_category(moments_by_liquid_wealth_quantile, make.names(wealth_quantile_set))
wealth_quantile_output=output
wealth_quantile_params = output$category_params
wealth_quantile_se = output$category_se
wealth_quantile_obs = output$category_obs
wealth_quantile_total_var = output$category_total_var
wealth_quantile_set = c(paste('$0-',format(round(moments_by_liquid_wealth_quantile$quantiles[[1]],round_digits),big.mark=",", trim=TRUE),sep=''))
wealth_quantile_set = c(wealth_quantile_set,paste('$',format(round(moments_by_liquid_wealth_quantile$quantiles[[1]],round_digits),big.mark=",", trim=TRUE),'-',format(round(moments_by_liquid_wealth_quantile$quantiles[[2]],round_digits),big.mark=",", trim=TRUE),sep=''))
wealth_quantile_set = c(wealth_quantile_set,paste('$',format(round(moments_by_liquid_wealth_quantile$quantiles[[2]],round_digits),big.mark=",", trim=TRUE),'-',format(round(moments_by_liquid_wealth_quantile$quantiles[[3]],round_digits),big.mark=",", trim=TRUE),sep=''))
wealth_quantile_set = c(wealth_quantile_set,paste('$',format(round(moments_by_liquid_wealth_quantile$quantiles[[3]],round_digits),big.mark=",", trim=TRUE),'-',format(round(moments_by_liquid_wealth_quantile$quantiles[[4]],round_digits),big.mark=",", trim=TRUE),sep=''))
wealth_quantile_set = c(wealth_quantile_set,paste('> $',format(round(moments_by_liquid_wealth_quantile$quantiles[[4]],round_digits),big.mark=",", trim=TRUE),sep=''))
plot_estimataion_output(wealth_quantile_params,wealth_quantile_se,wealth_quantile_set ,"Liquid Wealth Quantile","LiquidWealth")
###############################################################################

###############################################################################
# load net weath quintile data and create graph
load(paste(moments_dir,'moments_by_net_wealth_quantile',tag,'.RData',sep=''))
num_quantiles = 5
round_digits = -3
wealth_quantile_set = as.character(1:num_quantiles)
output =estimation_by_category(moments_by_net_wealth_quantile, make.names(wealth_quantile_set))
wealth_quantile_output=output
wealth_quantile_params = output$category_params
wealth_quantile_se = output$category_se
wealth_quantile_obs = output$category_obs
wealth_quantile_total_var = output$category_total_var
wealth_quantile_set = c(paste('< ',format(round(moments_by_net_wealth_quantile$quantiles[[1]],round_digits),big.mark=",", trim=TRUE),sep=''))
wealth_quantile_set = c(wealth_quantile_set,paste('$',format(round(moments_by_net_wealth_quantile$quantiles[[1]],round_digits),big.mark=",", trim=TRUE),'-',format(round(moments_by_net_wealth_quantile$quantiles[[2]],round_digits),big.mark=",", trim=TRUE),sep=''))
wealth_quantile_set = c(wealth_quantile_set,paste('$',format(round(moments_by_net_wealth_quantile$quantiles[[2]],round_digits),big.mark=",", trim=TRUE),'-',format(round(moments_by_net_wealth_quantile$quantiles[[3]],round_digits),big.mark=",", trim=TRUE),sep=''))
wealth_quantile_set = c(wealth_quantile_set,paste('$',format(round(moments_by_net_wealth_quantile$quantiles[[3]],round_digits),big.mark=",", trim=TRUE),'-',format(round(moments_by_net_wealth_quantile$quantiles[[4]],round_digits),big.mark=",", trim=TRUE),sep=''))
wealth_quantile_set = c(wealth_quantile_set,paste('> $',format(round(moments_by_net_wealth_quantile$quantiles[[4]],round_digits),big.mark=",", trim=TRUE),sep=''))
plot_estimataion_output(wealth_quantile_params,wealth_quantile_se,wealth_quantile_set ,"Net Wealth Quantile","NetWealth")
###############################################################################

###############################################################################
# Moments by age
# saved data is divided into two files to reduce file size, put them together first
moments_by_age=list()
load(paste(moments_dir,'moments_by_age_28to55',tag,'.RData',sep=''))
load(paste(moments_dir,'moments_by_age_56to80',tag,'.RData',sep=''))
for (this_age in 28:55){
  this_moment = paste('age',this_age,sep='')
  moments_by_age[[this_moment]] = moments_by_age_28to55[[this_moment]]
}
for (this_age in 56:80){
  this_moment = paste('age',this_age,sep='')
  moments_by_age[[this_moment]] = moments_by_age_56to80[[this_moment]]
}
age_set = 28:80
age_set = 28:70
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
age_set = age_set-5 #age is age at the end of 2004-2015. Take away 5 years to represent mid-period
#png(filename=paste(figures_dir,'VarianceByAge',tag,'.png',sep=''))
pdf(file=paste(figures_dir,'VarianceByAge',tag,'.pdf',sep=''))
#svg(filename=paste(figures_dir,'VarianceByAge',tag,'.svg',sep=''))
par(mar=c(8,7,4,5)+0.1,cex.axis=1.2,cex.lab=1.5)
plot(age_set, age_params[,1],col="green",main="Permanent and Transitory Variance by Age",xlab="Age",ylab="Shock Variance",ylim=c(0,0.025))
points(age_set, age_params[,2],col="red")
points(age_set, age_total_var,col="black")
lines(age_set, rollmean(age_params[,1],5,fill=NA), col="green")
lines(age_set, rollmean(age_params[,2],5,fill=NA), col="red")
lines(age_set, rollmean(age_total_var,5,fill=NA), col="black")
lines(age_set, rollmean(2.0/3.0*age_params[,1]+2.0*age_params[,2],5,fill=NA), col="black",lty="dashed")
legend(40, 0.022, legend=c(expression(paste(sigma[p]^2," Permanent Var")), expression(paste(sigma[q]^2," Transitory Var")), expression(paste("var(",Delta,"y)")),expression(paste(frac(2,3),sigma[p]^2,"+2",sigma[q]^2,sep=""))), col=c("green","red","black","black"),lty=c("solid","solid","solid","dashed"),bty="n")
dev.off()

#png(filename=paste(figures_dir,'MPXByAge',tag,'.png',sep=''))
pdf(file=paste(figures_dir,'MPXByAge',tag,'.pdf',sep=''))
#svg(filename=paste(figures_dir,'MPXByAge',tag,'.svg',sep=''))
par(mar=c(8,7,4,5)+0.1,cex.axis=1.2,cex.lab=1.5)
plot(age_set, age_params[,3],col="green",main=paste(title_string, " by Age",sep=""),xlab="Age",ylab=axis_string,ylim=c(0,1))
points(age_set, age_params[,4],col="red")
lines(age_set, rollmean(age_params[,3],5,fill=NA), col="green")
lines(age_set, rollmean(age_params[,4],5,fill=NA), col="red")
legend(38, 0.24, legend=c(expression(paste(phi," Permanent MPX")),expression(paste(psi," Transitory MPX"))), col=c("green","red"),lty=c("solid","solid"),bty="n")
dev.off()
###############################################################################
# 
###############################################################################
# Moments by different growth period
# saved data is divided into two files to reduce file size, put them together first
moments_loop=list()
load(paste(moments_dir,'moments_loop_4to7',tag,'.RData',sep=''))
load(paste(moments_dir,'moments_loop_8to10',tag,'.RData',sep=''))
for (i in 4:7){
  this_moment = paste('moments_',i,sep='')
  moments_loop[[this_moment]] = moments_loop_4to7[[this_moment]]
}
for (i in 8:10){
  this_moment = paste('moments_',i,sep='')
  moments_loop[[this_moment]] = moments_loop_8to10[[this_moment]]
}
#instead just load 10 diff moment
# load(paste(moments_dir,'moments_loop_10',tag,'.RData',sep=''))

###############################################################################
# Plot regression coefficient of expenditure growth vs income growth for different growth periods
max_diff = 10
this_moment = paste('moments_',max_diff,sep='')
T = moments_loop[[this_moment]]$T
reg_coefs = array(0,dim=c(max_diff,1))
std_errors = array(0,dim=c(max_diff,1))
# Use an average of the variance of delta y over every year, and an average of the covariance of delta y and delta c
for (n in 1:max_diff){
  moments_used =c()
  k=1
  for (i in 1:max_diff){
    if (i==n){
      moments_used = c(moments_used, k:(k+max_diff-i))
    }
    k=k+max_diff-i+1
  }
  moments_used_all=c()
  for (i in 0:(2*(T-max_diff+1)-1)){
    moments_used_all = c(moments_used_all,moments_used+i*(max_diff*(max_diff+1))/2)
  }
  num_moments = length(moments_used_all)/2
  reg_coefs[n] = mean(moments_loop[[this_moment]]$c_vector[moments_used_all[(num_moments+1):(2*num_moments)]])/mean(moments_loop[[this_moment]]$c_vector[moments_used_all[1:num_moments]])
  omega = moments_loop[[this_moment]]$omega
  function_gradient = array(0,dim=c(dim(omega)[1],1))
  function_gradient[moments_used_all[(num_moments+1):(2*num_moments)],] = 1.0/((moments_loop[[this_moment]]$c_vector[moments_used_all[1:num_moments]])*length(moments_used_all[(num_moments+1):(2*num_moments)]))
  function_gradient[moments_used_all[1:num_moments],] = -moments_loop[[this_moment]]$c_vector[moments_used_all[(num_moments+1):(2*num_moments)]]/((moments_loop[[this_moment]]$c_vector[moments_used_all[1:num_moments]])**2 *length(moments_used_all[(num_moments+1):(2*num_moments)]))
  std_errors[n] = (t(function_gradient)  %*% omega %*% function_gradient)**0.5
}
###############################################################################
# Find regression coefficients for high and low liquid wealth quantiles
load(paste(moments_dir,'moments_by_liquid_wealth_quantile_10_1and5',tag,'.RData',sep=''))
max_diff = 10
reg_coefs_liquid_wealth = list()
std_errors_liquid_wealth = list()
for (this_moment in c("X1","X5")) {
  reg_coefs_liquid_wealth[[this_moment]] = array(0,dim=c(max_diff,1))
  std_errors_liquid_wealth[[this_moment]] = array(0,dim=c(max_diff,1))
  T = moments_by_liquid_wealth_quantile_10_1and5[[this_moment]]$T
  # Use an average of the variance of delta y over every year, and an average of the covariance of delta y and delta c
  for (n in 1:max_diff){
    moments_used =c()
    k=1
    for (i in 1:max_diff){
      if (i==n){
        moments_used = c(moments_used, k:(k+max_diff-i))
      }
      k=k+max_diff-i+1
    }
    moments_used_all=c()
    for (i in 0:(2*(T-max_diff+1)-1)){
      moments_used_all = c(moments_used_all,moments_used+i*(max_diff*(max_diff+1))/2)
    }
    num_moments = length(moments_used_all)/2
    reg_coefs_liquid_wealth[[this_moment]][n] = mean(moments_by_liquid_wealth_quantile_10_1and5[[this_moment]]$c_vector[moments_used_all[(num_moments+1):(2*num_moments)]])/mean(moments_by_liquid_wealth_quantile_10_1and5[[this_moment]]$c_vector[moments_used_all[1:num_moments]])
    omega = moments_by_liquid_wealth_quantile_10_1and5[[this_moment]]$omega
    function_gradient = array(0,dim=c(dim(omega)[1],1))
    function_gradient[moments_used_all[(num_moments+1):(2*num_moments)],] = 1.0/((moments_by_liquid_wealth_quantile_10_1and5[[this_moment]]$c_vector[moments_used_all[1:num_moments]])*length(moments_used_all[(num_moments+1):(2*num_moments)]))
    function_gradient[moments_used_all[1:num_moments],] = -moments_by_liquid_wealth_quantile_10_1and5[[this_moment]]$c_vector[moments_used_all[(num_moments+1):(2*num_moments)]]/((moments_by_liquid_wealth_quantile_10_1and5[[this_moment]]$c_vector[moments_used_all[1:num_moments]])**2 *length(moments_used_all[(num_moments+1):(2*num_moments)]))
    std_errors_liquid_wealth[[this_moment]][n] = (t(function_gradient)  %*% omega %*% function_gradient)**0.5
  }
}
###############################################################################
# Pull in consumption saving numbers from Python output
FromPython <- scan(paste(PythonResults_folder,'basic_regressions.txt',sep=''), what=double(), sep=",")
FromPython <- scan(paste(PythonResults_folder,'benchmark_br_all.txt',sep=''), what=double(), sep=",")
solow_spending = 0.75
# Now draw graph
#png(paste(figures_dir, "basic_regression_complete",tag,".png",sep=""))
pdf(paste(figures_dir, "basic_regression_complete",tag,".pdf",sep=""))
#svg(paste(figures_dir, "basic_regression_complete",tag,".svg",sep=""))
par(mar=c(8,7,4,5)+0.1,cex.axis=1.2,cex.lab=1.5)
plot(array(0.0,dim=dim(reg_coefs)), ylim=c(0,1),lty='solid',col='blue',type='o', xlab='N, Years of Growth',ylab=TeX('$\\beta^N$, Regression Coefficient'),main='Regressing Consumption Growth on Income Growth')
legend(6, 0.53, legend=c("Complete Markets","","",""), col=c("blue","green","red","black"),lty=c("solid","solid","solid","solid"),bty="n")
dev.off()
#png(paste(figures_dir, "basic_regression_solow",tag,".png",sep=""))
pdf(paste(figures_dir, "basic_regression_solow",tag,".pdf",sep=""))
#svg(paste(figures_dir, "basic_regression_solow",tag,".svg",sep=""))
par(mar=c(8,7,4,5)+0.1,cex.axis=1.2,cex.lab=1.5)
plot(array(0.0,dim=dim(reg_coefs)), ylim=c(0,1),lty='solid',col='blue',type='o', xlab='N, Years of Growth',ylab=TeX('$\\beta^N$, Regression Coefficient'),main='Regressing Consumption Growth on Income Growth')
lines(array(solow_spending,dim=dim(reg_coefs)),lty='solid',col='green',type='o')
legend(6, 0.53, legend=c("Complete Markets","Solow","",""), col=c("blue","green","red","black"),lty=c("solid","solid","solid","solid"),bty="n")
dev.off()
#png(paste(figures_dir, "basic_regression_BS",tag,".png",sep=""))
pdf(paste(figures_dir, "basic_regression_BS",tag,".pdf",sep=""))
#svg(paste(figures_dir, "basic_regression_BS",tag,".svg",sep=""))
par(mar=c(8,7,4,5)+0.1,cex.axis=1.2,cex.lab=1.5)
plot(array(0.0,dim=dim(reg_coefs)), ylim=c(0,1),lty='solid',col='blue',type='o', xlab='N, Years of Growth',ylab=TeX('$\\beta^N$, Regression Coefficient'),main='Regressing Consumption Growth on Income Growth')
lines(array(solow_spending,dim=dim(reg_coefs)),lty='solid',col='green',type='o')
lines(FromPython[1:dim(reg_coefs)[1]],lty='solid',col='red',type='o')
legend(6, 0.53, legend=c("Complete Markets","Solow","Buffer-Stock",""), col=c("blue","green","red","black"),lty=c("solid","solid","solid","solid"),bty="n")
arrows(2, 0.12, 9, 0.12)
arrows(9, 0.12, 2, 0.12)
text(3.5, 0.22, labels = "Relatively more \n transitory variance")
text(7.5, 0.22, labels = "Relatively more \n permanent variance")
dev.off()
#png(paste(figures_dir, "basic_regression",tag,".png",sep=""))
pdf(paste(figures_dir, "basic_regression",tag,".pdf",sep=""))
#svg(paste(figures_dir, "basic_regression",tag,".svg",sep=""))
par(mar=c(8,7,4,5)+0.1,cex.axis=1.2,cex.lab=1.5)
plot(array(0.0,dim=dim(reg_coefs)), ylim=c(0,1),lty='solid',col='blue',type='o', xlab='N, Years of Growth',ylab=TeX('$\\beta^N$, Regression Coefficient'),main='Regressing Consumption Growth on Income Growth')
lines(array(solow_spending,dim=dim(reg_coefs)),lty='solid',col='green',type='o')
lines(FromPython[1:dim(reg_coefs)[1]],lty='solid',col='red',type='o')
points(reg_coefs)
lines(reg_coefs)
lines(reg_coefs+1.96*std_errors,lty='dashed')
lines(reg_coefs-1.96*std_errors,lty='dashed')
legend(6, 0.53, legend=c("Complete Markets","Solow","Buffer-Stock","Data"), col=c("blue","green","red","black"),lty=c("solid","solid","solid","solid"),bty="n")
arrows(2, 0.12, 9, 0.12)
arrows(9, 0.12, 2, 0.12)
text(3.5, 0.22, labels = "Relatively more \n transitory variance")
text(7.5, 0.22, labels = "Relatively more \n permanent variance")
dev.off()
#png(paste(figures_dir, "basic_regression_liquid_wealth",tag,".png",sep=""))
pdf(paste(figures_dir, "basic_regression_liquid_wealth",tag,".pdf",sep=""))
#svg(paste(figures_dir, "basic_regression_liquid_wealth",tag,".svg",sep=""))
par(mar=c(8,7,4,5)+0.1,cex.axis=1.2,cex.lab=1.5)
plot(array(0.0,dim=dim(reg_coefs)), ylim=c(0,1),lty='solid',col='blue',type='o', xlab='N, Years of Growth',ylab=TeX('$\\beta^N$, Regression Coefficient'),main='Regressing Consumption Growth on Income Growth')
lines(array(solow_spending,dim=dim(reg_coefs)),lty='solid',col='green',type='o')
lines(FromPython[1:dim(reg_coefs)[1]],lty='solid',col='red',type='o')
points(reg_coefs,col='gray')
lines(reg_coefs,col='gray')
lines(reg_coefs+1.96*std_errors,lty='dashed',col='gray')
lines(reg_coefs-1.96*std_errors,lty='dashed',col='gray')
#Add high and low liquid quantiles
points(reg_coefs_liquid_wealth$X1)
lines(reg_coefs_liquid_wealth$X1)
lines(reg_coefs_liquid_wealth$X1+1.96*std_errors_liquid_wealth$X1,lty='dashed')
lines(reg_coefs_liquid_wealth$X1-1.96*std_errors_liquid_wealth$X1,lty='dashed')
points(reg_coefs_liquid_wealth$X5)
lines(reg_coefs_liquid_wealth$X5)
lines(reg_coefs_liquid_wealth$X5+1.96*std_errors_liquid_wealth$X5,lty='dashed')
lines(reg_coefs_liquid_wealth$X5-1.96*std_errors_liquid_wealth$X5,lty='dashed')
legend(6, 0.53, legend=c("Complete Markets","Solow","Buffer-Stock","Data"), col=c("blue","green","red","black"),lty=c("solid","solid","solid","solid"),bty="n")
arrows(2, 0.12, 9, 0.12)
arrows(9, 0.12, 2, 0.12)
text(3.5, 0.22, labels = "Relatively more \n transitory variance")
text(7.5, 0.22, labels = "Relatively more \n permanent variance")
text(3.0, 0.92, labels = "Least Liquid")
text(3.75, 0.44, labels = "Most liquid")
dev.off()

##############################################################################
# Plot how estimates vary using different growth periods
params_loop = list()
for (max_diff in 4:10){
  this_moments = paste('moments_',max_diff,sep='')
  moments_all = moments_loop[[this_moments]]

  var_tran_array = array(0.0, dim=c(max_diff,max_diff))
  var_perm_array = array(0.0, dim=c(max_diff,max_diff))
  ins_tran_array = array(0.0, dim=c(max_diff,max_diff))
  ins_perm_array = array(0.0, dim=c(max_diff,max_diff))

  var_tran_array_se = array(0.0, dim=c(max_diff,max_diff))
  var_perm_array_se = array(0.0, dim=c(max_diff,max_diff))
  ins_tran_array_se = array(0.0, dim=c(max_diff,max_diff))
  ins_perm_array_se = array(0.0, dim=c(max_diff,max_diff))

  for (n1 in 1:(max_diff-1)){
    for (n2 in (n1+1):max_diff){
      diff_to_use = c(n1,n2)
      cols_per_diff = max_diff-diff_to_use+1
      moments_used =c()
      j=1
      k=1
      for (i in 1:max_diff){
        if (i==diff_to_use[j]){
          moments_used = c(moments_used, k:(k+max_diff-i))
          j = j+1
          if (j>2){
            break
          }
        }
        k=k+max_diff-i+1
      }
      moments_used_all=c()
      for (i in 0:(2*(T-max_diff+1)-1)){
        moments_used_all = c(moments_used_all,moments_used+i*(max_diff*(max_diff+1))/2)
      }
      c_vector_sub = moments_all$c_vector[moments_used_all]
      omega_sub    = moments_all$omega[moments_used_all,][,moments_used_all]
      CS_output_sub = CS_parameter_estimation(c_vector_sub, omega_sub, T-(max_diff-diff_to_use[-1]),diff_to_use,cols_per_diff)
      var_perm_array[n1,n2] = CS_output_sub$var_perm
      var_tran_array[n1,n2] = CS_output_sub$var_tran
      ins_perm_array[n1,n2] = CS_output_sub$ins_perm
      ins_tran_array[n1,n2] = CS_output_sub$ins_tran

      var_perm_array_se[n1,n2] = CS_output_sub$var_perm_se
      var_tran_array_se[n1,n2] = CS_output_sub$var_tran_se
      ins_perm_array_se[n1,n2] = CS_output_sub$ins_perm_se
      ins_tran_array_se[n1,n2] = CS_output_sub$ins_tran_se
    }
  }
  params_loop[[paste('var_perm_array_',max_diff,sep='')]] =  var_perm_array
  params_loop[[paste('var_tran_array_',max_diff,sep='')]] =  var_tran_array
  params_loop[[paste('ins_perm_array_',max_diff,sep='')]] =  ins_perm_array
  params_loop[[paste('ins_tran_array_',max_diff,sep='')]] =  ins_tran_array

  params_loop[[paste('var_perm_array_se_',max_diff,sep='')]] =  var_perm_array_se
  params_loop[[paste('var_tran_array_se_',max_diff,sep='')]] =  var_tran_array_se
  params_loop[[paste('ins_perm_array_se_',max_diff,sep='')]] =  ins_perm_array_se
  params_loop[[paste('ins_tran_array_se_',max_diff,sep='')]] =  ins_tran_array_se

  write.table(var_perm_array, file=paste(tables_dir,'var_perm_array_',max_diff,tag,'.txt',sep=''), row.names=FALSE, col.names=FALSE)
  write.table(var_tran_array, file=paste(tables_dir,'var_tran_array_',max_diff,tag,'.txt',sep=''), row.names=FALSE, col.names=FALSE)
  write.table(ins_perm_array, file=paste(tables_dir,'ins_perm_array_',max_diff,tag,'.txt',sep=''), row.names=FALSE, col.names=FALSE)
  write.table(ins_tran_array, file=paste(tables_dir,'ins_tran_array_',max_diff,tag,'.txt',sep=''), row.names=FALSE, col.names=FALSE)
}

to_plot = 'ins_tran_array_'
n2_minum_n1 = 2
max_diff = 8
plot(diag(params_loop[[paste(to_plot,max_diff,sep='')]][,-(1:n2_minum_n1)]), ylim=c(0.5,0.75))
for (i in 4:max_diff){
  points(diag(params_loop[[paste(to_plot,i,sep='')]][,-(1:n2_minum_n1)]))
  lines(diag(params_loop[[paste(to_plot,i,sep='')]][,-(1:n2_minum_n1)]))
}
lines(diag(params_loop[[paste(to_plot,7,sep='')]][,-(1:n2_minum_n1)]+1.96*params_loop[[paste(to_plot,'se_',7,sep='')]][,-(1:n2_minum_n1)]),lty='dashed')
lines(diag(params_loop[[paste(to_plot,7,sep='')]][,-(1:n2_minum_n1)]-1.96*params_loop[[paste(to_plot,'se_',7,sep='')]][,-(1:n2_minum_n1)]),lty='dashed')
lines(diag(params_loop[[paste(to_plot,max_diff,sep='')]][,-(1:n2_minum_n1)]+1.96*params_loop[[paste(to_plot,'se_',max_diff,sep='')]][,-(1:n2_minum_n1)]),lty='dashed')
lines(diag(params_loop[[paste(to_plot,max_diff,sep='')]][,-(1:n2_minum_n1)]-1.96*params_loop[[paste(to_plot,'se_',max_diff,sep='')]][,-(1:n2_minum_n1)]),lty='dashed')
###############################################################################
# Plot both variance and covariance at different growth periods
max_diff=10
y2_diff = array(0.0, dim=c(max_diff,1))
cy_diff = array(0.0, dim=c(max_diff,1))
regcoef_diff = array(0.0, dim=c(max_diff,1))

this_col=1
for (i in 1:max_diff){
  y2_diff[i] = mean(moments_all$moment_y2[,(this_col):(this_col+max_diff-i)])
  cy_diff[i] = mean(moments_all$moment_cy[,(this_col):(this_col+max_diff-i)])
  regcoef_diff[i] = mean(moments_all$reg_coef[,(this_col):(this_col+max_diff-i)])
  this_col = this_col+max_diff-i+1
}
n1=3
n2=5
diff_to_use = c(n1,n2)
cols_per_diff = max_diff-diff_to_use+1
moments_used =c()
j=1
k=1
for (i in 1:max_diff){
  if (i==diff_to_use[j]){
    moments_used = c(moments_used, k:(k+max_diff-i))
    j = j+1
    if (j>2){
      break
    }
  }
  k=k+max_diff-i+1
}
moments_used_all=c()
for (i in 0:(2*(T-max_diff+1)-1)){
  moments_used_all = c(moments_used_all,moments_used+i*(max_diff*(max_diff+1))/2)
}
c_vector_sub = moments_all$c_vector[moments_used_all]
omega_sub    = moments_all$omega[moments_used_all,][,moments_used_all]
CS_output_sub = CS_parameter_estimation(c_vector_sub, omega_sub, T-(max_diff-diff_to_use[-1]),diff_to_use,cols_per_diff)

#png(paste(figures_dir, "IncreasingDiff",tag,".png",sep=""))
pdf(paste(figures_dir, "IncreasingDiff",tag,".pdf",sep=""))
#svg(paste(figures_dir, "IncreasingDiff",tag,".svg",sep=""))
par(mar=c(8,7,4,5)+0.1,cex.axis=1.2,cex.lab=1.5)
plot(1:max_diff,y2_diff,ylim=c(0,1.2*max(y2_diff)),xlim=c(0,max_diff),
     main="Covariance with Increasing Difference Operator",xlab="N",ylab="Variance/Covariance")
lines(1:max_diff,y2_diff)
points(1:max_diff,cy_diff)
lines(1:max_diff,cy_diff,lty="dashed")
lines(0:max_diff,(0:max_diff-1.0/3.0)*CS_output_sub$var_perm + 2*CS_output_sub$var_tran, col="red")
lines(0:max_diff,(0:max_diff-1.0/3.0)*CS_output_sub$ins_perm*CS_output_sub$var_perm + 2*CS_output_sub$ins_tran*CS_output_sub$var_tran, col="green")
legend(0, 0.037, legend=c(expression(paste("var(",Delta^N,"y) Empirical"),paste("var(",Delta^N,"y) matched to N=3,4,5"), paste("cov(",Delta^N,"y,",Delta^N,"c) Empirical"),paste("cov(",Delta^N,"y,",Delta^N,"c) matched to n=3,4,5"))),lty=c("solid","solid","dashed","solid"),col=c("black","red","black","green"),bty="n")
dev.off()
###############################################################################
#
# ###############################################################################
# # load homeowner data and create graph
# load(paste(moments_dir,'moments_by_home_owner',tag,'.RData',sep=''))
# output =estimation_by_category(moments_by_home_owner, c("X0","X1"))
# home_owner_output=output
# home_owner_params = output$category_params
# home_owner_se = output$category_se
# home_owner_obs = output$category_obs
# home_owner_total_var = output$category_total_var
# home_owner_set = c("Renter","Owner")
# plot_estimataion_output(home_owner_params,home_owner_se,home_owner_set ,"Homeownership","")
# ###############################################################################

###############################################################################
# load liquid weath quintile data by non-durable proxyand create graph
load(paste(moments_dir,'moments_by_liquid_wealth_quantile_head_0nocar','.RData',sep=''))
num_quantiles = 5
round_digits = -3
wealth_quantile_set = as.character(1:num_quantiles)
output =estimation_by_category(moments_by_liquid_wealth_quantile, make.names(wealth_quantile_set))
wealth_quantile_output=output
wealth_quantile_params = output$category_params
wealth_quantile_se = output$category_se
wealth_quantile_obs = output$category_obs
wealth_quantile_total_var = output$category_total_var

load(paste(moments_dir,'moments_by_liquid_wealth_quantile_head_nocar','.RData',sep=''))
wealth_quantile_set = as.character(1:num_quantiles)
output =estimation_by_category(moments_by_liquid_wealth_quantile, make.names(wealth_quantile_set))
wealth_quantile_output_nocar=output
wealth_quantile_params_nocar = output$category_params
wealth_quantile_se_nocar = output$category_se
wealth_quantile_obs_nocar = output$category_obs
wealth_quantile_total_var_nocar = output$category_total_var

load(paste(moments_dir,'moments_by_liquid_wealth_quantile_head_nodurableproxy','.RData',sep=''))
wealth_quantile_set = as.character(1:num_quantiles)
output =estimation_by_category(moments_by_liquid_wealth_quantile, make.names(wealth_quantile_set))
wealth_quantile_output_nodurableproxy=output
wealth_quantile_params_nodurableproxy = output$category_params
wealth_quantile_se_nodurableproxy = output$category_se
wealth_quantile_obs_nodurableproxy = output$category_obs
wealth_quantile_total_var_nodurableproxy = output$category_total_var

wealth_quantile_set = c(paste('$0-',format(round(moments_by_liquid_wealth_quantile$quantiles[[1]],round_digits),big.mark=",", trim=TRUE),sep=''))
wealth_quantile_set = c(wealth_quantile_set,paste('$',format(round(moments_by_liquid_wealth_quantile$quantiles[[1]],round_digits),big.mark=",", trim=TRUE),'-',format(round(moments_by_liquid_wealth_quantile$quantiles[[2]],round_digits),big.mark=",", trim=TRUE),sep=''))
wealth_quantile_set = c(wealth_quantile_set,paste('$',format(round(moments_by_liquid_wealth_quantile$quantiles[[2]],round_digits),big.mark=",", trim=TRUE),'-',format(round(moments_by_liquid_wealth_quantile$quantiles[[3]],round_digits),big.mark=",", trim=TRUE),sep=''))
wealth_quantile_set = c(wealth_quantile_set,paste('$',format(round(moments_by_liquid_wealth_quantile$quantiles[[3]],round_digits),big.mark=",", trim=TRUE),'-',format(round(moments_by_liquid_wealth_quantile$quantiles[[4]],round_digits),big.mark=",", trim=TRUE),sep=''))
wealth_quantile_set = c(wealth_quantile_set,paste('$',format(round(moments_by_liquid_wealth_quantile$quantiles[[4]],round_digits),big.mark=",", trim=TRUE),'+',sep=''))

dev.new()
par(mar=c(8,7,4,5)+0.1,cex.axis=1.2,cex.lab=1.5)
#par(mar=c(8,7,4,5)+0.1)
plotTop = max(max(wealth_quantile_params[,3:4]),1.0)
barCenters <- barplot(height=t(wealth_quantile_params[,3:4]),
                      names.arg=wealth_quantile_set,
                      cex.names=0.75,
                      beside=TRUE,col=c("grey90","grey85"),
                      las=2,ylim=c(-0,plotTop), xaxt="n",
                      main=paste("MPX by Liquid Wealth Quantile"),
                      ylab = axis_string, border=NA, axes=TRUE)
barCenters <- barplot(height=t(wealth_quantile_params_nocar[,3:4]),
                      names.arg=wealth_quantile_set,
                      cex.names=0.75,
                      beside=TRUE,col=c("grey80","grey75"),
                      las=2,ylim=c(0,plotTop), xaxt="n",
                      main=paste("MPX by Liquid Wealth Quantile"),
                      ylab = axis_string, border=NA, axes=TRUE,add=TRUE)
text(x=barCenters[1,]+1, y =-plotTop*0.02,srt=45, adj=1, labels=wealth_quantile_set,xpd=TRUE)
barCenters <- barplot(height=t(wealth_quantile_params_nodurableproxy[,3:4]),
                      names.arg=wealth_quantile_set,
                      cex.names=0.75,
                      beside=TRUE,col=c(colors[1],colors[2]),
                      las=2,ylim=c(0,plotTop), xaxt="n",
                      main=paste("MPX by Liquid Wealth Quantile"),
                      ylab = axis_string, border="black", axes=TRUE,add=TRUE)
text(x=barCenters[1,]+1, y =-plotTop*0.02,srt=45, adj=1, labels=wealth_quantile_set,xpd=TRUE)
segments(barCenters, t(wealth_quantile_params_nodurableproxy[,3:4]-wealth_quantile_se_nodurableproxy[,3:4]*1.96),
         barCenters,
         t(wealth_quantile_params_nodurableproxy[,3:4]+wealth_quantile_se_nodurableproxy[,3:4]*1.96), lwd=1.5)
arrows(barCenters, t(wealth_quantile_params_nodurableproxy[,3:4]-wealth_quantile_se_nodurableproxy[,3:4]*1.96),
       barCenters,
       t(wealth_quantile_params_nodurableproxy[,3:4]+wealth_quantile_se_nodurableproxy[,3:4]*1.96), lwd=1.5,
       angle=90,code=3, length=0.05)
text_x_pos = barCenters[1,4]
text_y_pos = 1.0
text(x=barCenters[1,4]+1, y =text_y_pos, adj = c(0,0), labels="All Expenditure",xpd=TRUE,col="grey90")
text(x=barCenters[1,4]+1, y =text_y_pos-0.05, adj = c(0,0), labels="Excluding Cars",xpd=TRUE,col="grey80")
text(x=barCenters[1,4]+1, y =text_y_pos-0.1, adj = c(0,0), labels="Non-durable Proxy",xpd=TRUE)
legend(2, plotTop, legend=c(expression(paste(phi," Permanent MPX")),expression(paste(psi," Transitory MPX"))), fill=c(colors[1],colors[2]),bty="n")
#dev.copy(png, paste(figures_dir, "MPXByDurables_nodurableproxy.png",sep=""))
dev.copy(pdf, paste(figures_dir, "MPXByDurables_nodurableproxy.pdf",sep=""))
#dev.copy(svg, paste(figures_dir, "MPXByDurables_nodurableproxy.svg",sep=""))
dev.off()


dev.new()
par(mar=c(8,7,4,5)+0.1,cex.axis=1.2,cex.lab=1.5)
plotTop = max(max(wealth_quantile_params[,3:4]), 1.0)
barCenters <- barplot(height=t(wealth_quantile_params[,3:4]),
                      names.arg=wealth_quantile_set,
                      cex.names=0.75,
                      beside=TRUE,col=c("grey90","grey85"),
                      las=2,ylim=c(0,plotTop), xaxt="n",
                      main=paste("MPX by Liquid Wealth Quantile"),
                      ylab = axis_string, border=NA, axes=TRUE)
text(x=barCenters[1,]+1, y =-plotTop*0.02,srt=45, adj=1, labels=wealth_quantile_set,xpd=TRUE)
barCenters <- barplot(height=t(wealth_quantile_params_nocar[,3:4]),
                      names.arg=wealth_quantile_set,
                      cex.names=0.75,
                      beside=TRUE,col=c(colors[1],colors[2]),
                      las=2,ylim=c(0,plotTop), xaxt="n",
                      main=paste("MPX by Liquid Wealth Quantile"),
                      ylab = axis_string, border="black", axes=TRUE,add=TRUE)
text(x=barCenters[1,]+1, y =-plotTop*0.02,srt=45, adj=1, labels=wealth_quantile_set,xpd=TRUE)
segments(barCenters, t(wealth_quantile_params_nocar[,3:4]-wealth_quantile_se_nocar[,3:4]*1.96),
         barCenters,
         t(wealth_quantile_params_nocar[,3:4]+wealth_quantile_se_nocar[,3:4]*1.96), lwd=1.5)
arrows(barCenters, t(wealth_quantile_params_nocar[,3:4]-wealth_quantile_se_nocar[,3:4]*1.96),
       barCenters,
       t(wealth_quantile_params_nocar[,3:4]+wealth_quantile_se_nocar[,3:4]*1.96), lwd=1.5,
       angle=90,code=3, length=0.05)
text(x=barCenters[1,4]+1, y =text_y_pos, adj = c(0,0), labels="All Expenditure",xpd=TRUE,col="grey80")
text(x=barCenters[1,4]+1, y =text_y_pos-0.05, adj = c(0,0), labels="Excluding Cars",xpd=TRUE)
legend(2, plotTop, legend=c(expression(paste(phi," Permanent MPX")),expression(paste(psi," Transitory MPX"))), fill=c(colors[1],colors[2]),bty="n")
#dev.copy(png, paste(figures_dir, "MPXByDurables_nocar.png",sep=""))
dev.copy(pdf, paste(figures_dir, "MPXByDurables_nocar.pdf",sep=""))
#dev.copy(svg, paste(figures_dir, "MPXByDurables_nocar.svg",sep=""))
dev.off()

dev.new()
par(mar=c(8,7,4,5)+0.1,cex.axis=1.2,cex.lab=1.5)
plotTop = max(max(wealth_quantile_params[,3:4]), 1.0)
barCenters <- barplot(height=t(wealth_quantile_params[,3:4]),
                      names.arg=wealth_quantile_set,
                      cex.names=0.75,
                      beside=TRUE,col=c(colors[1],colors[2]),
                      las=2,ylim=c(0,plotTop), xaxt="n",
                      main=paste("MPX by Liquid Wealth Quantile"),
                      ylab = axis_string, border="black", axes=TRUE)
text(x=barCenters[1,]+1, y =-plotTop*0.02,srt=45, adj=1, labels=wealth_quantile_set,xpd=TRUE)
segments(barCenters, t(wealth_quantile_params[,3:4]-wealth_quantile_se[,3:4]*1.96),
         barCenters,
         t(wealth_quantile_params[,3:4]+wealth_quantile_se[,3:4]*1.96), lwd=1.5)
arrows(barCenters, t(wealth_quantile_params[,3:4]-wealth_quantile_se[,3:4]*1.96),
       barCenters,
       t(wealth_quantile_params[,3:4]+wealth_quantile_se[,3:4]*1.96), lwd=1.5,
       angle=90,code=3, length=0.05)
text(x=barCenters[1,4]+1, y =text_y_pos, adj = c(0,0), labels="All Expenditure",xpd=TRUE)
legend(2, plotTop, legend=c(expression(paste(phi," Permanent MPX")),expression(paste(psi," Transitory MPX"))), fill=c(colors[1],colors[2]),bty="n")
#dev.copy(png, paste(figures_dir, "MPXByDurables_all.png",sep=""))
dev.copy(pdf, paste(figures_dir, "MPXByDurables_all.pdf",sep=""))
#dev.copy(svg, paste(figures_dir, "MPXByDurables_all.svg",sep=""))
dev.off()

#create blank white graph, for use in slides
dev.new()
par(mar=c(8,7,4,5)+0.1,cex.axis=1.2,cex.lab=1.5)
plotTop = max(max(wealth_quantile_params[,3:4]), 1.0)
barCenters <- barplot(height=t(wealth_quantile_params[,3:4]),
                      names.arg=wealth_quantile_set,
                      cex.names=0.75,
                      beside=TRUE,col=c(colors[1],colors[2]),
                      las=2,ylim=c(0,plotTop), xaxt="n",
                      main=paste("MPX by Liquid Wealth Quantile"),
                      ylab = axis_string, border="black", axes=TRUE)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "white",border=NA)
#dev.copy(png, paste(figures_dir, "MPXByDurables_blank.png",sep=""))
dev.copy(pdf, paste(figures_dir, "MPXByDurables_blank.pdf",sep=""))
#dev.copy(svg, paste(figures_dir, "MPXByDurables_blank.svg",sep=""))
dev.off()
###############################################################################

#reload liquid wealth data
###############################################################################
# load liquid weath quintile data and create graph
load(paste(moments_dir,'moments_by_liquid_wealth_quantile',tag,'.RData',sep=''))
num_quantiles = 5
round_digits = -3
wealth_quantile_set = as.character(1:num_quantiles)
output =estimation_by_category(moments_by_liquid_wealth_quantile, make.names(wealth_quantile_set))
wealth_quantile_output=output
wealth_quantile_params = output$category_params
wealth_quantile_se = output$category_se
wealth_quantile_obs = output$category_obs
wealth_quantile_total_var = output$category_total_var
wealth_quantile_set = c(paste('$0-',format(round(moments_by_liquid_wealth_quantile$quantiles[[1]],round_digits),big.mark=",", trim=TRUE),sep=''))
wealth_quantile_set = c(wealth_quantile_set,paste('$',format(round(moments_by_liquid_wealth_quantile$quantiles[[1]],round_digits),big.mark=",", trim=TRUE),'-',format(round(moments_by_liquid_wealth_quantile$quantiles[[2]],round_digits),big.mark=",", trim=TRUE),sep=''))
wealth_quantile_set = c(wealth_quantile_set,paste('$',format(round(moments_by_liquid_wealth_quantile$quantiles[[2]],round_digits),big.mark=",", trim=TRUE),'-',format(round(moments_by_liquid_wealth_quantile$quantiles[[3]],round_digits),big.mark=",", trim=TRUE),sep=''))
wealth_quantile_set = c(wealth_quantile_set,paste('$',format(round(moments_by_liquid_wealth_quantile$quantiles[[3]],round_digits),big.mark=",", trim=TRUE),'-',format(round(moments_by_liquid_wealth_quantile$quantiles[[4]],round_digits),big.mark=",", trim=TRUE),sep=''))
wealth_quantile_set = c(wealth_quantile_set,paste('> $',format(round(moments_by_liquid_wealth_quantile$quantiles[[4]],round_digits),big.mark=",", trim=TRUE),sep=''))


################################################################################
# Get CSTW estimated data from Python
benchmark_results <- read.csv(paste(PythonResults_folder,'benchmark_liquidwealth.txt',sep=''), sep=" ",header=FALSE)
prefshock_results <- read.csv(paste(PythonResults_folder,'prefshock_liquidwealth.txt',sep=''), sep=" ",header=FALSE)

#plot transitory model results
dev.new()
par(mar=c(8,7,4,5)+0.1,cex.axis=1.2,cex.lab=1.5)
quantile_names = c("1","2","3","4","5")
params = matrix(c(wealth_quantile_params[,4],benchmark_results[,4]),nrow=5,ncol=2)
se = matrix(c(wealth_quantile_se[,4],0,0,0,0,0),nrow=5,ncol=2)
plotTop = max(max(wealth_quantile_params[,3:4]), 1.2)
barCenters <- barplot(height=t(params),
                      names.arg=quantile_names,
                      cex.names=0.75,
                      beside=TRUE,col=c(colors[2],colors[3]),
                      las=2,ylim=c(0,plotTop), xaxt="n",
                      main=paste("Transitory MPX by Liquid Wealth Quantile: Model vs Data"),
                      ylab = axis_string, xlab = "Liquid Wealth Quintile", border="black", axes=TRUE)
text(x=barCenters[1,]+1, y =-plotTop*0.02, adj=1, labels=quantile_names,xpd=TRUE)
segments(barCenters, t(params-se*1.96),
         barCenters,
         t(params+se*1.96), lwd=1.5)
arrows(barCenters, t(params-se*1.96),
       barCenters,
       t(params+se*1.96), lwd=1.5,
       angle=90,code=3, length=0.05)
legend(2, plotTop, legend=c(expression(paste("Data")),expression(paste("Model"))), fill=c(colors[2],colors[3]),bty="n")
#dev.copy(png, paste(figures_dir, "benchmark_tran_denmark.png",sep=""))
dev.copy(pdf, paste(figures_dir, "benchmark_tran_denmark.pdf",sep=""))
#dev.copy(svg, paste(figures_dir, "benchmark_tran_denmark.svg",sep=""))
dev.off()

#plot transitory model results with preference shock model
dev.new()
par(mar=c(8,7,4,5)+0.1,cex.axis=1.2,cex.lab=1.5)
quantile_names = c("1","2","3","4","5")
params = matrix(c(wealth_quantile_params[,4],prefshock_results[,4]),nrow=5,ncol=2)
se = matrix(c(wealth_quantile_se[,4],0,0,0,0,0),nrow=5,ncol=2)
plotTop = max(max(wealth_quantile_params[,3:4]), 1.2)
barCenters <- barplot(height=t(params),
                      names.arg=quantile_names,
                      cex.names=0.75,
                      beside=TRUE,col=c(colors[2],"grey85"),
                      las=2,ylim=c(0,plotTop), xaxt="n", 
                      main=paste("Transitory MPX by Liquid Wealth Quantile: Model vs Data"),
                      ylab = axis_string, xlab = "Liquid Wealth Quintile", border=NA, axes=TRUE)
params = matrix(c(wealth_quantile_params[,4],benchmark_results[,4]),nrow=5,ncol=2)
se = matrix(c(wealth_quantile_se[,4],0,0,0,0,0),nrow=5,ncol=2)
plotTop = max(max(wealth_quantile_params[,3:4]), 1.2)
barCenters <- barplot(height=t(params),
                      names.arg=quantile_names,
                      cex.names=0.75,
                      beside=TRUE,col=c(colors[2],colors[3]),
                      las=2,ylim=c(0,plotTop), xaxt="n",
                      main=paste("Transitory MPX by Liquid Wealth Quantile: Model vs Data"),
                      ylab = axis_string, xlab = "Liquid Wealth Quintile", border="black", axes=TRUE, add=TRUE)
text(x=barCenters[1,]+1, y =-plotTop*0.02, adj=1, labels=quantile_names,xpd=TRUE)
segments(barCenters, t(params-se*1.96),
         barCenters,
         t(params+se*1.96), lwd=1.5)
arrows(barCenters, t(params-se*1.96),
       barCenters,
       t(params+se*1.96), lwd=1.5,
       angle=90,code=3, length=0.05)
legend(2, plotTop, legend=c(expression(paste("Data")),expression(paste("Model")),"Model with Preference Shocks"), fill=c(colors[2],colors[3],"grey85"),bty="n")
#dev.copy(png, paste(figures_dir, "prefshock_tran_denmark.png",sep=""))
dev.copy(pdf, paste(figures_dir, "prefshock_tran_denmark.pdf",sep=""))
#dev.copy(svg, paste(figures_dir, "prefshock_tran_denmark.svg",sep=""))
dev.off()


#plot permanent model results
dev.new()
par(mar=c(8,7,4,5)+0.1,cex.axis=1.2,cex.lab=1.5)
quantile_names = c("1","2","3","4","5")
params = matrix(c(wealth_quantile_params[,3],benchmark_results[,3]),nrow=5,ncol=2)
se = matrix(c(wealth_quantile_se[,3],0,0,0,0,0),nrow=5,ncol=2)
plotTop = max(max(wealth_quantile_params[,3:4]), 1.2)
barCenters <- barplot(height=t(params),
                      names.arg=quantile_names,
                      cex.names=0.75,
                      beside=TRUE,col=c(colors[1],colors[3]),
                      las=2,ylim=c(0,plotTop), xaxt="n",
                      main=paste("Permanent MPX by Liquid Wealth Quantile: Model vs Data"),
                      ylab = axis_string, xlab = "Liquid Wealth Quintile", border="black", axes=TRUE)
text(x=barCenters[1,]+1, y =-plotTop*0.02, adj=1, labels=quantile_names,xpd=TRUE)
segments(barCenters, t(params-se*1.96),
         barCenters,
         t(params+se*1.96), lwd=1.5)
arrows(barCenters, t(params-se*1.96),
       barCenters,
       t(params+se*1.96), lwd=1.5,
       angle=90,code=3, length=0.05)
legend(2, plotTop, legend=c(expression(paste("Data")),expression(paste("Model"))), fill=c(colors[1],colors[3]),bty="n")
#dev.copy(png, paste(figures_dir, "benchmark_perm_denmark.png",sep=""))
dev.copy(pdf, paste(figures_dir, "benchmark_perm_denmark.pdf",sep=""))
#dev.copy(svg, paste(figures_dir, "benchmark_perm_denmark.svg",sep=""))
dev.off()

#plot permanent model results
dev.new()
par(mar=c(8,7,4,5)+0.1,cex.axis=1.2,cex.lab=1.5)
quantile_names = c("1","2","3","4","5")
params = matrix(c(wealth_quantile_params[,3],prefshock_results[,3]),nrow=5,ncol=2)
se = matrix(c(wealth_quantile_se[,3],0,0,0,0,0),nrow=5,ncol=2)
plotTop = max(max(wealth_quantile_params[,3:4]), 1.2)
barCenters <- barplot(height=t(params),
                      names.arg=quantile_names,
                      cex.names=0.75,
                      beside=TRUE,col=c(colors[1],colors[3],"grey85"),
                      las=2,ylim=c(0,plotTop), xaxt="n",
                      main=paste("Permanent MPX by Liquid Wealth Quantile: Model vs Data"),
                      ylab = axis_string, xlab = "Liquid Wealth Quintile", border=NA, axes=TRUE)
params = matrix(c(wealth_quantile_params[,3],benchmark_results[,3]),nrow=5,ncol=2)
se = matrix(c(wealth_quantile_se[,3],0,0,0,0,0),nrow=5,ncol=2)
plotTop = max(max(wealth_quantile_params[,3:4]), 1.2)
barCenters <- barplot(height=t(params),
                      names.arg=quantile_names,
                      cex.names=0.75,
                      beside=TRUE,col=c(colors[1],colors[3]),
                      las=2,ylim=c(0,plotTop), xaxt="n",
                      main=paste("Permanent MPX by Liquid Wealth Quantile: Model vs Data"),
                      ylab = axis_string, xlab = "Liquid Wealth Quintile", border="black", axes=TRUE, add=TRUE)
text(x=barCenters[1,]+1, y =-plotTop*0.02, adj=1, labels=quantile_names,xpd=TRUE)
segments(barCenters, t(params-se*1.96),
         barCenters,
         t(params+se*1.96), lwd=1.5)
arrows(barCenters, t(params-se*1.96),
       barCenters,
       t(params+se*1.96), lwd=1.5,
       angle=90,code=3, length=0.05)
legend(2, plotTop, legend=c(expression(paste("Data")),expression(paste("Model"))), fill=c(colors[1],colors[3]),bty="n")
#dev.copy(png, paste(figures_dir, "prefshock_perm_denmark.png",sep=""))
dev.copy(pdf, paste(figures_dir, "prefshock_perm_denmark.pdf",sep=""))
#dev.copy(svg, paste(figures_dir, "prefshock_perm_denmark.svg",sep=""))
dev.off()


#plot data compared to model MPC
dev.new()
par(mar=c(8,7,4,5)+0.1,cex.axis=1.2,cex.lab=1.5)
quantile_names = c("1","2","3","4","5")
params = matrix(c(prefshock_results[,4],prefshock_results[,5]),nrow=5,ncol=2)
plotTop = max(max(wealth_quantile_params[,3:4]), 1.0)
barCenters <- barplot(height=t(params),
                      names.arg=quantile_names,
                      cex.names=0.75,
                      beside=TRUE,col=c(colors[2],colors[3]),
                      las=2,ylim=c(0,plotTop), xaxt="n",
                      main=paste("Empirical Estimates and Model Partial Derivatives"),
                      ylab = axis_string, border="black", axes=TRUE)
text(x=barCenters[1,]+1, y =-plotTop*0.02, adj=1, labels=quantile_names,xpd=TRUE)
legend(2, plotTop, legend=c(expression(paste("Empirical Method")),expression(paste("Model Partial Derivative (6m MPC)"))), fill=c(colors[2],colors[3]),bty="n")
#dev.copy(png, paste(figures_dir, "MPC_accuracy.png",sep=""))
dev.copy(pdf, paste(figures_dir, "MPC_accuracy.pdf",sep=""))
#dev.copy(svg, paste(figures_dir, "MPC_accuracy.svg",sep=""))
dev.off()

###########################################################
# Do Adrien Auclert Stuff
#durable_tag ="_head_nodurableproxy"
durable_tag =tag
mean_household_consumption = 328385

###############################################################################
# load URE quintile data and create graph
load(paste(moments_dir,'moments_by_URE_quantile',durable_tag,'.RData',sep=''))

num_quantiles = 10
round_digits = 2
URE_quantile_set = as.character(1:num_quantiles)
output =estimation_by_category(moments_by_URE_quantile, make.names(URE_quantile_set))
URE_quantile_output=output
URE_quantile_params = output$category_params
URE_quantile_se = output$category_se
URE_quantile_obs = output$category_obs
URE_quantile_total_var = output$category_total_var
URE_quantile_set = t(round(moments_by_URE_quantile$quantile_means/mean_household_consumption,round_digits))
plot_estimataion_output(URE_quantile_params,URE_quantile_se,URE_quantile_set ,"URE Quantile","URE",transitory_only = TRUE)
plot_estimataion_output(URE_quantile_params,URE_quantile_se,URE_quantile_set ,"URE Quantile","permURE",transitory_only = FALSE)


#Now calculate the sufficient statistic
elas_URE_NR = mean(URE_quantile_params[,4]*t(moments_by_URE_quantile$quantile_means /mean_household_consumption))
elas_URE = elas_URE_NR - mean(URE_quantile_params[,4])*mean(t(moments_by_URE_quantile$quantile_means /mean_household_consumption))

mean_URE_MPX = mean(URE_quantile_params[,4]*t(moments_by_URE_quantile$quantile_means))

###############################################################################
###############################################################################
# load NNP quintile data and create graph
load(paste(moments_dir,'moments_by_NNP_quantile',durable_tag,'.RData',sep=''))

num_quantiles = 10
round_digits = 2
NNP_quantile_set = as.character(1:num_quantiles)
output =estimation_by_category(moments_by_NNP_quantile, make.names(NNP_quantile_set))
NNP_quantile_output=output
NNP_quantile_params = output$category_params
NNP_quantile_se = output$category_se
NNP_quantile_obs = output$category_obs
NNP_quantile_total_var = output$category_total_var
NNP_quantile_set = t(round(moments_by_NNP_quantile$quantile_means /mean_household_consumption,round_digits))
plot_estimataion_output(NNP_quantile_params,NNP_quantile_se,NNP_quantile_set ,"NNP Quantile","NNP",transitory_only = TRUE)
plot_estimataion_output(NNP_quantile_params,NNP_quantile_se,NNP_quantile_set ,"NNP Quantile","permNNP",transitory_only = FALSE)

#Now calculate the sufficient statistic
elas_NNP_NR = mean(NNP_quantile_params[,4]*t(moments_by_NNP_quantile$quantile_means /mean_household_consumption))
elas_NNP = elas_NNP_NR - mean(NNP_quantile_params[,4])*mean(t(moments_by_NNP_quantile$quantile_means /mean_household_consumption))

mean_NNP_MPX = mean(NNP_quantile_params[,4]*t(moments_by_NNP_quantile$quantile_means))
###############################################################################
# load Income quintile data and create graph
load(paste(moments_dir,'moments_by_Income_quantile',durable_tag,'.RData',sep=''))

num_quantiles = 10
round_digits = 2
Income_quantile_set = as.character(1:num_quantiles)
output =estimation_by_category(moments_by_Income_quantile, make.names(Income_quantile_set))
Income_quantile_output=output
Income_quantile_params = output$category_params
Income_quantile_se = output$category_se
Income_quantile_obs = output$category_obs
Income_quantile_total_var = output$category_total_var
Income_quantile_set = round(t(moments_by_Income_quantile$quantile_means /mean_household_consumption),round_digits)
plot_estimataion_output(Income_quantile_params,Income_quantile_se,Income_quantile_set ,"Income Quantile","Income",transitory_only = TRUE)
plot_estimataion_output(Income_quantile_params,Income_quantile_se,Income_quantile_set ,"Income Quantile","permIncome",transitory_only = FALSE)

#Now calculate the sufficient statistic
elas_Income_NR = mean(Income_quantile_params[,4]*t(moments_by_Income_quantile$quantile_means /mean_household_consumption))
elas_Income = elas_Income_NR - mean(Income_quantile_params[,4])*mean(t(moments_by_Income_quantile$quantile_means /mean_household_consumption))

mean_Income_MPX = mean(Income_quantile_params[,4]*t(moments_by_Income_quantile$quantile_means))
cov_Income_MPX = mean_Income_MPX-mean(Income_quantile_params[,4])*mean(t(moments_by_Income_quantile$quantile_means))
mean_MPX = mean(Income_quantile_params[,4])
mean_Income = mean(t(moments_by_Income_quantile$quantile_means))
###############################################################################

###############################################################################
# load MeanCons quintile data and create graph
load(paste(moments_dir,'moments_by_MeanCons_quantile',durable_tag,'.RData',sep=''))

num_quantiles = 10
round_digits = 2
MeanCons_quantile_set = as.character(1:num_quantiles)
output =estimation_by_category(moments_by_MeanCons_quantile, make.names(MeanCons_quantile_set))
MeanCons_quantile_output=output
MeanCons_quantile_params = output$category_params
MeanCons_quantile_se = output$category_se
MeanCons_quantile_obs = output$category_obs
MeanCons_quantile_total_var = output$category_total_var
MeanCons_quantile_set = t(round(moments_by_MeanCons_quantile$quantile_means/mean_household_consumption,round_digits))
plot_estimataion_output(MeanCons_quantile_params,MeanCons_quantile_se,MeanCons_quantile_set ,"Consumption Quantile","MeanCons",transitory_only = TRUE)

#Now calculate the sufficient statistic
elas_MeanCons_NR = mean(MeanCons_quantile_params[,4]*t(moments_by_MeanCons_quantile$quantile_means /mean_household_consumption))
elas_MeanCons = elas_MeanCons_NR - mean(MeanCons_quantile_params[,4])*mean(t(moments_by_MeanCons_quantile$quantile_means /mean_household_consumption))

mean_cons_MPX = mean(MeanCons_quantile_params[,4]*t(moments_by_MeanCons_quantile$quantile_means))

cons_weighted_MPC = mean(MeanCons_quantile_params[,4]*t(moments_by_MeanCons_quantile$quantile_means) /mean(t(moments_by_MeanCons_quantile$quantile_means)))

###############################################################################

###############################################################################
# Do robustness graphs 


# Function to plot shock variances and consumption elasticities
robustness_plot<- function(tag_list, moments_name, quantile_labels, tag_list_legend, title_string, filename, param_col=4, legend_xpos = NULL, x_label="Quantile") {
  
  this_colors = c('#fb8072','#bebada','#ffffb3','#80b1d3','#fdb462','#b3de69','#fccde5','#d9d9d9','#8dd3c7')
  this_colors = this_colors[1:length(tag_list)]
  num_quantiles = length(quantile_labels)
  xlabel_pos = (length(tag_list)-1)/2
  params = matrix(0.0,num_quantiles,length(tag_list))
  se = matrix(0.0,num_quantiles,length(tag_list))
  for (i in 1:length(tag_list)) {
    this_tag = tag_list[i]
    load(paste(moments_dir,moments_name,this_tag,'.RData',sep=''))
    output =estimation_by_category(eval(parse(text = moments_name)), make.names(quantile_labels))
    params[,i] = output$category_params[,param_col]
    se[,i] = output$category_se[,param_col]
  }
  dev.new()
  par(mar=c(8,7,4,5)+0.1,cex.axis=1.2,cex.lab=1.5)
  plotTop = max(max(params),1.0)
  #plotTop = max(params)
  barCenters <- barplot(height=t(params),
                        names.arg=quantile_labels,
                        cex.names=0.75,
                        beside=TRUE,col=this_colors,
                        las=2,ylim=c(0,plotTop), xaxt="n",
                        main=title_string,
                        ylab = axis_string, 
                        xlab = x_label,
                        border="black", 
                        axes=TRUE)
  text(x=barCenters[1,]+xlabel_pos, y =-plotTop*0.02,srt=0, adj=1, labels=quantile_labels,xpd=TRUE)
  segments(barCenters, t(params-se*1.96),
           barCenters,
           t(params+se*1.96), lwd=1.5)
  arrows(barCenters, t(params-se*1.96),
         barCenters,
         t(params+se*1.96), lwd=1.5,
         angle=90,code=3, length=0.05)
  if (is.null(legend_xpos)){
    legend_xpos = 3*num_quantiles
  }
  legend(legend_xpos, plotTop, legend=tag_list_legend, fill=this_colors,bty="n")
  #dev.copy(png, paste(figures_dir, filename,".png",sep=""))
  dev.copy(pdf, paste(figures_dir, filename,".pdf",sep=""))
  #dev.copy(svg, paste(figures_dir, filename,".svg",sep=""))
  dev.off()
}

tag_list = c("_level_lincome_head","_level_lincome_head_nostocks","_level_lincome_head_negcons","_level_lincome_head_ConsOutliers25")
tag_list = c("_level_lincome_head","_level_lincome_head_nostocks")

tag_list_legend = c("Baseline","No Stocks","Include Neg Cons", "Strict Outliers" )

#First do liquid wealth
num_quantiles = 5
#transitory
robustness_plot(tag_list, "moments_by_liquid_wealth_quantile", as.character(1:num_quantiles), tag_list_legend, "Transitory MPX By Liquid Wealth Quintile", "Robust_tranMPX_liquidwealth", param_col=4, x_label="Quintile")
#permanent
robustness_plot(tag_list, "moments_by_liquid_wealth_quantile", as.character(1:num_quantiles), tag_list_legend, "Permanent MPX By Liquid Wealth Quintile", "Robust_permMPX_liquidwealth", param_col=3, x_label="Quintile")


#Net wealth
num_quantiles = 5
#transitory
robustness_plot(tag_list, "moments_by_net_wealth_quantile", as.character(1:num_quantiles), tag_list_legend, "Transitory MPX By Net Wealth Quintile", "Robust_tranMPX_netwealth", param_col=4, x_label="Quintile")
#permanent
robustness_plot(tag_list, "moments_by_net_wealth_quantile", as.character(1:num_quantiles), tag_list_legend, "Permanent MPX By Net Wealth Quintile", "Robust_permMPX_netwealth", param_col=3, x_label="Quintile")


#URE
#transitory
robustness_plot(tag_list, "moments_by_URE_quantile", as.character(c(1,4,7,10)), tag_list_legend, "Transitory MPX By URE Decile", "Robust_tranMPX_URE", param_col=4,legend_xpos = 2, x_label="Decile")
#permanent
robustness_plot(tag_list, "moments_by_URE_quantile", as.character(c(1,4,7,10)), tag_list_legend, "Permanent MPX By URE Decile", "Robust_permMPX_URE", param_col=3,legend_xpos = 2, x_label="Decile")

#NNP
#transitory
robustness_plot(tag_list, "moments_by_NNP_quantile", as.character(c(1,4,7,10)), tag_list_legend, "Transitory MPX By NNP Decile", "Robust_tranMPX_NNP", param_col=4,legend_xpos = 2, x_label="Decile")
#permanent
robustness_plot(tag_list, "moments_by_NNP_quantile", as.character(c(1,4,7,10)), tag_list_legend, "Permanent MPX By NNP Decile", "Robust_permMPX_NNP", param_col=3,legend_xpos = 2, x_label="Decile")

#Income
num_quantiles = 10
#transitory
robustness_plot(tag_list, "moments_by_Income_quantile", as.character(c(1,4,7,10)), tag_list_legend, "Transitory MPX By Income Decile", "Robust_tranMPX_Income", param_col=4, x_label="Decile")
#permanent
robustness_plot(tag_list, "moments_by_Income_quantile", as.character(c(1,4,7,10)), tag_list_legend, "Permanent MPX By Income Decile", "Robust_permMPX_Income", param_col=3,legend_xpos = 6, x_label="Decile")


#MeanCons
num_quantiles = 10
#transitory
robustness_plot(tag_list, "moments_by_MeanCons_quantile", as.character(c(1,4,7,10)), tag_list_legend, "Transitory MPX By Consumption Decile", "Robust_tranMPX_MeanCons", param_col=4, x_label="Decile")
#permanent
robustness_plot(tag_list, "moments_by_MeanCons_quantile", as.character(c(1,4,7,10)), tag_list_legend, "Permanent MPX By Consumption Decile", "Robust_permMPX_MeanCons", param_col=3,legend_xpos = 6, x_label="Decile")

# Compare head with spouse
tag_list = c("_level_lincome","_level_lincome_head","_level_lincome_spouse")
tag_list_legend = c("Total","Head","Spouse")

#First do liquid wealth
num_quantiles = 5
#transitory
robustness_plot(tag_list, "moments_by_liquid_wealth_quantile", as.character(1:num_quantiles), tag_list_legend, "Transitory MPX By Liquid Wealth Quintile", "Spouse_tranMPX_liquidwealth", param_col=4, x_label="Quintile")
#permanent
robustness_plot(tag_list, "moments_by_liquid_wealth_quantile", as.character(1:num_quantiles), tag_list_legend, "Permanent MPX By Liquid Wealth Quintile", "Spouse_permMPX_liquidwealth", param_col=3, x_label="Quintile")

#Net wealth
num_quantiles = 5
#transitory
robustness_plot(tag_list, "moments_by_net_wealth_quantile", as.character(1:num_quantiles), tag_list_legend, "Transitory MPX By Net Wealth Quintile", "Spouse_tranMPX_netwealth", param_col=4, x_label="Quintile")
#permanent
robustness_plot(tag_list, "moments_by_net_wealth_quantile", as.character(1:num_quantiles), tag_list_legend, "Permanent MPX By Net Wealth Quintile", "Spouse_permMPX_netwealth", param_col=3, x_label="Quintile")

# Compare head with total
tag_list = c("_level_lincome","_level_lincome_head")
tag_list_legend = c("Total","Head")

#URE
#transitory
robustness_plot(tag_list, "moments_by_URE_quantile", as.character(1:10), tag_list_legend, "Transitory MPX By URE Decile", "total_tranMPX_URE", param_col=4,legend_xpos = 2, x_label="Decile")
#permanent
robustness_plot(tag_list, "moments_by_URE_quantile", as.character(1:10), tag_list_legend, "Permanent MPX By URE Decile", "total_permMPX_URE", param_col=3,legend_xpos = 2, x_label="Decile")

#NNP
#transitory
robustness_plot(tag_list, "moments_by_NNP_quantile", as.character(1:10), tag_list_legend, "Transitory MPX By NNP Decile", "total_tranMPX_NNP", param_col=4,legend_xpos = 2, x_label="Decile")
#permanent
robustness_plot(tag_list, "moments_by_NNP_quantile", as.character(1:10), tag_list_legend, "Permanent MPX By NNP Decile", "total_permMPX_NNP", param_col=3,legend_xpos = 2, x_label="Decile")

#Income
num_quantiles = 10
#transitory
robustness_plot(tag_list, "moments_by_Income_quantile", as.character(1:10), tag_list_legend, "Transitory MPX By Income Decile", "total_tranMPX_Income", param_col=4 ,legend_xpos = 2, x_label="Decile")
#permanent
robustness_plot(tag_list, "moments_by_Income_quantile", as.character(1:10), tag_list_legend, "Permanent MPX By Income Decile", "total_permMPX_Income", param_col=3,legend_xpos = 6, x_label="Decile")

#MeanCons
num_quantiles = 10
#transitory
robustness_plot(tag_list, "moments_by_MeanCons_quantile", as.character(1:10), tag_list_legend, "Transitory MPX By Consumption Decile", "total_tranMPX_MeanCons", param_col=4 ,legend_xpos = 2, x_label="Decile")
#permanent
robustness_plot(tag_list, "moments_by_MeanCons_quantile", as.character(1:10), tag_list_legend, "Permanent MPX By Consumption Decile", "total_permMPX_MeanCons", param_col=3,legend_xpos = 6, x_label="Decile")


# Compare levels with log
tag_list = c("_level_lincome_head","")
tag_list_legend = c("Baseline","Log Total (elasticity)")

#First do liquid wealth
num_quantiles = 5
#transitory
robustness_plot(tag_list, "moments_by_liquid_wealth_quantile", as.character(1:num_quantiles), tag_list_legend, "Transitory MPX By Liquid Wealth Quintile", "Logs_tranMPX_liquidwealth", param_col=4,legend_xpos = 6, x_label="Quintile")
#permanent
robustness_plot(tag_list, "moments_by_liquid_wealth_quantile", as.character(1:num_quantiles), tag_list_legend, "Permanent MPX By Liquid Wealth Quintile", "Logs_permMPX_liquidwealth", param_col=3,legend_xpos = 6, x_label="Quintile")

#Net wealth
num_quantiles = 5
#transitory
robustness_plot(tag_list, "moments_by_net_wealth_quantile", as.character(1:num_quantiles), tag_list_legend, "Transitory MPX By Net Wealth Quintile", "Logs_tranMPX_netwealth", param_col=4,legend_xpos = 6, x_label="Quintile")
#permanent
robustness_plot(tag_list, "moments_by_net_wealth_quantile", as.character(1:num_quantiles), tag_list_legend, "Permanent MPX By Net Wealth Quintile", "Logs_permMPX_netwealth", param_col=3,legend_xpos = 6, x_label="Quintile")
#URE
#transitory
robustness_plot(tag_list, "moments_by_URE_quantile", as.character(1:10), tag_list_legend, "Transitory MPX By URE Decile", "Logs_tranMPX_URE", param_col=4,legend_xpos = 6, x_label="Decile")
#permanent
robustness_plot(tag_list, "moments_by_URE_quantile", as.character(1:10), tag_list_legend, "Permanent MPX By URE Decile", "Logs_permMPX_URE", param_col=3,legend_xpos = 6, x_label="Decile")

#NNP
#transitory
robustness_plot(tag_list, "moments_by_NNP_quantile", as.character(1:10), tag_list_legend, "Transitory MPX By NNP Decile", "Logs_tranMPX_NNP", param_col=4,legend_xpos = 6, x_label="Decile")
#permanent
robustness_plot(tag_list, "moments_by_NNP_quantile", as.character(1:10), tag_list_legend, "Permanent MPX By NNP Decile", "Logs_permMPX_NNP", param_col=3,legend_xpos = 6, x_label="Decile")

#Income
num_quantiles = 10
#transitory
robustness_plot(tag_list, "moments_by_Income_quantile", as.character(1:10), tag_list_legend, "Transitory MPX By Income Decile", "Logs_tranMPX_Income", param_col=4,legend_xpos = 6, x_label="Decile")
#permanent
robustness_plot(tag_list, "moments_by_Income_quantile", as.character(1:10), tag_list_legend, "Permanent MPX By Income Decile", "Logs_permMPX_Income", param_col=3,legend_xpos = 6, x_label="Decile")

#MeanCons
num_quantiles = 10
#transitory
robustness_plot(tag_list, "moments_by_MeanCons_quantile", as.character(1:10), tag_list_legend, "Transitory MPX By Consumption Decile", "Logs_tranMPX_MeanCons", param_col=4,legend_xpos = 6, x_label="Decile")
#permanent
robustness_plot(tag_list, "moments_by_MeanCons_quantile", as.character(1:10), tag_list_legend, "Permanent MPX By Consumption Decile", "Logs_permMPX_MeanCons", param_col=3,legend_xpos = 6, x_label="Decile")




tag_list = c("_level_lincome_head","_level_lincome_head_quantilesbyperminc")
tag_list_legend = c("Baseline", "LiqWealth/PermInc" )

#First do liquid wealth
num_quantiles = 5
#transitory
robustness_plot(tag_list, "moments_by_liquid_wealth_quantile", as.character(1:num_quantiles), tag_list_legend, "Transitory MPX By Liquid Wealth Quintile", "DivPerm_tranMPX_liquidwealth", param_col=4,legend_xpos = 6, x_label="Quintile")
#permanent
robustness_plot(tag_list, "moments_by_liquid_wealth_quantile", as.character(1:num_quantiles), tag_list_legend, "Permanent MPX By Liquid Wealth Quintile", "DivPerm_permMPX_liquidwealth", param_col=3,legend_xpos = 6, x_label="Quintile")


###############################################################################





