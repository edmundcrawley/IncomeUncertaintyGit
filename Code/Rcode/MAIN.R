
#################################################################################
# 
# This file takes the moment data from the servers, 
# performs GMM estimation on it, and creates graphs
#
# 
###############################################################################
tag = ""
title_string = "MPX"
axis_string = "MPX"

# Set folders
base_dir = "C:/Users/edmun/OneDrive/Documents/Research/Denmark/IncomeUncertaintyGit/"
Rcode_folder = paste(base_dir,"Code/Rcode/",sep='')
moments_dir = paste(base_dir,"Code/ServerRcode/ServerOutput/AEJ_revision/",sep='')
figures_dir = paste(base_dir,"Code/Rcode/Figures/AEJ_revision/",sep='')
tables_dir = paste(base_dir,"Code/Rcode/Tables/AEJ_revision/",sep='')
txt_dir = paste(base_dir,"Code/ServerRcode/ServerOutput/AEJ_revision/TxtFilesFromAndreas/",sep='')
PythonResults_folder = paste(base_dir,"Code/PrefShockModel/Results/",sep='')

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
plot_estimataion_output<- function(params, se, labels, category_for_title, category_for_save,transitory_only=FALSE,category_label="") {
  # First plot the variances
  #dev.new()
  pdf(paste(figures_dir, "VarianceBy",category_for_save,tag,".pdf",sep=""))
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
                        las=2,ylim=c(0,plotTop*1.05), xaxt="n",
                        main=paste("Permanent and Transitory Variance by ",category_for_title),
                        ylab = "Shock Variance\n", border="black", axes=TRUE,xlab=category_label)
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
  #dev.copy(pdf, paste(figures_dir, "VarianceBy",category_for_save,tag,".pdf",sep=""))
  #dev.copy(svg, paste(figures_dir, "VarianceBy",category_for_save,tag,".svg",sep=""))
  dev.off()
  

  # Now plot the Expenditure Elasticities
  #dev.new()
  pdf(paste(figures_dir, "MPXBy",category_for_save,tag,".pdf",sep=""))
  if (transitory_only){
    param_cols=4
    this_legend=c(expression(paste("Transitory MPX")))
  } else {
    param_cols=3:4
    this_legend=c(expression(paste("Permanent MPX")),expression(paste("Transitory MPX")))
  }
  par(mar=c(8,7,4,5)+0.1,cex.axis=1.2,cex.lab=1.5)
  #barCenters <- barplot(t(params[,param_cols]),names.arg=labels,cex.names=0.8,beside=TRUE,col=this_colors)
  plotTop = max(max(params[,param_cols]),1.0)
  barCenters <- barplot(height=t(params[,param_cols]),
                        names.arg=labels,
                        cex.names=0.75,
                        beside=TRUE,col=this_colors,
                        las=2,ylim=c(0,plotTop), xaxt="n",
                        main=paste(title_string, " by ",category_for_title),
                        ylab = axis_string, border="black", axes=TRUE,xlab=category_label)
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
  #dev.copy(pdf, paste(figures_dir, "MPXBy",category_for_save,tag,".pdf",sep=""))
  #dev.copy(svg, paste(figures_dir, "MPXBy",category_for_save,tag,".svg",sep=""))
  dev.off()
  
  #print for 508 compliance for FEDS paper
  print(labels)
  print(params)

}
###############################################################################
#Get an overall estimate for all household
c_vector_all = as.vector(read.table(paste(txt_dir,"moments_all_c_vector.txt",sep=''), header = FALSE, sep = ",", dec = "."))
omega_all = as.matrix(read.table(paste(txt_dir,"moments_all_omega.txt",sep=''), header = FALSE, sep = ",", dec = "."))
output_all = CS_parameter_estimation(c_vector_all$V1, omega_all,T=12)
c_matrix_all = matrix(0,nrow=12,ncol=8)
for (i in 1:12){
  c_matrix_all[i,] = c_vector_all$V1[((i-1)*8+1):(i*8)]
}
c_matrix_all[7:12,]/c_matrix_all[1:6,]
for (i in 1:8){
  c_matrix_all[,i] = c_vector_all$V1[((i-1)*12+1):(i*12)]
}
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
legend(40, 0.025, legend=c(expression(paste(sigma[p]^2," Permanent Var")), expression(paste(sigma[q]^2," Transitory Var")), expression(paste("Var(",Delta,"y)")),expression(paste(frac(2,3),sigma[p]^2,"+2",sigma[q]^2,sep=""))), col=c("green","red","black","black"),lty=c("solid","solid","solid","dashed"),bty="n")
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
load(paste(moments_dir,'moments_loop',tag,'.RData',sep=''))
###############################################################################
# Plot regression coefficient of expenditure growth vs income growth for different growth periods
max_diff = 7
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
load(paste(moments_dir,'moments_by_liquid_wealth_quantile_7',tag,'.RData',sep=''))
moments_by_liquid_wealth_quantile_7_1and5 = moments_by_liquid_wealth_quantile_7
max_diff = 7
reg_coefs_liquid_wealth = list()
std_errors_liquid_wealth = list()
for (this_moment in c("X1","X5")) {
  reg_coefs_liquid_wealth[[this_moment]] = array(0,dim=c(max_diff,1))
  std_errors_liquid_wealth[[this_moment]] = array(0,dim=c(max_diff,1))
  T = moments_by_liquid_wealth_quantile_7_1and5[[this_moment]]$T
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
    reg_coefs_liquid_wealth[[this_moment]][n] = mean(moments_by_liquid_wealth_quantile_7_1and5[[this_moment]]$c_vector[moments_used_all[(num_moments+1):(2*num_moments)]])/mean(moments_by_liquid_wealth_quantile_7_1and5[[this_moment]]$c_vector[moments_used_all[1:num_moments]])
    omega = moments_by_liquid_wealth_quantile_7_1and5[[this_moment]]$omega
    function_gradient = array(0,dim=c(dim(omega)[1],1))
    function_gradient[moments_used_all[(num_moments+1):(2*num_moments)],] = 1.0/((moments_by_liquid_wealth_quantile_7_1and5[[this_moment]]$c_vector[moments_used_all[1:num_moments]])*length(moments_used_all[(num_moments+1):(2*num_moments)]))
    function_gradient[moments_used_all[1:num_moments],] = -moments_by_liquid_wealth_quantile_7_1and5[[this_moment]]$c_vector[moments_used_all[(num_moments+1):(2*num_moments)]]/((moments_by_liquid_wealth_quantile_7_1and5[[this_moment]]$c_vector[moments_used_all[1:num_moments]])**2 *length(moments_used_all[(num_moments+1):(2*num_moments)]))
    std_errors_liquid_wealth[[this_moment]][n] = (t(function_gradient)  %*% omega %*% function_gradient)**0.5
  }
}
###############################################################################
# Pull in consumption saving numbers from Python output
FromPython <- scan(paste(PythonResults_folder,'basic_regressions.txt',sep=''), what=double(), sep=",")
FromPython <- scan(paste(PythonResults_folder,'benchmark_br_all.txt',sep=''), what=double(), sep=",")
solow_spending = 0.695
# Now draw graph
#png(paste(figures_dir, "basic_regression_complete",tag,".png",sep=""))
pdf(paste(figures_dir, "basic_regression_complete",tag,".pdf",sep=""))
#svg(paste(figures_dir, "basic_regression_complete",tag,".svg",sep=""))
par(mar=c(8,7,4,5)+0.1,cex.axis=1.2,cex.lab=1.5)
plot(array(0.0,dim=dim(reg_coefs)), ylim=c(0,1),lty='solid',col='blue',type='o', xlab='N, Years of Growth',ylab=TeX('$\\beta^N$, Regression Coefficient'),main='Regressing Consumption Growth on Income Growth')
legend(4, 0.45, legend=c("Complete Markets","","",""), col=c("blue","green","red","black"),lty=c("solid","solid","solid","solid"),bty="n")
dev.off()
#png(paste(figures_dir, "basic_regression_solow",tag,".png",sep=""))
pdf(paste(figures_dir, "basic_regression_solow",tag,".pdf",sep=""))
#svg(paste(figures_dir, "basic_regression_solow",tag,".svg",sep=""))
par(mar=c(8,7,4,5)+0.1,cex.axis=1.2,cex.lab=1.5)
plot(array(0.0,dim=dim(reg_coefs)), ylim=c(0,1),lty='solid',col='blue',type='o', xlab='N, Years of Growth',ylab=TeX('$\\beta^N$, Regression Coefficient'),main='Regressing Consumption Growth on Income Growth')
lines(array(solow_spending,dim=dim(reg_coefs)),lty='solid',col='green',type='o')
legend(4, 0.45, legend=c("Complete Markets","Solow","",""), col=c("blue","green","red","black"),lty=c("solid","solid","solid","solid"),bty="n")
dev.off()
#png(paste(figures_dir, "basic_regression_BS",tag,".png",sep=""))
pdf(paste(figures_dir, "basic_regression_BS",tag,".pdf",sep=""))
#svg(paste(figures_dir, "basic_regression_BS",tag,".svg",sep=""))
par(mar=c(8,7,4,5)+0.1,cex.axis=1.2,cex.lab=1.5)
plot(array(0.0,dim=dim(reg_coefs)), ylim=c(0,1),lty='solid',col='blue',type='o', xlab='N, Years of Growth',ylab=TeX('$\\beta^N$, Regression Coefficient'),main='Regressing Consumption Growth on Income Growth')
lines(array(solow_spending,dim=dim(reg_coefs)),lty='solid',col='green',type='o')
lines(FromPython[1:dim(reg_coefs)[1]],lty='solid',col='red',type='o')
legend(4, 0.45, legend=c("Complete Markets","Solow","Buffer-Stock",""), col=c("blue","green","red","black"),lty=c("solid","solid","solid","solid"),bty="n")
arrows(1, 0.06, 7, 0.06)
arrows(7, 0.06, 1, 0.06)
text(2.2, 0.16, labels = "Relatively more \n transitory variance")
text(5.8, 0.16, labels = "Relatively more \n permanent variance")
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
legend(4, 0.45, legend=c("Complete Markets","Solow","Buffer-Stock","Data"), col=c("blue","green","red","black"),lty=c("solid","solid","solid","solid"),bty="n")
arrows(1, 0.06, 7, 0.06)
arrows(7, 0.06, 1, 0.06)
text(2.2, 0.16, labels = "Relatively more \n transitory variance")
text(5.8, 0.16, labels = "Relatively more \n permanent variance")
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
legend(4, 0.45, legend=c("Complete Markets","Solow","Buffer-Stock","Data"), col=c("blue","green","red","black"),lty=c("solid","solid","solid","solid"),bty="n")
arrows(1, 0.06, 7, 0.06)
arrows(7, 0.06, 1, 0.06)
text(2.2, 0.16, labels = "Relatively more \n transitory variance")
text(5.8, 0.16, labels = "Relatively more \n permanent variance")
text(3.0, 0.88, labels = "Least Liquid", cex=0.8)
text(2.75, 0.39, labels = "Most Liquid", cex=0.8)
dev.off()

pdf(paste(figures_dir, "basic_regression_for_paper",tag,".pdf",sep=""))
par(mar=c(8,7,4,5)+0.1,cex.axis=1.2,cex.lab=1.5)
plot(array(0.0,dim=dim(reg_coefs)), ylim=c(0,1),lty='solid',col='blue',type='o', xlab='N, Years of Growth',ylab=TeX('$\\beta^N$, Regression Coefficient'),main='Regressing Consumption Growth on Income Growth')
lines(array(solow_spending,dim=dim(reg_coefs)),lty='solid',col='green',type='o')
lines(FromPython[1:dim(reg_coefs)[1]],lty='solid',col='red',type='o')
points(reg_coefs,col='black')
lines(reg_coefs,col='black')
lines(reg_coefs+1.96*std_errors,lty='dashed',col='black')
lines(reg_coefs-1.96*std_errors,lty='dashed',col='black')
#Add high and low liquid quantiles
points(reg_coefs_liquid_wealth$X1)
lines(reg_coefs_liquid_wealth$X1)
lines(reg_coefs_liquid_wealth$X1+1.96*std_errors_liquid_wealth$X1,lty='dashed')
lines(reg_coefs_liquid_wealth$X1-1.96*std_errors_liquid_wealth$X1,lty='dashed')
points(reg_coefs_liquid_wealth$X5)
lines(reg_coefs_liquid_wealth$X5)
lines(reg_coefs_liquid_wealth$X5+1.96*std_errors_liquid_wealth$X5,lty='dashed')
lines(reg_coefs_liquid_wealth$X5-1.96*std_errors_liquid_wealth$X5,lty='dashed')
legend(4, 0.45, legend=c("Complete Markets","Solow","Buffer-Stock","Data"), col=c("blue","green","red","black"),lty=c("solid","solid","solid","solid"),bty="n")
arrows(1, 0.06, 7, 0.06)
arrows(7, 0.06, 1, 0.06)
text(2.2, 0.16, labels = "Relatively more \n transitory variance")
text(5.8, 0.16, labels = "Relatively more \n permanent variance")
text(5.5, 0.665, labels = "All Households", cex=0.8)
text(3.0, 0.88, labels = "Least Liquid", cex=0.8)
text(2.75, 0.39, labels = "Most Liquid", cex=0.8)
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
legend(0, 0.037, legend=c(expression(paste("Var(",Delta^N,"y) Empirical"),paste("Var(",Delta^N,"y) matched to N=3,4,5"), paste("Cov(",Delta^N,"y,",Delta^N,"c) Empirical"),paste("Cov(",Delta^N,"y,",Delta^N,"c) matched to n=3,4,5"))),lty=c("solid","solid","dashed","solid"),col=c("black","red","black","green"),bty="n")
dev.off()
###############################################################################

###############################################################################
# load liquid weath quintile data by non-durable proxyand create graph
load(paste(moments_dir,'moments_by_liquid_wealth_quantile_0nocar','.RData',sep=''))
num_quantiles = 5
round_digits = -3
wealth_quantile_set = as.character(1:num_quantiles)
output =estimation_by_category(moments_by_liquid_wealth_quantile, make.names(wealth_quantile_set))
wealth_quantile_output=output
wealth_quantile_params = output$category_params
wealth_quantile_se = output$category_se
wealth_quantile_obs = output$category_obs
wealth_quantile_total_var = output$category_total_var

load(paste(moments_dir,'moments_by_liquid_wealth_quantile_nocar','.RData',sep=''))
wealth_quantile_set = as.character(1:num_quantiles)
output =estimation_by_category(moments_by_liquid_wealth_quantile, make.names(wealth_quantile_set))
wealth_quantile_output_nocar=output
wealth_quantile_params_nocar = output$category_params
wealth_quantile_se_nocar = output$category_se
wealth_quantile_obs_nocar = output$category_obs
wealth_quantile_total_var_nocar = output$category_total_var

load(paste(moments_dir,'moments_by_liquid_wealth_quantile_nodurableproxy','.RData',sep=''))
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

#dev.new()
pdf(paste(figures_dir, "MPXByDurables_nodurableproxy.pdf",sep=""))
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
text(x=barCenters[1,4]+1, y =text_y_pos, adj = c(0,0), labels="All Expenditure",xpd=TRUE,col="grey60")
text(x=barCenters[1,4]+1, y =text_y_pos-0.05, adj = c(0,0), labels="Excluding Cars",xpd=TRUE,col="grey40")
text(x=barCenters[1,4]+1, y =text_y_pos-0.1, adj = c(0,0), labels="Nondurable Proxy",xpd=TRUE)
legend(2, plotTop, legend=c(expression(paste(phi," Permanent MPX")),expression(paste(psi," Transitory MPX"))), fill=c(colors[1],colors[2]),bty="n")
#dev.copy(png, paste(figures_dir, "MPXByDurables_nodurableproxy.png",sep=""))
#dev.copy(pdf, paste(figures_dir, "MPXByDurables_nodurableproxy.pdf",sep=""))
#dev.copy(svg, paste(figures_dir, "MPXByDurables_nodurableproxy.svg",sep=""))
dev.off()


#dev.new()
pdf(paste(figures_dir, "MPXByDurables_nocar.pdf",sep=""))
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
#dev.copy(pdf, paste(figures_dir, "MPXByDurables_nocar.pdf",sep=""))
#dev.copy(svg, paste(figures_dir, "MPXByDurables_nocar.svg",sep=""))
dev.off()

#dev.new()
pdf(paste(figures_dir, "MPXByDurables_all.pdf",sep=""))
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
#dev.copy(pdf, paste(figures_dir, "MPXByDurables_all.pdf",sep=""))
#dev.copy(svg, paste(figures_dir, "MPXByDurables_all.svg",sep=""))
dev.off()

#create blank white graph, for use in slides
#dev.new()
pdf(paste(figures_dir, "MPXByDurables_blank.pdf",sep=""))
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
#dev.copy(pdf, paste(figures_dir, "MPXByDurables_blank.pdf",sep=""))
#dev.copy(svg, paste(figures_dir, "MPXByDurables_blank.svg",sep=""))
dev.off()
###############################################################################


###########################################################
# Do Adrien Auclert Stuff
durable_tag =tag
mean_household_consumption = 318083.36 # from URE_NNP_positions_NationalAccounts.xlxs. This is only used in the graphs to calc URE/consumption etc

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
plot_estimataion_output(URE_quantile_params,URE_quantile_se,URE_quantile_set ,"URE Quantile","URE",transitory_only = TRUE, category_label = "URE/Mean Expenditure")
plot_estimataion_output(URE_quantile_params,URE_quantile_se,URE_quantile_set ,"URE Quantile","permURE",transitory_only = FALSE, category_label = "URE/Mean Expenditure")

mean_URE_MPX = mean(URE_quantile_params[,4]*t(moments_by_URE_quantile$quantile_means))
mean_URE_MPX_se = (sum((URE_quantile_se[,4]*t(moments_by_URE_quantile$quantile_means))^2)^0.5)/num_quantiles

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
plot_estimataion_output(NNP_quantile_params,NNP_quantile_se,NNP_quantile_set ,"NNP Quantile","NNP",transitory_only = TRUE, category_label = "NNP/Mean Expenditure")
plot_estimataion_output(NNP_quantile_params,NNP_quantile_se,NNP_quantile_set ,"NNP Quantile","permNNP",transitory_only = FALSE, category_label = "NNP/Mean Expenditure")

mean_NNP_MPX = mean(NNP_quantile_params[,4]*t(moments_by_NNP_quantile$quantile_means))
mean_NNP_MPX_se = (sum((NNP_quantile_se[,4]*t(moments_by_NNP_quantile$quantile_means))^2)^0.5)/num_quantiles

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
plot_estimataion_output(Income_quantile_params,Income_quantile_se,Income_quantile_set ,"Income Quantile","Income",transitory_only = TRUE, category_label = "Income/Mean Expenditure")
plot_estimataion_output(Income_quantile_params,Income_quantile_se,Income_quantile_set ,"Income Quantile","permIncome",transitory_only = FALSE, category_label = "Income/Mean Expenditure")

mean_Income_MPX = mean(Income_quantile_params[,4]*t(moments_by_Income_quantile$quantile_means))
mean_Income_MPX_se = (sum((Income_quantile_se[,4]*t(moments_by_Income_quantile$quantile_means))^2)^0.5)/num_quantiles

cov_Income_MPX = mean_Income_MPX-mean(Income_quantile_params[,4])*mean(t(moments_by_Income_quantile$quantile_means))
mean_MPX = mean(Income_quantile_params[,4])
mean_MPX_se = (sum(Income_quantile_se[,4]^2)^0.5)/num_quantiles
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

mean_cons_MPX = mean(MeanCons_quantile_params[,4]*t(moments_by_MeanCons_quantile$quantile_means))
mean_cons_MPX_se = (sum((MeanCons_quantile_se[,4]*t(moments_by_MeanCons_quantile$quantile_means))^2)^0.5)/num_quantiles


cons_weighted_MPC = mean(MeanCons_quantile_params[,4]*t(moments_by_MeanCons_quantile$quantile_means) /mean(t(moments_by_MeanCons_quantile$quantile_means)))

# Take these numbers to Excel:
mean_URE_MPX
mean_NNP_MPX
mean_Income_MPX
mean_cons_MPX
mean_MPX
mean_Income

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
  #dev.new()
  pdf(paste(figures_dir, filename,".pdf",sep=""))
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
  #dev.copy(pdf, paste(figures_dir, filename,".pdf",sep=""))
  #dev.copy(svg, paste(figures_dir, filename,".svg",sep=""))
  dev.off()
  params
}

tag_list = c("","_level_lincome_nostocks","_level_lincome_negcons","_level_lincome_ConsOutliers25")
tag_list_legend = c("Baseline","No Stocks","Include Neg Cons", "Strict Outliers" )

#First do liquid wealth
num_quantiles = 5
#transitory
robustness_plot(tag_list, "moments_by_liquid_wealth_quantile", as.character(1:num_quantiles), tag_list_legend, "Transitory MPX by Liquid Wealth Quintile", "Robust_tranMPX_liquidwealth", param_col=4, x_label="Quintile")
#permanent
robustness_plot(tag_list, "moments_by_liquid_wealth_quantile", as.character(1:num_quantiles), tag_list_legend, "Permanent MPX by Liquid Wealth Quintile", "Robust_permMPX_liquidwealth", param_col=3, x_label="Quintile")

#URE
#transitory
robustness_plot(tag_list, "moments_by_URE_quantile", as.character(c(1,4,7,10)), tag_list_legend, "Transitory MPX by URE Decile", "Robust_tranMPX_URE", param_col=4,legend_xpos = 2, x_label="Decile")
#permanent
robustness_plot(tag_list, "moments_by_URE_quantile", as.character(c(1,4,7,10)), tag_list_legend, "Permanent MPX by URE Decile", "Robust_permMPX_URE", param_col=3,legend_xpos = 2, x_label="Decile")

# Compare head with spouse
tag_list = c("","_level_lincome_head","_level_lincome_spouse")
tag_list_legend = c("Total","Head","Spouse")

#First do liquid wealth
num_quantiles = 5
#transitory
robustness_plot(tag_list, "moments_by_liquid_wealth_quantile", as.character(1:num_quantiles), tag_list_legend, "Transitory MPX by Liquid Wealth Quintile", "Spouse_tranMPX_liquidwealth", param_col=4, x_label="Quintile")
#permanent
robustness_plot(tag_list, "moments_by_liquid_wealth_quantile", as.character(1:num_quantiles), tag_list_legend, "Permanent MPX by Liquid Wealth Quintile", "Spouse_permMPX_liquidwealth", param_col=3, x_label="Quintile")

# Compare head with total
tag_list = c("","_level_lincome_head")
tag_list_legend = c("Total","Head")

#URE
#transitory
robustness_plot(tag_list, "moments_by_URE_quantile", as.character(1:10), tag_list_legend, "Transitory MPX by URE Decile", "total_tranMPX_URE", param_col=4,legend_xpos = 2, x_label="Decile")
#permanent
robustness_plot(tag_list, "moments_by_URE_quantile", as.character(1:10), tag_list_legend, "Permanent MPX by URE Decile", "total_permMPX_URE", param_col=3,legend_xpos = 2, x_label="Decile")

# Compare levels with log
tag_list = c("","_level_lincome_logs")
tag_list_legend = c("Baseline","Log Total (Elasticity)")

#First do liquid wealth
num_quantiles = 5
#transitory
robustness_plot(tag_list, "moments_by_liquid_wealth_quantile", as.character(1:num_quantiles), tag_list_legend, "Transitory MPX by Liquid Wealth Quintile", "Logs_tranMPX_liquidwealth", param_col=4,legend_xpos = 6, x_label="Quintile")
#permanent
robustness_plot(tag_list, "moments_by_liquid_wealth_quantile", as.character(1:num_quantiles), tag_list_legend, "Permanent MPX by Liquid Wealth Quintile", "Logs_permMPX_liquidwealth", param_col=3,legend_xpos = 6, x_label="Quintile")

#URE
#transitory
robustness_plot(tag_list, "moments_by_URE_quantile", as.character(1:10), tag_list_legend, "Transitory MPX by URE Decile", "Logs_tranMPX_URE", param_col=4,legend_xpos = 6, x_label="Decile")
#permanent
robustness_plot(tag_list, "moments_by_URE_quantile", as.character(1:10), tag_list_legend, "Permanent MPX by URE Decile", "Logs_permMPX_URE", param_col=3,legend_xpos = 6, x_label="Decile")

tag_list = c("","_level_lincome_quantilesbyperminc")
tag_list_legend = c("Baseline", "Liquid Wealth/Permanent Income" )

#First do liquid wealth
num_quantiles = 5
#transitory
robustness_plot(tag_list, "moments_by_liquid_wealth_quantile", as.character(1:num_quantiles), tag_list_legend, "Transitory MPX by Liquid Wealth Quintile", "DivPerm_tranMPX_liquidwealth", param_col=4,legend_xpos = 6, x_label="Quintile")
#permanent
robustness_plot(tag_list, "moments_by_liquid_wealth_quantile", as.character(1:num_quantiles), tag_list_legend, "Permanent MPX by Liquid Wealth Quintile", "DivPerm_permMPX_liquidwealth", param_col=3,legend_xpos = 6, x_label="Quintile")


#################################################################################
# 
# Output Auclert sufficient statistics
# aggregated up to include out of sample MPX
#
# 
###############################################################################

# Order of groups
# Our Sample
# Head <30
# Head >55
# Pensions and Special Saving
# General government (+NPISH)
# Non-financial corporations
# Financial sector (excl. pension funds)
# Rest of the world

# Data below comes from National accounts and Stata files (see URE_NNP_positions_NationalAccounts.xlsx in the Code directory)
NNP_by_group = c(-1402.5259604757, -216.9792779, -161.2326999, 939.6772886, -583.8741698, -333.746706, 1532.6105, 227.0940084)
URE_by_group = c(-421.5768154,  -105.331947, 38.6470548,  257.4458325,  -159.965526,  -91.4374537,  419.8932876,  62.21753654)
MPX_by_group = c(-999, 0.5, 0.5, 0.1, 0.0, 0.1, 0.1, 0.0) #note first category will be replaced with estimations
Total_consumption = 921.3977035
Total_consumption_under_30 = 149.357466144686
Total_consumption_over_55 = 265.951785816733
sample_size = 1289481.85714286 #note not an integer because this is an average over many years
under_30_size = 669565.714285714
over_55_size = 937670.1429
total_size = sample_size+under_30_size+over_55_size

Total_income_under_30 = 138.996539325257
Total_income_over_55 = 251.828251187066

E_R_component = URE_by_group*MPX_by_group/Total_consumption
E_R_component[1] = mean_URE_MPX*sample_size/(Total_consumption*1000000000)
E_R_component_se = mean_URE_MPX_se*sample_size/(Total_consumption*1000000000)

E_P_component = NNP_by_group*MPX_by_group/Total_consumption
E_P_component[1] = mean_NNP_MPX*sample_size/(Total_consumption*1000000000)
E_P_component_se = mean_NNP_MPX_se*sample_size/(Total_consumption*1000000000)


M_component = c(0,0,0)
M_component[1] = mean_Income_MPX*sample_size/(Total_consumption*1000000000)
M_component[2] = Total_income_under_30*MPX_by_group[2]/Total_consumption
M_component[3] = Total_income_over_55*MPX_by_group[3]/Total_consumption
M_component_se = mean_Income_MPX_se*sample_size/(Total_consumption*1000000000)

Mean_MPX_component = c(0,0,0)
Mean_MPX_component[1] = mean_MPX*sample_size/total_size
Mean_MPX_component[2] = MPX_by_group[2]*under_30_size/total_size
Mean_MPX_component[3] = MPX_by_group[3]*over_55_size/total_size
Mean_MPX_component_se = mean_MPX_se*sample_size/total_size

Income_over_C_component = c(0,0,0)
Income_over_C_component[1] = mean_Income*sample_size/(Total_consumption*1000000000)
Income_over_C_component[2] = Total_income_under_30/Total_consumption
Income_over_C_component[3] = Total_income_over_55/Total_consumption

E_R_auclert = sum(E_R_component)
E_R_auclert_se = E_R_component_se
E_P_auclert = sum(E_P_component)
E_P_auclert_se = E_P_component_se
M_auclert = sum(M_component)
M_auclert_se = M_component_se
E_Y_auclert = sum(M_component) - sum(Mean_MPX_component)*sum(Income_over_C_component)
E_Y_auclert_se = (M_component_se^2 + Mean_MPX_component_se^2*sum(Income_over_C_component)^2)^0.5
S_auclert = 1.0 - (mean_cons_MPX*sample_size/(Total_consumption*1000000000) + MPX_by_group[2]*Total_consumption_under_30/Total_consumption + MPX_by_group[2]*Total_consumption_over_55/Total_consumption ) 
S_auclert_se = mean_cons_MPX_se*sample_size/(Total_consumption*1000000000)


# Convert to dollars for the paper
x_rate_2015 = 6.87
NNP_dollar_by_group = NNP_by_group/x_rate_2015
URE_dollar_by_group = URE_by_group/x_rate_2015

# Write outputs to csv file
output = matrix(NA,nrow=9,ncol=7)
output[2:8,1] = MPX_by_group[2:8]
output[1:8,2] = NNP_dollar_by_group
output[9,2] = sum(NNP_dollar_by_group)
output[1:8,3] = URE_dollar_by_group
output[9,3] = sum(URE_dollar_by_group)
output[1:8,4] = E_P_component
output[9,4] = sum(E_P_component)
output[1:8,5] = E_R_component
output[9,5] = sum(E_R_component)
output[1:5,6] = c(M_auclert, E_Y_auclert, E_P_auclert, E_R_auclert, S_auclert)
output[1:5,7] = c(M_auclert_se, E_Y_auclert_se, E_P_auclert_se, E_R_auclert_se, S_auclert_se)

write.table(output, file = paste(tables_dir,"URE_NNP_positions_text.csv",sep=""),row.names=FALSE, na="",col.names=FALSE, sep=",")



###############################################################################
# Finally plot Auclert details on URE

require(shape)
# Function to plot MPX along with homeownership and liquid wealth
plot_Auclert_details<- function(params, se, labels, home_ownership, liquid_wealth, category_for_title, category_for_save) {
  
  params_input = cbind(params,home_ownership, liquid_wealth/max(liquid_wealth))
  se_input = cbind(se, matrix(0,nrow(se),2))
  category_for_save_input = category_for_save
  param_cols=4:6
  xlabel_pos = 2
  ylabel = "MPX"
  
  # Loop through plots for presentation
  for (i in 0:7){
    if (i==0 | i==1 | i==2){
      this_legend=c(expression(paste("Transitory MPX")))
      colors = c("#91bfdb","#ffffbf","#fc8d59")
      params = params_input*cbind(matrix(1,nrow(params_input),ncol(params_input)-2),matrix(0,nrow(params_input),2))
      se = se_input
      category_for_save = paste(category_for_save_input,"1",sep="")
      right_axis=FALSE
      if (i==0){
        category_for_save = paste(category_for_save_input,"blank",sep="")
      }
      if (i==2){
        category_for_save = paste(category_for_save_input,"1a",sep="")
      }
    }
    if (i==3 | i==4){
      this_legend=c(expression(paste("Transitory MPX")),"Homeownership")
      colors = c("#91bfdb","#ffffbf","#fc8d59")
      params = params_input*cbind(matrix(1,nrow(params_input),ncol(params_input)-1),matrix(0,nrow(params_input),1))
      se = se_input
      category_for_save = paste(category_for_save_input,"2",sep="")
      if (i==4){
        category_for_save = paste(category_for_save_input,"2a",sep="")
      }
      right_axis=FALSE
    }
    if (i==5 | i==6 | i==7){
      this_legend=c(expression(paste("Transitory MPX")),"Homeownership","Liquid Assets (Right Axis)")
      colors = c("#91bfdb","#ffffbf","#fc8d59")
      params = params_input
      se = se_input
      category_for_save = paste(category_for_save_input,"3",sep="")
      if (i==6){
        category_for_save = paste(category_for_save_input,"3a",sep="")
      }
      if (i==7){
        category_for_save = paste(category_for_save_input,"Paper",sep="")
        ylabel = "MPX or Homeownership rate"
      }
      right_axis=TRUE
    }
    #dev.new()
    pdf(paste(figures_dir, "MPXBy",category_for_save,tag,".pdf",sep=""))
    par(mar=c(8,7,4,5)+1,cex.axis=1.2,cex.lab=1.5)
    #barCenters <- barplot(t(params[,param_cols]),names.arg=labels,cex.names=0.8,beside=TRUE,col=colors)
    plotTop = max(max(params[,param_cols]),1.0)
    barCenters <- barplot(height=t(params[,param_cols]),
                          names.arg=labels,
                          cex.names=0.75,
                          beside=TRUE,col=colors,
                          las=2,ylim=c(0,plotTop), xaxt="n",
                          main=paste(title_string, " by ",category_for_title),
                          ylab = ylabel,xlab="URE/Mean Expenditure", border="black", axes=TRUE, width = c(2,1,1))
    text(x=barCenters[1,]+xlabel_pos, y =-plotTop*0.02,srt=45, adj=1, labels=labels,xpd=TRUE)
    segments(barCenters, t(params[,param_cols]-se[,param_cols]*1.96),
             barCenters,
             t(params[,param_cols]+se[,param_cols]*1.96), lwd=1.5)
    arrows(barCenters, t(params[,param_cols]-se[,param_cols]*1.96),
           barCenters,
           t(params[,param_cols]+se[,param_cols]*1.96), lwd=1.5,
           angle=90,code=3, length=0.05)
    legend(10, plotTop, legend=this_legend, fill=colors,bty="n")
    if (right_axis){
      myRightAxisTics = pretty(seq(0, max(liquid_wealth), length.out = 10))
      myRightAxisAt = myRightAxisTics/max(liquid_wealth)
      myRightAxisLabs = paste('$',formatC(myRightAxisTics,format="d",big.mark=","))
      axis(4, at = myRightAxisAt, labels = myRightAxisLabs,las=1)
    }
    if (i==0){
      rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "white",border=NA)
    }
    if (i==2){
      #label Medium MPC
      xpos=10
      ypos=0.4
      roundrect(mid = c(xpos,ypos), col = colors[1], radx = 6.5, rady = 0.03,dr = 0.001, rx=1,lcol=NA)
      text(xpos,ypos,"Medium MPX",col="black")
      #label Renters
      xpos=31.5
      ypos=0.57
      roundrect(mid = c(xpos,ypos), col = colors[1], radx = 6.5, rady = 0.03,dr = 0.001, rx=1,lcol=NA)
      text(xpos,ypos,"High MPX",col="black")
      #label Wealth Homeowners
      xpos=48
      ypos=0.1
      roundrect(mid = c(xpos,ypos), col = colors[1], radx = 4, rady = 0.03,dr = 0.001, rx=1,lcol=NA)
      text(xpos,ypos,"Low MPX",col="black")
    }
    if (i==4){
      #label Mortgaged Homeowners
      xpos=10
      ypos=0.7
      roundrect(mid = c(xpos,ypos), col = colors[2], radx = 6.5, rady = 0.03,dr = 0.001, rx=1,lcol=NA)
      text(xpos,ypos,"Homeowners",col="black")
      #label Renters
      xpos=35
      ypos=0.1
      roundrect(mid = c(xpos,ypos), col = colors[2], radx = 6.5, rady = 0.03,dr = 0.001, rx=1,lcol=NA)
      text(xpos,ypos,"Renters",col="black")
      #label Wealth Homeowners
      xpos=46
      ypos=0.5
      roundrect(mid = c(xpos,ypos), col = colors[2], radx = 6.5, rady = 0.03,dr = 0.001, rx=1,lcol=NA)
      text(xpos,ypos,"Homeowners",col="black")
    }
    if (i==6){
      #label wealthy Hand-to-Mouth
      xpos=13
      ypos=0.235
      roundrect(mid = c(xpos,ypos), col = colors[3], radx = 12, rady = 0.03,dr = 0.001, rx=1,lcol=NA)
      text(xpos,ypos,"Wealthy Hand-to-Mouth",col="black")
      #label Poor Hand-to-Mouth
      xpos=35
      ypos=0.135
      roundrect(mid = c(xpos,ypos), col = colors[3], radx = 10, rady = 0.03,dr = 0.001, rx=1,lcol=NA)
      text(xpos,ypos,"Poor Hand-to-Mouth",col="black")
      #label Wealthy
      xpos=50
      ypos=0.85
      roundrect(mid = c(xpos,ypos), col = colors[3], radx = 4, rady = 0.03,dr = 0.001, rx=1,lcol=NA)
      text(xpos,ypos,"Wealthy",col="black")
    }
    if (i==7){
      #label Medium MPC
      xpos=13
      ypos=0.4
      roundrect(mid = c(xpos,ypos), col = colors[1], radx = 12, rady = 0.03,dr = 0.001, rx=1,lcol=NA)
      text(xpos,ypos,"Wealthy Hand-to-Mouth",col="black")
      #label Renters
      xpos=33
      ypos=0.57
      roundrect(mid = c(xpos,ypos), col = colors[1], radx = 10, rady = 0.03,dr = 0.001, rx=1,lcol=NA)
      text(xpos,ypos,"Poor Hand-to-Mouth",col="black")
      #label Wealth Homeowners
      xpos=48
      ypos=0.1
      roundrect(mid = c(xpos,ypos), col = colors[1], radx = 4, rady = 0.03,dr = 0.001, rx=1,lcol=NA)
      text(xpos,ypos,"Wealthy",col="black")
    }
    #dev.copy(pdf, paste(figures_dir, "MPXBy",category_for_save,tag,".pdf",sep=""))
    dev.off()
  }
}
###############################################################################


home_ownership_URE  = read.csv(paste(txt_dir,"URE_decile_stats.txt",sep=""), header = FALSE)[,1]
liquid_wealth_URE  = read.csv(paste(txt_dir,"URE_decile_stats.txt",sep=""), header = FALSE)[,3] #median liquid assets
liquid_wealth_URE = liquid_wealth_URE/(6.87)  #convert to 2015 USD

plot_Auclert_details(URE_quantile_params,URE_quantile_se,URE_quantile_set,home_ownership_URE,liquid_wealth_URE ,"URE Decile","UREdetails")

#################################################################################
# 
# Below makes graphs for the presentation
# Nicer colors and larger fonts
#
# 
###############################################################################

thickness = 2
plot_width = 9.5
right_mar = 17
left_mar = 7
col_complete = "cyan"
col_solow = "blue"
col_bs = "red"


###############################################################################
# Pull in consumption saving numbers from Python output
FromPython <- scan(paste(PythonResults_folder,'basic_regressions.txt',sep=''), what=double(), sep=",")
FromPython <- scan(paste(PythonResults_folder,'benchmark_br_all.txt',sep=''), what=double(), sep=",")
solow_spending = 0.75
# Now draw graph
#png(paste(figures_dir, "basic_regression_complete_slides",tag,".png",sep=""))
pdf(paste(figures_dir, "basic_regression_complete_slides",tag,".pdf",sep=""),width=plot_width)
#svg(paste(figures_dir, "basic_regression_complete_slides",tag,".svg",sep=""))
par(mar=c(8,left_mar,4,right_mar)+0.1,cex.axis=1.2,cex.lab=1.5,xpd=TRUE)
plot(array(0.0,dim=dim(reg_coefs)), ylim=c(0,1),lty='solid', lwd=thickness,col=col_complete,type='o', pch=15, las=1,xlab='N, Years of Growth',ylab='',main='Regressing Consumption Growth on Income Growth')
mtext(TeX('$\\beta^N$'),side=2,las=1,line=4, cex = 1.5)
legend("right", inset=c(-0.6,0), legend=c("Complete Markets","","",""), col=c(col_complete,col_solow,col_bs,"black"),lty=c("solid","solid","solid","solid"),lwd=c(thickness,thickness,thickness,thickness), pch=c(15,16,17,15), pt.cex = 1, cex = 1.3, y.intersp=1.3,bty="n")
dev.off()

#png(paste(figures_dir, "basic_regression_solow_slides",tag,".png",sep=""))
pdf(paste(figures_dir, "basic_regression_solow_slides",tag,".pdf",sep=""),width=plot_width)
#svg(paste(figures_dir, "basic_regression_solow_slides",tag,".svg",sep=""))
par(mar=c(8,left_mar,4,right_mar)+0.1,cex.axis=1.2,cex.lab=1.5,xpd=TRUE)
plot(array(0.0,dim=dim(reg_coefs)), ylim=c(0,1),lty='solid', lwd=thickness,col=col_complete,type='o', pch=15, las=1, xlab='N, Years of Growth',ylab='',main='Regressing Consumption Growth on Income Growth')
lines(array(solow_spending,dim=dim(reg_coefs)),lty='solid', lwd=thickness,col=col_solow,type='o', pch=16)
mtext(TeX('$\\beta^N$'),side=2,las=1,line=4, cex = 1.5)
legend("right", inset=c(-0.6,0), legend=c("Complete Markets","Solow","",""), col=c(col_complete,col_solow,col_bs,"black"),lty=c("solid","solid","solid","solid"),lwd=c(thickness,thickness,thickness,thickness), pch=c(15,16,17,15), pt.cex = 1, cex = 1.3, y.intersp=1.3,bty="n")
dev.off()

#png(paste(figures_dir, "basic_regression_BS_slides",tag,".png",sep=""))
pdf(paste(figures_dir, "basic_regression_BS_slides",tag,".pdf",sep=""),width=plot_width)
#svg(paste(figures_dir, "basic_regression_BS_slides",tag,".svg",sep=""))
par(mar=c(8,left_mar,4,right_mar)+0.1,cex.axis=1.2,cex.lab=1.5,xpd=TRUE)
plot(array(0.0,dim=dim(reg_coefs)), ylim=c(0,1),lty='solid', lwd=thickness,col=col_complete,type='o', pch=15, las=1, xlab='N, Years of Growth',ylab='',main='Regressing Consumption Growth on Income Growth')
lines(array(solow_spending,dim=dim(reg_coefs)),lty='solid', lwd=thickness,col=col_solow,type='o', pch=16)
lines(FromPython[1:dim(reg_coefs)[1]],lty='solid',col=col_bs, lwd=thickness,type='o', pch=17)
mtext(TeX('$\\beta^N$'),side=2,las=1,line=4, cex = 1.5)
legend("right", inset=c(-0.6,0),legend=c("Complete Markets","Solow","Buffer-Stock",""), col=c(col_complete,col_solow,col_bs,"black"),lty=c("solid","solid","solid","solid"),lwd=c(thickness,thickness,thickness,thickness), pch=c(15,16,17,15), pt.cex = 1, cex = 1.3, y.intersp=1.3,bty="n")
arrows(2, 0.12, 9, 0.12)
arrows(9, 0.12, 2, 0.12)
text(3.5, 0.22, labels = "Relatively more \n transitory variance")
text(7.5, 0.22, labels = "Relatively more \n permanent variance")
dev.off()

#png(paste(figures_dir, "basic_regression_slides",tag,".png",sep=""))
pdf(paste(figures_dir, "basic_regression_slides",tag,".pdf",sep=""),width=plot_width)
#svg(paste(figures_dir, "basic_regression_slides",tag,".svg",sep=""))
par(mar=c(8,left_mar,4,right_mar)+0.1,cex.axis=1.2,cex.lab=1.5,xpd=TRUE)
plot(array(0.0,dim=dim(reg_coefs)), ylim=c(0,1),lty='solid', lwd=thickness,col=col_complete,type='o', pch=15, las=1, xlab='N, Years of Growth',ylab='',main='Regressing Consumption Growth on Income Growth')
lines(array(solow_spending,dim=dim(reg_coefs)),lty='solid',col=col_solow, lwd=thickness,type='o', pch=16)
lines(FromPython[1:dim(reg_coefs)[1]],lty='solid',lwd=thickness,col=col_bs,type='o', pch=17)
points(reg_coefs, pch=15,lwd=thickness)
lines(reg_coefs)
lines(reg_coefs+1.96*std_errors,lty='dashed')
lines(reg_coefs-1.96*std_errors,lty='dashed')
mtext(TeX('$\\beta^N$'),side=2,las=1,line=4, cex = 1.5)
legend("right", inset=c(-0.6,0), legend=c("Complete Markets","Solow","Buffer-Stock","Data"), col=c(col_complete,col_solow,col_bs,"black"),lty=c("solid","solid","solid","solid"),lwd=c(thickness,thickness,thickness,thickness), pch=c(15,16,17,15), pt.cex = 1, cex = 1.3, y.intersp=1.3,bty="n")
arrows(2, 0.12, 9, 0.12)
arrows(9, 0.12, 2, 0.12)
text(3.5, 0.22, labels = "Relatively more \n transitory variance")
text(7.5, 0.22, labels = "Relatively more \n permanent variance")
dev.off()

#png(paste(figures_dir, "basic_regression_liquid_wealth_slides",tag,".png",sep=""))
pdf(paste(figures_dir, "basic_regression_liquid_wealth_slides",tag,".pdf",sep=""),width=plot_width)
#svg(paste(figures_dir, "basic_regression_liquid_wealth",tag,".svg",sep=""))
par(mar=c(8,left_mar,4,right_mar)+0.1,cex.axis=1.2,cex.lab=1.5,xpd=TRUE)
plot(array(0.0,dim=dim(reg_coefs)), ylim=c(0,1),lty='solid', lwd=thickness,col=col_complete,type='o', pch=15, las=1, xlab='N, Years of Growth',ylab='',main='Regressing Consumption Growth on Income Growth')
lines(array(solow_spending,dim=dim(reg_coefs)),lty='solid', lwd=thickness,col=col_solow,type='o', pch=16)
lines(FromPython[1:dim(reg_coefs)[1]],lty='solid', lwd=thickness,col=col_bs,type='o', pch=17)
points(reg_coefs,col='gray', pch=15,lwd=thickness)
lines(reg_coefs,col='gray')
lines(reg_coefs+1.96*std_errors,lty='dashed',col='gray')
lines(reg_coefs-1.96*std_errors,lty='dashed',col='gray')
#Add high and low liquid quantiles
points(reg_coefs_liquid_wealth$X1, pch=15,lwd=thickness)
lines(reg_coefs_liquid_wealth$X1)
lines(reg_coefs_liquid_wealth$X1+1.96*std_errors_liquid_wealth$X1,lty='dashed')
lines(reg_coefs_liquid_wealth$X1-1.96*std_errors_liquid_wealth$X1,lty='dashed')
points(reg_coefs_liquid_wealth$X5, pch=15,lwd=thickness)
lines(reg_coefs_liquid_wealth$X5)
lines(reg_coefs_liquid_wealth$X5+1.96*std_errors_liquid_wealth$X5,lty='dashed')
lines(reg_coefs_liquid_wealth$X5-1.96*std_errors_liquid_wealth$X5,lty='dashed')
mtext(TeX('$\\beta^N$'),side=2,las=1,line=4, cex = 1.5)
legend("right", inset=c(-0.6,0), legend=c("Complete Markets","Solow","Buffer-Stock","Data"), col=c(col_complete,col_solow,col_bs,"black"),lty=c("solid","solid","solid","solid"),lwd=c(thickness,thickness,thickness,thickness), pch=c(15,16,17,15), pt.cex = 1, cex = 1.3, y.intersp=1.3,bty="n")
arrows(2, 0.12, 9, 0.12)
arrows(9, 0.12, 2, 0.12)
text(3.5, 0.22, labels = "Relatively more \n transitory variance")
text(7.5, 0.22, labels = "Relatively more \n permanent variance")
text(3.0, 0.92, labels = "Least Liquid", cex=1.3)
text(6, 0.47, labels = "Most Liquid",cex=1.3)
dev.off()

#######################################################
income_growth_year = read.csv(paste(txt_dir,"descriptives_stddev.txt",sep=""), header = TRUE)[,1]
income_growth_std = read.csv(paste(txt_dir,"descriptives_stddev.txt",sep=""), header = TRUE)[,3]
# plot income growth std
#dev.new()
pdf(paste(figures_dir, "IncomeGrowthStd.pdf",sep=""))
par(mar=c(8,7,4,5),cex.axis=1.2,cex.lab=1.5)
plot(income_growth_year,income_growth_std,col=colors[1],ylim=c(0.0,0.15),xlab="Year",ylab="Income Growth Std.",
     main="Income Growth Standard Deviation by Year")
lines(income_growth_year,income_growth_std,col=colors[1])
dev.off()

##############################################################################
# Extra code for "Is Liquid Wealth Sufficient" section
source(paste(Rcode_folder,"IsLiquidWealthSufficient_investigation.r",sep=""))

