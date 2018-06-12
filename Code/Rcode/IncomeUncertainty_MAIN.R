###############################################################################
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
# if running for production store figures here:
#figures_dir = "C:/Users/edmun/OneDrive/Documents/Research/Denmark/IncomeUncertaintyGit/Paper/Figures"
require(zoo)
require(latex2exp)
source(paste(Rcode_folder,"min_distance_CS.r",sep=""))
###############################################################################
colors = c("#fc8d59","#91bfdb")


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
                        beside=TRUE,col=c(colors[1],colors[2]),
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
  legend(2, plotTop, legend=c(expression(paste(sigma[p]^2," Permanent Var")), expression(paste(sigma[q]^2," Transitory Var"))), fill=c(colors[1],colors[2]),bty="n")
  dev.copy(png, paste(figures_dir, "VarianceBy",category_for_save,tag,".png",sep=""))
  dev.off()
  
  # Now plot the Expenditure Elasticities
  dev.new()
  barCenters <- barplot(t(params[,3:4]),names.arg=labels,cex.names=0.8,beside=TRUE,col=c(colors[1],colors[2]))
  par(mar=c(8,7,4,5)+0.1)
  plotTop = max(params[,3:4])*1.2
  barCenters <- barplot(height=t(params[,3:4]),
                        names.arg=labels,
                        cex.names=0.75,
                        beside=TRUE,col=c(colors[1],colors[2]),
                        las=2,ylim=c(0,plotTop), xaxt="n",
                        main=paste(title_string, " by ",category_for_title),
                        ylab = axis_string, border="black", axes=TRUE)
  text(x=barCenters[1,]+1, y =-plotTop*0.02,srt=45, adj=1, labels=labels,xpd=TRUE)
  segments(barCenters, t(params[,3:4]-se[,3:4]*1.96),
           barCenters,
           t(params[,3:4]+se[,3:4]*1.96), lwd=1.5)
  arrows(barCenters, t(params[,3:4]-se[,3:4]*1.96),
         barCenters,
         t(params[,3:4]+se[,3:4]*1.96), lwd=1.5,
         angle=90,code=3, length=0.05)
  legend(2, plotTop, legend=c(expression(paste(phi," Permanent MPX")),expression(paste(psi," Transitory MPX"))), fill=c(colors[1],colors[2]),bty="n")
  dev.copy(png, paste(figures_dir, "MPXBy",category_for_save,tag,".png",sep=""))
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
wealth_quantile_set = c(wealth_quantile_set,paste('$',format(round(moments_by_liquid_wealth_quantile$quantiles[[4]],round_digits),big.mark=",", trim=TRUE),'+',sep=''))
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
wealth_quantile_set = c(paste('$...-',format(round(moments_by_net_wealth_quantile$quantiles[[1]],round_digits),big.mark=",", trim=TRUE),sep=''))
wealth_quantile_set = c(wealth_quantile_set,paste('$',format(round(moments_by_net_wealth_quantile$quantiles[[1]],round_digits),big.mark=",", trim=TRUE),'-',format(round(moments_by_net_wealth_quantile$quantiles[[2]],round_digits),big.mark=",", trim=TRUE),sep=''))
wealth_quantile_set = c(wealth_quantile_set,paste('$',format(round(moments_by_net_wealth_quantile$quantiles[[2]],round_digits),big.mark=",", trim=TRUE),'-',format(round(moments_by_net_wealth_quantile$quantiles[[3]],round_digits),big.mark=",", trim=TRUE),sep=''))
wealth_quantile_set = c(wealth_quantile_set,paste('$',format(round(moments_by_net_wealth_quantile$quantiles[[3]],round_digits),big.mark=",", trim=TRUE),'-',format(round(moments_by_net_wealth_quantile$quantiles[[4]],round_digits),big.mark=",", trim=TRUE),sep=''))
wealth_quantile_set = c(wealth_quantile_set,paste('$',format(round(moments_by_net_wealth_quantile$quantiles[[4]],round_digits),big.mark=",", trim=TRUE),'+',sep=''))
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
png(filename=paste(figures_dir,'VarianceByAge',tag,'.png',sep=''))
plot(age_set, age_params[,1],col="green",main="Permanent and Transitory Variance by Age",xlab="Age",ylab="Shock Variance",ylim=c(0,0.025))
points(age_set, age_params[,2],col="red")
points(age_set, age_total_var,col="black")
lines(age_set, rollmean(age_params[,1],5,fill=NA), col="green")
lines(age_set, rollmean(age_params[,2],5,fill=NA), col="red")
lines(age_set, rollmean(age_total_var,5,fill=NA), col="black")
lines(age_set, rollmean(2.0/3.0*age_params[,1]+2.0*age_params[,2],5,fill=NA), col="black",lty="dashed")
legend(40, 0.022, legend=c(expression(paste(sigma[p]^2," Permanent Var")), expression(paste(sigma[q]^2," Transitory Var")), expression(paste("var(",Delta,"y)")),expression(paste(frac(2,3),sigma[p]^2,"+2",sigma[q]^2,sep=""))), col=c("green","red","black","black"),lty=c("solid","solid","solid","dashed"),bty="n")
dev.off()

png(filename=paste(figures_dir,'MPXByAge',tag,'.png',sep=''))
plot(age_set, age_params[,3],col="green",main=paste(title_string, " by Age",sep=""),xlab="Age",ylab=axis_string,ylim=c(0,1))
points(age_set, age_params[,4],col="red")
lines(age_set, rollmean(age_params[,3],5,fill=NA), col="green")
lines(age_set, rollmean(age_params[,4],5,fill=NA), col="red")
legend(38, 0.24, legend=c(expression(paste(phi," Permanent MPX")),expression(paste(psi," Transitory MPX"))), col=c("green","red"),lty=c("solid","solid"))
dev.off()
###############################################################################

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
# Pull in consumption saving numbers from Python output
PythonResults_folder = "C:/Users/edmun/OneDrive/Documents/Research/Denmark/IncomeUncertaintyGit/Code/PrefShockModel/Results/"
FromPython <- scan(paste(PythonResults_folder,'basic_regressions.txt',sep=''), what=double(), sep=",")
# Now draw graph
png(paste(figures_dir, "basic_regression_complete",tag,".png",sep=""))
plot(array(0.0,dim=dim(reg_coefs)), ylim=c(0,1),lty='solid',col='blue',type='o', xlab='N, Years of Growth',ylab=TeX('$\\beta^n$, Regression Coefficient'),main='Regressing Consumption Growth on Income Growth')
legend(6, 0.65, legend=c("Complete Markets","","",""), col=c("blue","green","red","black"),lty=c("solid","solid","solid","solid"))
dev.off()
png(paste(figures_dir, "basic_regression_solow",tag,".png",sep=""))
plot(array(0.0,dim=dim(reg_coefs)), ylim=c(0,1),lty='solid',col='blue',type='o', xlab='N, Years of Growth',ylab=TeX('$\\beta^n$, Regression Coefficient'),main='Regressing Consumption Growth on Income Growth')
lines(array(0.8,dim=dim(reg_coefs)),lty='solid',col='green',type='o')
legend(6, 0.65, legend=c("Complete Markets","Solow","",""), col=c("blue","green","red","black"),lty=c("solid","solid","solid","solid"))
dev.off()
png(paste(figures_dir, "basic_regression_BS",tag,".png",sep=""))
plot(array(0.0,dim=dim(reg_coefs)), ylim=c(0,1),lty='solid',col='blue',type='o', xlab='N, Years of Growth',ylab=TeX('$\\beta^n$, Regression Coefficient'),main='Regressing Consumption Growth on Income Growth')
lines(array(0.8,dim=dim(reg_coefs)),lty='solid',col='green',type='o')
lines(FromPython[1:dim(reg_coefs)[1]],lty='solid',col='red',type='o')
legend(6, 0.65, legend=c("Complete Markets","Solow","Buffer-Stock",""), col=c("blue","green","red","black"),lty=c("solid","solid","solid","solid"))
arrows(2, 0.2, 9, 0.2)
arrows(9, 0.2, 2, 0.2)
text(3.5, 0.3, labels = "Relatively more \n transitory variance")
text(7.5, 0.3, labels = "Relatively more \n permanent variance")
dev.off()
png(paste(figures_dir, "basic_regression",tag,".png",sep=""))
plot(array(0.0,dim=dim(reg_coefs)), ylim=c(0,1),lty='solid',col='blue',type='o', xlab='N, Years of Growth',ylab=TeX('$\\beta^n$, Regression Coefficient'),main='Regressing Consumption Growth on Income Growth')
lines(array(0.8,dim=dim(reg_coefs)),lty='solid',col='green',type='o')
lines(FromPython[1:dim(reg_coefs)[1]],lty='solid',col='red',type='o')
points(reg_coefs)
lines(reg_coefs)
lines(reg_coefs+1.96*std_errors,lty='dashed')
lines(reg_coefs-1.96*std_errors,lty='dashed')
legend(6, 0.65, legend=c("Complete Markets","Solow","Buffer-Stock","Data"), col=c("blue","green","red","black"),lty=c("solid","solid","solid","solid"))
arrows(2, 0.2, 9, 0.2)
arrows(9, 0.2, 2, 0.2)
text(3.5, 0.3, labels = "Relatively more \n transitory variance")
text(7.5, 0.3, labels = "Relatively more \n permanent variance")
dev.off()

###############################################################################
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
plot(diag(params_loop[[paste(to_plot,max_diff,sep='')]][,-(1:n2_minum_n1)]))
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

png(paste(figures_dir, "IncreasingDiff",tag,".png",sep=""))
plot(1:max_diff,y2_diff,ylim=c(0,1.2*max(y2_diff)),xlim=c(0,max_diff),
     main="Covariance with Increasing Difference Operator",xlab="n",ylab="variance/covariance")
lines(1:max_diff,y2_diff)
points(1:max_diff,cy_diff)
lines(1:max_diff,cy_diff,lty="dashed")
lines(0:max_diff,(0:max_diff-1.0/3.0)*CS_output_sub$var_perm + 2*CS_output_sub$var_tran, col="red")
lines(0:max_diff,(0:max_diff-1.0/3.0)*CS_output_sub$ins_perm*CS_output_sub$var_perm + 2*CS_output_sub$ins_tran*CS_output_sub$var_tran, col="green")
legend(0.25, 0.05, legend=c(expression(paste("var(",Delta^n,"y) Empirical"),paste("var(",Delta^n,"y) matched to n=3,4,5"), paste("cov(",Delta^n,"y,",Delta^n,"c) Empirical"),paste("cov(",Delta^n,"y,",Delta^n,"c) matched to n=3,4,5"))),lty=c("solid","solid","dashed","solid"),col=c("black","red","black","green"))
dev.off()
###############################################################################

###############################################################################
# load net weath quintile data and create graph
load(paste(moments_dir,'moments_by_home_owner',tag,'.RData',sep=''))
output =estimation_by_category(moments_by_home_owner, c("X0","X1"))
home_owner_output=output
home_owner_params = output$category_params
home_owner_se = output$category_se
home_owner_obs = output$category_obs
home_owner_total_var = output$category_total_var
home_owner_set = c("Renter","Owner")
plot_estimataion_output(home_owner_params,home_owner_se,home_owner_set ,"Homeownership","")
###############################################################################

###############################################################################
# load liquid weath quintile data by non-durable proxyand create graph
load(paste(moments_dir,'moments_by_liquid_wealth_quantile_head_0nocar_2005','.RData',sep=''))
num_quantiles = 5
round_digits = -3
wealth_quantile_set = as.character(1:num_quantiles)
output =estimation_by_category(moments_by_liquid_wealth_quantile, make.names(wealth_quantile_set))
wealth_quantile_output=output
wealth_quantile_params = output$category_params
wealth_quantile_se = output$category_se
wealth_quantile_obs = output$category_obs
wealth_quantile_total_var = output$category_total_var

load(paste(moments_dir,'moments_by_liquid_wealth_quantile_head_nocar_2005','.RData',sep=''))
wealth_quantile_set = as.character(1:num_quantiles)
output =estimation_by_category(moments_by_liquid_wealth_quantile, make.names(wealth_quantile_set))
wealth_quantile_output_nocar=output
wealth_quantile_params_nocar = output$category_params
wealth_quantile_se_nocar = output$category_se
wealth_quantile_obs_nocar = output$category_obs
wealth_quantile_total_var_nocar = output$category_total_var

load(paste(moments_dir,'moments_by_liquid_wealth_quantile_head_nodurableproxy_2005','.RData',sep=''))
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
par(mar=c(8,7,4,5)+0.1)
plotTop = max(wealth_quantile_params[,3:4])*1.2
barCenters <- barplot(height=t(wealth_quantile_params[,3:4]),
                      names.arg=wealth_quantile_set,
                      cex.names=0.75,
                      beside=TRUE,col=c("grey90","grey85"),
                      las=2,ylim=c(-0.2,plotTop), xaxt="n",
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
segments(barCenters, t(wealth_quantile_params_nodurableproxy[,3:4]-se[,3:4]*1.96),
         barCenters,
         t(params[,3:4]+se[,3:4]*1.96), lwd=1.5)
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
dev.copy(png, paste(figures_dir, "MPXByDurables_nodurableproxy.png",sep=""))
dev.off()


dev.new()
par(mar=c(8,7,4,5)+0.1)
plotTop = max(wealth_quantile_params[,3:4])*1.2
barCenters <- barplot(height=t(wealth_quantile_params[,3:4]),
                      names.arg=wealth_quantile_set,
                      cex.names=0.75,
                      beside=TRUE,col=c("grey90","grey85"),
                      las=2,ylim=c(-0.2,plotTop), xaxt="n",
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
         t(params[,3:4]+se[,3:4]*1.96), lwd=1.5)
arrows(barCenters, t(wealth_quantile_params_nocar[,3:4]-wealth_quantile_se_nocar[,3:4]*1.96),
       barCenters,
       t(wealth_quantile_params_nocar[,3:4]+wealth_quantile_se_nocar[,3:4]*1.96), lwd=1.5,
       angle=90,code=3, length=0.05)
text(x=barCenters[1,4]+1, y =text_y_pos, adj = c(0,0), labels="All Expenditure",xpd=TRUE,col="grey80")
text(x=barCenters[1,4]+1, y =text_y_pos-0.05, adj = c(0,0), labels="Excluding Cars",xpd=TRUE)
legend(2, plotTop, legend=c(expression(paste(phi," Permanent MPX")),expression(paste(psi," Transitory MPX"))), fill=c(colors[1],colors[2]),bty="n")
dev.copy(png, paste(figures_dir, "MPXByDurables_nocar.png",sep=""))
dev.off()

dev.new()
par(mar=c(8,7,4,5)+0.1)
plotTop = max(wealth_quantile_params[,3:4])*1.2
barCenters <- barplot(height=t(wealth_quantile_params[,3:4]),
                      names.arg=wealth_quantile_set,
                      cex.names=0.75,
                      beside=TRUE,col=c(colors[1],colors[2]),
                      las=2,ylim=c(-0.2,plotTop), xaxt="n",
                      main=paste("MPX by Liquid Wealth Quantile"),
                      ylab = axis_string, border="black", axes=TRUE)
text(x=barCenters[1,]+1, y =-plotTop*0.02,srt=45, adj=1, labels=wealth_quantile_set,xpd=TRUE)
segments(barCenters, t(wealth_quantile_params[,3:4]-wealth_quantile_se[,3:4]*1.96),
         barCenters,
         t(params[,3:4]+se[,3:4]*1.96), lwd=1.5)
arrows(barCenters, t(wealth_quantile_params[,3:4]-wealth_quantile_se[,3:4]*1.96),
       barCenters,
       t(wealth_quantile_params[,3:4]+wealth_quantile_se[,3:4]*1.96), lwd=1.5,
       angle=90,code=3, length=0.05)
text(x=barCenters[1,4]+1, y =text_y_pos, adj = c(0,0), labels="All Expenditure",xpd=TRUE)
legend(2, plotTop, legend=c(expression(paste(phi," Permanent MPX")),expression(paste(psi," Transitory MPX"))), fill=c(colors[1],colors[2]),bty="n")
dev.copy(png, paste(figures_dir, "MPXByDurables_all.png",sep=""))
dev.off()

#create black white graph, for use in slides
dev.new()
par(mar=c(8,7,4,5)+0.1)
plotTop = max(wealth_quantile_params[,3:4])*1.2
barCenters <- barplot(height=t(wealth_quantile_params[,3:4]),
                      names.arg=wealth_quantile_set,
                      cex.names=0.75,
                      beside=TRUE,col=c(colors[1],colors[2]),
                      las=2,ylim=c(-0.2,plotTop), xaxt="n",
                      main=paste("MPX by Liquid Wealth Quantile"),
                      ylab = axis_string, border="black", axes=TRUE)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "white",border=NA)
dev.copy(png, paste(figures_dir, "MPXByDurables_blank.png",sep=""))
dev.off()
###############################################################################
