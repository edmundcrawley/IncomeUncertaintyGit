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
tables_dir = "C:/Users/edmun/OneDrive/Documents/Research/Denmark/IncomeUncertaintyGit/Code/Rcode/Tables/"
# if running for production store figures here:
#figures_dir = "C:/Users/edmun/OneDrive/Documents/Research/Denmark/IncomeUncertaintyGit/Paper/Figures"
require(zoo)
require(latex2exp)
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
png(paste(figures_dir, "basic_regression.png",sep=""))
plot(reg_coefs, ylim=c(0,1), xlab='n, Years of Growth',ylab=TeX('$\\beta^n$, Regression Coefficient'),main='Regressing Consumption Growth on Income Growth')
lines(reg_coefs)
lines(reg_coefs+1.96*std_errors,lty='dashed')
lines(reg_coefs-1.96*std_errors,lty='dashed')
lines(array(1,dim=dim(reg_coefs)),lty='solid',col='green',type='o')
lines(array(0.0,dim=dim(reg_coefs)),lty='solid',col='blue',type='o')
lines(FromPython[1:dim(reg_coefs)[1]],lty='solid',col='red',type='o')
legend(6, 0.65, legend=c("Data", "Solow", "Complete Markets", "Buffer-Stock"), col=c("black","green","blue","red"),lty=c("solid","solid","solid","solid"))
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
  
  write.table(var_perm_array, file=paste(tables_dir,'var_perm_array_',max_diff,'.txt',sep=''), row.names=FALSE, col.names=FALSE)
  write.table(var_tran_array, file=paste(tables_dir,'var_tran_array_',max_diff,'.txt',sep=''), row.names=FALSE, col.names=FALSE)
  write.table(ins_perm_array, file=paste(tables_dir,'ins_perm_array_',max_diff,'.txt',sep=''), row.names=FALSE, col.names=FALSE)
  write.table(ins_tran_array, file=paste(tables_dir,'ins_tran_array_',max_diff,'.txt',sep=''), row.names=FALSE, col.names=FALSE)
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

png(paste(figures_dir, "IncreasingDiff.png",sep=""))
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
