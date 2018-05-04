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

