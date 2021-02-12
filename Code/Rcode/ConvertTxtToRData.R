#################################################################################
# 
# This file turn txt files in RData files of relevant format
#
# 
###############################################################################

#############NOTE - the following file needs to be updated on MONDAY###########################################
#this_quantiles = as.vector(read.table(paste(txt_dir,'moments_by_liquid_wealth_quantile_quantiles.txt',sep=''), header = FALSE, sep = ",", dec = "."))
#############NOTE - the following file needs to be updated on MONDAY###########################################
#Also on Monday, delta_y_var can be replaced with actual data
#############NOTE - the following file needs to be updated on MONDAY###########################################


moments_dir = "C:/Users/edmun/OneDrive/Documents/Research/Denmark/IncomeUncertaintyGit/Code/ServerRcode/ServerOutput/AEJ_revision/"
txt_dir = "C:/Users/edmun/OneDrive/Documents/Research/Denmark/IncomeUncertaintyGit/Code/ServerRcode/ServerOutput/AEJ_revision/TxtFilesFromAndreas/"

MomentsForRDataFile<- function(moments_stub, num_quantiles=5) {
  moments <- list()
  for (i in 1:num_quantiles){
    moments[[paste("X",i,sep='')]] = list()
    this_c_vector = as.vector(read.table(paste(txt_dir,moments_stub,i,'c_vector.txt',sep=''), header = FALSE, sep = ",", dec = "."))
    moments[[paste("X",i,sep='')]][["c_vector"]] = this_c_vector$V1
    this_omega = as.matrix(read.table(paste(txt_dir,moments_stub,i,'_omega.txt',sep=''), header = FALSE, sep = ",", dec = "."))
    moments[[paste("X",i,sep='')]][["omega"]] = this_omega
    moments[[paste("X",i,sep='')]][["T"]] = 12
    
    #this_delta_y_var = as.vector(read.table(paste(txt_dir,moments_stub,i,'delta_y_var.txt',sep=''), header = FALSE, sep = ",", dec = "."))
    this_delta_y_var = 0.0
    moments[[paste("X",i,sep='')]][["delta_y_var"]] = this_delta_y_var
  }
  if (moments_stub == "moments_low_inc_vol_by_liquid_wealth_quantile" | moments_stub == "moments_high_inc_vol_by_liquid_wealth_quantile"){
    this_quantiles = as.vector(read.table(paste(txt_dir,"moments_by_liquid_wealth_quantile",'_quantiles.txt',sep=''), header = FALSE, sep = ",", dec = "."))
  }
  else{
    this_quantiles = as.vector(read.table(paste(txt_dir,moments_stub,'_quantiles.txt',sep=''), header = FALSE, sep = ",", dec = "."))
  }
  moments[["quantiles"]] = this_quantiles$V1
  return (moments)
}

moments_by_liquid_wealth_quantile = MomentsForRDataFile('moments_by_liquid_wealth_quantile', num_quantiles=5)
save(moments_by_liquid_wealth_quantile, file=paste(moments_dir,'moments_by_liquid_wealth_quantile.RData',sep=''))

moments_low_inc_vol_by_liquid_wealth_quantile = MomentsForRDataFile('moments_low_inc_vol_by_liquid_wealth_quantile', num_quantiles=5)
save(moments_low_inc_vol_by_liquid_wealth_quantile, file=paste(moments_dir,'moments_low_inc_vol_by_liquid_wealth_quantile.RData',sep=''))

moments_high_inc_vol_by_liquid_wealth_quantile = MomentsForRDataFile('moments_high_inc_vol_by_liquid_wealth_quantile', num_quantiles=5)
save(moments_high_inc_vol_by_liquid_wealth_quantile, file=paste(moments_dir,'moments_high_inc_vol_by_liquid_wealth_quantile.RData',sep=''))

moments_by_URE_quantile = MomentsForRDataFile('moments_by_URE_quantile', num_quantiles=10)
save(moments_by_URE_quantile, file=paste(moments_dir,'moments_by_URE_quantile.RData',sep=''))

moments_by_NNP_quantile = MomentsForRDataFile('moments_by_NNP_quantile', num_quantiles=10)
save(moments_by_NNP_quantile, file=paste(moments_dir,'moments_by_NNP_quantile.RData',sep=''))

moments_by_net_wealth_quantile = MomentsForRDataFile('moments_by_net_wealth_quantile', num_quantiles=5)
save(moments_by_net_wealth_quantile, file=paste(moments_dir,'moments_by_net_wealth_quantile.RData',sep=''))

moments_by_MeanCons_quantile = MomentsForRDataFile('moments_by_MeanCons_quantile', num_quantiles=10)
save(moments_by_MeanCons_quantile, file=paste(moments_dir,'moments_by_MeanCons_quantile.RData',sep=''))

moments_by_Income_quantile = MomentsForRDataFile('moments_by_Income_quantile', num_quantiles=10)
save(moments_by_Income_quantile, file=paste(moments_dir,'moments_by_Income_quantile.RData',sep=''))

moments_by_liquid_wealth_quantile_10 = MomentsForRDataFile('moments_by_liquid_wealth_quantile_10', num_quantiles=5)
save(moments_by_liquid_wealth_quantile_10, file=paste(moments_dir,'moments_by_liquid_wealth_quantile_10.RData',sep=''))

#Liquid wealth DECILES are stored in a different directory
moments_dir = "C:/Users/edmun/OneDrive/Documents/Research/Denmark/IncomeUncertaintyGit/Code/ServerRcode/ServerOutput/AEJ_revision/IsLiquidWealthSufficient/"
txt_dir = "C:/Users/edmun/OneDrive/Documents/Research/Denmark/IncomeUncertaintyGit/Code/ServerRcode/ServerOutput/AEJ_revision/TxtFilesFromAndreas/LiqWealthDeciles/"
moments_by_liquid_wealth_quantile = MomentsForRDataFile('moments_by_liquid_wealth_quantile', num_quantiles=5)
save(moments_by_liquid_wealth_quantile, file=paste(moments_dir,'moments_by_liquid_wealth_quantile.RData',sep=''))
moments_dir = "C:/Users/edmun/OneDrive/Documents/Research/Denmark/IncomeUncertaintyGit/Code/ServerRcode/ServerOutput/AEJ_revision/"
txt_dir = "C:/Users/edmun/OneDrive/Documents/Research/Denmark/IncomeUncertaintyGit/Code/ServerRcode/ServerOutput/AEJ_revision/TxtFilesFromAndreas/"


