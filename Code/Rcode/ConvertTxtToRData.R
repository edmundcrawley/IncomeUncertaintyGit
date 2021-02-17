#################################################################################
# 
# This file turn txt files in RData files of relevant format
#
# 
###############################################################################
base_dir = "C:/Users/edmun/OneDrive/Documents/Research/Denmark/IncomeUncertaintyGit/"
moments_dir = paste(base_dir,"Code/ServerRcode/ServerOutput/AEJ_revision/",sep='')
txt_dir = paste(base_dir,"Code/ServerRcode/ServerOutput/AEJ_revision/TxtFilesFromAndreas/",sep='')
moments_dir_orig = paste(base_dir,"Code/ServerRcode/ServerOutput/",sep='')

#############NOTE - the following files needs to be updated on MONDAY###########################################
#moments_by_liquid_wealth_quantile_quantiles.txt
CopyQuantileMeans <- function(moment_stub){
  load(paste(moments_dir_orig,moment_stub,'.RData',sep=''))
  this_quantile_means = paste(moment_stub, '$quantile_means', sep="")
  this_file = paste(txt_dir,moment_stub,'_quantile_means','.txt',sep='')
  write.table(eval(parse(text = this_quantile_means)), file = this_file,row.names=FALSE, col.names=FALSE, na="",sep=',')
}
CopyQuantileMeans("moments_by_URE_quantile")
CopyQuantileMeans("moments_by_NNP_quantile")
CopyQuantileMeans("moments_by_MeanCons_quantile")
CopyQuantileMeans("moments_by_Income_quantile")
#############NOTE - the following file needs to be updated on MONDAY###########################################


MomentsForRDataFile<- function(moments_stub, num_quantiles=5, quantile_stub = "X", quantile_start = 0) {
  moments <- list()
  for (i in 1:num_quantiles){
    moments[[paste(quantile_stub,i+quantile_start,sep='')]] = list()
    this_c_vector = as.vector(read.table(paste(txt_dir,moments_stub,i+quantile_start,'c_vector.txt',sep=''), header = FALSE, sep = ",", dec = "."))
    moments[[paste(quantile_stub,i+quantile_start,sep='')]][["c_vector"]] = this_c_vector$V1
    this_omega = as.matrix(read.table(paste(txt_dir,moments_stub,i+quantile_start,'_omega.txt',sep=''), header = FALSE, sep = ",", dec = "."))
    moments[[paste(quantile_stub,i+quantile_start,sep='')]][["omega"]] = this_omega
    moments[[paste(quantile_stub,i+quantile_start,sep='')]][["T"]] = 12
    
    this_delta_y_var = as.vector(read.table(paste(txt_dir,moments_stub,i+quantile_start,'_delta_y_var.txt',sep=''), header = FALSE, sep = ",", dec = "."))
    #this_delta_y_var = 0.0
    moments[[paste(quantile_stub,i+quantile_start,sep='')]][["delta_y_var"]] = this_delta_y_var$V1
  }
  if (moments_stub == "moments_low_inc_vol_by_liquid_wealth_quantile" | moments_stub == "moments_high_inc_vol_by_liquid_wealth_quantile"){
    this_quantiles = as.vector(read.table(paste(txt_dir,"moments_by_liquid_wealth_quantile",'_quantiles.txt',sep=''), header = FALSE, sep = ",", dec = "."))
  }  else if (moments_stub == "moments_by_age"){
    this_quantiles = this_c_vector*0.0 #just a placeholder
  }  else{
    this_quantiles = as.vector(read.table(paste(txt_dir,moments_stub,'_quantiles.txt',sep=''), header = FALSE, sep = ",", dec = "."))
  }
  moments[["quantiles"]] = this_quantiles$V1
  if (moments_stub == "moments_by_URE_quantile" | moments_stub == "moments_by_NNP_quantile" | moments_stub == "moments_by_Income_quantile" | moments_stub == "moments_by_MeanCons_quantile"){
    this_quantile_means = as.vector(read.table(paste(txt_dir,moments_stub,'_quantile_means.txt',sep=''), header = FALSE, sep = ",", dec = "."))
    moments[["quantile_means"]] = this_quantile_means$V1
  }
  
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
moments_by_liquid_wealth_decile = MomentsForRDataFile('moments_by_liquid_wealth_decile', num_quantiles=10)
save(moments_by_liquid_wealth_decile, file=paste(moments_dir,'moments_by_liquid_wealth_quantile.RData',sep=''))
moments_by_net_wealth_decile = MomentsForRDataFile('moments_by_net_wealth_decile', num_quantiles=10)
save(moments_by_net_wealth_decile, file=paste(moments_dir,'moments_by_net_wealth_quantile.RData',sep=''))
moments_dir = "C:/Users/edmun/OneDrive/Documents/Research/Denmark/IncomeUncertaintyGit/Code/ServerRcode/ServerOutput/AEJ_revision/"

moments_by_age_28to55 = MomentsForRDataFile('moments_by_age', num_quantiles=28, quantile_stub="age", quantile_start=27 )
save(moments_by_age_28to55, file=paste(moments_dir,'moments_by_age_28to55.RData',sep=''))

moments_by_age_56to80 = MomentsForRDataFile('moments_by_age', num_quantiles=25, quantile_stub="age", quantile_start=55 )
save(moments_by_age_56to80, file=paste(moments_dir,'moments_by_age_56to80.RData',sep=''))

moments_low_inc_vol_by_liquid_wealth_quantile = MomentsForRDataFile('moments_low_inc_vol_by_liquid_wealth_quantile', num_quantiles=5)
save(moments_low_inc_vol_by_liquid_wealth_quantile, file=paste(moments_dir,'moments_low_inc_vol_by_liquid_wealth_quantile.RData',sep=''))

moments_high_inc_vol_by_liquid_wealth_quantile = MomentsForRDataFile('moments_high_inc_vol_by_liquid_wealth_quantile', num_quantiles=5)
save(moments_high_inc_vol_by_liquid_wealth_quantile, file=paste(moments_dir,'moments_high_inc_vol_by_liquid_wealth_quantile.RData',sep=''))
