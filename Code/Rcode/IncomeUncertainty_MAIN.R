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
source(paste(Rcode_folder,"min_distance_CS.r",sep=""))
###############################################################################


###############################################################################
# load weath quintile data and create graph
load(paste(moments_dir,'moments_by_liquid_wealth_quantile','.RData',sep=''))


###############################################################################
