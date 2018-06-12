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
var_perm = c()
var_tran = c()
ins_perm = c()
ins_tran = c()
for (tag in c("","_lincome","_level_lincome")) {
  load(paste(moments_dir,'moments_all',tag,'.RData',sep=''))
  c_vector_all = moments_all$c_vector
  omega_all    = moments_all$omega
  T = moments_all$T
  CS_output_all = CS_parameter_estimation(c_vector_all, omega_all, T) 
  var_perm = c(var_perm,CS_output_all$var_perm)
  var_tran = c(var_tran,CS_output_all$var_tran)
  ins_perm = c(ins_perm,CS_output_all$ins_perm)
  ins_tran = c(ins_tran,CS_output_all$ins_tran)
}
print(var_perm)
print(var_tran)
print(ins_perm)
print(ins_tran)
