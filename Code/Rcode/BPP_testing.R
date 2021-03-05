# Set folders
base_dir = "C:/Users/edmun/OneDrive/Documents/Research/Denmark/IncomeUncertaintyGit/"
Rcode_folder = paste(base_dir,"Code/Rcode/",sep='')
ServerRcode_folder = paste(base_dir,"Code/ServerRcode/",sep='')

library(zoo)
source(paste(ServerRcode_folder,"BPPLikeCarrollSamwick.r",sep=""))
source(paste(Rcode_folder,"min_distance_CS.r",sep=""))
source(paste(ServerRcode_folder,"BPP_original.r",sep=""))
source(paste(Rcode_folder,"min_distance_BPP.r",sep=""))


colors = c("#fc8d59","#91bfdb","#ffffbf")

num_subperiods = 100
years=7
T=years
ignore_periods = 5
num_agents = 10000

rho_annual = 0.0
rho =rho_annual**(1/num_subperiods)
perm_var = 1.0
tran_var = 1.0 
error_var = 0.0

phi = 0.8
psi = 0.8
phi_d = 0.5
set.seed(7)
perm_shocks = t(perm_var**0.5*replicate(num_subperiods*(years+ignore_periods), rnorm(num_agents)) )/num_subperiods**1.5
tran_shocks = t(tran_var**0.5*replicate(num_subperiods*(years+ignore_periods), rnorm(num_agents)) )/num_subperiods**0.5
error_shocks = t(error_var**0.5*replicate(years+1, rnorm(num_agents)) )

tran_y = matrix(0,num_subperiods*(years+ignore_periods),num_agents)
perm_y = matrix(0,num_subperiods*(years+ignore_periods),num_agents)
tran_c = matrix(0,num_subperiods*(years+ignore_periods),num_agents)
perm_c = matrix(0,num_subperiods*(years+ignore_periods),num_agents)
one_year_integral = sum(rho**(0:(num_subperiods-1)))
for (i in 1:(num_subperiods*(years+ignore_periods)-1)){
  tran_y[i+1,] = tran_shocks[i+1,]
  perm_y[i+1,] = perm_y[i,] + perm_shocks[i+1,]
  tran_c[i+1,] = rho*tran_c[i,] + psi*tran_shocks[i+1,]/one_year_integral
  perm_c[i+1,] = perm_c[i,] + phi*perm_shocks[i+1,]
}
y = tran_y + perm_y 
c = tran_c + perm_c + phi_d*perm_shocks*num_subperiods**1
y_annual = matrix(0,years,num_agents)
c_annual = matrix(0,years,num_agents)
for (year in (1:years)){
  if (num_subperiods==1){
    y_annual[year,] = (y[(num_subperiods*(year+ignore_periods-1)+1):(num_subperiods*(year+ignore_periods)),])
    c_annual[year,] = (c[(num_subperiods*(year+ignore_periods-1)+1):(num_subperiods*(year+ignore_periods)),])
  }else{
    y_annual[year,] = colSums(y[(num_subperiods*(year+ignore_periods-1)+1):(num_subperiods*(year+ignore_periods)),])
    c_annual[year,] = colSums(c[(num_subperiods*(year+ignore_periods-1)+1):(num_subperiods*(year+ignore_periods)),])
  }
  c_annual[year,] = c_annual[year,] + error_shocks[year+1,]#-error_shocks[year,]
}

delta_y = (y_annual[2:nrow(y_annual),]-y_annual[1:(nrow(y_annual)-1),])
delta_c = (c_annual[2:nrow(c_annual),]-c_annual[1:(nrow(c_annual)-1),])
all_data = matrix(0,nrow(delta_y)*ncol(delta_y), 6)
for (i in (1:ncol(delta_y))){
  all_data[((i-1)*nrow(delta_y)+1):(i*nrow(delta_y)),1] = i   # id column
  all_data[((i-1)*nrow(delta_y)+1):(i*nrow(delta_y)),2] = 1:nrow(delta_y)   # year column
  all_data[((i-1)*nrow(delta_y)+1):(i*nrow(delta_y)),3] = delta_y[,i]   # id column
  all_data[((i-1)*nrow(delta_y)+1):(i*nrow(delta_y)),4] = 1   # id column
  all_data[((i-1)*nrow(delta_y)+1):(i*nrow(delta_y)),5] = delta_c[,i]   # id column
  all_data[((i-1)*nrow(delta_y)+1):(i*nrow(delta_y)),6] = 1   # id column
}


BPP_moments_all <- create_moments_BPP(all_data)
c_vector_BPP = BPP_moments_all$c_vector
omega_BPP = BPP_moments_all$omega
BPP_output = BPP_parameter_estimation(c_vector_BPP, omega_BPP,T-1, ma=0, taste=1)
round(c_vector_BPP,1)

moments_all <- create_moments_CS(all_data)
c_vector = moments_all$c_vector
omega = moments_all$omega
#T = moments_all$T
CS_output = CS_parameter_estimation(c_vector, omega,T-1) 

compare = matrix(0,4,2)
compare[1,2] = BPP_output$ins_perm
compare[2,2] = BPP_output$ins_tran
compare[3,2] = mean(BPP_output$var_perm)
compare[4,2] = mean(BPP_output$var_tran)


compare[1,1] = CS_output$ins_perm
compare[2,1] = CS_output$ins_tran
compare[3,1] = mean(CS_output$var_perm)
compare[4,1] = mean(CS_output$var_tran)



compare

