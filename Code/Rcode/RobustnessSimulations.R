#################################################################################
# 
# This file does simulations for the robustness
# section of the paper
#
# 
###############################################################################

ServerRcode_folder = "C:/Users/edmun/OneDrive/Documents/Research/Denmark/IncomeUncertaintyGit/Code/ServerRcode/"
Rcode_folder = "C:/Users/edmun/OneDrive/Documents/Research/Denmark/IncomeUncertaintyGit/Code/Rcode/"
tables_dir = "C:/Users/edmun/OneDrive/Documents/Research/Denmark/IncomeUncertaintyGit/Code/Rcode/Tables/"
figures_dir = "C:/Users/edmun/OneDrive/Documents/Research/Denmark/IncomeUncertaintyGit/Code/Rcode/Figures/"
library(zoo)
source(paste(ServerRcode_folder,"BPPLikeCarrollSamwick.r",sep=""))
source(paste(Rcode_folder,"min_distance_CS.r",sep=""))

colors = c("#fc8d59","#91bfdb","#ffffbf")

# First simulate AR(1) and the process in Fagereng, Holm and Natvik (2018)
num_subperiods = 20
years=13
ignore_periods = num_subperiods*5
num_agents = 1000
num_sims = 50
n_rhos = 40

phi = 1.0

psi_estimates = matrix(0,n_rhos)
phi_estimates = matrix(0,n_rhos)
psi_se_estimates = matrix(0,n_rhos)
phi_se_estimates = matrix(0,n_rhos)
true_MPC_estimates = matrix(0,n_rhos)
rho_values = seq(0.0, 1.0, length.out = n_rhos)**0.1
for (j in (1:n_rhos)){
  rho = rho_values[j]
  true_MPC_estimates[j] = mean(1-rho**(1:num_subperiods))
}


for (j in (1:n_rhos)){
  rho = rho_values[j]
  for (k in 1:num_sims){
    perm_shocks = t(replicate(num_subperiods*(years+ignore_periods), rnorm(num_agents)) )/num_subperiods**1.5
    tran_shocks = t(replicate(num_subperiods*(years+ignore_periods), rnorm(num_agents)) )/num_subperiods**0.5
    
    tran_y = matrix(0,num_subperiods*(years+ignore_periods),num_agents)
    perm_y = matrix(0,num_subperiods*(years+ignore_periods),num_agents)
    tran_c = matrix(0,num_subperiods*(years+ignore_periods),num_agents)
    perm_c = matrix(0,num_subperiods*(years+ignore_periods),num_agents)
    for (i in 1:(num_subperiods*(years+ignore_periods)-1)){
      tran_y[i+1,] = tran_shocks[i+1,]
      perm_y[i+1,] = perm_y[i,] + perm_shocks[i+1,]
      tran_c[i+1,] = rho*tran_c[i,] + (1-rho)*tran_shocks[i+1,]
      perm_c[i+1,] = perm_c[i,] + phi*perm_shocks[i+1,]
    }
    y = tran_y + perm_y
    c = tran_c + perm_c
    y_annual = matrix(0,years,num_agents)
    c_annual = matrix(0,years,num_agents)
    for (year in (1:years)){
      y_annual[year,] = colSums(y[(num_subperiods*(year+ignore_periods-1)):(num_subperiods*(year+ignore_periods)),])
      c_annual[year,] = colSums(c[(num_subperiods*(year+ignore_periods-1)):(num_subperiods*(year+ignore_periods)),])
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
    moments_all <- create_moments_CS(all_data)
    c_vector = moments_all$c_vector
    omega = moments_all$omega
    T = moments_all$T
    CS_output = CS_parameter_estimation(c_vector, omega,T) 
    true_MPC = mean(1-rho**(1:num_subperiods))
    psi_estimates[j] = psi_estimates[j]+CS_output$ins_tran/num_sims
    phi_estimates[j] = phi_estimates[j]+CS_output$ins_perm/num_sims
    psi_se_estimates[j] = psi_se_estimates[j]+CS_output$ins_tran_se/num_sims
    phi_se_estimates[j] = phi_se_estimates[j]+CS_output$ins_perm_se/num_sims
  }
}
write.csv(psi_estimates, file = paste(tables_dir,"psi_estimates_ar1.csv",sep=""))
write.csv(phi_estimates, file = paste(tables_dir,"phi_estimates_ar1.csv",sep=""))
write.csv(true_MPC_estimates, file = paste(tables_dir,"true_MPC_estimates_ar1.csv",sep=""))

########################################################################
# Now simulate the estimate of decay from Fagereng et al.
########################################################################

n_thetas = n_rhos
theta_values = seq(0.0,1.19,length.out = n_thetas)
theta2 = 0.2142

psi_estimates = matrix(0,n_rhos)
phi_estimates = matrix(0,n_rhos)
psi_se_estimates = matrix(0,n_rhos)
phi_se_estimates = matrix(0,n_rhos)
true_MPC_estimates = matrix(0,n_rhos)

for (j in (1:n_rhos)){
  theta1 = theta_values[j]
  for (lag in (1:(num_subperiods))){
    true_MPC_estimates[j] = true_MPC_estimates[j] + theta1*((lag/num_subperiods)**(theta2)  )/num_subperiods
  }
}


for (j in (1:n_thetas)){
  theta1 = theta_values[j]
  for (k in 1:num_sims){
    perm_shocks = t(replicate(num_subperiods*(years+ignore_periods), rnorm(num_agents)) )/num_subperiods**1.5
    tran_shocks = t(replicate(num_subperiods*(years+ignore_periods), rnorm(num_agents)) )/num_subperiods**0.5
    
    tran_y = matrix(0,num_subperiods*(years+ignore_periods),num_agents)
    perm_y = matrix(0,num_subperiods*(years+ignore_periods),num_agents)
    tran_c = matrix(0,num_subperiods*(years+ignore_periods),num_agents)
    perm_c = matrix(0,num_subperiods*(years+ignore_periods),num_agents)
    for (i in (num_subperiods*(ignore_periods)-1):(num_subperiods*(years+ignore_periods)-1)){
      tran_y[i+1,] = tran_shocks[i+1,]
      perm_y[i+1,] = perm_y[i,] + perm_shocks[i+1,]
      perm_c[i+1,] = perm_c[i,] + phi*perm_shocks[i+1,]
      for (lag in (0:(num_subperiods*ignore_periods-1))){
        tran_c[i+1,] = tran_c[i+1,] + theta1*tran_shocks[i+1-lag,]*(((lag+1.0)/num_subperiods)**(theta2) - (lag/num_subperiods)**(theta2) )
      }
    }
    y = tran_y + perm_y
    c = tran_c + perm_c
    y_annual = matrix(0,years,num_agents)
    c_annual = matrix(0,years,num_agents)
    for (year in (1:years)){
      y_annual[year,] = colSums(y[(num_subperiods*(year+ignore_periods-1)):(num_subperiods*(year+ignore_periods)),])
      c_annual[year,] = colSums(c[(num_subperiods*(year+ignore_periods-1)):(num_subperiods*(year+ignore_periods)),])
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
    moments_all <- create_moments_CS(all_data)
    c_vector = moments_all$c_vector
    omega = moments_all$omega
    T = moments_all$T
    CS_output = CS_parameter_estimation(c_vector, omega,T) 
    psi_estimates[j] = psi_estimates[j]+CS_output$ins_tran/num_sims
    phi_estimates[j] = phi_estimates[j]+CS_output$ins_perm/num_sims
    psi_se_estimates[j] = psi_se_estimates[j]+CS_output$ins_tran_se/num_sims
    phi_se_estimates[j] = phi_se_estimates[j]+CS_output$ins_perm_se/num_sims
  }
}
write.csv(psi_estimates, file = paste(tables_dir,"psi_estimates_fagereng.csv",sep=""))
write.csv(phi_estimates, file = paste(tables_dir,"phi_estimates_fagereng.csv",sep=""))
write.csv(true_MPC_estimates, file = paste(tables_dir,"true_MPC_estimates_fagereng.csv",sep=""))

psi_estimates_ar1 = read.csv(paste(tables_dir,"psi_estimates_ar1.csv",sep=""), header = TRUE)[,2]
phi_estimates_ar1 = read.csv(paste(tables_dir,"phi_estimates_ar1.csv",sep=""), header = TRUE)[,2]
true_MPC_estimates_ar1 = read.csv(paste(tables_dir,"true_MPC_estimates_ar1.csv",sep=""), header = TRUE)[,2]

psi_estimates_fagereng = read.csv(paste(tables_dir,"psi_estimates_fagereng.csv",sep=""), header = TRUE)[,2]
phi_estimates_fagereng = read.csv(paste(tables_dir,"phi_estimates_fagereng.csv",sep=""), header = TRUE)[,2]
true_MPC_estimates_fagereng = read.csv(paste(tables_dir,"true_MPC_estimates_fagereng.csv",sep=""), header = TRUE)[,2]


dev.new()
plot(true_MPC_estimates_ar1,psi_estimates_ar1,col=colors[1],xlab="True MPC",ylab="Estimated MPC",type="l",lwd=4, 
     main = "Bias Due to Persistent Consumption")
lines(c(0,1),c(0,1),lwd=4)
lines(true_MPC_estimates_fagereng,psi_estimates_fagereng,col=colors[2],lwd=4)
legend(0.1,0.9,legend=c("Exponential Decay","Fagereng et al. Decay"), col=colors,bty="n",lwd=4)
dev.copy(pdf, paste(figures_dir, "DecayBias.pdf",sep=""))
dev.off()

