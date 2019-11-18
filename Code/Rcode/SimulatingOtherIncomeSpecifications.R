#####################################
# What happens if permanent shocks actually decay at some rate?
# This code is just for play - it simulates data with transitory, AR(1) and permanent shocks
# Then it simulates consumption path (assuming response to AR(1) is same as rw)
# Then run our estimation proceedure to see how much the misspecification matters
#####################################


sample_size = 100000
phi = 0.8
psi = 0.05
rho = 0.95

ar_shock   = 1.0^0.5*rnorm(sample_size)
tran_shock = 1.0^0.5*rnorm(sample_size)
rw_shock   = 0.0^0.5*rnorm(sample_size)

ar_income = cumsum(ar_shock)
for (i in 2:sample_size){
  ar_income[i] = rho*ar_income[i-1] + ar_shock[i]
}
rw_income = cumsum(rw_shock)
perm_income = rw_income + ar_income
income = perm_income + tran_shock
consumption = phi*perm_income + psi*tran_shock



income_2 = income[4:sample_size]-income[2:(sample_size-2)]
income_3 = income[4:sample_size]-income[1:(sample_size-3)]
cons_2   = consumption[4:sample_size]-consumption[2:(sample_size-2)]
cons_3   = consumption[4:sample_size]-consumption[1:(sample_size-3)]

sigma_perm = mean(income_3^2 - income_2^2)
sigma_tran = 0.5*mean(income_2^2) - sigma_perm

phi_estimate = mean(income_3*cons_3 - income_2*cons_2)/sigma_perm
psi_estimate = 0.5*mean(income_2*cons_2 - 2.0*phi_estimate*sigma_perm)/sigma_tran