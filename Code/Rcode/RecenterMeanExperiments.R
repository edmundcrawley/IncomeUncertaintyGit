

periods = 12
households = 100000
MPC = 0.5

perm_shocks = matrix( rnorm(periods*households,mean=0,sd=1), periods, households) 
tran_shocks = matrix( rnorm(periods*households,mean=0,sd=1), periods, households) 

perm_income = matrix(0.0,periods,households)
perm_income[1,] = perm_shocks[1,] 

income = matrix(0.0,periods,households)
income[1,] = perm_income[1,] + tran_shocks[1,]

cons = matrix(0.0,periods,households)
cons[1,] = perm_income[1,] + MPC*tran_shocks[1,]
for (t in 2:periods) {
  perm_income[t,] = perm_income[t-1,] + perm_shocks[t,] 
  income[t,] = perm_income[t,] + tran_shocks[t,]
  cons[t,] = perm_income[t,] + MPC*tran_shocks[t,]
}
delta_y = income[2:periods,]-income[1:(periods-1),]
delta_c = cons[2:periods,]-cons[1:(periods-1),]

# select on having a high income at the end
high_income = income[periods,]>= 3
income = income[,high_income]
cons = cons[,high_income]

N1=3
N2=5
delta_y1 = income[(1+N1):periods,]-income[1:(periods-N1),]
delta_y2 = income[(1+N2):periods,]-income[1:(periods-N2),]
delta_c1 = cons[(1+N1):periods,]-cons[1:(periods-N1),]
delta_c2 = cons[(1+N2):periods,]-cons[1:(periods-N2),]

remean=TRUE
if (remean==FALSE) {
  y1var = mean(delta_y1^2)
  y2var = mean(delta_y2^2)
  cy1covar = mean(delta_y1*delta_c1)
  cy2covar = mean(delta_y2*delta_c2)
} else {
  y1var = mean(delta_y1^2) - mean(delta_y1)^2
  y2var = mean(delta_y2^2) - mean(delta_y2)^2
  cy1covar = mean(delta_y1*delta_c1) - mean(delta_y1)*mean(delta_c1)
  cy2covar = mean(delta_y2*delta_c2) - mean(delta_y2)*mean(delta_c2)
}
  
perm_var = (y2var-y1var)/(N2-N1)
tran_var = (y1var-N1*perm_var)/2

perm_MPC = (cy2covar- cy1covar)/((N2-N1)*perm_var)
tran_MPC = (cy1covar - N1*perm_MPC*perm_var)/(2*tran_var)
  
  
  