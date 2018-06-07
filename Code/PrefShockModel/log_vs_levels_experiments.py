



import numpy as np
import pylab as plt                 # Python's plotting library
import pandas as pd

#####################################################################
periods = 1000000
shocks = np.random.normal(0,1,periods)
error_shocks = np.random.normal(0,1,periods)
cons_error_shocks = np.random.normal(0,1,periods)

inc_std = 0.1
error_std = 0.0
beta = 0.2
cons_error_std = 0.6

income_true = np.exp(inc_std*shocks-0.5*inc_std**2)
cons_true = income_true**beta
income = income_true + error_std*error_shocks
consumption = cons_true + error_std*error_shocks + cons_error_std*cons_error_shocks

positive = np.logical_and(income>0,consumption>0)
income_pos = income[positive]
consumption_pos = consumption[positive]

delta_y = income_pos[1:]-income_pos[:-1]
delta_c = consumption_pos[1:]-consumption_pos[:-1]

log_inc_pos = np.log(income_pos)
log_con_pos = np.log(consumption_pos)

delta_ly = log_inc_pos[1:]-log_inc_pos[:-1]
delta_lc = log_con_pos[1:]-log_con_pos[:-1]



beta_levels = np.cov(delta_y,delta_c)[1,0]/np.var(delta_y)
beta_logs = np.cov(delta_ly,delta_lc)[1,0]/np.var(delta_ly)

print(beta_levels)
print(beta_logs)
print((consumption_pos.size*1.0)/(1.0*consumption.size))

#####################################################################

periods = 11
agents = 100000
perm_var = 0.005
tran_var = 0.005
perm_shocks = perm_var**0.5*np.random.normal(0,1,(periods,agents))
tran_shocks = tran_var**0.5*np.random.normal(0,1,(periods,agents))

#uniform = np.random.uniform(0,1,(periods,agents))
#low = 0.5
#percentage = 0.98
#tran_shocks[uniform<percentage] = low*(1.0-percentage)
#tran_shocks[uniform>=percentage] = -low
#
#percentage2 = 1.0
#low2 = 1
#tran_shocks[uniform>=percentage2] = -low2


perm_income = np.cumsum(perm_shocks,axis=0)
rho = 1.0
for i in range(periods-1):
    perm_income[i+1,:] = rho*perm_income[i,:] + perm_shocks[i+1,:]


income = perm_income + tran_shocks
delta_y = income[1:,:]-income[:-1,:]
delta3_y = income[3:,:]-income[:-3,:]
delta5_y = income[5:,:]-income[:-5,:]
var_delta3_y = np.var(delta3_y)
var_delta5_y = np.var(delta5_y)

perm_var = (var_delta5_y-var_delta3_y)/2.0
tran_var = (var_delta3_y - 3.0/2.0*(var_delta5_y-var_delta3_y))/2.0

exp_income = np.exp(income)/np.mean(np.exp(income),0)
delta_expy = exp_income[1:,:]-exp_income[:-1,:]
delta3_expy = exp_income[3:,:]-exp_income[:-3,:]
delta5_expy = exp_income[5:,:]-exp_income[:-5,:]
var_delta3_expy = np.var(delta3_expy)
var_delta5_expy = np.var(delta5_expy)

perm_exp_var = (var_delta5_expy-var_delta3_expy)/2.0
tran_exp_var = (var_delta3_expy - 3.0/2.0*(var_delta5_expy-var_delta3_expy))/2.0

print(perm_var)
print(perm_exp_var)

print(tran_var)
print(tran_exp_var)


#cons = perm_income + 0.4*tran_shocks +0.6*tran_shocks*(tran_shocks<-low)
#exp_cons = np.exp(cons)/np.mean(np.exp(income),0)

#exp_cons = np.exp(perm_income) + 0.4*(np.exp(income)-np.exp(perm_income))
#cons = np.log(exp_cons)

cons = perm_income - 1.0/2.0*0.4*(tran_shocks-1) **2
exp_cons = np.exp(cons)/np.mean(np.exp(income),0)

delta_c = cons[1:,:]-cons[:-1,:]
delta3_c = cons[3:,:]-cons[:-3,:]
delta5_c = cons[5:,:]-cons[:-5,:]
cov_delta3_cy = np.cov(delta3_y.flatten(),delta3_c.flatten())[0,1]
cov_delta5_cy = np.cov(delta5_y.flatten(),delta5_c.flatten())[0,1]

phi = (cov_delta5_cy-cov_delta3_cy)/(2.0*perm_var)
psi = (cov_delta3_cy-3.0*phi*perm_var)/(2.0*tran_var)


delta_exc = exp_cons[1:,:]-exp_cons[:-1,:]
delta3_expc = exp_cons[3:,:]-exp_cons[:-3,:]
delta5_expc = exp_cons[5:,:]-exp_cons[:-5,:]
var_delta3_expc = np.var(delta3_expc)
var_delta5_expc = np.var(delta5_expc)

cov_delta3_expcy = np.cov(delta3_expy.flatten(),delta3_expc.flatten())[0,1]
cov_delta5_expcy = np.cov(delta5_expy.flatten(),delta5_expc.flatten())[0,1]

exp_phi = (cov_delta5_expcy-cov_delta3_expcy)/(2.0*perm_exp_var)
exp_psi = (cov_delta3_expcy-3.0*exp_phi*perm_exp_var)/(2.0*tran_exp_var)

print(phi)
print(exp_phi)
print(psi)
print(exp_psi)

