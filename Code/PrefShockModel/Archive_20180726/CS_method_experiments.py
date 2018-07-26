# -*- coding: utf-8 -*-
"""
AR(1) test of Carroll-Samwick
"""
import numpy as np

num_shocks=100000

ignore_periods = 5
extra = 0
n1 = 3+extra
n2 = 5+extra

rho_inc = 0.0
rho_cons = 0.5


tran_shocks = np.random.normal(0,1,(num_shocks,1))
perm_shocks = np.random.normal(0,1,(num_shocks,1))
y = np.zeros((num_shocks,1))
t = np.zeros((num_shocks,1))
p = np.zeros((num_shocks,1))
t_inc = np.zeros((num_shocks,1))
y[0] = tran_shocks[0]
t[0] = tran_shocks[0]
p[0] = perm_shocks[0]
t_inc[0] = tran_shocks[0]
for i in range(num_shocks-1):
    t[i+1,:] = rho_cons*t[i,:] + tran_shocks[i+1,:]
    p[i+1,:] = p[i,:] + perm_shocks[i+1,:]
    t_inc[i+1,:] = rho_inc*t[i,:] + tran_shocks[i+1,:]
    

logLaborInc = p+t_inc
logCons = 0.5*t + p
IncGrowth1 = logLaborInc[ignore_periods+n1:,:]-logLaborInc[ignore_periods:-n1,:]
IncGrowth2 = logLaborInc[ignore_periods+n2:,:]-logLaborInc[ignore_periods:-n2,:]
ConGrowth1 = logCons[ignore_periods+n1:,:]-logCons[ignore_periods:-n1,:]
ConGrowth2 = logCons[ignore_periods+n2:,:]-logCons[ignore_periods:-n2,:]

varIncGrowth1 = np.var(IncGrowth1)
varIncGrowth2 = np.var(IncGrowth2)
impliedPermVar = (varIncGrowth2-varIncGrowth1)/(n2-n1)
impliedPermStd = impliedPermVar**0.5
impliedTranVar = (varIncGrowth1 - n1*impliedPermVar)/2.0
impliedTranStd = impliedTranVar**0.5

covGrowth1 = np.cov(IncGrowth1.flatten(),ConGrowth1.flatten())[1][0]
covGrowth2 = np.cov(IncGrowth2.flatten(),ConGrowth2.flatten())[1][0]

phi = (covGrowth2-covGrowth1)/((n2-n1)*impliedPermVar)
psi = (covGrowth1 - n1*phi*impliedPermVar)/(2.0*impliedTranVar)

print('Carroll-Samwick Results')
print('phi')
print(phi)
print('psi')
print(psi)
print('impliedPermStd')
print(impliedPermStd)
print('impliedTranStd')
print(impliedTranStd)

IncGrowthOnePeriod = logLaborInc[ignore_periods+1:,:]-logLaborInc[ignore_periods:-1,:]
ConGrowthOnePeriod = logCons[ignore_periods+1:,:]-logCons[ignore_periods:-1,:]
covYYnext = np.cov(IncGrowthOnePeriod[2:,].flatten(),IncGrowthOnePeriod[1:-1,].flatten())[1][0]
covCYnext = np.cov(IncGrowthOnePeriod[2:,].flatten(),ConGrowthOnePeriod[1:-1,].flatten())[1][0]

IncGrowthThreePeriod = logLaborInc[ignore_periods+4:,:]-logLaborInc[ignore_periods:-4,:]
covYYnext3 = np.cov(IncGrowthThreePeriod.flatten(),IncGrowthOnePeriod[2:-1,].flatten())[1][0]
covCYnext3 = np.cov(IncGrowthThreePeriod.flatten(),ConGrowthOnePeriod[2:-1,].flatten())[1][0]

psiBPP = covCYnext/covYYnext
phiBPP = covCYnext3/covYYnext3

impliedPermStdBPP = covYYnext3**0.5
impliedTranStdBPP = (-covYYnext)**0.5

print('BPP Results')
print('phi')
print(phiBPP)
print('psi')
print(psiBPP)
print('impliedPermStd')
print(impliedPermStdBPP)
print('impliedTranStd')
print(impliedTranStdBPP)