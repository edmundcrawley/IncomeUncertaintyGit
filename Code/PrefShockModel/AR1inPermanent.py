# -*- coding: utf-8 -*-
"""
Created on Mon Apr 30 11:34:22 2018

@author: edmun
"""

import numpy as np
import pylab as plt                 # Python's plotting library
import pandas as pd

periods = 100000
shocks = np.random.normal(0,1,periods)
tran_shocks = (1.0**0.5)*np.random.normal(0,1,periods)
rho = 0.95
perm_y = np.zeros_like(shocks)
y = np.zeros_like(shocks)
perm_y[0] = shocks[0]
for i in range(periods-1):
    perm_y[i+1] = rho*perm_y[i] + shocks[i]
y = perm_y +tran_shocks
    
c = 0.7*perm_y + 0.5*tran_shocks

    
num_var = 10
var = np.zeros(num_var)
covar = np.zeros(num_var)
for n in range(num_var):
    growth_n = y[n+1:]-y[:-(n+1)]
    cgrowth_n = c[n+1:]-c[:-(n+1)]
    var[n] = np.var(growth_n)
    covar[n] = np.cov(growth_n,cgrowth_n)[0,1]
    
   
imputed_var = np.zeros(num_var)
imputed_covar = np.zeros(num_var)
n1 = 3
n2=5
perm_var = (var[n2-1]-var[n1-1])/(n2-n1)
tran_var = (var[n1-1]-n1*perm_var)/2
phi = ((covar[n2-1]-covar[n1-1])/(n2-n1))/perm_var
psi = (covar[n1-1]-n1*phi*perm_var)/(2*tran_var)

for n in range(num_var):
    imputed_var[n] = (n+1)*perm_var +2*tran_var
    imputed_covar[n] = (n+1)*phi*perm_var +2*psi*tran_var


x_plot = range(1,num_var+1)
plt.plot(x_plot,var)
plt.plot(x_plot,imputed_var)
plt.plot(x_plot,covar)
plt.plot(x_plot,imputed_covar)

perm_var = np.zeros((10,10))
tran_var = np.zeros((10,10))
phi = np.zeros((10,10))
psi = np.zeros((10,10))

for n1 in range(1,9):
    for n2 in range(n1+2,11):
        perm_var[n1-1,n2-1] = (var[n2-1]-var[n1-1])/(n2-n1)
        tran_var[n1-1,n2-1] = (var[n1-1]-n1*perm_var[n1-1,n2-1])/2
        phi[n1-1,n2-1] = ((covar[n2-1]-covar[n1-1])/(n2-n1))/perm_var[n1-1,n2-1]
        psi[n1-1,n2-1] = (covar[n1-1]-n1*phi[n1-1,n2-1]*perm_var[n1-1,n2-1])/(2*tran_var[n1-1,n2-1])
        
    
    
