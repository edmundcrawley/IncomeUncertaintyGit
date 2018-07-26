# -*- coding: utf-8 -*-
"""
Created on Mon Apr 30 11:34:22 2018

@author: edmun
"""

import numpy as np
import pylab as plt                 # Python's plotting library
import pandas as pd

periods = 100000
subperiods = 100
totalperiods = periods*subperiods
shocks = np.random.normal(0,1.0/(subperiods**0.5),totalperiods)
tran_shocks = (subperiods**0.5)*np.random.normal(0,1,totalperiods)

phi = 0.8
psi = 0.0
theta = 1.0

perm_y = np.cumsum(shocks)
y = perm_y + tran_shocks
c = phi*perm_y + psi*tran_shocks

for t in range(totalperiods-2*subperiods):
#    c[t+2*subperiods] = c[t+2*subperiods] + theta*shocks[t+subperiods+0]*subperiods
    for i in range(subperiods):
        c[t+2*subperiods] = c[t+2*subperiods] + 2*theta*shocks[t+2*subperiods-i]*(subperiods-i)/(1.0*subperiods)

# time aggregate
y_agg = np.zeros(periods)
c_agg = np.zeros(periods)
for t in range(periods):
    y_agg[t] = np.mean(y[t*subperiods:(t+1)*subperiods])
    c_agg[t] = np.mean(c[t*subperiods:(t+1)*subperiods])

delta_y = y_agg[1:]-y_agg[:-1]
delta_c = c_agg[1:]-c_agg[:-1]

var3y = np.var(delta_y[4:]+delta_y[3:-1]+delta_y[2:-2])
var5y = np.var(delta_y[4:]+delta_y[3:-1]+delta_y[2:-2]+delta_y[1:-3]+delta_y[:-4])

cov3cy = np.cov(delta_y[4:]+delta_y[3:-1]+delta_y[2:-2],delta_c[4:]+delta_c[3:-1]+delta_c[2:-2])[1,0]
cov5cy = np.cov(delta_y[4:]+delta_y[3:-1]+delta_y[2:-2]+delta_y[1:-3]+delta_y[:-4],delta_c[4:]+delta_c[3:-1]+delta_c[2:-2]+delta_c[1:-3]+delta_c[:-4])[1,0]

perm_var = (var5y-var3y)/2
tran_var = (var3y - (3.0-1.0/3.0)*perm_var)/2.0 

phi = (cov5cy-cov3cy)/(2.0*perm_var)
psi = (cov3cy-(3.0-1.0/3.0)*phi*perm_var) / (2.0*tran_var) 

print(psi)
print(phi)

