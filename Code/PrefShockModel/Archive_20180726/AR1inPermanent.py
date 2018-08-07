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
n2_add = 2
phi = 0.8
psi = 0.8


rho_values = [1.0,0.98,0.96,0.94,0.92,0.9]
n1_values  = [1,2,3,4,5,6,7,8]

perm_var = np.zeros((len(rho_values),len(n1_values)))
tran_var = np.zeros((len(rho_values),len(n1_values)))
phi_estimate = np.zeros((len(rho_values),len(n1_values)))
psi_estimate = np.zeros((len(rho_values),len(n1_values)))

for i in range(len(rho_values)):
    rho = rho_values[i]
    perm_y = np.zeros_like(shocks)
    y = np.zeros_like(shocks)
    perm_y[0] = shocks[0]
    for k in range(periods-1):
        perm_y[k+1] = rho*perm_y[k] + shocks[k]
    y = perm_y +tran_shocks
    c = phi*perm_y + psi*tran_shocks
    
    for j in range(len(n1_values)):
        n1 = n1_values[j]
        n2 = n1+n2_add
        growth_n1 = y[n1:]-y[:-n1]
        cgrowth_n1 = c[n1:]-c[:-n1]
        var_n1 = np.var(growth_n1)
        covar_n1 = np.cov(growth_n1,cgrowth_n1)[0,1]
        growth_n2 = y[n2:]-y[:-n2]
        cgrowth_n2 = c[n2:]-c[:-n2]
        var_n2 = np.var(growth_n2)
        covar_n2 = np.cov(growth_n2,cgrowth_n2)[0,1]
        
        perm_var[i,j] = (var_n2-var_n1)/(n2-n1)
        tran_var[i,j] = (var_n1-n1*perm_var[i,j])/2.0
        phi_estimate[i,j] = ((covar_n2-covar_n1)/(n2-n1))/perm_var[i,j]
        psi_estimate[i,j] = (covar_n1-n1*phi_estimate[i,j]*perm_var[i,j])/(2.0*tran_var[i,j])

#### Save arrays to LaTex tables
def PrintAR1Table(estimate_array,rho_values,n1_values,filename,width=0.45,name=""):
    num_rho_values = len(rho_values)
    num_n1_values = len(n1_values)
    output = "\\begin{minipage}{" + str(width) + "\\textwidth}\n"
   
    output += "\\resizebox{\\textwidth}{!}{\\begin{tabular}{lc|*{" + str(num_n1_values) + "}{c}}  \n"
    output += "& " + name 
    for i in range(num_rho_values/2):
        output +=  "& "
    output += " $n_1$ \n"
    output += "\\\\ &  "
    for i in range(num_n1_values):
        output += " & " + str(n1_values[i]) 
    output += "\\\\ \\toprule  \n"
    for row in range(num_rho_values):
        if row==num_pref_vals/2-1:
            output += " $\\rho$ & " + str(rho_values[row]) + " & "
        else:
            output += " & " + str(rho_values[row]) + " & "
        for column in range(num_n1_values):
            if ~np.isnan(estimate_array[row,column]):
                output += "{:.2f}".format(estimate_array[row,column])
            if column!=num_n1_values-1:
                output += " & "
            else:
                output += " \n "
                output += "\\\\ "
    output += "\\\\ \\bottomrule  \n"
    output += "\end{tabular}}\n"
    output += "\end{minipage}\n"
    with open('./Tables/' + filename + '.tex','w') as f:
        f.write(output)
        f.close()
        
PrintAR1Table(phi_estimate,rho_values,n1_values,'phi_AR1',name="$\\phi$")
PrintAR1Table(psi_estimate,rho_values,n1_values,'psi_AR1',name="$\\psi$")
