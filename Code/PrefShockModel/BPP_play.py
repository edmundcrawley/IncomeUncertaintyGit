# -*- coding: utf-8 -*-
"""
Created on Tue Mar  2 13:10:25 2021

@author: edmun
"""
from scipy.optimize import minimize
import numpy as np

def cov_vec_true(var_perm, var_tran, phi, psi):
    ytyt1 =  1.0/6.0*var_perm -   var_tran
    ytyt  =  2.0/3.0*var_perm + 2*var_tran
    ytytm1=  1.0/6.0*var_perm -   var_tran  
    
    ytct1 =  1.0/6.0*var_perm*phi -   psi*var_tran
    ytct  =  2.0/3.0*var_perm*phi + 2*psi*var_tran
    ytctm1=  1.0/6.0*var_perm*phi -   psi*var_tran 
    return np.array([ytyt1, ytyt, ytytm1,ytct1,ytct,ytctm1])

def cov_vec_BPP(var_perm, var_tran, phi, psi):
    ytyt1 =   -   var_tran
    ytyt  =  var_perm + 2*var_tran
    ytytm1=   -   var_tran 
    
    ytct1 =  0
    ytct  =  var_perm*phi + psi*var_tran
    ytctm1=  -psi*var_tran
    return np.array([ytyt1, ytyt, ytytm1,ytct1,ytct,ytctm1])
    
weight = np.ones(6)
weight[4]=0.5
weight[1]=0.5

def objective(params, true_params, weight):
    cov_true = cov_vec_true(true_params[0], true_params[1], true_params[2], true_params[3])
    cov_BPP = cov_vec_BPP(params[0], params[1], params[2], params[3])
    diff = cov_true - cov_BPP
    penalty = np.sum(diff**2*weight)
    return penalty

true_params = np.array([1.0,1.0,0.8,0.8])

sol  = minimize(objective ,true_params,args=(true_params,weight))



# def objective2(phipsi, true_params, weight):
#     cov_true = cov_vec_true(true_params[0], true_params[1], true_params[2], true_params[3])
#     cov_BPP = cov_vec_BPP(true_params[0], true_params[1], phipsi[0], phipsi[1])
#     diff = cov_true - cov_BPP
#     penalty = np.sum(diff**2*weight)
#     return penalty

# phipsi = true_params[2:]
# var = true_params[0:2]
#sol2 = minimize(objective2,phipsi     ,args=(true_params,weight))

