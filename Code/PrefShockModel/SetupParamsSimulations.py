'''
Loads parameters used in the cstwMPC estimations.
'''
import numpy as np
import csv
from copy import  deepcopy
import os

# Choose percentiles of the data to match and which estimation to run
spec_name = 'BetaDistPY'
dist_type = 'uniform'         # Which type of distribution to use
do_param_dist = False         # Do param-dist version if True, param-point if False
param_name = 'DiscFac'
run_estimation = True         # Runs the estimation if True
find_beta_vs_KY = False       # Computes K/Y ratio for a wide range of beta; should have do_beta_dist = False
do_sensitivity = [False, False, False, False, False, False, False, False] # Choose which sensitivity analyses to run: rho, xi_sigma, psi_sigma, mu, urate, mortality, g, R
do_agg_shocks = False         # Solve the FBS aggregate shocks version of the model
percentiles_to_match = [0.2, 0.4, 0.6, 0.8]    # Which points of the Lorenz curve to match in beta-dist (must be in (0,1))
#percentiles_to_match = [0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9] # Can use this line if you want to match more percentiles
if do_param_dist:
    pref_type_count = 7       # Number of discrete beta types in beta-dist
else:
    pref_type_count = 1       # Just one beta type in beta-point

# Set basic parameters for the lifecycle micro model
Rfree = 1.04**(0.25)          # Quarterly interest factor
CRRA = 2.0                    # Coefficient of relative risk aversion   
DiscFac_guess = 0.99          # Initial starting point for discount factor
UnempPrb = 0.00000001               # Probability of unemployment while working
IncUnemp = 0.0001               # Unemployment benefit replacement rate
BoroCnstArt = 0.0             # Artificial borrowing constraint

# Set grid sizes
PermShkCount = 5              # Number of points in permanent income shock grid
TranShkCount = 5              # Number of points in transitory income shock grid
aXtraMin = 0.00001            # Minimum end-of-period assets in grid
aXtraMax = 20                 # Maximum end-of-period assets in grid
aXtraCount = 20               # Number of points in assets grid
aXtraNestFac = 3              # Number of times to 'exponentially nest' when constructing assets grid
CubicBool = False             # Whether to use cubic spline interpolation
vFuncBool = False             # Whether to calculate the value function during solution

# Set simulation parameters
if do_param_dist:
        Population = 14000
else:
        Population = 10000    # Total number of simulated agents in the population
T_sim_PY = 1200               # Number of periods to simulate (idiosyncratic shocks model, perpetual youth)
ignore_periods_PY = 400       # Number of periods to throw out when looking at history (perpetual youth)
pLvlInitStd = 0.4             # Standard deviation of initial permanent income
aNrmInitMean = np.log(0.5)    # log initial wealth/income mean
aNrmInitStd  = 0.5            # log initial wealth/income standard deviation

# Set population macro parameters
PopGroFac = 1.0     # Population growth rate
PermGroFacAgg = 1.0 # TFP growth rate


# Set indiividual parameters for the infinite horizon model
IndL = 1.0               # Labor supply per individual (constant)
PermGroFac_i = [1.000**0.25]  # Permanent income growth factor (no perm growth)
DiscFac_i = 0.97              # Default intertemporal discount factor
LivPrb_i = [1.0 - 1.0/160.0]  # Survival probability
PermShkStd_i = [0.06/2.0] # Standard deviation of permanent shocks to income
TranShkStd_i = [0.06*2.0]    # Standard deviation of transitory shocks to income


# Make a dictionary for the infinite horizon type
init_infinite = {"CRRA":CRRA,
                "Rfree":1.01/LivPrb_i[0],
                "PermGroFac":PermGroFac_i,
                "PermGroFacAgg":1.0,
                "BoroCnstArt":BoroCnstArt,
                "CubicBool":CubicBool,
                "vFuncBool":vFuncBool,
                "PermShkStd":PermShkStd_i,
                "PermShkCount":PermShkCount,
                "TranShkStd":TranShkStd_i,
                "TranShkCount":TranShkCount,
                "UnempPrb":UnempPrb,
                "IncUnemp":IncUnemp,
                "UnempPrbRet":None,
                "IncUnempRet":None,
                "aXtraMin":aXtraMin,
                "aXtraMax":aXtraMax,
                "aXtraCount":aXtraCount,
                "aXtraExtra":[None],
                "aXtraNestFac":aXtraNestFac,
                "LivPrb":LivPrb_i,
                "DiscFac":DiscFac_i, # dummy value, will be overwritten
                "cycles":0,
                "T_cycle":1,
                "T_retire":0,
                'T_sim':T_sim_PY,
                'T_age': 400,
                'IndL': IndL,
                'aNrmInitMean':np.log(0.00001),
                'aNrmInitStd':0.0,
                'pLvlInitMean':0.0,
                'pLvlInitStd':0.0,
                'AgentCount':0, # will be overwritten by parameter distributor
                }
                
# Make a base dictionary for the cstwMPCmarket
init_market = {'LorenzBool': False,
               'ManyStatsBool': False,
               'ignore_periods':0,    # Will get overwritten
               'PopGroFac':0.0,       # Will get overwritten
               'T_retire':0,          # Will get overwritten
               'TypeWeights':[],      # Will get overwritten
               'Population':Population,
               'act_T':0,             # Will get overwritten
               'IncUnemp':IncUnemp,
               'cutoffs':[(0.99,1),(0.9,1),(0.8,1),(0.6,0.8),(0.4,0.6),(0.2,0.4),(0.0,0.2)],
               'LorenzPercentiles':percentiles_to_match,
               'AggShockBool':do_agg_shocks
               }

