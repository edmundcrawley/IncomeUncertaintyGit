'''
Loads parameters used in the cstwMPC estimations.
'''
import numpy as np
import csv
from copy import  deepcopy
import os

small_sample_test = False

spec_name = "Benchmark_"
percentiles_to_match = [0.2, 0.4, 0.6, 0.8]     # Which points of the Lorenz curve to match in beta-dist (must be in (0,1))
# Following data comes from Danish administrative records and can be found in the file Lorenz_data_Denmark.txt
lorenz_target        = [0.0045, 0.0247, 0.0793, 0.2366]
lorenz_all_data      = np.genfromtxt('Lorenz_data_Denmark.txt', delimiter=',')[2:]  # contains data for each percentile of the curve
KY_target            = 201475.92/321188.22*4.0  # Mean liquid wealth/Mean Income, adjusted for quarterly data

do_param_dist = True
if do_param_dist:
    pref_type_count = 5       # Number of discrete beta types in beta-dist
else:
    pref_type_count = 1       # Just one beta type in beta-point

# Set grid sizes
PermShkCount = 5              # Number of points in permanent income shock grid
TranShkCount = 5              # Number of points in transitory income shock grid
PrefShkCount = 2              # Number of points in preference shock grid - model seems to only work if this is at least 2
aXtraMin = 0.00001            # Minimum end-of-period assets in grid
aXtraMax = 20                 # Maximum end-of-period assets in grid
aXtraCount = 20               # Number of points in assets grid
aXtraNestFac = 3              # Number of times to 'exponentially nest' when constructing assets grid

# Set simulation parameters
Population = 14000  # Total number of simulated agents in the population
T_sim = 1200               # Number of periods to simulate (idiosyncratic shocks model, perpetual youth)
ignore_periods = 400       # Number of periods to throw out when looking at history
pLvlInitStd = 0.4             # Standard deviation of initial permanent income
aNrmInitMean = np.log(2.0)    # log initial wealth/income mean
aNrmInitStd  = 0.5            # log initial wealth/income standard deviation
AgeDstn = np.array(1.0)

# Set population macro parameters
PopGroFac = 1.0               # Population growth rate
PermGroFacAgg = 1.0           # TFP growth rate

# Set indiividual parameters for the infinite horizon model
CRRA  = 2.0                   # Coefficient relative risk aversion
PermGroFac = [1.000]          # Permanent income growth factor (no perm growth)
DiscFac = 0.97                # Default intertemporal discount factor
LivPrb = [1.0 - 1.0/160.0]    # Survival probability
PermShkStd =  [(0.004/4.0)**0.5]      # Standard deviation of permanent shocks to income
TranShkStd =  [(0.0035*4)**0.5 ]      # Standard deviation of transitory shocks to income
PrefShkStd =  [0.000001]                  # Standard deviation of preference shocks
UnempPrb = 0.001             # Probability of unemployment
IncUnemp = 0.5               # Unemployment benefit replacement rate
LaborElas = 0.0              # Labor elasticity

if small_sample_test:
    Population = 1400
    T_sim = 400  
    ignore_periods = 100
    PermShkCount = 3
    TranShkCount = 3
    pref_type_count = 2
    PrefShkCount = 2
    PrefShkStd =  [0.000001]



# Make a dictionary for the infinite horizon type
init_infinite = {"CRRA":CRRA,
                "Rfree":1.01/LivPrb[0],
                "PermGroFac":PermGroFac,
                "PermGroFacAgg":1.0,
                "PermShkStd":PermShkStd,
                "PermShkCount":PermShkCount,
                "TranShkStd":TranShkStd,
                "TranShkCount":TranShkCount,
                "PrefShkStd":PrefShkStd,
                "PrefShkCount":PrefShkCount,
                "UnempPrb":UnempPrb,
                "IncUnemp":IncUnemp,
                "LaborElas":LaborElas,
                "aXtraMin":aXtraMin,
                "aXtraMax":aXtraMax,
                "aXtraCount":aXtraCount,
                "aXtraExtra":[None],
                "aXtraNestFac":aXtraNestFac,
                "LivPrb":LivPrb,
                "DiscFac":DiscFac, # dummy value, will be overwritten
                "cycles":0,
                "T_cycle":1,
                'T_sim':T_sim,
                'T_age': 400,
                'aNrmInitMean':np.log(0.00001),
                'aNrmInitStd':0.0,
                'pLvlInitMean':0.0,
                'pLvlInitStd':0.0,
                'AgentCount':0, # will be overwritten by parameter distributor
                'AgeDstn':AgeDstn
                }
                
# Make a base dictionary for the cstwMPCmarket
init_market = {'LorenzBool': False,
               'ManyStatsBool': False,
               'ignore_periods':ignore_periods,
               'PopGroFac':1.0,       
               'TypeWeight':[1.0],
               'Population':Population,
               'act_T':T_sim,
               'cutoffs':[(0.99,1),(0.9,1),(0.8,1),(0.6,0.8),(0.4,0.6),(0.2,0.4),(0.0,0.2)],
               'LorenzPercentiles':percentiles_to_match,
               'LorenzTarget':lorenz_target,
               'LorenzAllData':lorenz_all_data,
               'KYratioTarget':KY_target
               }

