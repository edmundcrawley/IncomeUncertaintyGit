"""
Run simulations on the preference shock model with labor
"""
import sys 
sys.path.insert(0,'C:\Users\edmun\OneDrive\Documents\Research\HARK')
sys.path.insert(0,'C:\Users\edmun\OneDrive\Documents\Research\HARK\ConsumptionSaving')
sys.path.insert(0,'C:\Users\edmun\OneDrive\Documents\Research\HARK\cstwMPC')

from time import clock
import numpy as np
from PrefShockModel import PrefLaborConsumerType
from simulations import SetupAndSimulate, PrintTables
from BPPestimation import SelectMicroSample, CS_estimation
import SetupParamsSimulations as Params
from copy import copy, deepcopy

#boolean determines whether to overide the slow root finding proccess
override=False

#Econonmy: very low assets
KY_target = 0.5
lorenz_target = np.array([0.0, 0.004, 0.025,0.117])    #note this is not used for calibration
agent_params = copy(Params.init_infinite)
market_params = copy(Params.init_market)
if override:
    #override_center1 = 0.960774615797
    override_center = 0.97
    override_spread = 0.0
else:
    override_center = False
    override_spread = False

agent_params['PrefShkStd'] = [0.3]
agent_params['PrefShkCount'] = 5
agent_params['LaborElas'] = 0.5

#agent_params['PrefShkStd'] = [0.00001]
#agent_params['PrefShkCount'] = 5
#agent_params['LaborElas'] = 0.00001 

EconomiesList = []
KY_target = 0.5
num_pref_vals = 2
num_labelas_vals = 2
pref_vals = np.linspace(0.0001,0.5,2)
labor_elas = np.linspace(0.0001,0.5,2)
dc_agg_std = np.zeros((num_pref_vals,num_labelas_vals))
dy_agg_std = np.zeros((num_pref_vals,num_labelas_vals))
sigma_p_array = np.zeros((num_pref_vals,num_labelas_vals))
sigma_q_array = np.zeros((num_pref_vals,num_labelas_vals))
phi_array = np.zeros((num_pref_vals,num_labelas_vals))
psi_array = np.zeros((num_pref_vals,num_labelas_vals))
n1=5
n2=7
override_vals = [[0.98969176446617346, 0.98879860253255925],[0.98936009948530024,0.98863515013811432]]
for i in range(num_pref_vals):
    agent_params['PrefShkStd'] = [pref_vals[i]]
    for j in range(num_labelas_vals):
        agent_params['LaborElas'] = labor_elas[j]
        if override:
            #override_center1 = 0.960774615797
            override_center = override_vals[i][j]
            override_spread = 0.0
        else:
            override_center = False
            override_spread = False
        this_EstimationEconomy = SetupAndSimulate(agent_params, market_params, KY_target, lorenz_target,override_center=override_center,override_spread=override_spread,PrefShk=True)
        EconomiesList.append(this_EstimationEconomy)
        log_C_agg, log_Y_agg = SelectMicroSample(this_EstimationEconomy,20,4,True)
        dc_agg_std[i,j] = np.std(log_C_agg[1:,]-log_C_agg[:-1,])
        dy_agg_std[i,j] = np.std(log_Y_agg[1:,]-log_Y_agg[:-1,])
        sigma_p_array[i,j],sigma_q_array[i,j],phi_array[i,j],psi_array[i,j] = CS_estimation(log_C_agg, log_Y_agg,n1,n2)
        

EstimationEconomy = SetupAndSimulate(agent_params, market_params, KY_target, lorenz_target,override_center=override_center,override_spread=override_spread,PrefShk=True)

PrintTables(EstimationEconomy,'PrefShock_newcode2',True)

log_C_agg, log_Y_agg = SelectMicroSample(EstimationEconomy,20,4,True)
n1=3
n2=5
sigma_p,sigma_q,phi,psi = CS_estimation(log_C_agg, log_Y_agg,n1,n2)
