"""
Run simulations on the preference shock model with labor

Looks at estimates of psi and phi for varying parameters of labor elasticity
and preference shock

Keeps the saving to income ratio and income variance constant by changing beta and wage variance

"""
import sys 
import os
sys.path.insert(0,'C:\Users\edmun\OneDrive\Documents\Research\HARK')
sys.path.insert(0,'C:\Users\edmun\OneDrive\Documents\Research\HARK\ConsumptionSaving')
sys.path.insert(0,'C:\Users\edmun\OneDrive\Documents\Research\HARK\cstwMPC')
sys.path.insert(0, os.path.abspath('../'))

from time import clock
import numpy as np
import scipy as sc
from PrefShockModel import PrefLaborConsumerType
from simulations import SetupAndSimulate, PrintTables
from BPPestimation import SelectMicroSample, CS_estimation
from cstwMPC import cstwMPCmarket
import SetupParamsSimulations as Params
from copy import copy, deepcopy
from time import clock
mystr = lambda number : "{:.4f}".format(number)

##################################################################
#These are the inputs
b_to_yagg_target = 0.2 #1.0/12.0*2.0
income_var_target = 0.012
sim_periods = 1200
ignore_periods = 200
AgentCount = 1000

#sim_periods = 400
#ignore_periods = 10
#AgentCount = 100

bounds=[(0.85,0.99),(0.002,0.2)]
run_optimization = True

#following is for 0-1 range for both pref shocks and elasticity, with 5 points
optimized_DiscFac = np.array([[ 0.9849,  0.9858,  0.9864,  0.98,  0.98],
       [ 0.9817,  0.9838,  0.9851,   0.98,   0.98],
       [ 0.9692,  0.9754,  0.9804,  0.9830,  0.9846],
       [ 0.9406,  0.9607,  0.9718,  0.9782,  0.9820],
       [ 0.8886,  0.9348,   0.98,   0.98,   0.98]])
optimized_TranShkStd = np.array([[ 0.1478,  0.1215,  0.1024,  0.1,  0.1],
       [ 0.1478,  0.1225,  0.1033,  0.1,  0.1],
       [ 0.1478,  0.1247,  0.1013,  0.0843,  0.0713],
       [ 0.1478,  0.1184,  0.0837,  0.0548,  0.0311],
       [ 0.1478,  0.0989,  0.1,  0.1,  0.1]])
        
num_pref_vals = 5
max_pref_val = 0.8
pref_vals = np.linspace(0.0001,max_pref_val,num_pref_vals)
num_labelas_vals = 5
max_labor_elas = 0.5
labor_elas = np.linspace(0.0001,max_labor_elas,num_labelas_vals)
n1=3
n2=5
##################################################################

agent_params = copy(Params.init_infinite)
market_params = copy(Params.init_market)

agent_params['PrefShkStd'] = [0.3]
agent_params['PrefShkCount'] = 3
agent_params['LaborElas'] = 0.5

PerpetualYouthType = PrefLaborConsumerType(**agent_params)
PerpetualYouthType.AgeDstn = np.array(1.0)
PerpetualYouthType.AgentCount = AgentCount
PerpetualYouthType.T_sim = sim_periods
PerpetualYouthType.track_vars = ['bNrmNow','cNrmNow','MPCnow','lNow','TranShkNow','PrefShkNow','pLvlNow','cLvlNow','lIncomeLvl','t_age']

EstimationAgentList = [PerpetualYouthType]
# Make an economy for the consumers to live in
EstimationEconomy = cstwMPCmarket(**market_params)
EstimationEconomy.agents = EstimationAgentList
EstimationEconomy.PopGroFac = 1.0
EstimationEconomy.TypeWeight = [1.0]
EstimationEconomy.act_T = sim_periods
EstimationEconomy.ignore_periods = ignore_periods

#options for root finding
options = dict()
options['maxiter']=10
options['disp']=True

def ObjectiveFunc(params_to_solve, Economy, b_to_yagg_target, income_var_target):
    this_beta = params_to_solve[0]
    this_tran_std = params_to_solve[1]
    Economy.agents[0].DiscFac = this_beta
    Economy.agents[0].TranShkStd = [this_tran_std]
    Economy.agents[0].update()
    Economy.solveAgents()
    Economy.makeHistory()
    log_C_agg, log_Y_agg, B_agg = SelectMicroSample(Economy,20,4,True)
    income_var = np.var(log_Y_agg[1:,]-log_Y_agg[:-1,])
    #b_to_yagg = np.mean(B_agg)/np.mean(np.exp(log_Y_agg))
    b_to_yagg = np.median(B_agg/np.exp(log_Y_agg))
    #return [b_to_yagg-b_to_yagg_target, income_var-income_var_target]
    return (b_to_yagg-b_to_yagg_target)**2 + ((income_var-income_var_target)*b_to_yagg_target/income_var_target)**2

dc_agg_std = np.zeros((num_pref_vals,num_labelas_vals))
dy_agg_std = np.zeros((num_pref_vals,num_labelas_vals))
sigma_p_array = np.zeros((num_pref_vals,num_labelas_vals))
sigma_q_array = np.zeros((num_pref_vals,num_labelas_vals))
phi_array = np.zeros((num_pref_vals,num_labelas_vals))
psi_array = np.zeros((num_pref_vals,num_labelas_vals))
b_to_yagg_array = np.zeros((num_pref_vals,num_labelas_vals))
MPC_array = np.zeros((num_pref_vals,num_labelas_vals))
DiscFac_array = np.zeros((num_pref_vals,num_labelas_vals))
TranShkStd_array = np.zeros((num_pref_vals,num_labelas_vals))

for i in range(len(pref_vals)):
    EstimationEconomy.agents[0].PrefShkStd = [pref_vals[i]]
    for j in range(len(labor_elas)):
        EstimationEconomy.agents[0].LaborElas = labor_elas[j]
        if run_optimization:
            start_time = clock()
            solution = sc.optimize.minimize(ObjectiveFunc, [0.985,0.06], (EstimationEconomy,b_to_yagg_target, income_var_target),bounds=bounds,options=options)
            end_time = clock()
            print('Finding parameters took ' + mystr((end_time-start_time)/60.0) + ' minutes.')
            print('Solution is ['+ mystr(solution.x[0]) +',' + mystr(solution.x[1]) + ']')
            solution
            EstimationEconomy.agents[0].DiscFac = solution.x[0]
            EstimationEconomy.agents[0].TranShkStd = [solution.x[1]]
        else:
            EstimationEconomy.agents[0].DiscFac = optimized_DiscFac[i,j]
            EstimationEconomy.agents[0].TranShkStd = [optimized_TranShkStd[i,j]]
            print('Ongoing...')
        EstimationEconomy.agents[0].update()
        EstimationEconomy.solveAgents()
        EstimationEconomy.makeHistory()
        log_C_agg, log_Y_agg, B_agg = SelectMicroSample(EstimationEconomy,20,4,True)
        dc_agg_std[i,j] = np.std(log_C_agg[1:,]-log_C_agg[:-1,])
        dy_agg_std[i,j] = np.std(log_Y_agg[1:,]-log_Y_agg[:-1,])
        b_to_yagg_array[i,j] = np.mean(B_agg)/np.mean(np.exp(log_Y_agg))
        sigma_p_array[i,j],sigma_q_array[i,j],phi_array[i,j],psi_array[i,j] = CS_estimation(log_C_agg, log_Y_agg,n1,n2)
        MPC_array[i,j] = np.mean(EstimationEconomy.agents[0].MPCnow_hist)
        DiscFac_array[i,j] = EstimationEconomy.agents[0].DiscFac
        TranShkStd_array[i,j] = EstimationEconomy.agents[0].TranShkStd[0]
        
#### Save arrays to LaTex tables
def PrintEstimateTable(estimate_array,num_labelas_vals,num_pref_vals,labor_elas,pref_vals,filename,width=0.45,name=""):
    output = "\\begin{minipage}{" + str(width) + "\\textwidth}\n"
   
    output += "\\resizebox{\\textwidth}{!}{\\begin{tabular}{lc|*{" + str(num_labelas_vals) + "}{c}}  \n"
    output += "& " + name 
    for i in range(num_labelas_vals/2):
        output +=  "& "
    output += " Frisch Elasticity \n"
    output += "\\\\ &  "
    for i in range(num_labelas_vals):
        output += " & " + "{:.2f}".format(labor_elas[i]) 
    output += "\\\\ \\toprule  \n"
    for row in range(num_pref_vals):
        if row==num_pref_vals/2-1:
            output += " Preference shock & " + "{:.2f}".format(pref_vals[row]*0.5) + " & "
        else:
            output += " & " + "{:.2f}".format(pref_vals[row]*0.5) + " & "
        for column in range(num_labelas_vals):
            if ~np.isnan(estimate_array[row,column]):
                output += "{:.2f}".format(estimate_array[row,column])
            if column!=num_labelas_vals-1:
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
   
PrintEstimateTable(phi_array,num_labelas_vals,num_pref_vals,labor_elas,pref_vals,'phi_laborsupply',name="$\\phi$")
PrintEstimateTable(psi_array,num_labelas_vals,num_pref_vals,labor_elas,pref_vals,'psi_laborsupply',name="$\\psi$")
PrintEstimateTable(DiscFac_array,num_labelas_vals,num_pref_vals,labor_elas,pref_vals,'beta_laborsupply',name="$\\beta$")
# divide TranShkStd_array by 2 to get annualized std
PrintEstimateTable(TranShkStd_array/2.0,num_labelas_vals,num_pref_vals,labor_elas,pref_vals,'TranShk_laborsupply',name="$\\sigma_q$")
#Also annualize the MPC
PrintEstimateTable(1-(1-MPC_array)**2,num_labelas_vals,num_pref_vals,labor_elas,pref_vals,'mpc_laborsupply',name="MPC")
PrintEstimateTable(DiscFac_array,num_labelas_vals,num_pref_vals,labor_elas,pref_vals,'beta_laborsupply',name="$\\beta$")
PrintEstimateTable(dc_agg_std,num_labelas_vals,num_pref_vals,labor_elas,pref_vals,'c_std_laborsupply',name="Std($\Delta \log c$)")



