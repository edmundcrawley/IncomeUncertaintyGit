"""
Run simulations to test the Carroll-Samwick/BPP methods for eliciting
consumption responses
"""
import sys 
sys.path.insert(0,'C:\Users\edmun\OneDrive\Documents\Research\HARK')
sys.path.insert(0,'C:\Users\edmun\OneDrive\Documents\Research\HARK\ConsumptionSaving')
sys.path.insert(0,'C:\Users\edmun\OneDrive\Documents\Research\HARK\cstwMPC')

from time import clock
import numpy as np
from HARKinterpolation import LinearInterp, LinearInterpOnInterp1D, BilinearInterpOnInterp1D, LowerEnvelope
from HARKutilities import combineIndepDstns,approxMeanOneLognormal, addDiscreteOutcomeConstantMean
from ConsIndShockModel import ConsPerfForesightSolver, ConsumerSolution, IndShockConsumerType, \
                        PerfForesightConsumerType, utilityP_inv, utilityP_invP, utility_invP, \
                        MargValueFunc
from HARKsimulation import drawDiscrete, drawBernoulli, drawLognormal, drawUniform
from cstwMPC import cstwMPCmarket, getKYratioDifference, findLorenzDistanceAtTargetKY, cstwMPCagent
from BPPestimation import SelectMicroSample
import SetupParamsSimulations as Params
from scipy.optimize import golden, brentq
from copy import copy, deepcopy
import matplotlib.pyplot as plt

def SetupAndSimulate(agent_params, market_params, KY_target, lorenz_target=[], pref_type_count=1, sim_periods=1200,ignore_periods=400, override_center=False, override_spread=False):
    # First set up a model at quarterly frequency calibrated to median wealth
    PerpetualYouthType = cstwMPCagent(**agent_params)
    PerpetualYouthType.AgeDstn = np.array(1.0)
    EstimationAgentList = []
    for n in range(pref_type_count):
        EstimationAgentList.append(deepcopy(PerpetualYouthType))
            
    # Give all the AgentTypes different seeds
    for j in range(len(EstimationAgentList)):
        EstimationAgentList[j].seed = j
        
    # Make an economy for the consumers to live in
    EstimationEconomy = cstwMPCmarket(**market_params)
    EstimationEconomy.agents = EstimationAgentList
    EstimationEconomy.KYratioTarget = KY_target
    EstimationEconomy.LorenzTarget = lorenz_target
    
    EstimationEconomy.PopGroFac = 1.0
    EstimationEconomy.TypeWeight = [1.0]
    EstimationEconomy.act_T = sim_periods
    EstimationEconomy.ignore_periods = ignore_periods
        
    param_range = [0.95,0.995]
    spread_range = [0.006,0.008]
            
    if pref_type_count>1:
        # Run the param-dist estimation
        paramDistObjective = lambda spread : findLorenzDistanceAtTargetKY(
                                                        Economy = EstimationEconomy,
                                                        param_name = 'DiscFac',
                                                        param_count = pref_type_count,
                                                        center_range = param_range,
                                                        spread = spread,
                                                        dist_type = 'uniform')
        if override_center!=False:
            spread_estimate = override_spread
            center_estimate = override_center
            t_start = 0.0
            t_end = 0.0
        else:
            t_start = clock()
            spread_estimate = golden(paramDistObjective,brack=spread_range,tol=1e-4)
            center_estimate = EstimationEconomy.center_save
            t_end = clock()
    else:
        # Run the param-point estimation only
        paramPointObjective = lambda center : getKYratioDifference(Economy = EstimationEconomy,
                                             param_name = 'DiscFac',
                                             param_count = pref_type_count,
                                             center = center,
                                             spread = 0.0,
                                             dist_type = 'uniform')
        if override_center!=False:
            spread_estimate = override_spread
            center_estimate = override_center
            t_start = 0.0
            t_end = 0.0
        else:
            t_start = clock()
            center_estimate = brentq(paramPointObjective,param_range[0],param_range[1],xtol=1e-6)
            spread_estimate = 0.0
            t_end = clock()
        
    EstimationEconomy.LorenzBool = True
    EstimationEconomy.ManyStatsBool = True
    EstimationEconomy.distributeParams('DiscFac',pref_type_count,center_estimate,spread_estimate,'uniform')
    EstimationEconomy.solve()
    EstimationEconomy.calcLorenzDistance()
    print('Estimate is center=' + str(center_estimate) + ', spread=' + str(spread_estimate) + ', took ' + str(t_end-t_start) + ' seconds.')
    EstimationEconomy.center_estimate = center_estimate
    EstimationEconomy.spread_estimate = spread_estimate
    for i in range(len(EstimationEconomy.agents)):
        this_agent = EstimationEconomy.agents[i]
        this_agent.track_vars = ['cNrmNow','pLvlNow','TranShkNow','t_age']
    EstimationEconomy.makeHistory()
    
    return EstimationEconomy


#Run estimate twice, first with very low assets to replicate the kind of 
#numbers in the Danish data, then with numbers chosen to match the
#Danish wealth distribution

#boolean determines whether to overide the slow root finding proccess
override=True

#Econonmy1: very low assets
KY_target1 = 0.4
lorenz_target1 = np.array([0.0, 0.004, 0.025,0.117])    #note this is not used for calibration
agent_params1 = Params.init_infinite
market_params1 = Params.init_market
if override:
    #override_center1 = 0.960774615797
    override_center1 = 0.93
    override_spread1 = 0.0
else:
    override_center1 = False
    override_spread1 = False
EstimationEconomy1 = SetupAndSimulate(agent_params1, market_params1, KY_target1, lorenz_target1,override_center=override_center1,override_spread=override_spread1)


#Economy2: Fit to distribution of assets
KY_target2 = 6.6
lorenz_target2 = np.array([0.0, 0.004, 0.025,0.117])    #note this is not used for calibration
agent_params2 = Params.init_infinite
market_params2 = Params.init_market
if override:
    override_center2 = 0.981410478305
    override_spread2 = 0.0173080674799
else:
    override_center2 = False
    override_spread2 = False
EstimationEconomy2 = SetupAndSimulate(agent_params2, market_params2, KY_target2, lorenz_target2,pref_type_count=7,override_center=override_center2,override_spread=override_spread2)

def PrintTables(Economy, filename_end, max_n1=12,width = 0.4):
    log_C_agg, log_Y_agg = SelectMicroSample(Economy,20,4)
    sigma_p_array = np.zeros((max_n1,max_n1)) + np.nan
    sigma_q_array = np.zeros((max_n1,max_n1)) + np.nan
    phi_array = np.zeros((max_n1,max_n1)) + np.nan
    psi_array = np.zeros((max_n1,max_n1)) + np.nan
    for n1 in range(1,max_n1+1):
        for n2 in range(n1+1,max_n1+1):
            sigma_p,sigma_q,phi,psi = CS_estimation(log_C_agg, log_Y_agg,n1,n2)
            sigma_p_array[n1-1,n2-1] = sigma_p
            sigma_q_array[n1-1,n2-1] = sigma_q
            phi_array[n1-1,n2-1] = phi
            psi_array[n1-1,n2-1] = psi
            
    EstimateTable(psi_array[0:10,0:10], 'Psi_array'+filename_end,True,width=width)
    EstimateTable(phi_array[0:10,0:10], 'Phi_array'+filename_end,True,width=width)
    EstimateTable(sigma_p_array[0:10,0:10], 'SigmaP_array'+filename_end,True,width=width)
    EstimateTable(sigma_q_array[0:10,0:10], 'SigmaQ_array'+filename_end,True,width=width)

PrintTables(EstimationEconomy1,'1')
PrintTables(EstimationEconomy2,'2')