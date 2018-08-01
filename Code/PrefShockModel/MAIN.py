"""
This file produces the modeling results for the paper
Consumption Heterogeneity: Micro Drivers and Macro Implications
"""
from time import clock
import numpy as np
from PrefShockModel import PrefLaborConsumerType, PrefLaborMarket, findLorenzDistanceAtTargetKY
from PrefShockModel_tools import SelectMicroSample, CS_estimation, BasicRegressionTables
import PrefShockModel_params as Params
from scipy.optimize import golden
from copy import copy, deepcopy
import matplotlib.pyplot as plt
mystr = lambda number : "{:.4f}".format(number)

estimate_benchmark = True
estimate_pref_shock = False
drop_80th_percentile = True

###############################################################################
# First calibrate a model with no preference shocks nor labor elasticity
###############################################################################
agent_params = copy(Params.init_infinite)
BenchmarkType = PrefLaborConsumerType(**agent_params)
# Make a number of copies (each will have a different discount factor)
BenchmarkAgentList = []
for n in range(Params.pref_type_count):
    BenchmarkAgentList.append(deepcopy(BenchmarkType))
# Give all the AgentTypes different seeds
for j in range(len(BenchmarkAgentList)):
    BenchmarkAgentList[j].seed = j
    BenchmarkAgentList[j].track_vars = ['t_age','cNrmNow','pLvlNow','TranShkNow','bNrmNow','MPCnow','lIncomeLvl','PrefShkNow']
# Make an economy for the consumers to live in
BenchmarkEconomy = PrefLaborMarket(**Params.init_market)
BenchmarkEconomy.agents = BenchmarkAgentList

# Do estimation if required, else use fixed numbers
if estimate_benchmark:
    param_range = [0.95,0.995]
    spread_range = [0.006,0.008]
    # Run the param-dist estimation
    paramDistObjective = lambda spread : findLorenzDistanceAtTargetKY(
                                                    Economy = BenchmarkEconomy,
                                                    param_name = "DiscFac",
                                                    param_count = Params.pref_type_count,
                                                    center_range = param_range,
                                                    spread = spread,
                                                    dist_type = "uniform")
    t_start = clock()
    benchmark_spread_estimate = golden(paramDistObjective,brack=spread_range,tol=1e-4)
    benchmark_center_estimate = BenchmarkEconomy.center_save
    t_end = clock()
    print('Benchmark Estimate is center=' + str(benchmark_center_estimate) + ', spread=' + str(benchmark_spread_estimate) + ', took ' + str((t_end-t_start)/60.0) + ' minutes.')
else:
    benchmark_spread_estimate = 0.00546993654236
    benchmark_center_estimate = 0.985946108692
    if drop_80th_percentile:
        benchmark_spread_estimate = 0.00719314936721
        benchmark_center_estimate = 0.984848310842
    
# Set economy with estimated (or not) discount factors
BenchmarkEconomy.LorenzBool = True
BenchmarkEconomy.ManyStatsBool = True
BenchmarkEconomy.distributeParams("DiscFac",Params.pref_type_count,benchmark_center_estimate,benchmark_spread_estimate,"uniform")
print('Estimate is center=' + str(benchmark_center_estimate) + ', spread=' + str(benchmark_spread_estimate) )
BenchmarkEconomy.center_estimate = benchmark_center_estimate
BenchmarkEconomy.spread_estimate = benchmark_spread_estimate
BenchmarkEconomy.solve()
BenchmarkEconomy.calcLorenzDistance()
BenchmarkEconomy.showManyStats("Benchmark")

# Get sample data time aggregated over 4 quarters
benchmark_Cons_sample, benchmark_Inc_sample, benchmark_B_sample, benchmark_MPC_sample = SelectMicroSample(BenchmarkEconomy)
# Normalize by 'permanent' income
benchmark_Inc_sample_nrm = benchmark_Inc_sample/np.mean(benchmark_Inc_sample,0)
benchmark_Cons_sample_nrm = benchmark_Cons_sample/np.mean(benchmark_Inc_sample,0)
# Estimate the parameters
benchmark_var_perm,benchmark_var_tran,benchmark_phi,benchmark_psi = CS_estimation(benchmark_Cons_sample_nrm, benchmark_Inc_sample_nrm)
benchmark_mean_MPC = np.mean(benchmark_MPC_sample)
print("Benchmark model:")
print("var_perm = " + str(benchmark_var_perm))
print("var_tran = " + str(benchmark_var_tran))
print("phi = " + str(benchmark_phi))
print("psi = " + str(benchmark_psi))
print("MPC = " + str(benchmark_mean_MPC))
# Now estimate the parameters by liquid wealth quantile
benchmark_B_sample_mean = np.mean(benchmark_B_sample,0)
num_quantiles = 5
quantiles = np.array([20,40,60,80])
quantile_cutoffs = np.percentile(benchmark_B_sample_mean,quantiles)
benchmark_which_quantile = np.digitize(benchmark_B_sample_mean,quantile_cutoffs)

benchmark_estimation_output = np.zeros((num_quantiles,5))
for i in range(num_quantiles):
    benchmark_estimation_output[i,0:4] = CS_estimation(benchmark_Cons_sample_nrm[:,benchmark_which_quantile==i], benchmark_Inc_sample_nrm[:,benchmark_which_quantile==i])
    benchmark_estimation_output[i,4] = np.mean(benchmark_MPC_sample[:,benchmark_which_quantile==i])
np.savetxt('./Results/benchmark_liquidwealth.txt',benchmark_estimation_output)
np.savetxt('./Results/benchmark_centerspread.txt',[benchmark_center_estimate,benchmark_spread_estimate])

#Draw Lorenz curve
benchmark_LorenzSim = np.hstack((np.array(0.0),np.mean(np.array(BenchmarkEconomy.LorenzLong_hist)[BenchmarkEconomy.ignore_periods:,:],axis=0),np.array(1.0)))
LorenzAxis = np.arange(101,dtype=float)
plt.plot(LorenzAxis,np.append(0.0,BenchmarkEconomy.LorenzAllData)/100.0,'-k',linewidth=1.5)
plt.plot(LorenzAxis,benchmark_LorenzSim,'--k',linewidth=1.5)
plt.scatter(np.array(np.append(np.append([0],BenchmarkEconomy.LorenzPercentiles),[1]))*100,np.append(np.append([0],BenchmarkEconomy.LorenzTarget),[1]))
plt.xlabel('Percentile',fontsize=12)
plt.ylabel('Cumulative liquid wealth share',fontsize=12)
plt.ylim([-0.02,1.0])
plt.legend(['Data','Simulation', 'Target'])
plt.title('Lorenz Curve in the Benchmark Model')
plt.savefig('./Figures/benchmark_Lorenz.png')
plt.show()

#Get BasicRegression results
benchmark_br_all = BasicRegressionTables(benchmark_Cons_sample_nrm, benchmark_Inc_sample_nrm,max_diff=10,filename='benchmark_br_all')
benchmark_br_quintiles = np.zeros((10,num_quantiles))
for i in range(num_quantiles):
    benchmark_br_quintiles[:,i] = BasicRegressionTables(benchmark_Cons_sample_nrm[:,benchmark_which_quantile==i], benchmark_Inc_sample_nrm[:,benchmark_which_quantile==i],max_diff=10,filename='benchmark_br_'+str(i+1)+'_quintile')

###############################################################################
# Next calibrate a model with preference shocks but no labor elasticity
###############################################################################
agent_params = copy(Params.init_infinite)
PrefShockType = PrefLaborConsumerType(**agent_params)
PrefShockType.PrefShkCount =3
PrefShockType.PrefShkStd = [0.6]
PrefShockType.update()
# Make a number of copies (each will have a different discount factor)
PrefShockAgentList = []
for n in range(Params.pref_type_count):
    PrefShockAgentList.append(deepcopy(PrefShockType))
# Give all the AgentTypes different seeds
for j in range(len(PrefShockAgentList)):
    PrefShockAgentList[j].seed = j
    PrefShockAgentList[j].track_vars = ['t_age','cNrmNow','pLvlNow','TranShkNow','bNrmNow','MPCnow','lIncomeLvl','PrefShkNow']
# Make an economy for the consumers to live in
PrefShockEconomy = PrefLaborMarket(**Params.init_market)
PrefShockEconomy.agents = PrefShockAgentList

# Do estimation if required, else use fixed numbers
if estimate_pref_shock:
    param_range = [0.94,0.99]
    spread_range = [0.01,0.02]
    # Run the param-dist estimation
    paramDistObjective = lambda spread : findLorenzDistanceAtTargetKY(
                                                    Economy = PrefShockEconomy,
                                                    param_name = "DiscFac",
                                                    param_count = Params.pref_type_count,
                                                    center_range = param_range,
                                                    spread = spread,
                                                    dist_type = "uniform")
    t_start = clock()
    prefshock_spread_estimate = golden(paramDistObjective,brack=spread_range,tol=1e-4)
    prefshock_center_estimate = PrefShockEconomy.center_save
    t_end = clock()
    print('PrefShock Estimate is center=' + str(prefshock_center_estimate) + ', spread=' + str(prefshock_spread_estimate) + ', took ' + str((t_end-t_start)/60.0) + ' minutes.')
else:
    prefshock_spread_estimate = 0.0253938031979
    prefshock_center_estimate = 0.969997962799
#    if drop_80th_percentile:
#        benchmark_spread_estimate =
#        benchmark_center_estimate =
# Set economy with estimated (or not) discount factors
PrefShockEconomy.LorenzBool = True
PrefShockEconomy.ManyStatsBool = True
PrefShockEconomy.distributeParams("DiscFac",Params.pref_type_count,prefshock_center_estimate,prefshock_spread_estimate,"uniform")
print('Estimate is center=' + str(prefshock_center_estimate) + ', spread=' + str(prefshock_spread_estimate) )
PrefShockEconomy.center_estimate = prefshock_center_estimate
PrefShockEconomy.spread_estimate = prefshock_spread_estimate
PrefShockEconomy.solve()
PrefShockEconomy.calcLorenzDistance()
PrefShockEconomy.showManyStats("PrefShock")

# Get sample data time aggregated over 4 quarters
prefshock_Cons_sample, prefshock_Inc_sample, prefshock_B_sample, prefshock_MPC_sample = SelectMicroSample(PrefShockEconomy)
# Normalize by 'permanent' income
prefshock_Inc_sample_nrm = prefshock_Inc_sample/np.mean(prefshock_Inc_sample,0)
prefshock_Cons_sample_nrm = prefshock_Cons_sample/np.mean(prefshock_Inc_sample,0)
# Estimate the parameters
prefshock_var_perm,prefshock_var_tran,prefshock_phi,prefshock_psi = CS_estimation(prefshock_Cons_sample_nrm, prefshock_Inc_sample_nrm)
prefshock_mean_MPC = np.mean(prefshock_MPC_sample)
print("PrefShock model:")
print("var_perm = " + str(prefshock_var_perm))
print("var_tran = " + str(prefshock_var_tran))
print("phi = " + str(prefshock_phi))
print("psi = " + str(prefshock_psi))
print("MPC = " + str(prefshock_mean_MPC))
# Now estimate the parameters by liquid wealth quantile
prefshock_B_sample_mean = np.mean(prefshock_B_sample,0)
num_quantiles = 5
quantiles = np.array([20,40,60,80])
quantile_cutoffs = np.percentile(prefshock_B_sample_mean,quantiles)
prefshock_which_quantile = np.digitize(prefshock_B_sample_mean,quantile_cutoffs)

prefshock_estimation_output = np.zeros((num_quantiles,5))
for i in range(num_quantiles):
    prefshock_estimation_output[i,0:4] = CS_estimation(prefshock_Cons_sample_nrm[:,prefshock_which_quantile==i], prefshock_Inc_sample_nrm[:,prefshock_which_quantile==i])
    prefshock_estimation_output[i,4] = np.mean(prefshock_MPC_sample[:,prefshock_which_quantile==i])
np.savetxt('./Results/prefshock_liquidwealth.txt',prefshock_estimation_output)
np.savetxt('./Results/prefshock_centerspread.txt',[prefshock_center_estimate,prefshock_spread_estimate])

#Draw Lorenz curve
prefshock_LorenzSim = np.hstack((np.array(0.0),np.mean(np.array(PrefShockEconomy.LorenzLong_hist)[PrefShockEconomy.ignore_periods:,:],axis=0),np.array(1.0)))
LorenzAxis = np.arange(101,dtype=float)
plt.plot(LorenzAxis,np.append(0.0,PrefShockEconomy.LorenzAllData)/100.0,'-k',linewidth=1.5)
plt.plot(LorenzAxis,prefshock_LorenzSim,'--k',linewidth=1.5)
plt.scatter(np.array(np.append(np.append([0],PrefShockEconomy.LorenzPercentiles),[1]))*100,np.append(np.append([0],PrefShockEconomy.LorenzTarget),[1]))
plt.xlabel('Percentile',fontsize=12)
plt.ylabel('Cumulative liquid wealth share',fontsize=12)
plt.ylim([-0.02,1.0])
plt.legend(['Data','Simulation', 'Target'])
plt.title('Lorenz Curve in the Preference Shock Model')
plt.savefig('./Figures/prefshock_Lorenz.png')
plt.show()

#Get BasicRegression results
prefshock_br_all = BasicRegressionTables(prefshock_Cons_sample_nrm, prefshock_Inc_sample_nrm,max_diff=10,filename='benchmark_br_all')
prefshock_br_quintiles = np.zeros((10,num_quantiles))
for i in range(num_quantiles):
    prefshock_br_quintiles[:,i] = BasicRegressionTables(prefshock_Cons_sample_nrm[:,prefshock_which_quantile==i], prefshock_Inc_sample_nrm[:,prefshock_which_quantile==i],max_diff=10,filename='prefshock_br_'+str(i+1)+'_quintile')

