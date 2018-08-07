"""
Estimate beta_dist model for Danish economy

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
from HARKutilities import getPercentiles, getLorenzShares, calcSubpopAvg
from cstwMPC import cstwMPCmarket, cstwMPCagent,getKYratioDifference, findLorenzDistanceAtTargetKY
from PrefShockModel import PrefLaborConsumerType
import CSTW_denmark_params as Params
from BPPestimation import SelectMicroSample, CS_estimation
from scipy.optimize import golden, brentq
from copy import copy, deepcopy
from time import clock
import matplotlib.pyplot as plt
mystr = lambda number : "{:.4f}".format(number)

class cstwMPCmarket_denmark(cstwMPCmarket):
    def calcStats(self,aLvlNow,pLvlNow,MPCnow,TranShkNow,EmpNow,t_age,LorenzBool,ManyStatsBool):
        '''
        Calculate various statistics about the current population in the economy.
        
        Parameters
        ----------
        aLvlNow : [np.array]
            Arrays with end-of-period assets, listed by each ConsumerType in self.agents.
        pLvlNow : [np.array]
            Arrays with permanent income levels, listed by each ConsumerType in self.agents.
        MPCnow : [np.array]
            Arrays with marginal propensity to consume, listed by each ConsumerType in self.agents.
        TranShkNow : [np.array]
            Arrays with transitory income shocks, listed by each ConsumerType in self.agents.
        EmpNow : [np.array]
            Arrays with employment states: True if employed, False otherwise.
        t_age : [np.array]
            Arrays with periods elapsed since model entry, listed by each ConsumerType in self.agents.
        LorenzBool: bool
            Indicator for whether the Lorenz target points should be calculated.  Usually False,
            only True when DiscFac has been identified for a particular nabla.
        ManyStatsBool: bool
            Indicator for whether a lot of statistics for tables should be calculated. Usually False,
            only True when parameters have been estimated and we want values for tables.
            
        Returns
        -------
        None
        '''
        # Combine inputs into single arrays
        aLvl = np.hstack(aLvlNow)
        pLvl = np.hstack(pLvlNow)
        age  = np.hstack(t_age)
        TranShk = np.hstack(TranShkNow)
        Emp = np.hstack(EmpNow)
        
        # Calculate the capital to income ratio in the economy
        CohortWeight = self.PopGroFac**(-age)
        CapAgg = np.sum(aLvl*CohortWeight)
        IncAgg = np.sum(pLvl*TranShk*CohortWeight)
        KtoYnow = CapAgg/IncAgg
        self.KtoYnow = KtoYnow
        
        # Store Lorenz data if requested
        self.LorenzLong = np.nan
        if LorenzBool:
            order = np.argsort(aLvl)
            aLvl = aLvl[order]
            CohortWeight = CohortWeight[order]
            wealth_shares = getLorenzShares(aLvl,weights=CohortWeight,percentiles=self.LorenzPercentiles,presorted=True)
            self.Lorenz = wealth_shares
            if ManyStatsBool:
                self.LorenzLong = getLorenzShares(aLvl,weights=CohortWeight,percentiles=np.arange(0.01,1.0,0.01),presorted=True)                
        else:
            self.Lorenz = np.nan # Store nothing if we don't want Lorenz data
            
        # Calculate a whole bunch of statistics if requested
        if ManyStatsBool:
            # Reshape other inputs
            MPC  = np.hstack(MPCnow)
            
            # Sort other data items if aLvl and CohortWeight were sorted
            if LorenzBool:
                pLvl = pLvl[order]
                MPC  = MPC[order]
                TranShk = TranShk[order]
                age = age[order]
                Emp = Emp[order]
            aNrm = aLvl/pLvl # Normalized assets (wealth ratio)
            IncLvl = TranShk*pLvl # Labor income this period
                
            # Calculate overall population MPC and by subpopulations
            #MPCsixmonths = 1.0 - 0.25*((1.0 - MPC) + (1.0 - MPC)**2 + (1.0 - MPC)**3 + (1.0 - MPC)**4)
            MPCsixmonths = 1.0 - (1.0 - MPC)**2 
            self.MPCall = np.sum(MPCsixmonths*CohortWeight)/np.sum(CohortWeight)
            employed =  Emp
            unemployed = np.logical_not(employed)
            if self.T_retire > 0: # Adjust for the lifecycle model, where agents might be retired instead
                unemployed = np.logical_and(unemployed,age < self.T_retire)
                employed   = np.logical_and(employed,age < self.T_retire)
                retired    = age >= self.T_retire
            else:
                retired    = np.zeros_like(unemployed,dtype=bool)
            self.MPCunemployed = np.sum(MPCsixmonths[unemployed]*CohortWeight[unemployed])/np.sum(CohortWeight[unemployed])
            self.MPCemployed   = np.sum(MPCsixmonths[employed]*CohortWeight[employed])/np.sum(CohortWeight[employed])
            self.MPCretired    = np.sum(MPCsixmonths[retired]*CohortWeight[retired])/np.sum(CohortWeight[retired])
            self.MPCbyWealthRatio = calcSubpopAvg(MPCsixmonths,aNrm,self.cutoffs,CohortWeight)
            self.MPCbyIncome      = calcSubpopAvg(MPCsixmonths,IncLvl,self.cutoffs,CohortWeight)
            
            # Calculate the wealth quintile distribution of "hand to mouth" consumers
            quintile_cuts = getPercentiles(aLvl,weights=CohortWeight,percentiles=[0.2, 0.4, 0.6, 0.8])
            wealth_quintiles = np.ones(aLvl.size,dtype=int)
            wealth_quintiles[aLvl > quintile_cuts[0]] = 2
            wealth_quintiles[aLvl > quintile_cuts[1]] = 3
            wealth_quintiles[aLvl > quintile_cuts[2]] = 4
            wealth_quintiles[aLvl > quintile_cuts[3]] = 5
            MPC_cutoff = getPercentiles(MPCsixmonths,weights=CohortWeight,percentiles=[2.0/3.0]) # Looking at consumers with MPCs in the top 1/3
            these = MPCsixmonths > MPC_cutoff
            in_top_third_MPC = wealth_quintiles[these]
            temp_weights = CohortWeight[these]
            hand_to_mouth_total = np.sum(temp_weights)
            hand_to_mouth_pct = []
            for q in range(1,6):
                hand_to_mouth_pct.append(np.sum(temp_weights[in_top_third_MPC == q])/hand_to_mouth_total)
            self.HandToMouthPct = np.array(hand_to_mouth_pct)
            
        else: # If we don't want these stats, just put empty values in history
            self.MPCall = np.nan
            self.MPCunemployed = np.nan
            self.MPCemployed = np.nan
            self.MPCretired = np.nan
            self.MPCbyWealthRatio = np.nan
            self.MPCbyIncome = np.nan
            self.HandToMouthPct = np.nan
            
    def showManyStats(self,spec_name=None):
        '''
        Calculates the "many statistics" by averaging histories across simulated periods.  Displays
        the results as text and saves them to files if spec_name is not None.
        
        Parameters
        ----------
        spec_name : string
            A name or label for the current specification.
            
        Returns
        -------
        None
        '''
        # Calculate MPC overall and by subpopulations
        MPCall = np.mean(self.MPCall_hist[self.ignore_periods:])
        MPCemployed = np.mean(self.MPCemployed_hist[self.ignore_periods:])
        MPCunemployed = np.mean(self.MPCunemployed_hist[self.ignore_periods:])
        MPCretired = np.mean(self.MPCretired_hist[self.ignore_periods:])
        MPCbyIncome = np.mean(np.array(self.MPCbyIncome_hist)[self.ignore_periods:,:],axis=0)
        MPCbyWealthRatio = np.mean(np.array(self.MPCbyWealthRatio_hist)[self.ignore_periods:,:],axis=0)
        HandToMouthPct = np.mean(np.array(self.HandToMouthPct_hist)[self.ignore_periods:,:],axis=0)
        
        
        # Make a string of results to display
        results_string = 'Estimate is center=' + str(self.center_estimate) + ', spread=' + str(self.spread_estimate) + '\n'
        results_string += 'KtoY is ' + str(self.KtoYnow) + '\n'
        results_string += 'Lorenz distance is ' + str(self.LorenzDistance) + '\n'
        results_string += 'Average MPC for all consumers is ' + mystr(MPCall) + '\n'
        results_string += 'Average MPC in the top percentile of W/Y is ' + mystr(MPCbyWealthRatio[0]) + '\n'
        results_string += 'Average MPC in the top decile of W/Y is ' + mystr(MPCbyWealthRatio[1]) + '\n'
        results_string += 'Average MPC in the top quintile of W/Y is ' + mystr(MPCbyWealthRatio[2]) + '\n'
        results_string += 'Average MPC in the second quintile of W/Y is ' + mystr(MPCbyWealthRatio[3]) + '\n'
        results_string += 'Average MPC in the middle quintile of W/Y is ' + mystr(MPCbyWealthRatio[4]) + '\n'
        results_string += 'Average MPC in the fourth quintile of W/Y is ' + mystr(MPCbyWealthRatio[5]) + '\n'
        results_string += 'Average MPC in the bottom quintile of W/Y is ' + mystr(MPCbyWealthRatio[6]) + '\n'
        results_string += 'Average MPC in the top percentile of y is ' + mystr(MPCbyIncome[0]) + '\n'
        results_string += 'Average MPC in the top decile of y is ' + mystr(MPCbyIncome[1]) + '\n'
        results_string += 'Average MPC in the top quintile of y is ' + mystr(MPCbyIncome[2]) + '\n'
        results_string += 'Average MPC in the second quintile of y is ' + mystr(MPCbyIncome[3]) + '\n'
        results_string += 'Average MPC in the middle quintile of y is ' + mystr(MPCbyIncome[4]) + '\n'
        results_string += 'Average MPC in the fourth quintile of y is ' + mystr(MPCbyIncome[5]) + '\n'
        results_string += 'Average MPC in the bottom quintile of y is ' + mystr(MPCbyIncome[6]) + '\n'
        results_string += 'Average MPC for the employed is ' + mystr(MPCemployed) + '\n'
        results_string += 'Average MPC for the unemployed is ' + mystr(MPCunemployed) + '\n'
        results_string += 'Average MPC for the retired is ' + mystr(MPCretired) + '\n'
        results_string += 'Of the population with the 1/3 highest MPCs...' + '\n'
        results_string += mystr(HandToMouthPct[0]*100) + '% are in the bottom wealth quintile,' + '\n'
        results_string += mystr(HandToMouthPct[1]*100) + '% are in the second wealth quintile,' + '\n'
        results_string += mystr(HandToMouthPct[2]*100) + '% are in the third wealth quintile,' + '\n'
        results_string += mystr(HandToMouthPct[3]*100) + '% are in the fourth wealth quintile,' + '\n'
        results_string += 'and ' + mystr(HandToMouthPct[4]*100) + '% are in the top wealth quintile.' + '\n'
        print(results_string)
        
        # Save results to disk
        if spec_name is not None:
            with open('./Results/' + spec_name + 'Results.txt','w') as f:
                f.write(results_string)
                f.close()


## Target wealth distribution - need to check this with Danish data
#lorenz_target = np.array([0.0, 0.004, 0.025,0.117])
#KY_target = 6.60
                
# From Danish Data
lorenz_target = np.array([0.0031, 0.0176866, 0.0584487,0.1828])
KY_target = 2.87939

if Params.do_pref_shocks:
    agent_params = copy(Params.init_infinite)
    
    agent_params['PrefShkStd'] = [0.6]
    agent_params['PrefShkCount'] = 3
    agent_params['LaborElas'] = 0.0

    PerpetualYouthType = PrefLaborConsumerType(**agent_params)
    PerpetualYouthType.AgeDstn = np.array(1.0)
else:
    PerpetualYouthType = cstwMPCagent(**Params.init_infinite)
    PerpetualYouthType.AgeDstn = np.array(1.0)
    
EstimationAgentList = []
for n in range(Params.pref_type_count):
    EstimationAgentList.append(deepcopy(PerpetualYouthType))
    
# Give all the AgentTypes different seeds
for j in range(len(EstimationAgentList)):
    EstimationAgentList[j].seed = j
    EstimationAgentList[j].track_vars = ['t_age','cNrmNow','pLvlNow','TranShkNow','pLvlNow','bNrmNow','MPCnow']
    
# Make an economy for the consumers to live in
EstimationEconomy = cstwMPCmarket_denmark(**Params.init_market)
EstimationEconomy.agents = EstimationAgentList
EstimationEconomy.KYratioTarget = KY_target
EstimationEconomy.LorenzTarget = lorenz_target
EstimationEconomy.LorenzData = lorenz_target

EstimationEconomy.PopGroFac = 1.0
EstimationEconomy.TypeWeight = [1.0]
EstimationEconomy.act_T = Params.T_sim_PY
EstimationEconomy.ignore_periods = Params.ignore_periods_PY

param_range = [0.95,0.995]
spread_range = [0.006,0.008]

# Run the param-dist estimation
paramDistObjective = lambda spread : findLorenzDistanceAtTargetKY(
                                                Economy = EstimationEconomy,
                                                param_name = Params.param_name,
                                                param_count = Params.pref_type_count,
                                                center_range = param_range,
                                                spread = spread,
                                                dist_type = Params.dist_type)
#t_start = clock()
#spread_estimate = golden(paramDistObjective,brack=spread_range,tol=1e-4)
#center_estimate = EstimationEconomy.center_save
#t_end = clock()
#print('Estimate is center=' + str(center_estimate) + ', spread=' + str(spread_estimate) + ', took ' + str((t_end-t_start)/60.0) + ' minutes.')

spread_estimate = 9.537961248790000068e-01
center_estimate = 4.559753666590000282e-02


EstimationEconomy.LorenzBool = True
EstimationEconomy.ManyStatsBool = True
EstimationEconomy.distributeParams(Params.param_name,Params.pref_type_count,center_estimate,spread_estimate,Params.dist_type)
EstimationEconomy.solve()
EstimationEconomy.calcLorenzDistance()
print('Estimate is center=' + str(center_estimate) + ', spread=' + str(spread_estimate) )
EstimationEconomy.center_estimate = center_estimate
EstimationEconomy.spread_estimate = spread_estimate
EstimationEconomy.showManyStats(Params.spec_name)

# Get time aggregated income and consumption over 1 year
C_agg, Y_agg, B_agg, MPC_agg = SelectMicroSample(EstimationEconomy,10,4,False,False,True)
#normalize by 'permanent' income
Y_agg_nrm = Y_agg/np.mean(Y_agg,0)
C_agg_nrm = C_agg/np.mean(Y_agg,0)

n1=3
n2=5
var_perm,var_tran,phi,psi = CS_estimation(C_agg_nrm, Y_agg_nrm,n1,n2)

#Sort by quatile of liquid asset
B_agg_average = np.mean(B_agg,0)
num_quantiles = 5
quantiles = np.array([20,40,60,80])
quantile_cutoffs = np.percentile(B_agg_average,quantiles)
which_quantile = np.digitize(B_agg_average,quantile_cutoffs)

estimation_output = np.zeros((num_quantiles,5))
for i in range(num_quantiles):
    estimation_output[i,0:4] = CS_estimation(C_agg_nrm[:,which_quantile==i], Y_agg_nrm[:,which_quantile==i],n1,n2)
    estimation_output[i,4] = np.mean(MPC_agg[:,which_quantile==i])

if Params.do_pref_shocks:
    np.savetxt('./Results/cstw_denmark_pref_shocks.txt',estimation_output)
    np.savetxt('./Results/cstw_denmark_pref_centerspread.txt',[center_estimate,spread_estimate])
else:
    np.savetxt('./Results/cstw_denmark.txt',estimation_output)
    np.savetxt('./Results/cstw_denmark_centerspread.txt',[center_estimate,spread_estimate])
    
LorenzSim = np.hstack((np.array(0.0),np.mean(np.array(EstimationEconomy.LorenzLong_hist)[EstimationEconomy.ignore_periods:,:],axis=0),np.array(1.0)))
LorenzAxis = np.arange(101,dtype=float)
#plt.plot(LorenzAxis,EstimationEconomy.LorenzData,'-k',linewidth=1.5)
plt.plot(np.array(np.append(np.append([0],EstimationEconomy.LorenzPercentiles),[1]))*100,np.append(np.append([0],EstimationEconomy.LorenzData),[1]),'-k',linewidth=1.5)
plt.plot(LorenzAxis,LorenzSim,'--k',linewidth=1.5)
plt.xlabel('Percentile',fontsize=12)
plt.ylabel('Cumulative liquid wealth share',fontsize=12)
plt.ylim([-0.02,1.0])
plt.savefig('./Figures/Lorenz.png')
plt.show()
        

#Print calibration table
paper_output = "\\begin{minipage}{\\textwidth}\n"
paper_output += "  \\begin{table}\n"
paper_output += "    \\caption{Calibration}\label{table:calibration}\n"

paper_output += "\\begin{tabular}{cd{5}l}  \n"
paper_output += "\\\\ \\toprule  \n"
# Idiosyncratic shock parameters
paper_output += "\multicolumn{3}{c}{ \\textbf{Calibrated Parameters} }  \n"
paper_output += "\\\\ $\sigma_{\\theta}^{2}$    & " + "{:.3f}".format(Params.init_infinite['TranShkStd'][0]**2) +"     & Variance Tran Shocks (=$4 \\times$ {:.3f}".format(0.25*Params.init_infinite['TranShkStd'][0]**2) +" Annual) \n"
paper_output += "\\\\ $\sigma_{\psi}^{2}$      &" + "{:.3f}".format(Params.init_infinite['PermShkStd'][0]**2) +"      & Variance Perm Shocks (=$0.25 \\times$ {:.3f}".format(4.0*Params.init_infinite['PermShkStd'][0]**2) +" Annual) \n"
paper_output += "\\\\ $\wp$                    & " + "{:.3f}".format(Params.init_infinite['UnempPrb']) +"  & Probability of Unemployment Spell \n"
paper_output += "\\\\ $\\theta^u$                    & " + "{:.3f}".format(Params.init_infinite['IncUnemp']) +"  & Income in Unemployment Spell \n"
paper_output += "\\\\ $\PDies$             & " + "{:.3f}".format(1.0-Params.init_infinite['LivPrb'][0]) +"  & Probability of Mortality \n"
paper_output += "\\\\ $\\rho$ & "+ "{:.0f}".format(Params.init_infinite['CRRA']) +". & Coefficient of Relative Risk Aversion \n"

paper_output += "\\\\ $R$ & " + "{:.3f}".format(Params.init_infinite['Rfree']) + "& Quarterly Interest Rate \n"

paper_output += "\\\\ \\midrule  \n"
paper_output += "\multicolumn{3}{c}{ \\textbf{Estimated Parameters} }  \n"
paper_output += "\\\\ $\\beta^c$ &  " + "{:.3f}".format(center_estimate) +" & Mean discount factor \n"
paper_output += "\\\\ $\\nabla$ &  " + "{:.3f}".format(spread_estimate) +" & Discount factor spread\n"

paper_output += "\\\\ \\bottomrule  \n"
paper_output += "\end{tabular}\n"
paper_output += "\end{table}\n"
paper_output += "\end{minipage}\n"
with open('./Tables/CalibrationTable.tex','w') as f:
    f.write(paper_output)
    f.close()
















