
"""
Builds a consumption-savings model with both preference shocks and 
wage shocks. 
"""
import sys 
sys.path.insert(0,'C:\Users\edmun\OneDrive\Documents\Research\HARK')
sys.path.insert(0,'C:\Users\edmun\OneDrive\Documents\Research\HARK\ConsumptionSaving')

import numpy as np
from HARKinterpolation import LinearInterp, LinearInterpOnInterp1D, BilinearInterpOnInterp1D, LowerEnvelope
from HARKutilities import combineIndepDstns,approxMeanOneLognormal, addDiscreteOutcomeConstantMean
from ConsIndShockModel import ConsPerfForesightSolver, ConsumerSolution, IndShockConsumerType, \
                        PerfForesightConsumerType, utilityP_inv, utilityP_invP, utility_invP, \
                        MargValueFunc
from HARKsimulation import drawDiscrete, drawBernoulli, drawLognormal, drawUniform
from copy import copy, deepcopy
import matplotlib.pyplot as plt

class PrefLaborShockSetup(ConsPerfForesightSolver):
    '''
    Setup for models where consumers have see preference to
    shocks to consumption as well as income shocks
    '''
    def __init__(self,solution_next,IncomeAndPrefDstn,LivPrb,DiscFac,CRRA,
                      LaborElas,Rfree,PermGroFac,aXtraGrid,WageShkVals,PrefShkVals):
        '''
        Constructor for a new solver-setup for problems with income subject to
        permanent and transitory shocks.
        
        Parameters
        ----------
        solution_next : ConsumerSolution
            The solution to next period's one period problem.
        IncomeAndPrefDstn : [np.array]
            A list containing four arrays of floats, representing a discrete
            approximation to the income process between the period being solved
            and the one immediately following (in solution_next). Order: event
            probabilities, permanent shocks, transitory shocks, preference shocks.
        LivPrb : float
            Survival probability; likelihood of being alive at the beginning of
            the succeeding period.    
        DiscFac : float
            Intertemporal discount factor for future utility.        
        CRRA : float
            Coefficient of relative risk aversion.
        LaborElas : float
            Elasticity of labor with respect to wage
        Rfree : float
            Risk free interest factor on end-of-period assets.
        PermGroFac : float
            Expected permanent income growth factor at the end of this period.
        aXtraGrid: np.array
            Array of "extra" end-of-period asset values-- assets above the
            absolute minimum acceptable level.
                        
        Returns
        -------
        None
        '''
        self.assignParameters(solution_next,IncomeAndPrefDstn,LivPrb,DiscFac,CRRA,LaborElas,Rfree,
                                PermGroFac,aXtraGrid,WageShkVals,PrefShkVals)
        self.defUtilityFuncs()

    def assignParameters(self,solution_next,IncomeAndPrefDstn,LivPrb,DiscFac,CRRA,LaborElas,Rfree,
                                PermGroFac,aXtraGrid,WageShkVals,PrefShkVals):
        '''
        Assigns period parameters as attributes of self for use by other methods
        
        Parameters
        ----------
        see __init__
                        
        Returns
        -------
        none
        '''
        ConsPerfForesightSolver.assignParameters(self,solution_next,DiscFac,LivPrb,
                                                CRRA,Rfree,PermGroFac)
        self.IncomeAndPrefDstn     = IncomeAndPrefDstn
        self.LaborElas       = LaborElas
        self.aXtraGrid      = aXtraGrid
        self.PrefShkVals    = PrefShkVals
        self.WageShkVals    = WageShkVals

    def defUtilityFuncs(self):
        '''
        Defines CRRA utility function for this period (and its derivatives,
        and their inverses), saving them as attributes of self for other methods
        to use.
        
        Note - the input here is xi*c-v(L), not c itself (GHH preferences with
        preference shock xi)
        
        Parameters
        ----------
        none
        
        Returns
        -------
        none
        '''
        ConsPerfForesightSolver.defUtilityFuncs(self)
        self.uPinv     = lambda u : utilityP_inv(u,gam=self.CRRA)
        self.uPinvP    = lambda u : utilityP_invP(u,gam=self.CRRA)
        self.uinvP     = lambda u : utility_invP(u,gam=self.CRRA)        


    def setAndUpdateValues(self,solution_next,IncomeAndPrefDstn,LaborElas,LivPrb,DiscFac):
        '''
        Unpacks some of the inputs (and calculates simple objects based on them),
        storing the results in self for use by other methods.  These include:
        income shocks and probabilities, next period's marginal value function
        (etc), the probability of getting the worst income shock next period,
        the patience factor, human wealth, and the bounding MPCs.
        
        Parameters
        ----------
        solution_next : ConsumerSolution
            The solution to next period's one period problem.
        IncomeAndPrefDstn : [np.array]
            A list containing four arrays of floats, representing a discrete
            approximation to the income process between the period being solved
            and the one immediately following (in solution_next). Order: event
            probabilities, permanent shocks, transitory shocks, preference shocks.
        LaborElas : float
            Elasticity of labor with respect to wages
        LivPrb : float
            Survival probability; likelihood of being alive at the beginning of
            the succeeding period.    
        DiscFac : float
            Intertemporal discount factor for future utility.
            
        Returns
        -------
        None
        '''
        self.DiscFacEff       = DiscFac*LivPrb # "effective" discount factor
        self.IncomeAndPrefDist     = IncomeAndPrefDstn
        self.ShkPrbsNext      = IncomeAndPrefDstn[0]
        self.PermShkValsNext  = IncomeAndPrefDstn[1]
        self.TranShkValsNext  = IncomeAndPrefDstn[2]
        self.PrefShkValsNext  = IncomeAndPrefDstn[3]
#        self.PermShkMinNext   = np.min(self.PermShkValsNext)    
#        self.TranShkMinNext   = np.min(self.TranShkValsNext)
        self.vPfuncNext       = solution_next.vPfunc        
#        self.WorstIncPrb      = np.sum(self.ShkPrbsNext[
#                                (self.PermShkValsNext*self.TranShkValsNext)==
#                                (self.PermShkMinNext*self.TranShkMinNext)]) 
            
#        # Update the bounding MPCs and PDV of human wealth:
#        self.PatFac       = ((self.Rfree*self.DiscFacEff)**(1.0/self.CRRA))/self.Rfree
#        self.MPCminNow    = 1.0/(1.0 + self.PatFac/solution_next.MPCmin)
#        self.ExIncNext    = np.dot(self.ShkPrbsNext,self.TranShkValsNext*self.PermShkValsNext)
#        self.hNrmNow      = self.PermGroFac/self.Rfree*(self.ExIncNext + solution_next.hNrm)
#        self.MPCmaxNow    = 1.0/(1.0 + (self.WorstIncPrb**(1.0/self.CRRA))*
#                                        self.PatFac/solution_next.MPCmax)
        #self.cFuncNowCnst = LinearInterp(np.array([0.0, 1.0]), np.array([0.0, 1.0]))


    def prepareToSolve(self):
        '''
        Perform preparatory work before calculating the unconstrained consumption
        function.
        
        Parameters
        ----------
        none
        
        Returns
        -------
        none
        '''
        self.setAndUpdateValues(self.solution_next,self.IncomeAndPrefDstn,self.LaborElas,self.LivPrb,self.DiscFac)


class PrefLaborShockSolver(PrefLaborShockSetup):
    '''
    This class solves a single period of the consumption-saving problem with labor 
    and preference shocks,
    using linear interpolation and without the ability to calculate the value
    function.  
    '''    
    def prepareToCalcEndOfPrdvP(self):
        '''
        Prepare to calculate end-of-period marginal value by creating an array
        of market resources that the agent could have next period, considering
        the grid of end-of-period assets and the distribution of shocks he might
        experience next period.
        
        Parameters
        ----------
        none
        
        Returns
        -------
        aNrmNow : np.array
            A 1D array of end-of-period assets; also stored as attribute of self.
        '''               
        aNrmNow     = np.asarray(self.aXtraGrid) 
        ShkCount    = self.TranShkValsNext.size
        aNrm_temp   = np.tile(aNrmNow,(ShkCount,1))

        # Tile arrays of the income shocks and put them into useful shapes
        aNrmCount         = aNrmNow.shape[0]
        PermShkVals_temp  = (np.tile(self.PermShkValsNext,(aNrmCount,1))).transpose()
        TranShkVals_temp  = (np.tile(self.TranShkValsNext,(aNrmCount,1))).transpose()
        PrefShkVals_temp  = (np.tile(self.PrefShkValsNext,(aNrmCount,1))).transpose()
        ShkPrbs_temp      = (np.tile(self.ShkPrbsNext,(aNrmCount,1))).transpose()
        
        # Get cash on hand next period
        kNrmNext          = self.Rfree/(self.PermGroFac*PermShkVals_temp)*aNrm_temp

        # Store and report the results
        self.PermShkVals_temp  = PermShkVals_temp
        self.TranShkVals_temp  = TranShkVals_temp
        self.ShkPrbs_temp      = ShkPrbs_temp
        self.PrefShkVals_temp  = PrefShkVals_temp
        self.kNrmNext          = kNrmNext 
        self.aNrmNow           = aNrmNow               
        return aNrmNow


    def calcEndOfPrdvP(self):
        '''
        Calculate end-of-period marginal value of assets at each point in aNrmNow.
        Does so by taking a weighted sum of next period marginal values across
        income and preference shocks (in a preconstructed grid self.mNrmNext).
        
        Parameters
        ----------
        none
        
        Returns
        -------
        EndOfPrdvP : np.array
            A 1D array of end-of-period marginal value of assets
        '''        

        EndOfPrdvP  = self.DiscFacEff*self.Rfree*self.PermGroFac**(-self.CRRA)*np.sum(
                      self.PermShkVals_temp**(-self.CRRA)*
                      self.vPfuncNext(self.kNrmNext,self.TranShkVals_temp,self.PrefShkVals_temp)*self.ShkPrbs_temp,axis=0)
        return EndOfPrdvP
                    

    def makeBasicSolution(self,EndOfPrdvP,aNrm,wageShkVals,prefShkVals):
        '''
        Given end of period assets and end of period marginal value, construct
        the basic solution for this period.
        
        Parameters
        ----------
        EndOfPrdvP : np.array
            Array of end-of-period marginal values.
        aNrm : np.array
            Array of end-of-period asset values that yield the marginal values
            in EndOfPrdvP.
        wageShkVals : np.array
            Array of this period transitory wage shock values.
        prefShkVals : np.array
            Array of this period preference shock values.
            
        Returns
        -------
        solution_now : ConsumerSolution
            The solution to this period's consumption-saving problem, with a
            consumption function, marginal value function.
        '''
        num_pref_shocks = len(prefShkVals)
        num_wage_shocks = len(wageShkVals)
        cFuncBaseByPref_list = []
        vPFuncBaseByPref_list = []
        lFuncBaseByPref_list = []
        for i in range(num_wage_shocks):
            cFuncBaseByPref_list.append([])
            vPFuncBaseByPref_list.append([])
            lFuncBaseByPref_list.append([])
            for j in range(num_pref_shocks):
                c_temp = self.uPinv(EndOfPrdvP/prefShkVals[j])
                l_temp = self.LabSupply(wageShkVals[i]*EndOfPrdvP)
                b_temp = c_temp + aNrm - l_temp*wageShkVals[i]
                
                if wageShkVals[i]==0.0:
                    c_temp = np.insert(c_temp,0,0.,axis=-1)
                    l_temp = np.insert(l_temp,0,0.0,axis=-1)   
                    b_temp = np.insert(b_temp,0,0.0,axis=-1) 
                                           
                lFuncBaseByPref_list[i].append(LinearInterp(b_temp,l_temp,lower_extrap=True))
                cFunc1 = LinearInterp(b_temp,c_temp,lower_extrap=True)
                cFunc2 = LinearInterp(b_temp,l_temp*wageShkVals[i]+b_temp,lower_extrap=True)
                cFuncBaseByPref_list[i].append(LowerEnvelope(cFunc1,cFunc2))
                
                pseudo_inverse_vPfunc1 = LinearInterp(b_temp, prefShkVals[j]**(-1.0/self.CRRA)*c_temp,lower_extrap=True)
                pseudo_inverse_vPfunc2 = LinearInterp(b_temp,prefShkVals[j]**(-1.0/self.CRRA)*(l_temp*wageShkVals[i]+b_temp),lower_extrap=True)
                pseudo_inverse_vPfunc = LowerEnvelope(pseudo_inverse_vPfunc1,pseudo_inverse_vPfunc2)
                
                vPFuncBaseByPref_list[i].append(MargValueFunc(pseudo_inverse_vPfunc,self.CRRA))

        cFuncNow = BilinearInterpOnInterp1D(cFuncBaseByPref_list,wageShkVals,prefShkVals)
        vPfuncNow = BilinearInterpOnInterp1D(vPFuncBaseByPref_list,wageShkVals,prefShkVals)
        lFuncNow = BilinearInterpOnInterp1D(lFuncBaseByPref_list,wageShkVals,prefShkVals)

        # Pack up and return the solution
        solution_now = ConsumerSolution(cFunc=cFuncNow,vPfunc=vPfuncNow)
        solution_now.lFunc = lFuncNow
        return solution_now

    def LabDisutility(self,labor):
        '''
        Sets the interior disutility of labor
        '''
        lab_disutil = (labor**(1-1/self.LaborElas))/(1-1/self.LaborElas)
        return lab_disutil
        
    def LabSupply(self,marg_val_labor):
        '''
        Sets the interior disutility of labor
        '''
        lab_supply = marg_val_labor**self.LaborElas
        return lab_supply

    def solve(self):
        '''
        Solves a one period consumption saving problem with risky income.
        
        Parameters
        ----------
        None
            
        Returns
        -------
        solution : ConsumerSolution
            The solution to the one period problem.
        '''
        aNrm       = self.prepareToCalcEndOfPrdvP()           
        EndOfPrdvP = self.calcEndOfPrdvP()                        
        solution   = self.makeBasicSolution(EndOfPrdvP,aNrm,self.WageShkVals,self.PrefShkVals)
        return solution  


def constructLognormalIncomeAndPreferenceProcess(parameters):
    '''
    Generates a list of discrete approximations to the income and preference shock
    process .  Permanent shocks are mean one lognormally distributed 
    with standard deviation PermShkStd.  Transitory shocks
    are mean one lognormally distributed

    Parameters (passed as attributes of the input parameters)
    ----------
    PermShkStd : [float]
        List of standard deviations in log permanent income uncertainty during
        the agent's life.
    PermShkCount : int
        The number of approximation points to be used in the discrete approxima-
        tion to the permanent income shock distribution.
    TranShkStd : [float]
        List of standard deviations in log transitory income uncertainty during
        the agent's life.
    TranShkCount : int
        The number of approximation points to be used in the discrete approxima-
        tion to the permanent income shock distribution.
    PrefShkStd : [float]
        List of standard deviations in log preference uncertainty during
        the agent's life.
    PrefShkCount : int
        The number of approximation points to be used in the discrete approxima-
        tion to the preference shock distribution.

    Returns
    -------
    IncomeAndPrefDstn :  [[np.array]]
        A list with T_cycle elements, each of which is a list of four arrays
        representing a discrete approximation to the income and preference 
        process in a period.
        Order: probabilities, permanent shocks, transitory shocks, preference shocks.
    PermShkDstn : [[np.array]]
        A list with T_cycle elements, each of which is a list of two arrays
        representing a discrete approximation to the permanent income shocks.
    TranShkDstn : [[np.array]]
        A list with T_cycle elements, each of which is a list of two arrays
        representing a discrete approximation to the transitory income shocks.
    PrefShkDstn : [[np.array]]
        A list with T_cycle elements, each of which is a list of two arrays
        representing a discrete approximation to the preference shocks.
    '''
    # Unpack the parameters from the input
    PermShkStd    = parameters.PermShkStd
    PermShkCount  = parameters.PermShkCount
    TranShkStd    = parameters.TranShkStd
    TranShkCount  = parameters.TranShkCount
    PrefShkStd    = parameters.PrefShkStd
    PrefShkCount  = parameters.PrefShkCount
    T_cycle       = 1
    
    IncomeDstn    = [] # Discrete approximations to income process in each period
    PermShkDstn   = [] # Discrete approximations to permanent income shocks
    TranShkDstn   = [] # Discrete approximations to transitory income shocks
    PrefShkDstn   = [] # Discrete approximations to preference shocks

    t=0
    TranShkDstn_t    = approxMeanOneLognormal(N=TranShkCount, sigma=TranShkStd[t], tail_N=0)
    #add in a shock at zero to impose natural borrowing constraint
    TranShkDstn_t = addDiscreteOutcomeConstantMean(TranShkDstn_t, p=0.000000001, x=0.0)
    PermShkDstn_t    = approxMeanOneLognormal(N=PermShkCount, sigma=PermShkStd[t], tail_N=0)
    PrefShkDstn_t    = approxMeanOneLognormal(N=PrefShkCount, sigma=PrefShkStd[t], tail_N=0)
    IncomeDstn.append(combineIndepDstns(PermShkDstn_t,TranShkDstn_t,PrefShkDstn_t)) # mix the independent distributions
    PermShkDstn.append(PermShkDstn_t)
    TranShkDstn.append(TranShkDstn_t)
    PrefShkDstn.append(PrefShkDstn_t)
    return IncomeDstn, PermShkDstn, TranShkDstn, PrefShkDstn  


def solvePrefLaborShock(solution_next,IncomeAndPrefDstn,LivPrb,DiscFac,CRRA,
                      LaborElas,Rfree,PermGroFac,aXtraGrid,WageShkVals,PrefShkVals):
    '''
    Paired with PrefLaborShockSolver and PrefLaborShockSetup
    '''
    solver = PrefLaborShockSolver(solution_next,IncomeAndPrefDstn,LivPrb,DiscFac,CRRA,
                                         LaborElas,Rfree,PermGroFac,aXtraGrid,WageShkVals,PrefShkVals)        
    solver.prepareToSolve()       # Do some preparatory work
    solution_now = solver.solve() # Solve the period
    return solution_now   
                                  

class PrefLaborConsumerType(IndShockConsumerType):
    '''
    A consumer type with idiosyncratic shocks to permanent and transitory income,
    as well as shocks to preferences
    '''        
    # Define some universal values for all consumer types
    cFunc_terminal_      = BilinearInterpOnInterp1D([[LinearInterp(np.array([0.0, 1.0]),np.array([0.0, 1.0])),LinearInterp(np.array([0.0, 1.0]),np.array([0.0, 1.0]))],[LinearInterp(np.array([0.0, 1.0]),np.array([0.0, 1.0])),LinearInterp(np.array([0.0, 1.0]),np.array([0.0, 1.0]))]],np.array([0.0,1.0]),np.array([0.0,1.0])) # c=m in terminal period
    vFunc_terminal_      = BilinearInterpOnInterp1D([[LinearInterp(np.array([0.0, 1.0]),np.array([0.0, 1.0])),LinearInterp(np.array([0.0, 1.0]),np.array([0.0, 1.0]))],[LinearInterp(np.array([0.0, 1.0]),np.array([0.0, 1.0])),LinearInterp(np.array([0.0, 1.0]),np.array([0.0, 1.0]))]],np.array([0.0,1.0]),np.array([0.0,1.0])) # This is overwritten
    solution_terminal_   = ConsumerSolution(cFunc = cFunc_terminal_,
                                            vFunc = vFunc_terminal_)
    time_inv_ = PerfForesightConsumerType.time_inv_ + ['LaborElas','PrefShkVals','WageShkVals']
    shock_vars_ = ['PermShkNow','TranShkNow','PrefShkNow']
    
    def __init__(self,cycles=1,time_flow=True,**kwds):
        '''
        Instantiate a new ConsumerType with given data.
        See ConsumerParameters.init_idiosyncratic_shocks for a dictionary of
        the keywords that should be passed to the constructor.
        
        Parameters
        ----------
        cycles : int
            Number of times the sequence of periods should be solved.
        time_flow : boolean
            Whether time is currently "flowing" forward for this instance.
        
        Returns
        -------
        None
        '''       
        # Initialize a basic AgentType
        IndShockConsumerType.__init__(self,cycles=cycles,time_flow=time_flow,**kwds)

        # Add consumer-type specific objects, copying to create independent versions
        self.solveOnePeriod = solvePrefLaborShock # idiosyncratic shocks solver
        self.update() # Make assets grid, income process, terminal solution
        
    def reset(self):
        self.initializeSim()
        self.t_age = drawDiscrete(self.AgentCount,P=self.AgeDstn,X=np.arange(self.AgeDstn.size),exact_match=False,seed=self.RNG.randint(0,2**31-1)).astype(int)
        self.t_cycle = copy(self.t_age)
        
    def marketAction(self):
        self.simulate(1)
                      
    def updateIncomeProcess(self):
        '''
        Updates this agent's income process based on his own attributes.  The
        function that generates the discrete income process can be swapped out
        for a different process.
        
        Parameters
        ----------
        none
        
        Returns:
        -----------
        none
        '''
        original_time = self.time_flow
        self.timeFwd()
        IncomeAndPrefDstn, PermShkDstn, TranShkDstn, PrefShkDstn = constructLognormalIncomeAndPreferenceProcess(self)
        self.IncomeAndPrefDstn = IncomeAndPrefDstn
        self.PermShkDstn = PermShkDstn
        self.TranShkDstn = TranShkDstn
        self.PrefShkDstn = TranShkDstn
        self.PrefShkVals = PrefShkDstn[0][1]
        self.WageShkVals = TranShkDstn[0][1]
        self.addToTimeVary('IncomeAndPrefDstn','PermShkDstn','TranShkDstn','PrefShkDstn')
        if not original_time:
            self.timeRev()
            
    def updateSolutionTerminal(self):
        '''
        Update the terminal period solution.  
        
        Parameters
        ----------
        none
        
        Returns
        -------
        none
        '''
        #self.solution_terminal.vFunc   = ValueFunc(self.cFunc_terminal_,self.CRRA)
        terminal_instance = LinearInterp(np.array([0.0, 100.0]),np.array([0.01, 0.0]),lower_extrap=True)
        self.solution_terminal.vPfunc  = BilinearInterpOnInterp1D([[terminal_instance,terminal_instance],[terminal_instance,terminal_instance]],np.array([0.0,2.0]),np.array([0.0,2.0]))
        #self.solution_terminal.vPPfunc = MargMargValueFunc(self.cFunc_terminal_,self.CRRA)

    def getShocks(self):
        '''
        Gets permanent and transitory income shocks for this period.  Samples from IncomeDstn for
        each period in the cycle.
        
        Parameters
        ----------
        None
        
        Returns
        -------
        None
        '''
        PermShkNow = np.zeros(self.AgentCount) # Initialize shock arrays
        TranShkNow = np.zeros(self.AgentCount)
        PrefShkNow = np.zeros(self.AgentCount)
        newborn = self.t_age == 0
        for t in range(self.T_cycle):
            these = t == self.t_cycle
            N = np.sum(these)
            if N > 0:
                IncomeDstnNow    = self.IncomeAndPrefDstn[t-1] # set current income distribution
                PermGroFacNow    = self.PermGroFac[t-1] # and permanent growth factor
                Indices          = np.arange(IncomeDstnNow[0].size) # just a list of integers
                # Get random draws of income shocks from the discrete distribution
                EventDraws       = drawDiscrete(N,X=Indices,P=IncomeDstnNow[0],exact_match=False,seed=self.RNG.randint(0,2**31-1))
                PermShkNow[these] = IncomeDstnNow[1][EventDraws]*PermGroFacNow # permanent "shock" includes expected growth
                TranShkNow[these] = IncomeDstnNow[2][EventDraws]
                PrefShkNow[these] = IncomeDstnNow[3][EventDraws]
        
        # That procedure used the *last* period in the sequence for newborns, but that's not right
        # Redraw shocks for newborns, using the *first* period in the sequence.  Approximation.
        N = np.sum(newborn)
        if N > 0:
            these = newborn
            IncomeDstnNow    = self.IncomeAndPrefDstn[0] # set current income distribution
            PermGroFacNow    = self.PermGroFac[0] # and permanent growth factor
            Indices          = np.arange(IncomeDstnNow[0].size) # just a list of integers
            # Get random draws of income shocks from the discrete distribution
            EventDraws       = drawDiscrete(N,X=Indices,P=IncomeDstnNow[0],exact_match=False,seed=self.RNG.randint(0,2**31-1))
            PermShkNow[these] = IncomeDstnNow[1][EventDraws]*PermGroFacNow # permanent "shock" includes expected growth
            TranShkNow[these] = IncomeDstnNow[2][EventDraws]
            PrefShkNow[these] = IncomeDstnNow[3][EventDraws]
#        PermShkNow[newborn] = 1.0
        TranShkNow[newborn] = 1.0
              
        # Store the shocks in self
        self.EmpNow = np.ones(self.AgentCount,dtype=bool)
        self.EmpNow[TranShkNow == self.IncUnemp] = False
        self.PermShkNow = PermShkNow
        self.TranShkNow = TranShkNow
        self.PrefShkNow = PrefShkNow
                               
    def getStates(self):
        '''
        Calculates updated values of normalized market resources and permanent income level for each
        agent.  Uses pLvlNow, aNrmNow, PermShkNow, TranShkNow.
        
        Parameters
        ----------
        None
        
        Returns
        -------
        None
        '''
        pLvlPrev = self.pLvlNow
        aNrmPrev = self.aNrmNow
        RfreeNow = self.getRfree()
        
        # Calculate new states: normalized market resources and permanent income level
        self.pLvlNow = pLvlPrev*self.PermShkNow # Updated permanent income level
        ReffNow      = RfreeNow/self.PermShkNow # "Effective" interest factor on normalized assets
        self.bNrmNow = ReffNow*aNrmPrev         # Bank balances before labor income
        return None
        
    def getControls(self):
        '''
        Calculates consumption for each consumer of this type using the consumption functions.
        
        Parameters
        ----------
        None
        
        Returns
        -------
        None
        '''
        cNrmNow = np.zeros(self.AgentCount) + np.nan
        MPCnow  = np.zeros(self.AgentCount) + np.nan
        lNow  = np.zeros(self.AgentCount) + np.nan
        for t in range(self.T_cycle):
            these = t == self.t_cycle
            cNrmNow[these] = self.solution[t].cFunc(self.bNrmNow[these],self.TranShkNow,self.PrefShkNow)
            MPCnow[these] = self.solution[t].cFunc.derivativeX(self.bNrmNow[these],self.TranShkNow,self.PrefShkNow)
            lNow[these] = self.solution[t].lFunc(self.bNrmNow[these],self.TranShkNow,self.PrefShkNow)
        self.cNrmNow = cNrmNow
        self.MPCnow = MPCnow
        self.lNow = lNow
        self.lIncomeLvl = lNow*self.TranShkNow*self.pLvlNow
        self.cLvlNow = cNrmNow*self.pLvlNow
        return None
        
    def getPostStates(self):
        '''
        Calculates end-of-period assets for each consumer of this type.
        
        Parameters
        ----------
        None
        
        Returns
        -------
        None
        '''
        self.aNrmNow = self.bNrmNow + self.lNow*self.TranShkNow - self.cNrmNow
        self.aLvlNow = self.aNrmNow*self.pLvlNow   # Useful in some cases to precalculate asset level
        return None


####################################################################################################     
    
if __name__ == '__main__':
    import SetupParamsSimulations as Params
    from HARKutilities import plotFuncsDer, plotFuncs
    from time import clock
    mystr = lambda number : "{:.4f}".format(number)

    do_simulation           = True
    
    # Make and solve an example consumer with preference shocks
    init_pref = copy(Params.init_infinite)
    init_pref['TranShkStd'] = [0.06]
    init_pref['PermShkStd'] = [0.06]
    
    init_pref['PrefShkStd'] = [0.00001]
    init_pref['PrefShkCount'] = 5
    init_pref['LaborElas'] = 0.1
    
    init_pref['DiscFac'] = 0.9
    #init_pref['LivPrb'] = [1.0]
    
    #replicate no labor,no pref shock
#    init_pref['PrefShkStd'] = [0.000001]
#    init_pref['PrefShkCount'] = 5
#    init_pref['LaborElas'] = 0.000001
    
    PrefShockExample = PrefLaborConsumerType(**init_pref)
    PrefShockExample.cycles = 0 # Make this type have an infinite horizon
    
    start_time = clock()
    PrefShockExample.solve()
    end_time = clock()
    print('Solving a consumer with preference shocks took ' + mystr(end_time-start_time) + ' seconds.')
    PrefShockExample.unpackcFunc()
    PrefShockExample.timeFwd()
    
    print('Consumption function at each wage shock gridpoint:')
    b_grid = np.linspace(0,5,200)
    PrefShockExample.unpackcFunc()
    plt.figure()
    for W in PrefShockExample.WageShkVals.tolist():
        c_at_this_W = PrefShockExample.cFunc[0](b_grid,W*np.ones_like(b_grid),np.ones_like(b_grid))
        plt.plot(b_grid,c_at_this_W)
    plt.show()
    plt.figure()
    for W in PrefShockExample.WageShkVals.tolist():
        if W!=0:
            l_at_this_W = PrefShockExample.solution[0].lFunc(b_grid,W*np.ones_like(b_grid),1.0*np.ones_like(b_grid))
            plt.plot(b_grid,l_at_this_W)
    plt.show()
    
    plt.figure()
    for W in PrefShockExample.WageShkVals.tolist():
        c_at_this_W = PrefShockExample.cFunc[0](b_grid,W*np.ones_like(b_grid),np.ones_like(b_grid))
        l_at_this_W = PrefShockExample.solution[0].lFunc(b_grid,W*np.ones_like(b_grid),1.0*np.ones_like(b_grid))
        plt.plot(b_grid+W*l_at_this_W,c_at_this_W)
    plt.show()
    
    plt.figure()
    for P in PrefShockExample.PrefShkVals.tolist():
        c_at_this_P = PrefShockExample.cFunc[0](b_grid,np.ones_like(b_grid),P*np.ones_like(b_grid))
        l_at_this_P = PrefShockExample.solution[0].lFunc(b_grid,np.ones_like(b_grid),P**np.ones_like(b_grid))
        plt.plot(b_grid+l_at_this_P,c_at_this_P)
    plt.show()
    
    
# Now simulate
    if do_simulation:
        PrefShockExample.T_sim = 200
        PrefShockExample.track_vars = ['bNrmNow','cNrmNow','MPCnow','lNow','TranShkNow','PrefShkNow','pLvlNow','cLvlNow','lIncomeLvl','t_age']
        PrefShockExample.initializeSim()
        PrefShockExample.simulate()   

        AggB = np.mean(PrefShockExample.bNrmNow_hist,axis=1)   
        plt.figure()
        plt.plot(AggB)
        plt.show()
        AggMPC = np.mean(PrefShockExample.MPCnow_hist,axis=1) 
        plt.figure()
        plt.plot(AggMPC)
        plt.show()
        
        
    ignore_periods = 50
    extra = 0
    n1 = 3+extra
    n2 = 5+extra
    logLaborInc = np.log(PrefShockExample.lIncomeLvl_hist)
    logCons = np.log(PrefShockExample.cLvlNow_hist)
    IncGrowth1 = logLaborInc[ignore_periods+n1:,:]-logLaborInc[ignore_periods:-n1,:]
    IncGrowth2 = logLaborInc[ignore_periods+n2:,:]-logLaborInc[ignore_periods:-n2,:]
    ConGrowth1 = logCons[ignore_periods+n1:,:]-logCons[ignore_periods:-n1,:]
    ConGrowth2 = logCons[ignore_periods+n2:,:]-logCons[ignore_periods:-n2,:]
    
    varIncGrowth1 = np.var(IncGrowth1)
    varIncGrowth2 = np.var(IncGrowth2)
    impliedPermVar = (varIncGrowth2-varIncGrowth1)/(n2-n1)
    impliedPermStd = impliedPermVar**0.5
    impliedTranVar = (varIncGrowth1 - n1*impliedPermVar)/2.0
    impliedTranStd = impliedTranVar**0.5
    
    covGrowth1 = np.cov(IncGrowth1.flatten(),ConGrowth1.flatten())[1][0]
    covGrowth2 = np.cov(IncGrowth2.flatten(),ConGrowth2.flatten())[1][0]
    
    phi = (covGrowth2-covGrowth1)/((n2-n1)*impliedPermVar)
    psi = (covGrowth1 - n1*phi*impliedPermVar)/(2.0*impliedTranVar)
    
    print('Carroll-Samwick Results')
    print('phi')
    print(phi)
    print('psi')
    print(psi)
    print('impliedPermStd')
    print(impliedPermStd)
    print('impliedTranStd')
    print(impliedTranStd)
    
    IncGrowthOnePeriod = logLaborInc[ignore_periods+1:,:]-logLaborInc[ignore_periods:-1,:]
    ConGrowthOnePeriod = logCons[ignore_periods+1:,:]-logCons[ignore_periods:-1,:]
    covYYnext = np.cov(IncGrowthOnePeriod[2:,].flatten(),IncGrowthOnePeriod[1:-1,].flatten())[1][0]
    covCYnext = np.cov(IncGrowthOnePeriod[2:,].flatten(),ConGrowthOnePeriod[1:-1,].flatten())[1][0]
    
    IncGrowthThreePeriod = logLaborInc[ignore_periods+4:,:]-logLaborInc[ignore_periods:-4,:]
    covYYnext3 = np.cov(IncGrowthThreePeriod.flatten(),IncGrowthOnePeriod[2:-1,].flatten())[1][0]
    covCYnext3 = np.cov(IncGrowthThreePeriod.flatten(),ConGrowthOnePeriod[2:-1,].flatten())[1][0]
    
    psiBPP = covCYnext/covYYnext
    phiBPP = covCYnext3/covYYnext3
    
    impliedPermStdBPP = covYYnext3**0.5
    impliedTranStdBPP = (-covYYnext)**0.5
    
    print('BPP Results')
    print('phi')
    print(phiBPP)
    print('psi')
    print(psiBPP)
    print('impliedPermStd')
    print(impliedPermStdBPP)
    print('impliedTranStd')
    print(impliedTranStdBPP)
    
    MeanAggMPC = np.mean(AggMPC[ignore_periods:,])
    print('MeanAggMPC')
    print(MeanAggMPC)
    MeanAggB = np.mean(AggB[ignore_periods:,])
    print('MeanAggB')
    print(MeanAggB)
    
    
    #***********************
    # Do some loops
    
    num_loops = 5
    LaborElas_loops = np.linspace(0.00000001,0.8, num_loops)
    phi_loop = np.zeros(num_loops)
    psi_loop = np.zeros(num_loops)
    sigmaq_loop = np.zeros(num_loops)
    sigmap_loop = np.zeros(num_loops)
    
    phiBPP_loop = np.zeros(num_loops)
    psiBPP_loop = np.zeros(num_loops)
    sigmaqBPP_loop = np.zeros(num_loops)
    sigmapBPP_loop = np.zeros(num_loops)
    
    MeanAggMPC_loop = np.zeros(num_loops)
    MeanAggB_loop = np.zeros(num_loops)
    ConsGrowthVar_loop = np.zeros(num_loops)
    
    for i in range(num_loops):
    #    init_pref['LaborElas'] = LaborElas_loops[i]
        init_pref['PrefShkStd'] = [LaborElas_loops[i]]
        
       
        PrefShockExample = PrefLaborConsumerType(**init_pref)
        PrefShockExample.cycles = 0 # Make this type have an infinite horizon
        PrefShockExample.solve()
        
        PrefShockExample.T_sim = 200
        PrefShockExample.track_vars = ['bNrmNow','cNrmNow','MPCnow','lNow','TranShkNow','PrefShkNow','pLvlNow','cLvlNow','lIncomeLvl','t_age']
        PrefShockExample.initializeSim()
        PrefShockExample.simulate() 
        
        ignore_periods = 50
        extra = 0
        n1 = 3+extra
        n2 = 5+extra
        logLaborInc = np.log(PrefShockExample.lIncomeLvl_hist)
        logCons = np.log(PrefShockExample.cLvlNow_hist)
        IncGrowth1 = logLaborInc[ignore_periods+n1:,:]-logLaborInc[ignore_periods:-n1,:]
        IncGrowth2 = logLaborInc[ignore_periods+n2:,:]-logLaborInc[ignore_periods:-n2,:]
        ConGrowth1 = logCons[ignore_periods+n1:,:]-logCons[ignore_periods:-n1,:]
        ConGrowth2 = logCons[ignore_periods+n2:,:]-logCons[ignore_periods:-n2,:]
        
        varIncGrowth1 = np.var(IncGrowth1)
        varIncGrowth2 = np.var(IncGrowth2)
        impliedPermVar = (varIncGrowth2-varIncGrowth1)/(n2-n1)
        impliedPermStd = impliedPermVar**0.5
        impliedTranVar = (varIncGrowth1 - n1*impliedPermVar)/2.0
        impliedTranStd = impliedTranVar**0.5
        
        covGrowth1 = np.cov(IncGrowth1.flatten(),ConGrowth1.flatten())[1][0]
        covGrowth2 = np.cov(IncGrowth2.flatten(),ConGrowth2.flatten())[1][0]
        
        phi_loop[i] = (covGrowth2-covGrowth1)/((n2-n1)*impliedPermVar)
        psi_loop[i] = (covGrowth1 - n1*phi*impliedPermVar)/(2.0*impliedTranVar)
        sigmaq_loop[i] = impliedTranStd
        sigmap_loop[i] = impliedPermStd
        
        
        IncGrowthOnePeriod = logLaborInc[ignore_periods+1:,:]-logLaborInc[ignore_periods:-1,:]
        ConGrowthOnePeriod = logCons[ignore_periods+1:,:]-logCons[ignore_periods:-1,:]
        covYYnext = np.cov(IncGrowthOnePeriod[2:,].flatten(),IncGrowthOnePeriod[1:-1,].flatten())[1][0]
        covCYnext = np.cov(IncGrowthOnePeriod[2:,].flatten(),ConGrowthOnePeriod[1:-1,].flatten())[1][0]
        
        IncGrowthThreePeriod = logLaborInc[ignore_periods+4:,:]-logLaborInc[ignore_periods:-4,:]
        covYYnext3 = np.cov(IncGrowthThreePeriod.flatten(),IncGrowthOnePeriod[2:-1,].flatten())[1][0]
        covCYnext3 = np.cov(IncGrowthThreePeriod.flatten(),ConGrowthOnePeriod[2:-1,].flatten())[1][0]
        
        psiBPP_loop[i] = covCYnext/covYYnext
        phiBPP_loop[i] = covCYnext3/covYYnext3
        
        impliedPermStdBPP = covYYnext3**0.5
        impliedTranStdBPP = (-covYYnext)**0.5
        
        sigmaqBPP_loop[i] = impliedTranStdBPP
        sigmapBPP_loop[i] = impliedPermStdBPP
        
        
        AggB = np.mean(PrefShockExample.bNrmNow_hist,axis=1)   
        AggMPC = np.mean(PrefShockExample.MPCnow_hist,axis=1) 
        
        MeanAggMPC_loop[i] = np.mean(AggMPC[ignore_periods:,])
        MeanAggB_loop[i] = np.mean(AggB[ignore_periods:,])
        
        ConsGrowthVar_loop[i] = np.var(ConGrowthOnePeriod)