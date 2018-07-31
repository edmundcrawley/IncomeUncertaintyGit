"""
Tools used for PrefShockModel
"""
import numpy as np
import statsmodels.api as sm


def SelectMicroSample(Economy,years_in_sample=12,periods_per_year=4,logs=False):
    '''
    Selects consumption and income data to use in estimation
    Sorts into sample of a fixed length and eliminates agents who die in that
    period
    
    Parameters
    ----------
    Economy : Economy
        Must be already simulated and contain histories for cNrmNow and lIncomeLvl
    years_in_sample : int
        Number of years in the simulated sample data (choose 12 to replicate Denmark)
    period_per_year : int
        Number of periods in a year (4 is quarterly)
    logs : bool
        Whether or not to take logs of Income and Consumption
        
    Returns
    -------
    Cons_sample : np.array
        array containing log consumption histories
    Inc_sample : np.array
        array containing log income histories
    B_sample : np.array
        array containing mean bond holdings over the year
    MPC_sample : np.array
        array containing MPCs (over a period uniformly distributed over 1 period)
    '''
    not_newborns = (np.concatenate([this_type.t_age_hist for this_type in Economy.agents],axis=1) > 1)
    cLvlAll_hist = np.concatenate([this_type.cNrmNow_hist*this_type.pLvlNow_hist for this_type in Economy.agents],axis=1)
    yLvlAll_hist = np.concatenate([this_type.lIncomeLvl_hist for this_type in Economy.agents],axis=1)
    bLvlAll_hist = np.concatenate([this_type.bNrmNow_hist*this_type.pLvlNow_hist for this_type in Economy.agents],axis=1)
    # Calculate the MPC for a time equivalent to a shock occuring uniformly in the year
    for this_type in Economy.agents:
        cash_remaining = (1.0 - this_type.MPCnow_hist[periods_per_year:])
        this_type.this_MPC_hist = 1.0 - cash_remaining/periods_per_year
        for i in range(periods_per_year-1):
            cash_remaining = cash_remaining*(1.0 - this_type.MPCnow_hist[periods_per_year-(i+1):-(i+1)])
            this_type.this_MPC_hist = this_type.this_MPC_hist - cash_remaining/periods_per_year
    MPCAll_hist = np.concatenate([this_type.this_MPC_hist for this_type in Economy.agents],axis=1)
    MPCAll_hist = np.append(np.zeros((periods_per_year,MPCAll_hist.shape[1])),MPCAll_hist,0)
    
    ignore_periods = Economy.ignore_periods
    #ignore the periods at beginning of simulation
    for array in [not_newborns,cLvlAll_hist,yLvlAll_hist,bLvlAll_hist,MPCAll_hist]:
        array = array[ignore_periods:,:]

    sample_length = years_in_sample*periods_per_year
    C_all = np.concatenate([cLvlAll_hist[sample_length*i:sample_length*(i+1),:] for i in range(not_newborns.shape[0]/sample_length)],axis=1)
    Y_all = np.concatenate([yLvlAll_hist[sample_length*i:sample_length*(i+1),:] for i in range(not_newborns.shape[0]/sample_length)],axis=1)
    B_all = np.concatenate([bLvlAll_hist[sample_length*i:sample_length*(i+1),:] for i in range(not_newborns.shape[0]/sample_length)],axis=1)
    MPC_all = np.concatenate([MPCAll_hist[sample_length*i:sample_length*(i+1),:] for i in range(not_newborns.shape[0]/sample_length)],axis=1)
    not_newborns_all = np.concatenate([not_newborns[sample_length*i:sample_length*(i+1),:] for i in range(not_newborns.shape[0]/sample_length)],axis=1)
     
    #delete observations containing a newborn
    not_newborns_all = not_newborns_all.all(axis=0)
    C_all = C_all[:,not_newborns_all]
    Y_all = Y_all[:,not_newborns_all]
    B_all = B_all[:,not_newborns_all]
    MPC_all = MPC_all[:,not_newborns_all]
    
    #sum up over periods_per_year
    Cons_sample = np.stack([np.sum(C_all[periods_per_year*i:periods_per_year*(i+1),:],axis=0)for i in range(years_in_sample)])
    Inc_sample = np.stack([np.sum(Y_all[periods_per_year*i:periods_per_year*(i+1),:],axis=0)for i in range(years_in_sample)])
    B_sample = np.stack([np.mean(B_all[periods_per_year*i:periods_per_year*(i+1),:],axis=0)for i in range(years_in_sample)])
    MPC_sample = np.stack([np.mean(MPC_all[periods_per_year*i:periods_per_year*(i+1),:],axis=0)for i in range(years_in_sample)])  

    if logs:
        Cons_sample = np.log(Cons_sample)
        Inc_sample = np.log(Inc_sample)

    return Cons_sample, Inc_sample, B_sample, MPC_sample

def CS_estimation(Cons_sample, Inc_sample, n1=3, n2=5):
    '''
    Estimates income variances a la Carroll Samwick (1997)
    Also calculates consumption responses in similar manner
    
    Parameters
    ----------
    Cons_sample : np.array
        Usually output from SelectMicroSample. Time series (rows) of micro
        consumption data, time aggregated
    Inc_sample : np.array
        Usually output from SelectMicroSample. Time series (rows) of micro
        income data, time aggregated
    n1 : int
        Growth period over which first variance is calculated
    n2 : int
        Growth period over which second variance is calculated
        
    Returns
    -------
    impliedPermVar : float
        Implied permanent income variance
    impliedTranVar : float
        Implied transitory income variance
    phi : float
        Consumption response to permanent shocks
    psi : float
        Consumption response to transitory shocks
    '''
    time_agg_adj_fac =0.3125    #this is the value to adjust over for 4 quarters
    
    y_growth1 = Inc_sample[n1:,:]-Inc_sample[:-n1,:]
    y_growth2 = Inc_sample[n2:,:]-Inc_sample[:-n2,:]
    c_growth1 = Cons_sample[n1:,:]-Cons_sample[:-n1,:]
    c_growth2 = Cons_sample[n2:,:]-Cons_sample[:-n2,:]
    
    var_y_growth1 = np.var(y_growth1)
    var_y_growth2 = np.var(y_growth2)   
    impliedPermVar = (var_y_growth2-var_y_growth1)/(n2-n1)
    impliedTranVar = (var_y_growth1 - (n1-time_agg_adj_fac)*impliedPermVar)/2.0
    
    covGrowth1 = np.cov(y_growth1.flatten(),c_growth1.flatten())[1][0]
    covGrowth2 = np.cov(y_growth2.flatten(),c_growth2.flatten())[1][0]
    
    phi = (covGrowth2-covGrowth1)/((n2-n1)*impliedPermVar)
    psi = (covGrowth1 - (n1-time_agg_adj_fac)*phi*impliedPermVar)/(2.0*impliedTranVar)
    
    return impliedPermVar,impliedTranVar,phi,psi

def BasicRegressionTables(Cons_sample, Inc_sample,max_diff=10,filename=False):
    result = np.zeros(max_diff)
    for i in range(max_diff):
        diff_C = Cons_sample[i+1:]-Cons_sample[:-(i+1)]
        diff_Y = Inc_sample[i+1:]-Inc_sample[:-(i+1)]
        model = sm.OLS(diff_C.flatten(),diff_Y.flatten())
        solve_model = model.fit()
        result[i] = solve_model.params[0]
    if filename!=False:
        output = ""
        for i in range(max_diff):
            output += str(result[i])
            if i<max_diff-1:
                output += ","
            else:
                output += "\n"
        with open('./Results/' + filename + '.txt','w') as f:
            f.write(output)
            f.close()
    return result


#
#
##############################CODE BELOW NEEDS TO BE REDONE
#
##Print calibration table
#paper_output = "\\begin{minipage}{\\textwidth}\n"
#paper_output += "  \\begin{table}\n"
#paper_output += "    \\caption{Calibration}\label{table:calibration}\n"
#
#paper_output += "\\begin{tabular}{cd{5}l}  \n"
#paper_output += "\\\\ \\toprule  \n"
## Idiosyncratic shock parameters
#paper_output += "\multicolumn{3}{c}{ \\textbf{Calibrated Parameters} }  \n"
#paper_output += "\\\\ $\sigma_{\\theta}^{2}$    & " + "{:.3f}".format(Params.init_infinite['TranShkStd'][0]**2) +"     & Variance Tran Shocks (=$4 \\times$ {:.3f}".format(0.25*Params.init_infinite['TranShkStd'][0]**2) +" Annual) \n"
#paper_output += "\\\\ $\sigma_{\psi}^{2}$      &" + "{:.3f}".format(Params.init_infinite['PermShkStd'][0]**2) +"      & Variance Perm Shocks (=$0.25 \\times$ {:.3f}".format(4.0*Params.init_infinite['PermShkStd'][0]**2) +" Annual) \n"
#paper_output += "\\\\ $\wp$                    & " + "{:.3f}".format(Params.init_infinite['UnempPrb']) +"  & Probability of Unemployment Spell \n"
#paper_output += "\\\\ $\\theta^u$                    & " + "{:.3f}".format(Params.init_infinite['IncUnemp']) +"  & Income in Unemployment Spell \n"
#paper_output += "\\\\ $\PDies$             & " + "{:.3f}".format(1.0-Params.init_infinite['LivPrb'][0]) +"  & Probability of Mortality \n"
#paper_output += "\\\\ $\\rho$ & "+ "{:.0f}".format(Params.init_infinite['CRRA']) +". & Coefficient of Relative Risk Aversion \n"
#
#paper_output += "\\\\ $R$ & " + "{:.3f}".format(Params.init_infinite['Rfree']) + "& Quarterly Interest Rate \n"
#
#paper_output += "\\\\ \\midrule  \n"
#paper_output += "\multicolumn{3}{c}{ \\textbf{Estimated Parameters} }  \n"
#paper_output += "\\\\ $\\beta^c$ &  " + "{:.3f}".format(center_estimate) +" & Mean discount factor \n"
#paper_output += "\\\\ $\\nabla$ &  " + "{:.3f}".format(spread_estimate) +" & Discount factor spread\n"
#
#paper_output += "\\\\ \\bottomrule  \n"
#paper_output += "\end{tabular}\n"
#paper_output += "\end{table}\n"
#paper_output += "\end{minipage}\n"
#with open('./Tables/CalibrationTable.tex','w') as f:
#    f.write(paper_output)
#    f.close()
#

