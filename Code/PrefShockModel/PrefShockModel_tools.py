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
    C_all = np.concatenate([cLvlAll_hist[sample_length*i:sample_length*(i+1),:] for i in range(int(not_newborns.shape[0]/sample_length))],axis=1)
    Y_all = np.concatenate([yLvlAll_hist[sample_length*i:sample_length*(i+1),:] for i in range(int(not_newborns.shape[0]/sample_length))],axis=1)
    B_all = np.concatenate([bLvlAll_hist[sample_length*i:sample_length*(i+1),:] for i in range(int(not_newborns.shape[0]/sample_length))],axis=1)
    MPC_all = np.concatenate([MPCAll_hist[sample_length*i:sample_length*(i+1),:] for i in range(int(not_newborns.shape[0]/sample_length))],axis=1)
    not_newborns_all = np.concatenate([not_newborns[sample_length*i:sample_length*(i+1),:] for i in range(int(not_newborns.shape[0]/sample_length))],axis=1)
     
    #delete observations containing a newborn
    not_newborns_all = not_newborns_all.all(axis=0)
    C_all = C_all[:,not_newborns_all]
    Y_all = Y_all[:,not_newborns_all]
    B_all = B_all[:,not_newborns_all]
    MPC_all = MPC_all[:,not_newborns_all]
    
    #sum up over periods_per_year
    Cons_sample = np.vstack([np.sum(C_all[periods_per_year*i:periods_per_year*(i+1),:],axis=0)for i in range(years_in_sample)])
    Inc_sample = np.vstack([np.sum(Y_all[periods_per_year*i:periods_per_year*(i+1),:],axis=0)for i in range(years_in_sample)])
    B_sample = np.vstack([np.mean(B_all[periods_per_year*i:periods_per_year*(i+1),:],axis=0)for i in range(years_in_sample)])
    MPC_sample = np.vstack([np.mean(MPC_all[periods_per_year*i:periods_per_year*(i+1),:],axis=0)for i in range(years_in_sample)])  

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

#### Save arrays to LaTex tables
def PrintLaborTables(estimate_array,num_labelas_vals,num_pref_vals,labor_elas,pref_vals,filename,width=0.45,name=""):
    output = "\\begin{minipage}{" + str(width) + "\\textwidth}\n"
   
    output += "\\resizebox{\\textwidth}{!}{\\begin{tabular}{lc|*{" + str(num_labelas_vals) + "}{c}}  \n"
    output += "& " + name 
    for i in range(int(num_labelas_vals/2)):
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