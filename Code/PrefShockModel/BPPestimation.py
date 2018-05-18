# -*- coding: utf-8 -*-
"""
Functions to do BPP style estimations on simulated data
"""
import numpy as np
import statsmodels.api as sm

def SelectMicroSample(Economy,years_in_sample,periods_per_year,do_labor):
    '''
    Selects consumption and income data to use in estimation
    Sorts into sample of a fixed length and eliminates agents who die in that
    period
    
    Parameters
    ----------
    Economy : Economy
        Must be already simulated and contain histories for cNrmNow, TranShkNow
        and pLvlNow
    period_per_year : int
        Number of periods in a year (the period of study)
        
    Returns
    -------
    log_C_all : np.array
        array containing log consumption histories
    log_Y_all : np.array
        array containing log income histories
    B_agg : np.array
        array containing mean bond holdings over the year
    '''
       
    not_newborns = (np.concatenate([this_type.t_age_hist for this_type in Economy.agents],axis=1) > 1)
    cLvlAll_hist = np.concatenate([this_type.cNrmNow_hist*this_type.pLvlNow_hist for this_type in Economy.agents],axis=1)
    if do_labor:
        yLvlAll_hist = np.concatenate([this_type.lIncomeLvl_hist for this_type in Economy.agents],axis=1)
    else:
        yLvlAll_hist = np.concatenate([this_type.TranShkNow_hist*this_type.pLvlNow_hist for this_type in Economy.agents],axis=1)
    bLvlAll_hist = np.concatenate([this_type.bNrmNow_hist*this_type.pLvlNow_hist for this_type in Economy.agents],axis=1)
    
    ignore_periods = Economy.ignore_periods
    #ignore the periods at beginning of simulation
    for array in [not_newborns,cLvlAll_hist,yLvlAll_hist,bLvlAll_hist]:
        array = array[ignore_periods:,:]

    sample_length = years_in_sample*periods_per_year
    C_all = np.concatenate([cLvlAll_hist[sample_length*i:sample_length*(i+1),:] for i in range(not_newborns.shape[0]/sample_length)],axis=1)
    Y_all = np.concatenate([yLvlAll_hist[sample_length*i:sample_length*(i+1),:] for i in range(not_newborns.shape[0]/sample_length)],axis=1)
    B_all = np.concatenate([bLvlAll_hist[sample_length*i:sample_length*(i+1),:] for i in range(not_newborns.shape[0]/sample_length)],axis=1)
    not_newborns_all = np.concatenate([not_newborns[sample_length*i:sample_length*(i+1),:] for i in range(not_newborns.shape[0]/sample_length)],axis=1)
     
    #delete observations containing a newborn
    not_newborns_all = not_newborns_all.all(axis=0)
    C_all = C_all[:,not_newborns_all]
    Y_all = Y_all[:,not_newborns_all]
    B_all = B_all[:,not_newborns_all]
    
    #sum up over periods_per_year
    C_agg = np.stack([np.sum(C_all[periods_per_year*i:periods_per_year*(i+1),:],axis=0)for i in range(years_in_sample)])
    Y_agg = np.stack([np.sum(Y_all[periods_per_year*i:periods_per_year*(i+1),:],axis=0)for i in range(years_in_sample)])
    B_agg = np.stack([np.mean(B_all[periods_per_year*i:periods_per_year*(i+1),:],axis=0)for i in range(years_in_sample)])
 
    log_C_agg = np.log(C_agg)
    log_Y_agg = np.log(Y_agg)
    
    return log_C_agg, log_Y_agg, B_agg

def CS_estimation(log_C_agg, log_Y_agg,n1,n2):
    '''
    Estimates income variances a la Carroll Samwick (1997)
    Also calculates consumption responses in similar manner
    
    Parameters
    ----------
    log_C_agg : np.array
        Usually output from SelectMicroSample. Time series (rows) of micro
        consumption data, time aggregated
    log_Y_agg : np.array
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
    
    time_agg_adj_fac = 1.0/3.0
    #time_agg_adj_fac = 0.0
    
    y_growth1 = log_Y_agg[n1:,:]-log_Y_agg[:-n1,:]
    y_growth2 = log_Y_agg[n2:,:]-log_Y_agg[:-n2,:]
    c_growth1 = log_C_agg[n1:,:]-log_C_agg[:-n1,:]
    c_growth2 = log_C_agg[n2:,:]-log_C_agg[:-n2,:]
    
    var_y_growth1 = np.var(y_growth1)
    var_y_growth2 = np.var(y_growth2)
    var_c_growth1 = np.var(c_growth1)
    var_c_growth2 = np.var(c_growth2)
    
    impliedPermVar = (var_y_growth2-var_y_growth1)/(n2-n1)
    impliedPermStd = impliedPermVar**0.5
    impliedTranVar = (var_y_growth1 - (n1-time_agg_adj_fac)*impliedPermVar)/2.0
    impliedTranStd = impliedTranVar**0.5
    
    covGrowth1 = np.cov(y_growth1.flatten(),c_growth1.flatten())[1][0]
    covGrowth2 = np.cov(y_growth2.flatten(),c_growth2.flatten())[1][0]
    
    phi = (covGrowth2-covGrowth1)/((n2-n1)*impliedPermVar)
    psi = (covGrowth1 - (n1-time_agg_adj_fac)*phi*impliedPermVar)/(2.0*impliedTranVar)
    
    return impliedPermVar,impliedTranVar,phi,psi
    
def EstimateTable(EstimateArray, filename,circle3_5=False, width=1.0):
    '''
    Creates a Latex table of estimates for psi
    
    Parameters
    ----------
    EstimateArray : np.array
        An upper triangular array containing estimates for different values of
        n1 and n2
        
    Returns
    -------
    none
    '''
    num_columns = EstimateArray.shape[1]
    output = "\\begin{minipage}{" + str(width) + "\\textwidth}\n"
   
    output += "\\resizebox{\\textwidth}{!}{\\begin{tabular}{lc|*{" + str(num_columns) + "}{c}}  \n"
    for i in range(num_columns/2):
        output += "& "
    output += " $n_2$ \n"
    output += "\\\\ &  "
    for i in range(num_columns):
        output += " & " + str(i+1) 
    output += "\\\\ \\toprule  \n"
    for row in range(num_columns):
        if row==num_columns/2-1:
            output += " $n_1$ & " + str(row+1) + " & "
        else:
            output += " & " + str(row+1) + " & "
        for column in range(num_columns):
            if ~np.isnan(EstimateArray[row,column]):
                if circle3_5 and row==2 and column==4:
                    output += "\circled{" + "{:.2f}".format(EstimateArray[row,column]) + "}"
                else:
                    output += "{:.2f}".format(EstimateArray[row,column])
            if column!=num_columns-1:
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
        
        
    
def BasicRegressionTables(Economy,max_diff=10,filename=False,do_labor=False):
    log_C_agg, log_Y_agg = SelectMicroSample(Economy,20,4,do_labor)
    result = np.zeros(max_diff)
    for i in range(max_diff):
        diff_C = log_C_agg[i+1:]-log_C_agg[:-(i+1)]
        diff_Y = log_Y_agg[i+1:]-log_Y_agg[:-(i+1)]
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
        


    


        