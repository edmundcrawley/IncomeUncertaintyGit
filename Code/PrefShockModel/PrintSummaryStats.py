"""
Prints summary statistics from file into Latex table format

"""
import numpy as np

def mystr1(number):
    if not np.isnan(number):
        #out = "{:.0f}".format(number)
        out = "{:,.0f}".format(number)
    else:
        out = ''
    return out

def mystr2(number):
    if not np.isnan(number):
        out = "{:.2f}".format(number)
    else:
        out = ''
    return out

def mystr3(number):
    if not np.isnan(number):
        out = "{:.1f}".format(number)
    else:
        out = ''
    return out

filename = "../descriptives_text.csv"
summary_data = np.genfromtxt(filename, delimiter=',')

output = "\\begin{minipage}{" + str(0.9) + "\\textwidth}\n"
   
output += "\\resizebox{\\textwidth}{!}{\\begin{tabular}{lcccccc}  \n"
output += " & \multicolumn{3}{c}{Estimation Sample} & \multicolumn{3}{c}{Population (Age 30-55)} \n"
output += "\\\\ & Mean & Median & Std Dev & Mean & Median & Std Dev \n"
output += "\\\\ \\midrule"
output += "\\\\ After Tax Income & " +  mystr1(summary_data[0,0]) + " & " + mystr1(summary_data[0,2]) + " & "+ mystr1(summary_data[0,1]) + " & "+ mystr1(summary_data[0,3]) + " & "+ mystr1(summary_data[0,5]) + " & "+ mystr1(summary_data[0,4]) + " \n"
output += "\\\\ Consumption & " +       mystr1(summary_data[1,0]) + " & " + mystr1(summary_data[1,2]) + " & "+ mystr1(summary_data[1,1]) + " & "+ mystr1(summary_data[1,3]) + " & "+ mystr1(summary_data[1,5]) + " & "+ mystr1(summary_data[1,4]) + " \n"
output += "\\\\ Liquid Assets & " +     mystr1(summary_data[2,0]) + " & " + mystr1(summary_data[2,2]) + " & "+ mystr1(summary_data[2,1]) + " & "+ mystr1(summary_data[2,3]) + " & "+ mystr1(summary_data[2,5]) + " & "+ mystr1(summary_data[2,4]) + " \n"
output += "\\\\ Net Worth & " +         mystr1(summary_data[3,0]) + " & " + mystr1(summary_data[3,2]) + " & "+ mystr1(summary_data[3,1]) + " & "+ mystr1(summary_data[3,3]) + " & "+ mystr1(summary_data[3,5]) + " & "+ mystr1(summary_data[3,4]) + " \n"
output += "\\\\ Homeowner & " +         mystr2(summary_data[4,0]) + " & " + mystr2(summary_data[4,2]) + " & "+ mystr2(summary_data[4,1]) + " & "+ mystr2(summary_data[4,3]) + " & "+ mystr2(summary_data[4,5]) + " & "+ mystr2(summary_data[4,4]) + " \n"
output += "\\\\ Car Owner & " +         mystr2(summary_data[5,0]) + " & " + mystr2(summary_data[5,2]) + " & "+ mystr2(summary_data[5,1]) + " & "+ mystr2(summary_data[5,3]) + " & "+ mystr2(summary_data[5,5]) + " & "+ mystr2(summary_data[5,4]) + " \n"
output += "\\\\ Higher Education & " + mystr2(summary_data[6,0]) + " & " + mystr2(summary_data[6,2]) + " & "+ mystr2(summary_data[6,1]) + " & "+ mystr2(summary_data[6,3]) + " & "+ mystr2(summary_data[6,5]) + " & "+ mystr2(summary_data[6,4]) + " \n"
output += "\\\\ Age & " +               mystr3(summary_data[7,0]) + " & " + mystr3(summary_data[7,2]) + " & "+ mystr3(summary_data[7,1]) + " & "+ mystr3(summary_data[7,3]) + " & "+ mystr3(summary_data[7,5]) + " & "+ mystr3(summary_data[7,4]) + " \n"
output += "\\\\ URE & " +               mystr1(summary_data[8,0]) + " & " + mystr1(summary_data[8,2]) + " & "+ mystr1(summary_data[8,1]) + " & "+ mystr1(summary_data[8,3]) + " & "+ mystr1(summary_data[8,5]) + " & "+ mystr1(summary_data[8,4]) + " \n"
output += "\\\\ NNP & " +               mystr1(summary_data[9,0]) + " & " + mystr1(summary_data[9,2]) + " & "+ mystr1(summary_data[9,1]) + " & "+ mystr1(summary_data[9,3]) + " & "+ mystr1(summary_data[9,5]) + " & "+ mystr1(summary_data[9,4]) + " \n"
output += "\\\\ \\midrule"
output += "\\\\ No. Household-year obs & & " + mystr1(summary_data[10,0]) + " & "  + " & " + " & "+ mystr1(summary_data[10,3]) + " & " +  " \n"
output += "\\\\ \\bottomrule  \n"
output += "\end{tabular}}\n"
output += "\\\\ \\tiny \\textbf{Notes}: Values are 2015 USD. Age refers to the age in 2008 of the main income earner in the household. For the purposes of calculation of consumption in the population, top and bottom 1\% in terms of consumption have been excluded. URE and NNP can only be calculated in the period 2009-2015 due to mortgage information being insufficiently detailed in the previous years. \n"
output += "\end{minipage}\n"
with open('../Rcode/Tables/AEJ_revision/summary_statistics.tex','w') as f:
    f.write(output)
    f.close()
    
    
# This file also creates the URE and NNP table
    
filename = "../Rcode/Tables/AEJ_revision/URE_NNP_positions_text.csv"
URENNP_data = np.genfromtxt(filename, delimiter=',')

#Do large table for paper
output = "\\begin{minipage}{" + str(0.9) + "\\textwidth}\n"
output += "\\resizebox{\\textwidth}{!}{\\begin{tabular}{lccccc}  \n"
output += " & \\textbf{MPX} & \\textbf{NNP} & \\textbf{URE} &  \\textbf{$\mathcal{E}_P$ component} & \\textbf{$\mathcal{E}_R$ component} \n"
output += "\\\\ \\midrule"
output += "\\\\ Sample            & " +  "See Distribution"         + " & " + mystr1(URENNP_data[0,1]) + " & "+ mystr1(URENNP_data[0,2]) + " & "+ mystr2(URENNP_data[0,3]) + " & "+ mystr2(URENNP_data[0,4])  + " \n"
output += "\\\\ Young             & "    + mystr3(URENNP_data[1,0]) + " & " + mystr1(URENNP_data[1,1]) + " & "+ mystr1(URENNP_data[1,2]) + " & "+ mystr2(URENNP_data[1,3]) + " & "+ mystr2(URENNP_data[1,4])  + " \n"
output += "\\\\ Old               & "    + mystr3(URENNP_data[2,0]) + " & " + mystr1(URENNP_data[2,1]) + " & "+ mystr1(URENNP_data[2,2]) + " & "+ mystr2(URENNP_data[2,3]) + " & "+ mystr2(URENNP_data[2,4])  + " \n"
output += "\\\\ Pension Funds     & "    + mystr3(URENNP_data[3,0]) + " & " + mystr1(URENNP_data[3,1]) + " & "+ mystr1(URENNP_data[3,2]) + " & "+ mystr2(URENNP_data[3,3]) + " & "+ mystr2(URENNP_data[3,4])  + " \n"
output += "\\\\ Government        & "    + mystr3(URENNP_data[4,0]) + " & " + mystr1(URENNP_data[4,1]) + " & "+ mystr1(URENNP_data[4,2]) + " & "+ mystr2(URENNP_data[4,3]) + " & "+ mystr2(URENNP_data[4,4])  + " \n"
output += "\\\\ Non-financial Corp. & "  + mystr3(URENNP_data[5,0]) + " & " + mystr1(URENNP_data[5,1]) + " & "+ mystr1(URENNP_data[5,2]) + " & "+ mystr2(URENNP_data[5,3]) + " & "+ mystr2(URENNP_data[5,4])  + " \n"
output += "\\\\ Financial Sector  & "    + mystr3(URENNP_data[6,0]) + " & " + mystr1(URENNP_data[6,1]) + " & "+ mystr1(URENNP_data[6,2]) + " & "+ mystr2(URENNP_data[6,3]) + " & "+ mystr2(URENNP_data[6,4])  + " \n"
output += "\\\\ Rest of World     & "    + mystr3(URENNP_data[7,0]) + " & " + mystr1(URENNP_data[7,1]) + " & "+ mystr1(URENNP_data[7,2]) + " & "+ mystr2(URENNP_data[7,3]) + " & "+ mystr2(URENNP_data[7,4])  + " \n"
output += "\\\\ \\midrule"
output += "\\\\ \\textbf{Total} & "                                 + " & \\textbf{" + mystr1(URENNP_data[8,1]*(0.000001)) + "} & \\textbf{"+ mystr1(URENNP_data[8,2]*(0.000001)) + "} & \\textbf{"+ mystr2(URENNP_data[8,3]) + "} & \\textbf{"+ mystr2(URENNP_data[8,4])  + "} \n"
output += "\\\\ \\bottomrule  \n"
output += "\end{tabular}}\n"
output += "\\\\ \\tiny \\textbf{Notes}: NNP and URE numbers are in billions of 2015 USD. Pension Funds includes special saving such as children's savings accounts. See appendix \\ref{URE_NNP_appendix} for detail.\n"
output += "\end{minipage}\n"
with open('../Rcode/Tables/AEJ_revision/URE_NNP_table.tex','w') as f:
    f.write(output)
    f.close()
    
#And smaller table for presentation
output = "\\begin{minipage}{" + str(0.9) + "\\textwidth}\n"
output += "\\resizebox{\\textwidth}{!}{\\begin{tabular}{lccc}  \n"
output += " & MPX  & URE &  $\mathcal{E}_R$ component \n"
output += "\\\\ \\midrule"
output += "\\\\ \\textbf{Estimation Sample} & " +  "\\textbf{See Distribution}  & \\textbf{"+ mystr1(URENNP_data[0,2]) + "} &  \\textbf{"+ mystr2(URENNP_data[0,4])  + "} \n"
output += "\\\\ Young             &  " + mystr3(URENNP_data[1,0]) + " &  "+ mystr1(URENNP_data[1,2]) + " & "+ mystr2(URENNP_data[1,4])  + " \n"
output += "\\\\ Old               &  " + mystr3(URENNP_data[2,0]) + " &  "+ mystr1(URENNP_data[2,2]) + " & "+ mystr2(URENNP_data[2,4])  + " \n"
output += "\\\\ Pension Funds     &  " + mystr3(URENNP_data[3,0]) + " &  "+ mystr1(URENNP_data[3,2]) + " & "+ mystr2(URENNP_data[3,4])  + " \n"
output += "\\\\ Government        &  " + mystr3(URENNP_data[4,0]) + " &  "+ mystr1(URENNP_data[4,2]) + " & "+ mystr2(URENNP_data[4,4])  + " \n"
output += "\\\\ Non-financial Corp. &  " + mystr3(URENNP_data[5,0]) + " & "+ mystr1(URENNP_data[5,2]) + " & "+ mystr2(URENNP_data[5,4])  + " \n"
output += "\\\\ Financial Sector  &  " + mystr3(URENNP_data[6,0]) + " &  "+ mystr1(URENNP_data[6,2]) + " & "+ mystr2(URENNP_data[6,4])  + " \n"
output += "\\\\ Rest of World     &  " + mystr3(URENNP_data[7,0]) + "  & "+ mystr1(URENNP_data[7,2]) + " & "+ mystr2(URENNP_data[7,4])  + " \n"
output += "\\\\ \\midrule"
output += "\\\\ \\textbf{Total} & "                                 + " &  \\textbf{"+ mystr1(-URENNP_data[8,2]*(0.000001)) + "} & \\textbf{"+ mystr2(URENNP_data[8,4])  + "} \n"
output += "\\\\ \\bottomrule  \n"
output += "\end{tabular}}\n"
output += "\\\\ \\textbf{Notes}: URE numbers are in billions of 2015 USD. \n"
output += "\end{minipage}\n"
with open('../Rcode/Tables/AEJ_revision/URE_table.tex','w') as f:
    f.write(output)
    f.close()

# And a table for all sufficient stats    
filename = "../Rcode/Tables/AEJ_revision/URE_NNP_positions_text.csv"
URENNP_data = np.genfromtxt(filename, delimiter=',')

output = "\\begin{minipage}{" + str(0.4) + "\\textwidth}\n"
   
output += "\\resizebox{\\textwidth}{!}{\\begin{tabular}{ccccc}  \n"
output += "$\mathcal{M}$ & $\mathcal{E}_Y$ & $\mathcal{E}_P$  & $\mathcal{E}_R$ & $\mathcal{S}$ \n"
output += "\\\\ \\midrule "
output += mystr2(URENNP_data[0,5]) + " & " + mystr2(URENNP_data[1,5]) + " & "+ mystr2(URENNP_data[2,5]) + " & "+ mystr2(URENNP_data[3,5]) + " & "+ mystr2(URENNP_data[4,5]) + " \n"
output += "\\\\ (" + mystr2(URENNP_data[0,6]) + ") & (" + mystr2(URENNP_data[1,6]) + ") & ("+ mystr2(URENNP_data[2,6]) + ") & ("+ mystr2(URENNP_data[3,6]) + ") & ("+ mystr2(URENNP_data[4,6]) + ") \n"

output += "\\\\ \\bottomrule  \n"
output += "\end{tabular}}\n"
output += "\end{minipage}\n"
with open('../Rcode/Tables/AEJ_revision/sufficient_stats.tex','w') as f:
    f.write(output)
    f.close()
    

# And a table for all sufficient stats    
filename = "../Rcode/Tables/AEJ_revision/URE_NNP_positions_text.csv"
URENNP_data = np.genfromtxt(filename, delimiter=',')

output = "\\begin{minipage}{" + str(0.4) + "\\textwidth}\n"
   
output += "\\resizebox{\\textwidth}{!}{\\begin{tabular}{lc}  \n"
output += "\\\\ \\toprule"
output += "$\mathcal{M}$ & "+ mystr2(URENNP_data[0,5])+ " \n"
output += "\\\\  $\mathcal{E}_Y$ & "+ mystr2(URENNP_data[1,5])+ " \n"
output += "\\\\  $\mathcal{E}_P$  & "+ mystr2(URENNP_data[2,5])+ " \n"
output += "\\\\  $\mathcal{E}_R$ & "+ mystr2(URENNP_data[3,5])+ " \n"
output += "\\\\  $\mathcal{S}$ & "+ mystr2(URENNP_data[4,5])+ " \n"
output += "\\\\ \\bottomrule  \n"
output += "\end{tabular}}\n"
output += "\end{minipage}\n"
with open('../Rcode/Tables/AEJ_revision/sufficient_stats2.tex','w') as f:
    f.write(output)
    f.close()
    
    # And a table for all sufficient stats   with US  
filename = "../Rcode/Tables/AEJ_revision/URE_NNP_positions_text.csv"
URENNP_data = np.genfromtxt(filename, delimiter=',')
filename_US = "../Rcode/Tables/AEJ_revision/US_auclert_stats.csv"
US_auclert_stats = np.genfromtxt(filename_US, delimiter=',')

output = "\\begin{minipage}{" + str(0.3) + "\\textwidth}\n"
   
output += "\\resizebox{\\textwidth}{!}{\\begin{tabular}{lcc}  \n"
output += " & Denmark & U.S. \n"
output += "\\\\ \\midrule "
output += "$\mathcal{M}$ & "+ mystr2(URENNP_data[0,5])+ "&"+ mystr2(US_auclert_stats[0,0])+ " \n"
output += "\\\\  $\mathcal{E}_Y$ & "+ mystr2(URENNP_data[1,5])+ "&"+ mystr2(US_auclert_stats[0,1])+" \n"
output += "\\\\  $\mathcal{E}_P$  & "+ mystr2(URENNP_data[2,5])+ "&"+ mystr2(US_auclert_stats[0,2])+" \n"
output += "\\\\  $\mathcal{E}_R$ & "+ mystr2(URENNP_data[3,5])+ "&"+ mystr2(US_auclert_stats[0,3])+" \n"
output += "\\\\  $\mathcal{S}$ & "+ mystr2(URENNP_data[4,5])+ "& "+" \n"
output += "\\\\ \\bottomrule  \n"
output += "\end{tabular}}\n"
output += "\end{minipage}\n"
with open('../Rcode/Tables/AEJ_revision/sufficient_stats_with_US.tex','w') as f:
    f.write(output)
    f.close()

# Table for Denmark and US including standard errors
output = "\\begin{minipage}{" + str(0.7) + "\\textwidth}\n"
output += "\\resizebox{\\textwidth}{!}{\\begin{tabular}{lccccc}  \n"
output += "& $\mathcal{M}$ & $\mathcal{E}_Y$ & $\mathcal{E}_P$  & $\mathcal{E}_R$ & $\mathcal{S}$ \n"
output += "\\\\ \\midrule "
output += "Denmark & " + mystr2(URENNP_data[0,5]) + " & " + mystr2(URENNP_data[1,5]) + " & "+ mystr2(URENNP_data[2,5]) + " & "+ mystr2(URENNP_data[3,5]) + " & "+ mystr2(URENNP_data[4,5]) + " \n"
output += "\\\\ & (" + mystr2(URENNP_data[0,6]) + ") & (" + mystr2(URENNP_data[1,6]) + ") & ("+ mystr2(URENNP_data[2,6]) + ") & ("+ mystr2(URENNP_data[3,6]) + ") & ("+ mystr2(URENNP_data[4,6]) + ") \n"
output += "\\\\ US & " + mystr2(US_auclert_stats[0,0]) + " & " + mystr2(US_auclert_stats[0,1]) + " & "+ mystr2(US_auclert_stats[0,2]) + " & "+ mystr2(US_auclert_stats[0,3]) + " &    \n"
output += "\\\\ & (" + mystr2(US_auclert_stats[1,0]) + ") & (" + mystr2(US_auclert_stats[1,1]) + ") & ("+ mystr2(US_auclert_stats[1,2]) + ") & ("+ mystr2(US_auclert_stats[1,3]) + ") &  \n"
output += "\\\\ \\bottomrule  \n"
output += "\end{tabular}}\n"
output += "\end{minipage}\n"
with open('../Rcode/Tables/AEJ_revision/sufficient_stats_with_US_se.tex','w') as f:
    f.write(output)
    f.close()

# And a table for how well liquid wealth predicts MPX    
filename = "../Rcode/Tables/AEJ_revision/prediction_errors.csv"
prediction_errors = np.genfromtxt(filename, delimiter=',')

output = "\\begin{minipage}{" + str(0.65) + "\\textwidth}\n"
   
output += "\\resizebox{\\textwidth}{!}{\\begin{tabular}{lcccc}  \n"
output += " & \multicolumn{2}{c}{Transitory} & \multicolumn{2}{c}{Permanent} \n"
output += "\\\\  Interpolate using: & Absolute & Ratio & Absolute & Ratio  \n"
output += "\\\\ \\midrule "
output += "\\\\  URE Deciles" + " & " + mystr2(prediction_errors[0,0]) + " & "+ mystr2(prediction_errors[0,1]) + " & "+ mystr2(prediction_errors[0,2]) + " & "+ mystr2(prediction_errors[0,3]) + " \n"
output += "\\\\  NNP Deciles" + " & " + mystr2(prediction_errors[1,0]) + " & "+ mystr2(prediction_errors[1,1]) + " & "+ mystr2(prediction_errors[1,2]) + " & "+ mystr2(prediction_errors[1,3]) + " \n"
output += "\\\\  Income Deciles" + " & " + mystr2(prediction_errors[2,0]) + " & "+ mystr2(prediction_errors[2,1]) + " & "+ mystr2(prediction_errors[2,2]) + " & "+ mystr2(prediction_errors[2,3]) + " \n"
#output += "\\\\  Consumption Deciles" + " & " + mystr2(prediction_errors[3,0]) + " & "+ mystr2(prediction_errors[3,1]) + " & "+ mystr2(prediction_errors[3,2]) + " & "+ mystr2(prediction_errors[3,3]) + " \n"
output += "\\\\  Net Wealth Deciles" + " & " + mystr2(prediction_errors[4,0]) + " & "+ mystr2(prediction_errors[4,1]) + " & "+ mystr2(prediction_errors[4,2]) + " & "+ mystr2(prediction_errors[4,3]) + " \n"

output += "\\\\ \\bottomrule  \n"
output += "\end{tabular}}\n"
output += "\end{minipage}\n"
output += "\\\\ \\textbf{Notes}: Mean absolute errors are for the interpolated values relative to the MPX estimated using the full estimation procedure. Interpolation uses either the absolute value of liquid wealth, or the ratio of liquid wealth to mean income over the sample period. \n"
with open('../Rcode/Tables/AEJ_revision/prediction_errors.tex','w') as f:
    f.write(output)
    f.close()
    

# And a table for how well liquid wealth and income predicts MPX    
filename = "../Rcode/Tables/AEJ_revision/prediction_errors.csv"
prediction_errors = np.genfromtxt(filename, delimiter=',')

output = "\\begin{minipage}{" + str(1.0) + "\\textwidth}\n"
   
output += "\\resizebox{\\textwidth}{!}{\\begin{tabular}{lcccccc}  \n"
output += " & \multicolumn{3}{c}{Transitory} & \multicolumn{3}{c}{Permanent} \n"
output += "\\\\  Interpolate using: & Liq. Wealth & Liq. Wealth Ratio & Income & Liq. Wealth & Liq. Wealth Ratio & Income  \n"
output += "\\\\ \\midrule "
output += "\\\\  URE Deciles" + " & " + mystr2(prediction_errors[0,0]) + " & "+ mystr2(prediction_errors[0,1]) + " & "+ mystr2(prediction_errors[0,4]) + " & "+ mystr2(prediction_errors[0,2]) + " & "+ mystr2(prediction_errors[0,3]) + " & "+ mystr2(prediction_errors[0,5]) + " \n"
output += "\\\\  NNP Deciles" + " & " + mystr2(prediction_errors[1,0]) + " & "+ mystr2(prediction_errors[1,1]) + " & "+ mystr2(prediction_errors[1,4]) + " & "+ mystr2(prediction_errors[1,2]) + " & "+ mystr2(prediction_errors[1,3]) + " & "+ mystr2(prediction_errors[1,5]) + " \n"
output += "\\\\  Income Deciles" + " & " + mystr2(prediction_errors[2,0]) + " & "+ mystr2(prediction_errors[2,1]) + " & "+ mystr2(prediction_errors[2,4]) + " & "+ mystr2(prediction_errors[2,2]) + " & "+ mystr2(prediction_errors[2,3]) + " & "+ mystr2(prediction_errors[2,5]) + " \n"
output += "\\\\  Net Wealth Deciles" + " & " + mystr2(prediction_errors[4,0]) + " & "+ mystr2(prediction_errors[4,1]) + " & "+ mystr2(prediction_errors[4,4]) + " & "+ mystr2(prediction_errors[4,2]) + " & "+ mystr2(prediction_errors[4,3]) + " & "+ mystr2(prediction_errors[4,5]) + " \n"

output += "\\\\ \\bottomrule  \n"


    # A table for US sufficient stats under different interpolation methods
filename_US = "../Rcode/Tables/AEJ_revision/US_auclert_different_interpolations.csv"
US_auclert_stats = np.genfromtxt(filename_US, delimiter=',')

output = "\\begin{minipage}{" + str(0.65) + "\\textwidth}\n"
   
output += "\\resizebox{\\textwidth}{!}{\\begin{tabular}{lccc}  \n"
output += " & Baseline & Absolute Liquidity & Liquidy to Income Ratio \n"
output += "\\\\ \\midrule "
output += "$\mathcal{M}$ & "         + mystr2(US_auclert_stats[0,0])+ "&"+ mystr2(US_auclert_stats[1,0])+ "&"+mystr2(US_auclert_stats[2,0])+ " \n"
output += "\\\\  $\mathcal{E}_Y$ & " + mystr2(US_auclert_stats[0,1])+ "&"+ mystr2(US_auclert_stats[1,1])+ "&"+mystr2(US_auclert_stats[2,1])+ " \n"
output += "\\\\  $\mathcal{E}_P$  & "+ mystr2(US_auclert_stats[0,2])+ "&"+ mystr2(US_auclert_stats[1,2])+ "&"+mystr2(US_auclert_stats[2,2])+ " \n"
output += "\\\\  $\mathcal{E}_R$ & " + mystr2(US_auclert_stats[0,3])+ "&"+ mystr2(US_auclert_stats[1,3])+ "&"+mystr2(US_auclert_stats[2,3])+ " \n"
output += "\\\\ \\bottomrule  \n"
output += "\end{tabular}}\n"
output += "\end{minipage}\n"
with open('../Rcode/Tables/AEJ_revision/sufficient_stats_different_interpolation.tex','w') as f:
    f.write(output)
    f.close()



#### Save estimate arrays to LaTex
def PrintEstimateTable(estimate_array,num_labelas_vals,num_pref_vals,labor_elas,pref_vals,filename,width=0.45):
    output = "\\begin{minipage}{" + str(width) + "\\textwidth}\n"
   
    output += "\\resizebox{\\textwidth}{!}{\\begin{tabular}{lc|*{" + str(num_labelas_vals) + "}{c}}  \n"
    for i in range(num_labelas_vals//2):
        output += "& "
    output += " $n_2$ \n"
    output += "\\\\ &  "
    for i in range(num_labelas_vals):
        output += " & " +  "{:.0f}".format(labor_elas[i])
    output += "\\\\ \\toprule  \n"
    for row in range(num_pref_vals):
        if row==num_pref_vals/2-1:
            output += " $n_1$ & " + "{:.0f}".format(pref_vals[row]) + " & "
        else:
            output += " & " + "{:.0f}".format(pref_vals[row]) + " & "
        for column in range(num_labelas_vals):
            if (row==2 and column==4):
                output += "\\circled{" + "{:.2f}".format(estimate_array[row,column]) + "}"
            elif ~np.isnan(estimate_array[row,column]) and ~(estimate_array[row,column]==0.0):
                output += "{:.2f}".format(estimate_array[row,column])
            if column!=num_labelas_vals-1:
                output += " & "
            else:
                output += " \n "
                output += "\\\\ "
    output += "\\\\ \\bottomrule  \n"
    output += "\end{tabular}}\n"
    output += "\end{minipage}\n"
    with open('../Rcode/Tables/AEJ_revision/' + filename + '.tex','w') as f:
        f.write(output)
        f.close()
        
filename = "../Rcode/Tables/AEJ_revision/ins_tran_array_6.txt"
psi_array_different_n = np.genfromtxt(filename, delimiter=' ')
PrintEstimateTable(psi_array_different_n,6,6,np.array(range(6))+1,np.array(range(6))+1,'Psi_array_empirical')


for name in ["transition_liquid","transition_income"]:
    # A table for how much time is spent in each quintile  
    filename = "../ServerRcode/ServerOutput/AEJ_revision/TxtFilesFromAndreas/"+name+".csv"
    transition_prob = np.genfromtxt(filename, delimiter=',')
    
    output = "\\begin{minipage}{" + str(0.65) + "\\textwidth}\n"
       
    output += "\\resizebox{\\textwidth}{!}{\\begin{tabular}{ccccccc}  \n"
    output += " & & \multicolumn{5}{c}{Short-term Quintile}  \n"
    output += "\\\\ & &  1 & 2 & 3 & 4 & 5  \n"
    output += "\\\\ \\midrule "
    output += "\\\\  " + " & 1 &"           + mystr2(transition_prob[0,0]) + " & "+ mystr2(transition_prob[0,1]) + " & "+ mystr2(transition_prob[0,2]) + " & "+ mystr2(transition_prob[0,3]) + " & "+ mystr2(transition_prob[0,4])  + " \n"
    output += "\\\\  Long-term"  + " & 2 &" + mystr2(transition_prob[1,0]) + " & "+ mystr2(transition_prob[1,1]) + " & "+ mystr2(transition_prob[1,2]) + " & "+ mystr2(transition_prob[1,3]) + " & "+ mystr2(transition_prob[1,4])  + " \n"
    output += "\\\\  Quintile" + " & 3 &"   + mystr2(transition_prob[2,0]) + " & "+ mystr2(transition_prob[2,1]) + " & "+ mystr2(transition_prob[2,2]) + " & "+ mystr2(transition_prob[2,3]) + " & "+ mystr2(transition_prob[2,4])  + " \n"
    output += "\\\\  " + " & 4 &"           + mystr2(transition_prob[3,0]) + " & "+ mystr2(transition_prob[3,1]) + " & "+ mystr2(transition_prob[3,2]) + " & "+ mystr2(transition_prob[3,3]) + " & "+ mystr2(transition_prob[3,4])  + " \n"
    output += "\\\\  " + " & 5 &"           + mystr2(transition_prob[4,0]) + " & "+ mystr2(transition_prob[4,1]) + " & "+ mystr2(transition_prob[4,2]) + " & "+ mystr2(transition_prob[4,3]) + " & "+ mystr2(transition_prob[4,4])  + " \n"
    output += "\\\\ \\bottomrule  \n"
    output += "\end{tabular}}\n"
    output += "\end{minipage}\n"
    output += "\\\\ \\small \\textbf{Notes}: Each row shows the percentage of time a household of that long run quintile will spend in the respective short-run quintile  \n"
    with open('../Rcode/Tables/AEJ_revision/'+name+'.tex','w') as f:
        f.write(output)
        f.close()

