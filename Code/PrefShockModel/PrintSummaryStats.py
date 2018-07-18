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

filename = "C:\Users\edmun\OneDrive\Documents\Research\Denmark\IncomeUncertaintyGit\Code\descriptives_text.csv"
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
output += "\\\\ No. household-year obs & & " + mystr1(summary_data[10,0]) + " & "  + " & " + " & "+ mystr1(summary_data[10,3]) + " & " +  " \n"
output += "\\\\ \\bottomrule  \n"
output += "\end{tabular}}\n"
output += "\\\\ \\textbf{Notes}: Values are 2015 USD. Age refers to the age in 2008 of the main income earner in the household. For the purposes of calculation of consumption in the population, top and bottom 1\% in terms of consumption have been excluded. URE and NNP can only be calculated in the period 2009-2015 due to mortgage information being insufficiently detailed in the previous years. \n"
output += "\end{minipage}\n"
with open('./Tables/summary_statistics.tex','w') as f:
    f.write(output)
    f.close()

