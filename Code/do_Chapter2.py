"""
 Creates all tables and figures for Chapter 2 of my dissertation (Job Market Paper)
"""
from __main__ import *
import sys 
import os
import subprocess

Rscript_path = "C:/Program Files/R/R-3.4.3/bin/x64/Rscript"

print('Run R Script to create most figures in the paper')
r_status = subprocess.call([Rscript_path, "Rcode/Main.R"], shell=False)
if r_status!=0:
    print('R code could not run. Check the the path for Rscript')

print('Run R Script to create time aggregation figures')
r_status = subprocess.call([Rscript_path, "Rcode/time_agg_random_walk_graph.R"], shell=False)
if r_status!=0:
    print('R code could not run. Check the the path for Rscript')
    
   
#print('Run R Script to create robustness simulations')
#print('This is VERY slow, up to 24 hours')
#r_status = subprocess.call([Rscript_path, "Rcode/RobustnessSimulations.R"], shell=False)
#if r_status!=0:
#    print('R code could not run. Check the the path for Rscript')
#    
    
print('Now running the MAIN model code')
# Note the "do_labor_elas" tab controls whether the labor elasticity tables in the 
# appendix are created. This is very sloooow (possibly days) and is set to "False"
# as default
sys.path.append(os.getcwd() + '/PrefShockModel/')
os.chdir(os.getcwd() + '/PrefShockModel/')
import MAIN

print('Now create some remaining files')
import PrintSummaryStats

