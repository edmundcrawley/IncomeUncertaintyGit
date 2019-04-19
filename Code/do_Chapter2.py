"""
 Creates all tables and figures for Chapter 2 of my dissertation (Job Market Paper)
"""
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