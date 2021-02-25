* Calculates URE, NNP etc from SCF data as best as possible
clear all
set maxvar 6000
global savedirectory = "C:\Users\edmun\OneDrive\Documents\Research\SCF\StataDownload"

use "${savedirectory}\rscfp2016.dta", clear

gen NNP = liq +cds+gbmutf+obmutf+bond ///
			- mrthel - resdbt - othloc - ccbal - install - odebt	
xtile NNP_decile = NNP [pweight=wgt], nq(10)

gen URE = (liq-prepaid) +cds/5 + gbmutf/5 + obmutf/5 + bond/5 ///
			- mrthel/7 - resdbt/5 - othloc - ccbal - install/5 - odebt	
xtile URE_decile = URE [pweight=wgt], nq(10)

xtile liq_decile = liq [pweight=wgt], nq(10)
xtile liq_pctile = liq [pweight=wgt], nq(100)

xtile inc_decile = income [pweight=wgt], nq(10)
xtile inc_pctile = income [pweight=wgt], nq(100)


outsheet using "${savedirectory}\SCF_Auclert.csv",comma replace
