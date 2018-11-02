cd "E:\ProjektDB\706172\Workdata\706172\Husholdningsprojekt\Precautionary saving with time varying risk"

insheet using "E:\ProjektDB\706172\Workdata\706172\Husholdningsprojekt\Precautionary saving with time varying risk\family_firm.txt", clear

replace famnyformue_net = famqaktivf_ny05 - famqpassivn if famnyformue_net ==.
replace famnyformue = famqaktivf_ny05 if famnyformue ==.

save family_firm, replace
