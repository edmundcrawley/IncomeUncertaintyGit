*********************************************************************
* This program merges information on main workplace in November
* from FIDA and IDAN with the family level dataset
* Parts of this dataset is used for the analysis of 
* "Consumption Heterogeneity: Micro drivers and macro implications"
* By Edmund Crawley and Andreas Kuchler
* Parts of the information - in particular regarding firms - 
* are not used in the final paper
* Latest version: October 2018 / AKU
*********************************************************************/


/************************************************************************
* 1) Libraries											*
************************************************************************/;

libname databib "E:/ProjektDB/706172/Workdata/706172/Datasæt";
libname saving "E:\ProjektDB\706172\Workdata\706172\Husholdningsprojekt\Precautionary saving with time varying risk";
libname temp "E:/ProjektDB/706172/Workdata/706172/Datasæt/temp";
libname raw   "E:/ProjektDB/706172/DST_rawdata_706172";
libname rawkbb   'E:\ProjektDB\706172\DST_rawdata_706172\Konjunktur\KBB';
libname rawkbi   'E:\ProjektDB\706172\DST_rawdata_706172\Konjunktur\KBI';
libname rawkbs   'E:\ProjektDB\706172\DST_rawdata_706172\Konjunktur\KBS';
libname rawkbd   'E:\ProjektDB\706172\DST_rawdata_706172\Konjunktur\KBD';
libname kb     'E:\ProjektDB\706172\Workdata\706172\Virksomheder\Kapacitetspres\data\temp';
libname cars "E:\ProjektDB\706172\DST_rawdata_706172\_705741_andet\Bildata";
libname fmt  '\\srvfsenas1\data\formater\SAS formater i Danmarks Statistik\FORMATKATALOG';
options fmtsearch=(fmt.times_personstatistik fmt.brancher fmt.uddannelser fmt.geokoder fmt.disco work egnefmt);



/************************************************************************
* 2) Startaar og Slutaar 						*
************************************************************************/

%let startaar = 1998; /* Start year */
%let slutaar = 2015; /* End year */

%let startaar_1 = &startaar - 1;
%let startaar_2 = &startaar - 2;
%let slutaar_min_1 = &slutaar - 1;
%let slutaar_1 = &slutaar + 1;

* The following macro captures selected variables from a number of registers regarding personal characteristics and workplace;
%macro prep;
	%do i=&startaar %to &slutaar;
	data ind&i; set raw.ind&i;
	aar = &i;
	keep pnr lejev_egen_bolig dispon_13 aar perindkialt_13 skatmvialt_13;
	run;

		%if &i <=2013 %then %do; * Delete this if-condition when fida becomes available for years > 2013;
		data fida&i;
		set raw.fida&i;
		if PSTILL ~= 0;
		drop PSTILL;
		run;

		data fida_sec&i;
		set raw.fida&i;
		if PSTILL = 0 & SSTILL ~=0;
		rename cvrnr = cvr_sec;
		drop PSTILL SSTILL;
		run; 
		%end;
		%if &i > 2013 %then %do; * Delete this if-condition when fida becomes available for years > 2013;
		proc sql;
				create table fida&i like fida2013;
			quit;	
			data _null_;
			proc sql;
				create table fida_sec&i like fida_sec2013;
			quit;	
			data _null_;
			%end;

		%if &i <2009 %then %do;
		data lon&i;
		set raw.lon&i;
		keep pnr TIMPRAE FUNK;
		run;
		proc sort data=lon&i;
		by pnr descending TIMPRAE;
		run;
		proc sort data=lon&i nodupkey;
		by pnr;
		run;
		%end;

		%if &i >= 2009 %then %do;
		data lon&i;
		set raw.lonn&i;
		keep pnr TIMPRAE FUNK;
		run;
		proc sort data=lon&i;
		by pnr descending TIMPRAE;
		run;
		proc sort data=lon&i nodupkey;
		by pnr;
		run;
		%end;
		
		%if &i <=2013 %then %do; * Delete this if-condition when idan becomes available for years > 2013;
			data idan_temp&i;
			set raw.idan&i;
			if TYPE ="H";
			keep pnr cvrnr PERSBRC;
			rename cvrnr = cvrnr_alt; * See below IDAN is used as a complementary source of CVRNR;
			run;
			%end;
		%if &i >2013 %then %do; * Delete this if-condition when idan becomes available for years > 2013;
			proc sql;
				create table idan_temp&i like idan_temp2013;
			quit;	
			data _null_;
			%end;

			%if &i <=2013 %then %do;
			data idap&i; set raw.idap&i;
			keep pnr ARLEDGR PSTILL;
			run;
			%end;

			%if &i >2013 %then %do;
			data idap&i; set raw.idap&i;
			arledgr = LEDIGHED_BRUTTO / 52 * 1000;
			keep pnr arledgr PSOC_STATUS_KODE;
			run;
			%end;


		data fida_temp&i;
			merge fida&i 
			idap&i 
			idan_temp&i (keep=pnr PERSBRC)
			lon&i (keep=pnr FUNK)
			fida_sec&i;
			by pnr;
			if pnr ~= "";
			aar = &i;
			if cvrnr = "" then cvrnr = cvr_sec;
			drop cvr_sec;
			run;
		%if &i >= 2008 %then %do;
			data fida_temp&i;
			merge fida_temp&i
			idan_temp&i (keep=pnr cvrnr_alt);
			by pnr;
			if cvrnr ="" then cvrnr = cvrnr_alt; * Replaces empty CVRNRs with those from IDAN from 2008;
			drop cvrnr_alt;
			run;
		%end;
	%end;
%mend;
%prep;

* Join the datasets produced above and create additional variables;
data fida_all;
set fida_temp&startaar-fida_temp&slutaar;
rename pnr = pnr_1;
if length(FUNK)=4 then FUNK = cats(FUNK,'00');
if length(FUNK)=3 then FUNK = cats(FUNK,'000');
if length(FUNK)=2 then FUNK = cats(FUNK,'0000');
if aar >= 2008 then industry_agg_1 = put(PERSBRC,$DB07_L1L5_KT.);
if aar >=2008 then industry_level4_1 = put(PERSBRC,$DB07_L1L4_KT.);
if aar < 2008 and aar >= 2004 then industry_agg_1 = put(PERSBRC,$DB03_L1L5_KT.);
if aar < 2008 and aar >= 2004 then industry_level4_1 = put(PERSBRC,$DB03_L1L4_KT.);
if aar < 2004 then industry_agg_1 = put(PERSBRC,$DB93_L1L5_KT.);
if aar < 2004 then industry_level4_1 = put(PERSBRC,$DB93_L1L4_KT.);
work_function_agg_1 = put(FUNK,$DISCO08_L1L5_KT.);
work_function_level4_1 = put(FUNK,$DISCO08_L1L4_KT.);
run;
proc sort data=fida_all;
by pnr_1 aar;
run;
data fida_all_lag;
set fida_all; 
aar = aar + 1;
keep cvrnr pnr_1 aar;
rename cvrnr = cvrnr_lag;
run;
* Lagging and leading CVRNR in order to "fill missing blanks";
* CVR_i means that CVRNR is interpolated in years when CVRNR_lag = CVRNR_lead ;
data fida_all_lead;
set fida_all; 
aar = aar - 1;
keep cvrnr pnr_1 aar;
rename cvrnr = cvrnr_lead;
run;
data fida_all;
merge fida_all (in=a)
fida_all_lag
fida_all_lead;
by pnr_1 aar;
if a;
cvr_i_1 = cvrnr;
if cvrnr = "" and cvrnr_lead = cvrnr_lag and cvrnr_lead ~="" then cvr_i_1=cvrnr_lead ;
run;
data f1; set fida_all;
keep pnr_1 aar cvr_i_1;
rename cvr_i_1=cvr_i_1_lead;
aar = aar - 1;
run;
data fida_all; 
merge fida_all (in=a)
f1;
by pnr_1 aar;
if a;
if cvr_i_1 ~= "" and aar < &slutaar then job_change_1 = 0;
if cvr_i_1 ~= cvr_i_1_lead and cvr_i_1 ~="" and aar < &slutaar then job_change_1 = 1;
run;

proc sort data = fida_all out=fida_all_nodup nodupkey;
by pnr_1 aar cvrnr arbgnr arbnr DSKOD DSKODI lbnr PSTILL;
run;

* Extracts parts of the "familiedata" dataset to be used for further analysis;
data familiedata; set databib.familiedata;
keep familie_id aar famdispon_13 famloenmv_13 famperindkialt_13 famqaktivf_ny05 famqpassivn FAMANTALFSKATTEPLIGTIGE
									famaldaldbf famaldyngbf famantboernf kom FAMHOEJSTUDDA FAMBOLIGFORM FAMBOLIGTYPE 
									pnr_1 alder_1 civst_1 koen_1 hfaudd_1 erhvervsindk_13_1 perindkialt_13_1 socio13_1 
									pnr_2 alder_2 civst_2 koen_2 hfaudd_2 erhvervsindk_13_2 perindkialt_13_2 socio13_2
									koebt solgt ejerskift famboligvaerdi famforbrug1 famforbrug1a famforbrug2 famforbrug3 selvst ikkeskattepligtig famindkefterskat
									kobtnybil kobtbrugtbil kobtbil kobtbilvaerdi bilalder bilantal bilvaerdi fambankakt famindestpi famnyformue famnyformue_net bilvaerdi
									famkursakt famoblakt fampantakt famudlakt famrentudgpr famboligvaerdi fambankgaeld
									famoblgaeld fampantgaeld famqpassivn famafdrag_for famlejev_egen_bolig KomRobust_bopael;
if aar >= &startaar;
if aar <= &slutaar;

if aar le 2006 then do;
*Construction of a time-consistent municipality variable;
*Region Hovedstaden (29 kommuner);
if kom in (101,147,151,153,155,157,159,161,163,165,167,169,173,175,183,185,187,201,217,223) then KomRobust_bopael=kom; *uændret;
if kom in (189,207) then  KomRobust_bopael=190;
if kom in (208,227) then  KomRobust_bopael=210;
if kom in (219,231) then  KomRobust_bopael=219;
if kom in (181,205) then  KomRobust_bopael=230;
if kom in (171,235,237) then  KomRobust_bopael=240;
if kom in (209,225,229,233) then  KomRobust_bopael=250;
if kom in (211,221) then  KomRobust_bopael=260;
if kom in (213,215) then  KomRobust_bopael=270;
if kom in (401,403,405,407,409,400,411) then KomRobust_bopael=400; *Sammenlægninger på Bornholm fandt allerede sted i 2003. Desuden lægges Christiansø ind under Bornholm;

*Region Sjælland (17 kommuner);
if kom in (253,269,329) then KomRobust_bopael=kom; *uændret;
if kom in (259,267) then  KomRobust_bopael=259;
if kom in (255,263,265) then  KomRobust_bopael=265;
if kom in (305,327,343) then  KomRobust_bopael=306;
if kom in (315,321,339,341,345) then  KomRobust_bopael=316;
if kom in (313,351,385) then  KomRobust_bopael=320;
if kom in (301,309,317,319,323) then  KomRobust_bopael=326;
if kom in (311,325,331,333) then  KomRobust_bopael=330;
if kom in (271,389) then  KomRobust_bopael=336;
if kom in (303,335,337) then  KomRobust_bopael=340;
if kom in (251,257,261) then  KomRobust_bopael=350;
if kom in (355,359,363,367,379,381,383) then  KomRobust_bopael=360;
if kom in (307,353,357,373,393) then  KomRobust_bopael=370;
if kom in (369,371,375,387,391,395) then  KomRobust_bopael=376;
if kom in (361,365,377,397) then  KomRobust_bopael=390;

*Region Syddanmark (22 kommuner);
if kom in (461,563,607) then KomRobust_bopael=kom; *uændret;
if kom in (429,445,451) then  KomRobust_bopael=410;
if kom in (421,433,437,485,491,499) then  KomRobust_bopael=420;
if kom in (425,431,473,477,497) then  KomRobust_bopael=430;
if kom in (439,441,447) then  KomRobust_bopael=440;
if kom in (449,489,495) then  KomRobust_bopael=450;
if kom in (427,435,479) then  KomRobust_bopael=479;
if kom in (423,471,483) then  KomRobust_bopael=480;
if kom in (475,481,487) then  KomRobust_bopael=482;
if kom in (511,515,543) then  KomRobust_bopael=510;
if kom in (551,565) then  KomRobust_bopael=530;
if kom in (501,507,513,523,533,535,537) then  KomRobust_bopael=540;
if kom in (505,517,521,525,531,541) then  KomRobust_bopael=550;
if kom in (557,561,571) then  KomRobust_bopael=561;
if kom in (553,555,567,573,577) then  KomRobust_bopael=573;
if kom in (527,559,569,575) then  KomRobust_bopael=575;
if kom in (503,519,529,539,545) then  KomRobust_bopael=580;
if kom in (509,621,629,623) then  KomRobust_bopael=621;
if kom in (603,605,611,617,631) then  KomRobust_bopael=630;
if kom in (443,493,492) then KomRobust_bopael=492; *Marstal og Ærøskøbing slå sig allerede sammen i 2006 (til Ærø);

*Region Midtjylland (19 kommuner);
if kom in (727,741,751) then KomRobust_bopael=kom; *uændret;
if kom in (601,609,615) then  KomRobust_bopael=615;
if kom in (651,657,677,685) then  KomRobust_bopael=657;
if kom in (661,679,683) then  KomRobust_bopael=661;
if kom in (665,673) then  KomRobust_bopael=665;
if kom in (671,675) then  KomRobust_bopael=671;
if kom in (701,721,733,739) then  KomRobust_bopael=706;
if kom in (707,725,735,747) then  KomRobust_bopael=707;
if kom in (709,711,713,767) then  KomRobust_bopael=710;
if kom in (717,723,729,731,747) then  KomRobust_bopael=730;
if kom in (613,619,627) then  KomRobust_bopael=766;
if kom in (705,743,749,771) then  KomRobust_bopael=740;
if kom in (703,715,737,745) then  KomRobust_bopael=746;
if kom in (625,653,663) then  KomRobust_bopael=756;
if kom in (655,659,667,669,681) then  KomRobust_bopael=760;
if kom in (777,779,781,783) then  KomRobust_bopael=779;
if kom in (761,763,769,775,789,791) then  KomRobust_bopael=791;

*Region Nordjylland (11 kommuner);
if kom in (773,825) then KomRobust_bopael=kom; *uændret;
if kom in (765,785,787) then  KomRobust_bopael=787;
if kom in (805,807) then  KomRobust_bopael=810;
if kom in (813,841,847) then  KomRobust_bopael=813;
if kom in (793,809,827,861) then  KomRobust_bopael=820;
if kom in (833,843,845) then  KomRobust_bopael=840;
if kom in (719,801,815,823) then  KomRobust_bopael=846;
if kom in (803,811,835,849) then  KomRobust_bopael=849;
if kom in (817,831,837,851) then  KomRobust_bopael=851;
if kom in (819,821,829,839) then  KomRobust_bopael=860;
end;
else if aar ge 2007 then do;
KomRobust_bopael=kom;
if kom=411 then KomRobust_bopael=400; *Christiansø lægges ind under Bornholm;
end;
run; 


* Mortgage data regarding interest rate reset over the coming year (for URE-calculation, cf. Auclert);
%macro real;
%do i=2009 %to &slutaar;
%let j=%eval(&i+1);

data real&i; set databib.rk_individdata&i;
keep pnr restobl_rtl_tilp1 restobl_rtl_tilp_1;
run;

proc sort data=real&i; by pnr; run;

data real&i; 
merge real&i (in=a) raw.bef&j (keep = pnr familie_id);
by pnr;
if a;
tilp = restobl_rtl_tilp1 + restobl_rtl_tilp_1;
run;

proc sort data=real&i; by familie_id;

proc means data=real&i noprint;
output out=real_fam&i sum(tilp)=mortgage_refinance_1y; 
by familie_id;
run;

* proc means is done by year due to performance issues;

data real_fam&i; set real_fam&i; aar=&i; run;

%end;
%mend;
%real;

data real; set real_fam2009-real_fam&slutaar;
drop _TYPE_ _FREQ_; run;

proc sort data=real; by aar familie_id;
proc sort data=familiedata; by aar familie_id;


data familiedata; 
merge familiedata (in=a) real;
by aar familie_id;
if a;
if mortgage_refinance_1y = . & aar >=2009 then mortgage_refinance_1y=0;
run;





proc sort data=familiedata out=familiedata;
by pnr_1 aar;
run;


* IND-data regarding disposable income for each hh-member;
data ind; set ind&startaar-ind&slutaar;
dispindk_1 = dispon_13 - lejev_egen_bolig;
dispindk_2 = dispon_13 - lejev_egen_bolig;
indkefterskat_1 = perindkialt_13 - skatmvialt_13;
indkefterskat_2 = perindkialt_13 - skatmvialt_13;
pnr_1 = pnr;
pnr_2 = pnr;
run;

proc sort data=ind; by pnr aar; run;

* Merge datasets constructed above at the person level to pnr_1 in familiedata (the first of two adults in the family, no specific ordering;
data family_firm;
merge 	familiedata (in=a)
		fida_all_nodup (keep=pnr_1 aar cvrnr cvr_i_1 PSTILL ARLEDGR PERSBRC PSOC_STATUS_KODE FUNK job_change_1 industry_agg_1 industry_level4_1 work_function_agg_1 work_function_level4_1
		rename = (cvrnr=cvrnr_1 PSTILL=pstill_1 ARLEDGR=arledgr_1 PERSBRC=persbrc_1 FUNK=funk_1))
		ind (keep=pnr_1 aar dispindk_1 indkefterskat_1);
by pnr_1 aar; 
if a;
run;

data fida_all_nodup;
set fida_all_nodup;
rename pnr_1 = pnr_2;
run;
proc sort data=familiedata;
by pnr_2 aar;
run;
proc sort data=family_firm;
by pnr_2 aar;
run;

* Merge in information on pnr_2 in familiedata and construct additional variables;
data family_firm;
merge 	family_firm (in=a)
		fida_all_nodup (keep=pnr_2 aar cvrnr cvr_i_1 PSTILL ARLEDGR PERSBRC FUNK industry_agg_1 industry_level4_1 work_function_agg_1 work_function_level4_1 job_change_1 rename = (job_change_1=job_change_2 cvrnr=cvrnr_2 cvr_i_1=cvr_i_2 PSTILL=pstill_2 ARLEDGR=arledgr_2 industry_agg_1=industry_agg_2 industry_level4_1=industry_level4_2 work_function_agg_1=work_function_agg_2 work_function_level4_1=work_function_level4_2 PERSBRC=persbrc_2 FUNK=funk_2))
		ind (keep=pnr_2 aar dispindk_2 indkefterskat_2);
by pnr_2 aar; 
if a;
cvr1 = cvrnr_1 * 1;
cvr2 = cvrnr_2 * 1;
cvri1 = cvr_i_1 * 1;
cvri2 = cvr_i_2 * 1;
drop cvrnr_1 cvrnr_2 cvr_i_1 cvr_i_2;
rename cvr1 = cvrnr_1 cvr2 = cvrnr_2 cvri1 = cvr_i_1 cvri2 = cvr_i_2; *Converting character CVR to numeric;
rename aar = year;
if kobtnybil = . then kobtnybil = 0;
if kobtbrugtbil = . then kobtbrugtbil = 0;
kobtbil=0;
if kobtnybil = 1 then kobtbil=1;
if kobtbrugtbil = 1 then kobtbil=1;
if arledgr_1 ~=. then ledighed = 0;
if arledgr_1 > 0 then ledighed = 1;
if arledgr_2 > 0 then ledighed = 1;
region = put(KomRobust_bopael,KOMREG_V4_KT.);
part_of_country = put(KomRobust_bopael,KOMLAN_V4_KT.);
* highest_educ = put(famhoejstudda,AUDD2015_L1L5_T.);
run;

/*
* KBI;
proc sort data = kb.kbi out=kbi;
by year month cvrnr DESCENDING bvirk;
run;
proc sort data = kbi out=kbi nodupkey;
by cvrnr year month ;
run;
proc transpose data=kbi out=kbi_a prefix=exp_labor_m;
by cvrnr year;
id month;
var D_BESKAEFTIGELSE_FOR;
run;
proc transpose data=kbi out=kbi_b prefix=exp_prod_m;
by cvrnr year;
id month;
var D_PRODUKTION_FOR;
run;
data kbi_s;
merge kbi_a (drop = _NAME_) kbi_b (drop = _NAME_);
by cvrnr year;
survey = "manufacturing";
if cvrnr ~=.;
run;


* KBS;
proc sort data = kb.kbs out=kbs;
by year month cvrnr DESCENDING bvirk;
run;
proc sort data = kbs out=kbs nodupkey;
by cvrnr year month ;
run;
proc transpose data=kbs out=kbs_a prefix=exp_labor_m;
by cvrnr year;
id month;
var Beskæftigelse_for;
run;
proc transpose data=kbs out=kbs_b prefix=exp_turnover_m;
by cvrnr year;
id month;
var Omsætning_for;
run;
data kbs_s;
merge kbs_a (drop = _NAME_) kbs_b (drop = _NAME_);
by cvrnr year;
survey = "service";
if cvrnr ~=.;
run;



* KBB;
proc sort data = kb.kbb out=kbb;
by year month cvrnr DESCENDING BESK_TOT;
run;
proc sort data = kbb out=kbb nodupkey;
by cvrnr year month ;
run;
proc transpose data=kbb out=kbb_a prefix=exp_labor_m;
by cvrnr year;
id month;
var FOR_BESK_BOLIGER;
run;
proc transpose data=kbb out=kbb_b prefix=exp_turnover_m;
by cvrnr year;
id month;
var FOR_OMS_BOLIGER;
run;
data kbb_s;
merge kbb_a (drop = _NAME_) kbb_b (drop = _NAME_);
by cvrnr year;
survey = "construction";
if cvrnr ~=.;
run;

* Joining all surveys in one dataset;
data kb;
set kbb_s kbs_s kbi_s;
run;
proc sort data=kb dupout=dups nodupkey;
by cvrnr year;
run;
data dups; set dups;
del=1;
run;
* Removing duplicates (i.e. firms which have participated in more than one survey);
data kb; merge kb dups (keep = cvrnr year del);
by cvrnr year;
if del ~=1;
drop del;
run;

data kb1; * Suffix _1 (to be used for family member 1);
set kb (
rename =(exp_labor_m1 - exp_labor_m12 = exp_labor_1_m1 - exp_labor_1_m12)
rename =(exp_turnover_m1 - exp_turnover_m12 = exp_turnover_1_m1 - exp_turnover_1_m12 ) 
rename =(exp_prod_m1 - exp_prod_m12 = exp_prod_1_m1 - exp_prod_1_m12 ) 
);
rename cvrnr = cvr_i_1;
rename survey = survey_1;
run;
data kb2; * Suffix _2 (to be used for family member 2);
set kb (
rename =(exp_labor_m1 - exp_labor_m12 = exp_labor_2_m1 - exp_labor_2_m12)
rename =(exp_turnover_m1 - exp_turnover_m12 = exp_turnover_2_m1 - exp_turnover_2_m12 ) 
rename =(exp_prod_m1 - exp_prod_m12 = exp_prod_2_m1 - exp_prod_2_m12 ) 
);
rename cvrnr = cvr_i_2;
rename survey = survey_2;
run;

* Merging with family-level dataset;
proc sort data=family_firm; by cvr_i_1 year; run;
proc sort data=kb1; by cvr_i_1 year; run;

data family_firm; 
merge family_firm (in=a) kb1;
by cvr_i_1 year;
if a;
run;

proc sort data=family_firm; by cvr_i_2 year; run;
proc sort data=kb2; by cvr_i_2 year; run;

data saving.family_firm; 
merge family_firm (in=a) kb2;
by cvr_i_2 year;
if a;
if cvr_i_1 ~= . then interpolate_CVR_1 = 0;
if cvrnr_1 ~= cvr_i_1 then interpolate_CVR_1 = 1;
if cvr_i_2 ~= . then interpolate_CVR_2 = 0;
if cvrnr_2 ~= cvr_i_2 then interpolate_CVR_2 = 1;
run;
*/
*OBS slet hvis ovenstående ikke er udkommenteret;
data saving.family_firm; 
set family_firm; run;

proc sort data=saving.family_firm;
by familie_id year;
run;

proc export
data = saving.family_firm
outfile = "E:\ProjektDB\706172\Workdata\706172\Husholdningsprojekt\Precautionary saving with time varying risk/family_firm.txt"
DBMS=TAB REPLACE;
PUTNAMES=YES;
run;


ods csv file="E:\ProjektDB\706172\Workdata\706172\Husholdningsprojekt\Precautionary saving with time varying risk\varlist sas.csv";
proc contents data=saving.family_firm; run;
proc contents data=databib.familiedata; run;
ods csv close;
