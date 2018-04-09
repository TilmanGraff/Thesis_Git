*********************************************************************************************************************************
** Pre-colonial Ethnic Institutions and Contemporary African Development
** Econometrica 2012
** Stelios Michalopoulos and Elias Papaioannou
**********************************************************************************************************************************


***********************************************************************************************************************************
** Results at the Ethnic-country homeland level using data from the 2005 Afrobarometer Surveys
** Appendix Table 5 
** The data are extracted from Nunn and Watchekon (AER 2011)
** use afrobarometer-data-final.dta
***********************************************************************************************************************************

d , all



**** Preliminaries
***********************************************************************************************************************************

*** Set of control variables
****************************
global poly    	capdistance1 seadist1  borderdist1
global geo 		lnwaterkm mean_elev mean_suit malariasuit petroleum diamond lnkm2split


** Generate dependent variables
*******************************
gen lnliving=ln(living_conditions)
gen lneducation=ln(education)


label var lneducation			"Log of average years of schooling at the ethnicity-country level. Afrobarometer"
label var lnliving			"Log of of living conditions index at the ethnicity-country level. Afrobarometer"

** Visulalize dependent variables
*********************************
histogram lneducation
histogram lnliving

sum lnliving lneducation, det

*********************************************************************************************************************************
*********************************************************************************************************************************
*** Appendix Table 5
*********************************************************************************************************************************
*********************************************************************************************************************************

** Results with the Log of the Living Conditions Index
*******************************************************
**columns (1)-(4)

xi: cgmreg lnliving gr      	      			i.wbcode, cluster(cluster wbcode)
est store lliv1
xi: cgmreg lnliving gr 		lnpdmean00s			i.wbcode, cluster(cluster wbcode)
est store lliv2
xi: cgmreg lnliving gr	 	lnpdmean00s	$poly 	 i.wbcode , cluster(cluster wbcode)
est store lliv3
xi: cgmreg lnliving gr	 	lnpdmean00s	$poly $geo i.wbcode , cluster(cluster wbcode)
est store lliv4



** Results with Log Education 
******************************
** columns (5)-(8)

xi: cgmreg lneducation	gr      	      			i.wbcode, cluster(cluster wbcode)
est store led1
xi: cgmreg lneducation	gr 		lnpdmean00s			i.wbcode, cluster(cluster wbcode)
est store led2
xi: cgmreg lneducation	gr	 	lnpdmean00s	$poly 	i.wbcode , cluster(cluster wbcode)
est store led3
xi: cgmreg lneducation	gr	 	lnpdmean00s	$poly $geo i.wbcode , cluster(cluster wbcode)
est store led4


** Appendix Table 5 - Results
*****************************
estout lliv1 lliv2 lliv3 lliv4 led1 led2 led3 led4, cells(b(star fmt(%9.4f)) se(par) t(fmt(%9.2f)))   ///
       stats(N r2_a, fmt(%9.3f %9.0g) labels(R-squarpip)) keep(gr) starlevels(* 0.1 ** 0.05  *** 0.01) style(fixed)
*********************************************************************************************************************************
*********************************************************************************************************************************
*********************************************************************************************************************************





***** Not reported estimates but commented in the Appendix
***** Results with living conditions index and education expressed in levels
***** somewhat weaker significance between political centralization and education or wealth

xi: cgmreg living gr      	      			i.wbcode, cluster(cluster wbcode)
est store liv1
xi: cgmreg living gr 		lnpdmean00s			i.wbcode, cluster(cluster wbcode)
est store liv2
xi: cgmreg living gr	 	lnpdmean00s	$poly 	 i.wbcode , cluster(cluster wbcode)
est store liv3
xi: cgmreg living gr	 	lnpdmean00s	$poly $geo i.wbcode , cluster(cluster wbcode)
est store liv4


xi: cgmreg education	gr      	      			i.wbcode, cluster(cluster wbcode)
est store ed1
xi: cgmreg education	gr 		lnpdmean00s			i.wbcode, cluster(cluster wbcode)
est store ed2
xi: cgmreg education	gr	 	lnpdmean00s	$poly 	i.wbcode , cluster(cluster wbcode)
est store ed3
xi: cgmreg education	gr	 	lnpdmean00s	$poly $geo i.wbcode , cluster(cluster wbcode)
est store ed4


** Results (not reported)
*****************************
estout liv1 liv2 liv3 liv4 ed1 ed2 ed3 ed4, cells(b(star fmt(%9.4f)) se(par) t(fmt(%9.2f)))   ///
       stats(N r2_a, fmt(%9.3f %9.0g) labels(R-squarpip)) keep(gr) starlevels(* 0.1 ** 0.05  *** 0.01) style(fixed)



