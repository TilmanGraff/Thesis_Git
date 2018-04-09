*******************************************************************************************************************************
** Pre-colonial Ethnic Institutions and Contemporary African Development
** Econometrica 2012
** Stelios Michalopoulos and Elias Papaioannou
*******************************************************************************************************************************


*******************************************************************************************************************************
*******************************************************************************************************************************
** Contiguous Ethnic Homeland Analysis at the Pixel-level
** Tables 6- 7
*******************************************************************************************************************************
*******************************************************************************************************************************
*******************************************************************************************************************************

*******************************************************************************************************************************
** Preliminaries
*******************************************************************************************************************************

** set of control variables at the pixel-level
global pix lnkm pixpetro pixdia pixwaterd pixcapdist pixsead pixmal pixsuit pixelev  pixbdist

** describe variables
d, all

** visualize panel id; ethnic-pair in the same country
tab wbidd

********************************************************************************************************************************
** Table 6: Pre-colonial Ethnic Institutions (centr_tribe) and Various Geographic-Ecological-Natural Resource Variables within 
** Contiguous Ethnic Pairs in the Same Country
** all specifications include ethnic-pair-country FE (wbidd)
*********************************************************************************************************************************
set more off

xi: cgmreg  pixpetro      centr_tribe      i.wbidd ,   robust cluster(pixwbcode pixcluster)
est store centr_tribe1ia

xi: cgmreg  pixdia        centr_tribe      i.wbidd ,   robust cluster(pixwbcode pixcluster)
est store centr_tribe1ib

xi: cgmreg  pixwaterd     centr_tribe      i.wbidd ,   robust cluster(pixwbcode pixcluster)
est store centr_tribe2ia

xi: cgmreg  pixcap        centr_tribe      i.wbidd ,   robust cluster(pixwbcode pixcluster)
est store centr_tribe2ib

xi: cgmreg  pixsead       centr_tribe      i.wbidd ,   robust cluster(pixwbcode pixcluster)
est store centr_tribe3ia

xi: cgmreg  pixmal        centr_tribe      i.wbidd ,   robust cluster(pixwbcode pixcluster)
est store centr_tribe3ib

xi: cgmreg  pixsuit       centr_tribe      i.wbidd ,   robust cluster(pixwbcode pixcluster)
est store centr_tribe4ia

xi: cgmreg  pixelev       centr_tribe      i.wbidd ,   robust cluster(pixwbcode pixcluster)
est store centr_tribe4ib

xi: cgmreg  pixbdist      centr_tribe      i.wbidd ,   robust cluster(pixwbcode pixcluster)
est store centr_tribe5ib


** Table 6 - Results
*********************
estout  centr_tribe1ia centr_tribe1ib centr_tribe2ia centr_tribe2ib centr_tribe3ia centr_tribe3ib centr_tribe4ia centr_tribe4ib centr_tribe5ib, cells(b(star fmt(%9.4f)) se(par) t(fmt(%9.2f)))   ///
       stats(N r2_a, fmt(%9.3f %9.0g) labels(R-squared)) keep(centr_tribe) starlevels(* 0.1 ** 0.05  *** 0.01) style(fixed)



********************************************************************************************************************************
** Table 7: Pre-colonial Ethnic Institutions (centr_tribe) and Regional Development at the Pixel-level within 
** Contiguous Ethnic Pairs in the Same Country
** all specifications include ethnic-pair-country FE (wbidd)
*********************************************************************************************************************************
** columns (1)-(3): using all observations
** columns (4)-(6): differences in the jurisdictional hierarchy index > 1
** columns (7)-(9): one of the two ethnicities in each pair was part of a large pre-colonial state (centr_tribe = 3 or 4)
*********************************************************************************************************************************

xi: cgmreg l0708d 		centr_tribe                  						i.wbidd          				, cluster(pixwbcode pixcluster) 
est store cfeewbi1

xi: cgmreg l0708d 		centr_tribe  		lnpd0           				i.wbidd          				, cluster(pixwbcode pixcluster) 
est store cfeewbi2

xi: cgmreg l0708d 		centr_tribe  		lnpd0      $pix 				i.wbidd          				, cluster(pixwbcode pixcluster) 
est store cfeewbi3

xi: cgmreg l0708d 		centr_tribe                  						i.wbidd 	if diff_centr_tribe>1, cluster(pixwbcode pixcluster) 
est store cfeewbi4

xi: cgmreg l0708d 		centr_tribe  		lnpd0           				i.wbidd 	if diff_centr_tribe>1, cluster(pixwbcode pixcluster) 
est store cfeewbi5

xi: cgmreg l0708d 		centr_tribe  		lnpd0     $pix 				i.wbidd 	if diff_centr_tribe>1, cluster(pixwbcode pixcluster) 
est store cfeewbi6

xi: cgmreg l0708d 		centr_tribe                  						i.wbidd 	if max>2			, cluster(pixwbcode pixcluster) 
est store cfeewbi7

xi: cgmreg l0708d 		centr_tribe  		lnpd0           				i.wbidd 	if max>2			, cluster(pixwbcode pixcluster) 
est store cfeewbi8

xi: cgmreg l0708d 		centr_tribe  		lnpd0      $pix 				i.wbidd 	if max>2			, cluster(pixwbcode pixcluster) 
est store cfeewbi9

** Table 7 Results
*************************************************************************************************************************************
estout cfeewbi1 cfeewbi2 cfeewbi3 cfeewbi4 cfeewbi5 cfeewbi6 cfeewbi7 cfeewbi8 cfeewbi9, cells(b(star fmt(%9.4f)) se(par) t(fmt(%9.2f)))   ///
       stats(r2_a N , fmt(%9.3f %9.0g) labels(R-squared)) keep(centr_tribe) starlevels(* 0.1 ** 0.05  *** 0.01) style(fixed)




