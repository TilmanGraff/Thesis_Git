*******************************************************************************************************************************
** Pre-colonial Ethnic Institutions and Contemporary African Development
** Econometrica 2012
** Stelios Michalopoulos and Elias Papaioannou
*******************************************************************************************************************************


*******************************************************************************************************************************
** Baseline results at the pixel homeland level
** Table 5, Appendix Table 2 - Panel B, Appendix Table 4 - Panel B, Appendix Table 6
** use pixel-level-baseline-final.dta
*******************************************************************************************************************************
d, all



********************************************
***Pixel Level Analysis
***Introduction*****************************
*** Table 1: Summary Statistics
********************************************
***Summary Statistics
********************************************
tabstat 	l0708 l0708d  lnl0708s     	if  	centr_tribe!=. , stat(N mean sd p25 p50 p75 min max) col(stat)
tabstat  	l0708 l0708d  lnl0708s 		if 	centr_tribe==0, stat(N mean sd p25 p50 p75 min max) col(stat)
tabstat  	l0708 l0708d  lnl0708s 		if 	centr_tribe==1, stat(N mean sd p25 p50 p75 min max) col(stat)
tabstat  	l0708 l0708d  lnl0708s 		if 	centr_tribe==2, stat(N mean sd p25 p50 p75 min max) col(stat)
tabstat  	l0708 l0708d  lnl0708s 		if 	centr_tribe==3 | centr_tribe==4, stat(N mean sd p25 p50 p75 min max) col(stat)


** Appendix Table 2 - Panel B
*******************************************************************
** Summary Statistics for Main Control Variables at the Pixel-Level
*******************************************************************
d 			lnpd0 pixwaterd lnkm pixelev pixsuit ///
			pixmal pixpetro pixdia pixcapdist pixsead  pixbdist  centr_tribe centr_tribe1 gr
			

tabstat  lnpd0 pixwaterd lnkm pixelev pixsuit ///
         pixmal pixpetro pixdia pixcapdist pixsead  pixbdist  ///
         centr_tribe gr , stat(N mean sd p25 p50 p75 min max) col(stat)



** Pixel-level conditioning set
global pix   lnkm pixpetro pixdia pixwaterd pixcapdist pixmal pixsead pixsuit pixelev  pixbdist

** Additional controls at the ethnicity-country level
global geo   lnwaterkm lnkm2split mean_elev mean_suit malariasuit petroleum diamond
global poly  capdistance1 seadist1  borderdist1




***************************************
***Table 5*****************************
***************************************
**Panel A. Benchmark results with the Jurisditional hierarchy index (0-4)
***************************************************************************

set more off

xi: cgmreg l0708d 		centr_tribe                                     									, cluster(pixwbcode pixcluster)
est store pixgr1

xi: cgmreg l0708d 		centr_tribe                            							i.pixwbcode         	, cluster(pixwbcode pixcluster)
est store pixgr2

xi: cgmreg l0708d 		centr_tribe             	lnpd0         						i.pixwbcode 		, cluster(pixwbcode pixcluster)
est store pixgr3
	
xi: cgmreg l0708d 		centr_tribe              	lnpd0 		$pix    				i.pixwbcode 		, cluster(pixwbcode pixcluster)
est store pixgr4

xi: cgmreg l0708d 		centr_tribe              	lnpd0 		$pix $geo $poly 			i.pixwbcode			,  cluster(pixwbcode pixcluster)
est store pixgr5


xi: cgmreg lnl0708s  		centr_tribe                                       									, cluster(pixwbcode pixcluster)
est store lpixgr1

xi: cgmreg lnl0708s  		centr_tribe                        								i.pixwbcode          	, cluster(pixwbcode pixcluster)
est store lpixgr2

xi: cgmreg lnl0708s  		centr_tribe 			lnpd0 							i.pixwbcode   		, cluster(pixwbcode pixcluster)
est store lpixgr3

xi: cgmreg lnl0708s  		centr_tribe 			lnpd0 	$pix  					i.pixwbcode   		, cluster(pixwbcode pixcluster)
est store lpixgr4

xi: cgmreg lnl0708s  		centr_tribe		 		lnpd0 	$pix 	$geo $poly 				i.pixwbcode   		, cluster(pixwbcode pixcluster)
est store lpixgr5

*** Table 5 - Panel A 
***********************************************************************
** columns (1)-(5) : linear probability models (dep. vaiable lit/unlit)
** columns (6)-(10): dependent variable log luminosity + 0.01
***********************************************************************
estout pixgr1 pixgr2 pixgr3 pixgr4 pixgr5 lpixgr1 lpixgr2 lpixgr3 lpixgr4 lpixgr5, cells(b(star fmt(%9.4f)) se(par) t(fmt(%9.2f)))   ///
       stats(r2_a N, fmt(%9.3f %9.0g) labels(R-squared)) keep(centr_tribe  ) starlevels(* 0.1 ** 0.05  *** 0.01) style(fixed)




*** Table 5 - Panel B: Allowing the Effect to Differ between the vrious Pre-colonial Institutional Arrangements 
*************************************************************************************************************************
** columns (1)-(5) : linear probability models (dep. vaiable lit/unlit)
** columns (6)-(10): dependent variable log luminosity + 0.01
*************************************************************************************************************************

xi: cgmreg l0708d 		i.centr_tribe1                                    						 				, cluster(pixwbcode pixcluster)
est store fpixgr1

xi: cgmreg l0708d  		i.centr_tribe1 													i.pixwbcode    	, cluster(pixwbcode pixcluster)
est store fpixgr2

xi: cgmreg l0708d  		i.centr_tribe1 				   lnpd0 								i.pixwbcode 	, cluster(pixwbcode pixcluster)
est store fpixgr3

xi: cgmreg l0708d  		i.centr_tribe1 				   lnpd0 	$pix     						i.pixwbcode       , cluster(pixwbcode pixcluster)
est store fpixgr4

xi: cgmreg l0708d  		i.centr_tribe1 				   lnpd0 	$pix 	$geo $poly 					i.pixwbcode 	, cluster(pixwbcode pixcluster)
est store fpixgr5

xi: cgmreg lnl0708s  	i.centr_tribe1                                    						 					, cluster(pixwbcode pixcluster)
est store fpixgr6

xi: cgmreg lnl0708s  	i.centr_tribe1 														i.pixwbcode       , cluster(pixwbcode pixcluster)
est store fpixgr7

xi: cgmreg lnl0708s  	i.centr_tribe1 				   lnpd0 									i.pixwbcode 	, cluster(pixwbcode pixcluster)
est store fpixgr8

xi: cgmreg lnl0708s  	i.centr_tribe1 				   lnpd0 $pix     							i.pixwbcode       , cluster(pixwbcode pixcluster)
est store fpixgr9

xi: cgmreg lnl0708s  	i.centr_tribe1 				   lnpd0 $pix 		$geo $poly 					i.pixwbcode 	, cluster(pixwbcode pixcluster)
est store fpixgr10


*** Table 5 - Panel B: Allowing the Effect to Differ between the vrious Pre-colonial Institutional Arrangements 
*** Results 
*************************************************************************************************************************
** columns (1)-(5) : linear probability models (dep. vaiable lit/unlit)
** columns (6)-(10): dependent variable log luminosity + 0.01
*************************************************************************************************************************
xi: estout fpixgr1 fpixgr2 fpixgr3 fpixgr4 fpixgr5 fpixgr6 fpixgr7 fpixgr8 fpixgr9 fpixgr10, cells(b(star fmt(%9.4f)) se(par) t(fmt(%9.2f)))   ///
       stats(r2_a N, fmt(%9.3f %9.0g) labels(R-squared)) keep(i.centr_tribe1) starlevels(* 0.1 ** 0.05  *** 0.01) style(fixed)





************************************************************************************************************
**Appendix Table 4 - Panel B: Dependent Varable log population desnity at the pixel-level
************************************************************************************************************
************************************************************************************************************
** models (1)-(4): using jurisditional hierarchy index (0-4)
** models (5)-(8): using binary political centralization index (following Gennaioli and Rainer (2006, 2007)
************************************************************************************************************
set more off

xi: cgmreg lnpd0  centr_tribe                                     					, cluster(pixwbcode pixcluster)
est store pixpd1

xi: cgmreg lnpd0  centr_tribe                     						i.pixwbcode , cluster(pixwbcode pixcluster)
est store pixpd2

xi: cgmreg lnpd0  centr_tribe       $pix    							i.pixwbcode , cluster(pixwbcode pixcluster)
est store pixpd3

xi: cgmreg lnpd0  centr_tribe       $pix 				$geo $poly		 	i.pixwbcode	,  cluster(pixwbcode pixcluster)
est store pixpd4

xi: cgmreg lnpd0          gr                                     						, cluster(pixwbcode pixcluster)
est store pixpd5

xi: cgmreg lnpd0          gr       									i.pixwbcode , cluster(pixwbcode pixcluster)
est store pixpd6

xi: cgmreg lnpd0          gr       $pix    							i.pixwbcode , cluster(pixwbcode pixcluster)
est store pixpd7

xi: cgmreg lnpd0          gr       $pix 				$geo $poly		 	i.pixwbcode	,  cluster(pixwbcode pixcluster)
est store pixpd8

estout pixpd1 pixpd2 pixpd3 pixpd4 pixpd5 pixpd6 pixpd7 pixpd8, cells(b(star fmt(%9.4f)) se(par) t(fmt(%9.2f)))   ///
       stats(r2_a N, fmt(%9.3f %9.0g) labels(R-squared)) keep(centr_tribe gr) starlevels(* 0.1 ** 0.05  *** 0.01) style(fixed)





** Appendix Table 6 : Mirror table of Table 5 but with the binary political centralization index
**************************************************************************************************
** columns (1)-(5) : linear probability models (dep. vaiable lit/unlit)
** columns (6)-(10): dependent variable log luminosity + 0.01
***********************************************************************

xi: cgmreg l0708d gr                                    						 							, cluster(pixwbcode pixcluster)
est store pixgr6	

xi: cgmreg l0708d gr                        											i.pixwbcode         	, cluster(pixwbcode pixcluster)
est store pixgr7

xi: cgmreg l0708d gr 				   		lnpd0 								i.pixwbcode 		, cluster(pixwbcode pixcluster)
est store pixgr8

xi: cgmreg l0708d gr 				  		lnpd0 $pix     							i.pixwbcode         	, cluster(pixwbcode pixcluster)
est store pixgr9

xi: cgmreg l0708d gr 				   		lnpd0 $pix 			$geo $poly 				i.pixwbcode 		, cluster(pixwbcode pixcluster)
est store pixgr10



xi: cgmreg lnl0708s  gr                                     												, cluster(pixwbcode pixcluster)
est store lpixgr6

xi: cgmreg lnl0708s  gr                      			 								i.pixwbcode             , cluster(pixwbcode pixcluster)
est store lpixgr7

xi: cgmreg lnl0708s  gr		 				lnpd0 				 				i.pixwbcode 		, cluster(pixwbcode pixcluster)
est store lpixgr8

xi: cgmreg lnl0708s  gr 	 				lnpd0 $pix    			 				i.pixwbcode 		, cluster(pixwbcode pixcluster)
est store lpixgr9

xi: cgmreg lnl0708s  gr 	 				lnpd0 $pix 			$geo $poly		 		i.pixwbcode 		, cluster(pixwbcode pixcluster)
est store lpixgr10


** Appendix Table 6
** columns (1)-(5) : linear probability models (dep. vaiable lit/unlit)
** columns (6)-(10): dependent variable log luminosity + 0.01
***********************************************************************
estout pixgr6 pixgr7 pixgr8 pixgr9 pixgr10 lpixgr6 lpixgr7 lpixgr8 lpixgr9 lpixgr10, cells(b(star fmt(%9.4f)) se(par) t(fmt(%9.2f)))   ///
       stats(r2_a N, fmt(%9.3f %9.0g) labels(R-squared)) keep(gr  ) starlevels(* 0.1 ** 0.05  *** 0.01) style(fixed)


