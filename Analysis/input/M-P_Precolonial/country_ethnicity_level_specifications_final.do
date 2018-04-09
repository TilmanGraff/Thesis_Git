*******************************************************************************************************************************
** Pre-colonial Ethnic Institutions and Contemporary African Development
** Econometrica 2012
** Stelios Michalopoulos and Elias Papaioannou
*******************************************************************************************************************************


*******************************************************************************************************************************
** Results at the Ethnic-country homeland level
** Table 1, Table 2, Table 3, Appendix Table 2 - Panel A, Appendix Table 3, Appendix Table 4 - Panel A, Appendix Figures 1a-1c
** use ethnicity-country-data-final.dta
*******************************************************************************************************************************
d, all

** variables v1-v114 come from George Peter Murdock's Ethnolinguistic Atlas
*******************************************************************************************************************************
***Set of control vaiables
*******************************************************************************************************************************
** poly: location controls
** geo: geographic/ecological/natural resource controls

global poly    capdistance1 seadist1  borderdist1
global geo     lnwaterkm lnkm2split mean_elev mean_suit malariasuit petroleum diamond

d $poly
d $geo 



****************************************
**Table 1********************************
**Summary Statistics and test of means
*****************************************
*Light Density the full sample 
*****************************************
tabstat  mean_lights0708 lnlights_nozeros lnlights0708 			, stat(N mean sd p25 p50 p75 min max) col(stat)


**light density by degree of jurisditional hierarchy (merge 3 and 4 since only 3 ethnic groups where centr_tribe==4)
tabstat  mean_lights0708 lnlights_nozeros lnlights0708 if centr_tribe==0, stat(N mean sd p25 p50 p75 min max) col(stat)
tabstat  mean_lights0708 lnlights_nozeros lnlights0708 if centr_tribe==1, stat(N mean sd p25 p50 p75 min max) col(stat)
tabstat  mean_lights0708 lnlights_nozeros lnlights0708 if centr_tribe==2, stat(N mean sd p25 p50 p75 min max) col(stat)
tabstat  mean_lights0708 lnlights_nozeros lnlights0708 if centr_tribe==3 | centr_tribe==4, stat(N mean sd p25 p50 p75 min max) col(stat)


******************************************************************************************************
***Table 2********************************************************************************************
*** LS Estimates: Correlation Analysis - Preliminary Evidence*****************************************
******************************************************************************************************

**column (1): simple unconditional model
**column (2): controling for log population density (in 2000)
**column (3): controling for log population density (in 2000) and on location
**column (4): controling for log population density (in 2000), on location, and rich set of controls
**column (5): conditioning on all controls and rule of law (in 2007)
**column (6): conditioning on all controls and log GDP p.c. (in 2007)

** Please see at the end of the program file the programs to estimate Conley's GMM s.e.
********************************************************************************************************

xi: cgmreg  lnlights_nozeros    	centr_tribe                                         							,   robust cluster(wbcode cluster)
est store dir1

xi: cgmreg  lnlights_nozeros    	centr_tribe		lnpdmean00s    							 			,   robust cluster(wbcode cluster)
est store dir2

xi: cgmreg  lnlights_nozeros    	centr_tribe		lnpdmean00s    		$poly     			 				,   robust cluster(wbcode cluster)
est store dir3

xi: cgmreg  lnlights_nozeros    	centr_tribe		lnpdmean00s    		$poly     $geo    	 				,   robust cluster(wbcode cluster)
est store dir4

xi: cgmreg  lnlights_nozeros   	centr_tribe 	lnpdmean00s    		$poly     $geo     rlaw07        			,   robust cluster(wbcode cluster)
est store dir5

xi: cgmreg  lnlights_nozeros       	centr_tribe 	lnpdmean00s   		$poly     $geo     lnrgdpch07   			,   robust cluster(wbcode cluster)
est store dir6


estout dir1 dir2 dir3 dir4 dir5 dir6, cells(b(star fmt(%9.4f)) se(par) t(fmt(%9.2f)))   ///
       stats(r2_a N , fmt(%9.3f %9.0g) labels(R-squared)) keep(centr_tribe rlaw07 lnrgdpch07) starlevels(* 0.1 ** 0.05  *** 0.01) style(fixed)




********************************************************************************
**Table 3***********************************************************************
********************************************************************************
***Within Country Estimates on the Effect of Pre-colonial Institutions/Features
********************************************************************************
*models (1) and (5): only country FE
*models (2) and (6): country FE and rich set of controls (poly and geo)
*models (3) and (7): country FE and log PD 
*models (4) and (8): country FE, log PD, and rich set of controls (poly and geo)

**generate variable indicating number of ethnicity-country regions in each country
bysort wbcode: egen obs=count(gr) 


***Panel A: LS estimates using all (country-ethnicity) observations 
*********************************************************************************

xi:  cgmreg lnlights_nozeros        centr_tribe      		i.wbcode                  								if   obs>1 ,   robust cluster(cluster wbcode)
est store tir1

xi:  cgmreg lnlights_nozeros        centr_tribe      		i.wbcode  								$poly   $geo      if   obs>1 ,   robust cluster(cluster wbcode)
est store tir2

xi:  cgmreg lnlights_nozeros      	centr_tribe       	i.wbcode  			lnpdmean00s                				if   obs>1 ,   robust cluster(cluster wbcode)
est store tir3

xi:  cgmreg lnlights_nozeros    	centr_tribe       	i.wbcode 			lnpdmean00s  			$poly   $geo      if   obs>1 ,   robust cluster(cluster wbcode)
est store tir4

xi:  cgmreg lnlights_nozeros            	gr            	i.wbcode                  								if   obs>1 ,  robust cluster(cluster wbcode)
est store tir5

xi:  cgmreg lnlights_nozeros            	gr            	i.wbcode 								$poly $geo      	if  obs>1 ,  robust cluster(cluster wbcode)
est store tir6

xi:  cgmreg lnlights_nozeros    		gr             	i.wbcode    		lnpdmean00s               				if  obs>1 ,  robust cluster(cluster wbcode)
est store tir7

xi:  cgmreg lnlights_nozeros      		gr             	i.wbcode  			lnpdmean00s 			$poly $geo      	if  obs>1 ,  robust cluster(cluster wbcode)
est store tir8


*** Table 3 - Panel A: LS estimates (all observations) 
*** Results
******************************************************
estout tir1 tir2 tir3 tir4 tir5 tir6 tir7 tir8  , cells(b(star fmt(%9.4f)) se(par) t(fmt(%9.2f)))   ///
       stats(r2_a N, fmt(%9.3f %9.0g) labels(R-squared)) keep(centr_tribe  gr    ) starlevels(* 0.1 ** 0.05  *** 0.01 )   style(fixed)


***Panel B: LS estimates at the intensive margin of luminosity (excludiing country-ethnic regions with zero luminosity
************************************************************************************************************************************************************

**generate variable indicating number of ethnicity-country regions in each country with some lights (intensive margin)
bysort wbcode: egen obs1=count(gr) if mean_lights0708>0

xi:  cgmreg lnlights0708        	centr_tribe     i.wbcode                  							if   obs1>1 ,   robust cluster(cluster wbcode)
est store itir1

xi:  cgmreg lnlights0708       	centr_tribe     i.wbcode   							$poly $geo      	if   obs1>1 ,   robust cluster(cluster wbcode)
est store itir2

xi:  cgmreg lnlights0708 		centr_tribe 	i.wbcode  		lnpdmean00s  			         		if   obs1>1 ,   robust cluster(cluster wbcode)
est store itir3

xi:  cgmreg lnlights0708  		centr_tribe  	i.wbcode		lnpdmean00s   			$poly $geo      	if   obs1>1 ,   robust cluster(cluster wbcode)
est store itir4

xi:  cgmreg lnlights0708           	gr            	i.wbcode                  							if  obs1>1 ,  robust cluster(cluster wbcode)
est store itir5

xi:  cgmreg lnlights0708            gr            	i.wbcode   							$poly $geo      	if   obs1>1 ,  robust cluster(cluster wbcode)
est store itir6

xi:  cgmreg  lnlights0708  		gr             	i.wbcode 		lnpdmean00s                  				if   obs1>1 ,  robust cluster(cluster wbcode)
est store itir7

xi:  cgmreg lnlights0708      	gr             	i.wbcode 		lnpdmean00s  			$poly $geo      	if   obs1>1 ,  robust cluster(cluster wbcode)
est store itir8


*** Table 3 - Panel B: LS estimates at the intensive margin
*** Results
***********************************************************
estout itir1 itir2 itir3 itir4 itir5 itir6  itir7 itir8, cells(b(star fmt(%9.4f)) se(par) t(fmt(%9.2f)))   ///
       stats(r2_a N, fmt(%9.3f %9.0g) labels(R-squared)) keep(centr_tribe  gr    ) starlevels(* 0.1 ** 0.05  *** 0.01 )   style(fixed)



*** Table 3 - Panels A and B: LS estimates allowing the coefficient to differ for the various pre-colonial political organizations
*************************************************************************************************************************************************************
***columns (9)-(12)******************************************************************************************************************************************
***Allowing the coefficient on centr_tribe to differ with the degree of centarlization. omitted category centr_tribe1=0; stateless societies
***since there are only two ethnic grousp where the jurisdictional hierarchy index equals 4, we merge this category with 3
*************************************************************************************************************************************************************
*model (1): only country FE
*model (2): country FE and rich set of controls (poly and geo)
*model (3): country FE and log PD 
*model (4): country FE, log PD, and rich set of controls (poly and geo)


xi:  cgmreg lnlights_nozeros        i.centr_tribe1      i.wbcode                    		                 				if  obs>1 ,   robust cluster(cluster wbcode)
est store fig1

xi:  cgmreg lnlights_nozeros        i.centr_tribe1      i.wbcode    					$poly   $geo         			if   obs>1 ,   robust cluster(cluster wbcode)
est store fig2

xi:  cgmreg lnlights_nozeros        i.centr_tribe1      i.wbcode			lnpdmean00s           						if   obs>1 ,   robust cluster(cluster wbcode)
est store fig3

xi:  cgmreg lnlights_nozeros        i.centr_tribe1      i.wbcode			lnpdmean00s      	$poly   $geo        			if   obs>1 ,   robust cluster(cluster wbcode)
est store fig4

**

xi:  cgmreg lnlights0708        	i.centr_tribe1      i.wbcode                    		                 				if   obs>1 ,   robust cluster(cluster wbcode)
est store fig1i

xi:  cgmreg lnlights0708        	i.centr_tribe1      i.wbcode    						$poly   $geo                	if   obs>1 ,   robust cluster(cluster wbcode)
est store fig2i

xi:  cgmreg lnlights0708        	i.centr_tribe1      i.wbcode 			lnpdmean00s       						if  obs>1 ,   robust cluster(cluster wbcode)
est store fig3i

xi:  cgmreg lnlights0708        	i.centr_tribe1      i.wbcode 			lnpdmean00s 		$poly   $geo               	if   obs>1 ,   robust cluster(cluster wbcode)
est store fig4i


** Table 3A - Results : columns (9)-(12)
****************************************
xi: estout fig1 fig2 fig3 fig4 , cells(b(star fmt(%9.4f)) se(par) t(fmt(%9.2f)))   ///
       stats(r2_a N, fmt(%9.3f %9.0g) labels(R-squared)) keep(i.centr_tribe1     ) starlevels(* 0.1 ** 0.05  *** 0.01 )   style(fixed)

** Table 3B - Results: columns (9)-(12)
****************************************
xi: estout fig1i fig2i fig3i fig4i , cells(b(star fmt(%9.4f)) se(par) t(fmt(%9.2f)))   ///
       stats(r2_a N, fmt(%9.3f %9.0g) labels(R-squared)) keep(i.centr_tribe1     ) starlevels(* 0.1 ** 0.05  *** 0.01 )   style(fixed)



*************************************************************
**Appendix Figures 1a-1c
*************************************************************
*density plots of dependent variable
*************************************************************
kdensity mean_lights0708, name(Appendix_Figure_1a)  
kdensity lnlights_nozeros, name(Appendix_Figure_1b) 
kdensity lnlights0708, name(Appendix_Figure_1c)
**************************************************************


*************************************************************
** Appendix Table 2 - Panel A 
*************************************************************
** Summary Statistics for Control Variables 
*************************************************************
tabstat  lnpdmean00s lnwaterkm lnkm2split mean_elev mean_suit ///
         malariasuit petroleum diamond capdistance1 seadist1  borderdist1  ///
         rlaw07 lnrgdpch07 centr_tribe  gr , stat(N mean sd p25 p50 p75 min max) col(stat)


******************************************************************************************************
** Appendix Table 3 - Robustness Checks
** all specifications control for location, geography-ecology, and log population density
****************************************************************************************************** 
** column (1) Excluding Outliers (top 1 percent of luminosity) 
** column (2): excluding regions where Capital cities fall
** column (3)-(7): exlcuding each time a different Afican region (classification follows Nunn (2008)
******************************************************************************************************

egen top_lights0708=pctile(mean_lights0708), p(99)


xi:  cgmreg lnlights_nozeros   	   	lnpdmean00s		      	centr_tribe       i.wbcode   $poly   $geo      if   	obs>1 & mean_lights0708<top_lights0708,   robust cluster(cluster wbcode)
est store rr1

xi:  cgmreg lnlights_nozeros   		lnpdmean00s		      	centr_tribe       i.wbcode   $poly   $geo      if  	obs>1 & capital==0,   robust cluster(cluster wbcode)
est store rr2

xi:  cgmreg lnlights_nozeros   		lnpdmean00s		      	centr_tribe       i.wbcode   $poly   $geo      if   	obs>1 & region_n==0,   robust cluster(cluster wbcode)
est store rr3

xi:  cgmreg lnlights_nozeros   		lnpdmean00s		      	centr_tribe       i.wbcode   $poly   $geo      if  	obs>1 & region_s==0,   robust cluster(cluster wbcode)
est store rr4

xi:  cgmreg lnlights_nozeros   	   	lnpdmean00s		      	centr_tribe       i.wbcode   $poly   $geo      if   	obs>1 & region_c==0,   robust cluster(cluster wbcode)
est store rr5

xi:  cgmreg lnlights_nozeros   	   	lnpdmean00s		      	centr_tribe       i.wbcode   $poly   $geo      if   	obs>1 & region_e==0,   robust cluster(cluster wbcode)
est store rr6

xi:  cgmreg lnlights_nozeros   	   	lnpdmean00s		      	centr_tribe       i.wbcode   $poly   $geo      if   	obs>1 & region_w==0,   robust cluster(cluster wbcode)
est store rr7


*** Appendix Table 3 - Results
*******************************
estout rr1 rr2 rr3 rr4 rr5 rr6 rr7  , cells(b(star fmt(%9.4f)) se(par) t(fmt(%9.2f)))   ///
       stats(r2_a N, fmt(%9.3f %9.0g) labels(R-squared)) keep(centr_tribe     ) starlevels(* 0.1 ** 0.05  *** 0.01 )   style(fixed)


************************************************************************************
*** Appendix Table 4A: Results with log population density as the dependent variable
************************************************************************************
** columns (1)-(4): results with jurisdictional hierarchy index
** columns (5)-(8): results with binary political centralization index
************************************************************************************

xi:  cgmreg       	lnpdmean00s				centr_tribe       	 								,   robust cluster(cluster wbcode)
est store pd1

xi:  cgmreg     		lnpdmean00s		      	centr_tribe      				$poly   $geo      		  	,  robust cluster(cluster wbcode)
est store pd2

xi:  cgmreg       	lnpdmean00s				centr_tribe       i.wbcode                 	 		if   obs>1 	,   robust cluster(cluster wbcode)
est store pd3

xi:  cgmreg     		lnpdmean00s		      	centr_tribe       i.wbcode   		$poly   $geo      	if   obs>1 	,   robust cluster(cluster wbcode)
est store pd4

xi:  cgmreg       	lnpdmean00s				gr       	 										,   robust cluster(cluster wbcode)
est store pd5

xi:  cgmreg     		lnpdmean00s		      	gr      					$poly   $geo      		  	,   robust cluster(cluster wbcode)
est store pd6

xi:  cgmreg       	lnpdmean00s				gr       		i.wbcode                 	 		if   obs>1 	,   robust cluster(cluster wbcode)
est store pd7

xi:  cgmreg     		lnpdmean00s		      	gr       		i.wbcode   		$poly   $geo      	if   obs>1 ,   robust cluster(cluster wbcode)
est store pd8



** Appendix Table 4A - Results
********************************
estout pd1 pd2 pd3 pd4 pd5 pd6 pd7 pd8, cells(b(star fmt(%9.4f)) se(par) t(fmt(%9.2f)))   ///
       stats(r2_a N, fmt(%9.3f %9.0g) labels(R-squared)) keep(centr_tribe   gr  ) starlevels(* 0.1 ** 0.05  *** 0.01 )   style(fixed)





******************************************************************************************************
*** Table 2*******************************************************************************************
*** LS Estimates: Correlation Analysis - Preliminary Evidence*****************************************
*** Conley's GMM standard error correction for spatial correlation
******************************************************************************************************

** need to first run or install/run the .ado file `x_ols_Conley_se'  

** Column 1 ***
x_ols xaxis yaxis cutoff11 cutoff12 lnlights_nozeros const                         centr_tribe, xreg(2) coord(2)
drop  epsilon window dis1 dis2

** Column 2 ***
x_ols xaxis yaxis cutoff11 cutoff12 lnlights_nozeros const lnpdmean00s             centr_tribe, xreg(3) coord(2)
drop  epsilon window dis1 dis2

** Column 3 ***
x_ols xaxis yaxis cutoff11 cutoff12 lnlights_nozeros const lnpdmean00s $poly       centr_tribe, xreg(6) coord(2)
drop  epsilon window dis1 dis2

** Column 4 ***
x_ols xaxis yaxis cutoff11 cutoff12 lnlights_nozeros const lnpdmean00s $poly  $geo centr_tribe, xreg(13) coord(2)
drop  epsilon window dis1 dis2

drop if rlaw07==.

** Column 5 ***
x_ols xaxis yaxis cutoff11 cutoff12 lnlights_nozeros const lnpdmean00s $poly  $geo  rlaw07     centr_tribe, xreg(14) coord(2)
drop  epsilon window dis1 dis2

** Column 6 ***
x_ols xaxis yaxis cutoff11 cutoff12 lnlights_nozeros const lnpdmean00s $poly  $geo  lnrgdpch07 centr_tribe, xreg(14) coord(2)
drop  epsilon window dis1 dis2

