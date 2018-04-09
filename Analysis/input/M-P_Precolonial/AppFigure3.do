*******************************************************************************************************************************
** Pre-colonial Ethnic Institutions and Contemporary African Development
** Econometrica 2012
** Stelios Michalopoulos and Elias Papaioannou
*******************************************************************************************************************************


*******************************************************************************************************************************
** Appendix Figures 3a-3b
** Cross-region Association between Infant Mortality and Light Density
*******************************************************************************************************************************

d, all


** Figure 3a: Correlation between Log light Desnity and Infant Mortality
**************************************************************************
twoway (scatter lnlights0708 infant_mort,  mlabel(reg_name) msize(small) mlabpos(12) mlabsize(2)) || (lfit lnlights0708 infant_mort, clwidth(medthick)), ytitle("Log Light Density per African Region", size(small)) xlabel(,labsize(small)) xtitle("Infant Mortality per African Region", size(small)) title("Light Density and Infant Mortality Across African Regions", size(medlarge)) subtitle("Unconditional Relationship", size(vsmall)) legend(off) graphregion(color(white)) plotregion(color(white))



** Figure 3b: Conditional on log Population Density Correlation between Log light Desnity and Infant Mortality
**************************************************************************************************************
xi: reg infant_mort 		lnpopdens00 , cluster( wbcode )
predict x, res 
xi: reg lnlights0708	 	lnpopdens00 , cluster(wbcode)
predict xx, res 

twoway (scatter xx x,  mlabel(reg_name) msize(small) mlabpos(12) mlabsize(2)) || (lfit  xx x, clwidth(medthick)), ytitle("Log Light Density per African Region", size(small)) xlabel(,labsize(small)) xtitle("Infant Mortality per African Region", size(small)) title("Light Density and Infant Mortality Across African Regions", size(medlarge)) subtitle("Conditional on Population Density", size(vsmall)) legend(off) graphregion(color(white)) plotregion(color(white))
drop x xx

