*******************************************************************************************************************************
** Pre-colonial Ethnic Institutions and Contemporary African Development
** Econometrica 2012
** Stelios Michalopoulos and Elias Papaioannou
*******************************************************************************************************************************


*******************************************************************************************************************************
** Figures 2a-2d and Appendix Table 1
** Please run the program in each of the four datasets. 
** DHS_NGA2008-final, DHS_DRC2007-final, DHS_ZWE2005-final, and DHS_TZA2007-final
*******************************************************************************************************************************
d, all

*************************************************************************************
*************************************************************************************

** Appendix Table 1 - Unconditional and Conditional Correlations
*************************************************************************************
cor lnlights0708	wealthhnum elech eduyrsh

pcorr lnlights0708	wealthhnum lnpd0
pcorr lnlights0708	elech lnpd0
pcorr lnlights0708	eduyrsh lnpd0 

** Figures 2a- 2d (one from each data (.dta) file
**************************************************************************************

reg  wealthhnum lnpd0
predict wres if lnpd0!=., res

reg  lnlights0708		lnpd0 if wealthhnum!=.
predict lres if lnpd0!=., res

twoway (lfitci wres lres, clwidth(medthick) ciplot(rarea)) || (scatter  wres lres, mlabel() msize(small) mlabpos(12) mlabsize(2) mlabc(green)), ytitle("Household Wealth Index of a DHS Cluster", size(medsmall)) xlabel(,labsize(medsmall)) xtitle("Log Light Density in a 10km Radius of a DHS Cluster", size(medsmall)) title("Household Wealth and Light Density: DHS Clusters", size(medlarge)) subtitle("Conditional on Population Density in 2000", size(medsmall)) legend(off) graphregion(color(white)) plotregion(color(white))
