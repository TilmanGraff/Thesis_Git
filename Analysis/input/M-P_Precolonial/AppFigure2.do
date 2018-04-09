*******************************************************************************************************************************
** Pre-colonial Ethnic Institutions and Contemporary African Development
** Econometrica 2012
** Stelios Michalopoulos and Elias Papaioannou
*******************************************************************************************************************************


*******************************************************************************************************************************
** Appendix Figures 2a-2c
** Cross-country association between real GDP p.c. and Light Density
*******************************************************************************************************************************

d, all





** Figure 2a: Correlation between Log light Desnity and Log per capita GDP
**************************************************************************
twoway (scatter ln_maddison_pcgdp2000 lnlights0708 ,  mlabel(country) msize(small) mlabpos(12) mlabsize(2)) || (lfit ln_maddison_pcgdp2000 lnlights0708 , clwidth(medthick)), ytitle("Log(GDP per Capita in 2000)", size(small)) xlabel(,labsize(small)) xtitle("Log Light Density in 2007-2008", size(small)) title("Light Density and Income per Capita Across African Countries", size(medlarge)) subtitle("Unconditional Relationship", size(vsmall)) legend(off) graphregion(color(white)) plotregion(color(white))


** Figure 2b: Conditional on Log Population Density Correlation between Log light Desnity and Log per capita GDP
****************************************************************************************************************
reg ln_maddison_pcgdp2000 		lnpopdens00	, r
predict g							, res
reg lnlights0708 				lnpopdens00	, r
predict gg 							, res

twoway (scatter g gg ,  mlabel(country) msize(small) mlabpos(12) mlabsize(2)) || (lfit g gg , clwidth(medthick)), ytitle("Log (GDP per Capita in 2000)", size(small)) xlabel(,labsize(small)) xtitle("Log (Light Density in 2007-2008)", size(small)) title("Light Density and Income per Capita Across African Countries", size(medlarge)) subtitle("Conditional on Population Density", size(vsmall)) legend(off) graphregion(color(white)) plotregion(color(white))

