*******************************************************************************************************************************
** Pre-colonial Ethnic Institutions and Contemporary African Development
** Econometrica 2012
** Stelios Michalopoulos and Elias Papaioannou
*******************************************************************************************************************************


*******************************************************************************************************************************
** Sensitivity Analysis 
*******************************************************************************************************************************
** Table 8A  and Table 8B and Figure 5a and Figure 5b
** use robust-ethnic-pairs-final.dta
** Pre-colonial Ethnic Institutions and Regional Development within Contiguous Ethnic Homelands in the Same Country
** Pixel-Level Analysis in Areas Close to the Ethnic Border
*******************************************************************************************************************************

** Preliminaries
**********************************
d, all

sort idd
bysort idd: egen centr_tribe_mean=mean(centr_tribe)
gen high=0
replace high=1 if centr_tribe>centr_tribe_mean
order high 
label var high 		"indicator (dummy) variable for pixels falling in the ethnic homeland with the relatively high centr_tribe"


bysort idd: egen max=max(centr_tribe)
bysort idd: egen min=min(centr_tribe)
gen centr_tribe_diff=abs(max-min)

label var max				"maximum value of jurisdictional hierarcy index in each ethnic-pair"
label var min				"minimum value of jurisdictional hierarcy index in each ethnic-pair"
label var centr_tribe_diff		"difference in jurisdictional hierarcy index in each ethnic-pair"


*****************************************************************************************************
*** Figure 5A****************************************************************************************
*****************************************************************************************************
g bdh = bdeth if high==1
replace bdh=0 if high==0

g bdl = bdeth if high==0
replace bdl=0 if high==1

g bdlowplot = - bdl

** Figure 5a
**************
************** 
twoway (lpolyci l0708d bdh if bdh!=0 & bdeth<200 & centr_tribe_diff>1, degree(4) clwidth(medthick) ciplot(rbar) bwidth(200)) || (lpolyci l0708d bdlowplot if bdl!=0 & bdeth<200 & centr_tribe_diff>1, degree(4) clwidth(medthick) ciplot(rbar) bwidth(200)), ytitle("Light Density per Pixel)", size(small)) xlabel(,labsize(small)) xtitle("Distance to the Ethnic Border" "Positive  Values: Pixels in the Centralized Groups" "Negative Values: Pixels in the Non-Centralized Groups", size(small)) title("Pre-Colonial Political Centralization and Light Density in 2007-2008", size(medsmall)) subtitle("Local Mean Smoothing Plot with 95% Confidence Intervals" "", size(small)) legend(off) graphregion(color(white)) plotregion(color(white))




***************************************************************************************************
*** Table 8A***************************************************************************************
***************************************************************************************************
***************************************************************************************************
** Table 8 A - Panel A: Border Thickness: 50 km (25 km from each side of the border)
***************************************************************************************************
** columns (1)-(3): all observations
** columns (4)-(6): difference in jurisdictional hierarchy index greater than 1
** columns (7)-(9): one of the two ethnicities in each pair was part of large pre-colonial state
** in all specifications we control for log population density in 2000 and rich set of controls
***************************************************************************************************

** define set of control variables
**********************************
global pix lnkm pixpetro pixdia pixwaterd pixcapdist pixmal pixsead pixsuit pixelev pixbdist


xi: cgmreg  l0708d      centr_tribe       lnpd0  $pix      i.wbidd if 				  bdeth>25 & bdeth<100,                         robust cluster(pixwbcode pixcluster)
est store a1

xi: cgmreg  l0708d      centr_tribe       lnpd0  $pix      i.wbidd if 				  bdeth>25 & bdeth<150,                         robust cluster(pixwbcode pixcluster)
est store a2

xi: cgmreg  l0708d      centr_tribe       lnpd0  $pix      i.wbidd if 				  bdeth>25 & bdeth<200,                         robust cluster(pixwbcode pixcluster)
est store a3

xi: cgmreg  l0708d      centr_tribe       lnpd0  $pix      i.wbidd if centr_tribe_diff>1 	& bdeth>25 & bdeth<100,                         robust cluster(pixwbcode pixcluster)
est store a4

xi: cgmreg  l0708d      centr_tribe       lnpd0  $pix      i.wbidd if centr_tribe_diff>1 	& bdeth>25 & bdeth<150,                         robust cluster(pixwbcode pixcluster)
est store a5

xi: cgmreg  l0708d      centr_tribe       lnpd0  $pix      i.wbidd if centr_tribe_diff>1 	& bdeth>25 & bdeth<200,                         robust cluster(pixwbcode pixcluster)
est store a6

xi: cgmreg  l0708d      centr_tribe       lnpd0  $pix      i.wbidd if max>2 			& bdeth>25 & bdeth<100,                         robust cluster(pixwbcode pixcluster)
est store a7

xi: cgmreg  l0708d      centr_tribe       lnpd0  $pix      i.wbidd if max>2 			& bdeth>25 & bdeth<150,                         robust cluster(pixwbcode pixcluster)
est store a8

xi: cgmreg  l0708d      centr_tribe       lnpd0  $pix      i.wbidd if max>2 			& bdeth>25 & bdeth<200,                         robust cluster(pixwbcode pixcluster)
est store a9

** Table 8A - Panel A: Results
******************************
estout  a1 a2 a3 a4 a5 a6 a7 a8 a9, cells(b(star fmt(%9.4f)) se(par) t(fmt(%9.2f)))   ///
       stats(N r2_a, fmt(%9.3f %9.0g) labels(R-squared)) keep(centr_tribe) starlevels(* 0.1 ** 0.05  *** 0.01) style(fixed)



** Table 8A - Panel B: Border Thickness: 100 km (50 km from each side of the border)
***************************************************************************************************
** columns (1)-(3): all observations
** columns (4)-(6): difference in jurisdictional hierarchy index greater than 1
** columns (7)-(9): one of the two ethnicities in each pair was part of large pre-colonial state
** in all specifications we control for log population density in 2000 and rich set of controls
***************************************************************************************************

xi: cgmreg  l0708d      centr_tribe       lnpd0  $pix      i.wbidd if 				  bdeth>50 & bdeth<100,                         robust cluster(pixwbcode pixcluster)
est store b1

xi: cgmreg  l0708d      centr_tribe       lnpd0  $pix      i.wbidd if 				  bdeth>50 & bdeth<150,                         robust cluster(pixwbcode pixcluster)
est store b2

xi: cgmreg  l0708d      centr_tribe       lnpd0  $pix      i.wbidd if 				  bdeth>50 & bdeth<200,                         robust cluster(pixwbcode pixcluster)
est store b3

xi: cgmreg  l0708d      centr_tribe       lnpd0  $pix      i.wbidd if centr_tribe_diff>1 	& bdeth>50 & bdeth<100,                         robust cluster(pixwbcode pixcluster)
est store b4

xi: cgmreg  l0708d      centr_tribe       lnpd0  $pix      i.wbidd if centr_tribe_diff>1 	& bdeth>50 & bdeth<150,                         robust cluster(pixwbcode pixcluster)
est store b5

xi: cgmreg  l0708d      centr_tribe       lnpd0  $pix      i.wbidd if centr_tribe_diff>1 	& bdeth>50 & bdeth<200,                         robust cluster(pixwbcode pixcluster)
est store b6

xi: cgmreg  l0708d      centr_tribe       lnpd0  $pix      i.wbidd if max>2 			& bdeth>50 & bdeth<100,                         robust cluster(pixwbcode pixcluster)
est store b7

xi: cgmreg  l0708d      centr_tribe       lnpd0  $pix      i.wbidd if max>2 			& bdeth>50 & bdeth<150,                         robust cluster(pixwbcode pixcluster)
est store b8

xi: cgmreg  l0708d      centr_tribe       lnpd0  $pix      i.wbidd if max>2 			& bdeth>50 & bdeth<200,                         robust cluster(pixwbcode pixcluster)
est store b9

** Table 8A - Panel B: Results
******************************
estout  b1 b2 b3 b4 b5 b6 b7 b8 b9, cells(b(star fmt(%9.4f)) se(par) t(fmt(%9.2f)))   ///
       stats(N r2_a, fmt(%9.3f %9.0g) labels(R-squared)) keep(centr_tribe) starlevels(* 0.1 ** 0.05  *** 0.01) style(fixed)




***************************************************************************************************
***************************************************************************************************
** Table 8 B - Panel A: Border Thickness: 50 km (25 km from each side of the border)
***************************************************************************************************
** including a fourth-order RD polynomial in distnace to the "thick" ethnic border
***************************************************************************************************

*****************************************************************************************************
** Preliminaries
** Generate RD-type polynomial in distance to the ethnic border (in each ethnic pair in each country)
*****************************************************************************************************
drop   bdh bdl bdlowplot

drop if bdeth<25
g bd = bdeth - 25

g bdh=bd if high==1
replace bdh=0 if high==0

g bdl = bd if high==0
replace bdl=0 if high==1

g bdh2=bdh^2
g bdh3=bdh^3
g bdh4=bdh^4

g bdl2=bdl^2
g bdl3=bdl^3
g bdl4=bdl^4

g bdlowplot = - bdl

** Define 4th-order (common for all ethnic-pairs) RD-type polynomial in distance to the ethnic border
*****************************************************************************************************
global rdpoly0 bdh bdh2 bdh3 bdh4 bdl bdl2 bdl3 bdl4

** Define set of control variables (same as before)
***************************************************
global pix lnkm pixpetro pixdia pixwaterd pixcapdist pixmal pixsead pixsuit pixelev pixbdist

** Table 8 B - Panel A: Border Thickness: 50 km (25 km from each side of the border)
************************************************************************************************************
** columns (1)-(3): all observations
** columns (4)-(6): difference in jurisdictional hierarchy index greater than 1
** columns (7)-(9): one of the two ethnicities in each pair was part of large pre-colonial state
** in all specifications we control for log population density in 2000 and rich set of controls
** in all specifications we include an RD-type 4th order polynomial on distance to the "thick" ethnic border
*************************************************************************************************************

xi: cgmreg  l0708d      centr_tribe       lnpd0  $pix   	$rdpoly0   		i.wbidd if 				  	bdeth>25 & bdeth<100,                         robust cluster(pixwbcode pixcluster)
est store c1

xi: cgmreg  l0708d      centr_tribe       lnpd0  $pix   	$rdpoly0      	i.wbidd if 				  	bdeth>25 & bdeth<150,                         robust cluster(pixwbcode pixcluster)
est store c2

xi: cgmreg  l0708d      centr_tribe       lnpd0  $pix  	$rdpoly0       	i.wbidd if 				  	bdeth>25 & bdeth<200,                         robust cluster(pixwbcode pixcluster)
est store c3

xi: cgmreg  l0708d      centr_tribe       lnpd0  $pix   	$rdpoly0      	i.wbidd if centr_tribe_diff>1 	& bdeth>25 & bdeth<100,                         robust cluster(pixwbcode pixcluster)
est store c4

xi: cgmreg  l0708d      centr_tribe       lnpd0  $pix   	$rdpoly0      	i.wbidd if centr_tribe_diff>1 	& bdeth>25 & bdeth<150,                         robust cluster(pixwbcode pixcluster)
est store c5

xi: cgmreg  l0708d      centr_tribe       lnpd0  $pix   	$rdpoly0      	i.wbidd if centr_tribe_diff>1 	& bdeth>25 & bdeth<200,                         robust cluster(pixwbcode pixcluster)
est store c6

xi: cgmreg  l0708d      centr_tribe       lnpd0  $pix   	$rdpoly0       	i.wbidd if max>2 				& bdeth>25 & bdeth<100,                         robust cluster(pixwbcode pixcluster)
est store c7

xi: cgmreg  l0708d      centr_tribe       lnpd0  $pix   	$rdpoly0      	i.wbidd if max>2 				& bdeth>25 & bdeth<150,                         robust cluster(pixwbcode pixcluster)
est store c8

xi: cgmreg  l0708d      centr_tribe       lnpd0  $pix   	$rdpoly0      	i.wbidd if max>2 				& bdeth>25 & bdeth<200,                         robust cluster(pixwbcode pixcluster)
est store c9

** Table 8B - Panel A: Results
******************************
estout  c1 c2 c3 c4 c5 c6 c7 c8 c9, cells(b(star fmt(%9.4f)) se(par) t(fmt(%9.2f)))   ///
       stats(N r2_a, fmt(%9.3f %9.0g) labels(R-squared)) keep(centr_tribe) starlevels(* 0.1 ** 0.05  *** 0.01) style(fixed)


** Figure 5b
**************
************** 
twoway (lpolyci l0708d bdh if bdh!=0 & bdeth<200 &  centr_tribe_diff>1, degree(4) clwidth(medthick) ciplot(rbar) bwidth(200)) || (lpolyci l0708d bdlowplot if bdl!=0 & bdeth<200 &  centr_tribe_diff>1, degree(4) clwidth(medthick) ciplot(rbar) bwidth(200)), ytitle("Light Density per Pixel)", size(small)) xlabel(,labsize(small)) xtitle("Distance to the Ethnic Border" "Positive  Values: Pixels in the Centralized Groups" "Negative Values: Pixels in the Non-Centralized Groups", size(small)) title("Pre-Colonial Political Centralization and Light Density in 2007-2008", size(medsmall)) subtitle("Local Mean Smoothing Plot with 95% Confidence Intervals" "", size(small)) legend(off) graphregion(color(white)) plotregion(color(white))


****************************************************************************************************
** Table 8B - Panel B: - Panel B: Border Thickness: 100 km (50 km from each side of the border)
*****************************************************************************************************
** Preliminaries
** Generate RD-type polynomial in distance to the ethnic border (in each ethnic pair in each country)
*****************************************************************************************************
drop  bd bdh bdl bdh2 bdh3 bdh4 bdl2 bdl3 bdl4 bdlowplot

drop if bdeth<50
g bd = bdeth - 50

g bdh=bd if high==1
replace bdh=0 if high==0

g bdl = bd if high==0
replace bdl=0 if high==1

g bdh2=bdh^2
g bdh3=bdh^3
g bdh4=bdh^4

g bdl2=bdl^2
g bdl3=bdl^3
g bdl4=bdl^4

g bdlowplot = - bdl



** Define 4th-order (common for all ethnic-pairs) RD-type polynomial in distance to the ethnic border
*****************************************************************************************************
global rdpoly0 bdh bdh2 bdh3 bdh4 bdl bdl2 bdl3 bdl4

** Define set of control variables (same as before)
****************************************************
global pix lnkm pixpetro pixdia pixwaterd pixcapdist pixmal pixsead pixsuit pixelev pixbdist

** Table 8 B - Panel B: Border Thickness: 100 km (50 km from each side of the border)
*************************************************************************************************************
** columns (1)-(3): all observations
** columns (4)-(6): difference in jurisdictional hierarchy index greater than 1
** columns (7)-(9): one of the two ethnicities in each pair was part of large pre-colonial state
** in all specifications we control for log population density in 2000 and rich set of controls
** in all specifications we include an RD-type 4th order polynomial on distance to the "thick" ethnic border
*************************************************************************************************************

xi: cgmreg  l0708d      centr_tribe       lnpd0  $pix   	$rdpoly0   		i.wbidd if 				  	bdeth>50 & bdeth<100,                         robust cluster(pixwbcode pixcluster)
est store d1

xi: cgmreg  l0708d      centr_tribe       lnpd0  $pix   	$rdpoly0      	i.wbidd if 				  	bdeth>50 & bdeth<150,                         robust cluster(pixwbcode pixcluster)
est store d2

xi: cgmreg  l0708d      centr_tribe       lnpd0  $pix  	$rdpoly0       	i.wbidd if 				  	bdeth>50 & bdeth<200,                         robust cluster(pixwbcode pixcluster)
est store d3

xi: cgmreg  l0708d      centr_tribe       lnpd0  $pix   	$rdpoly0      	i.wbidd if centr_tribe_diff>1 	& bdeth>50 & bdeth<100,                         robust cluster(pixwbcode pixcluster)
est store d4

xi: cgmreg  l0708d      centr_tribe       lnpd0  $pix   	$rdpoly0      	i.wbidd if centr_tribe_diff>1 	& bdeth>50 & bdeth<150,                         robust cluster(pixwbcode pixcluster)
est store d5

xi: cgmreg  l0708d      centr_tribe       lnpd0  $pix   	$rdpoly0      	i.wbidd if centr_tribe_diff>1 	& bdeth>50 & bdeth<200,                         robust cluster(pixwbcode pixcluster)
est store d6

xi: cgmreg  l0708d      centr_tribe       lnpd0  $pix   	$rdpoly0       	i.wbidd if max>2 				& bdeth>50 & bdeth<100,                         robust cluster(pixwbcode pixcluster)
est store d7

xi: cgmreg  l0708d      centr_tribe       lnpd0  $pix   	$rdpoly0      	i.wbidd if max>2 				& bdeth>50 & bdeth<150,                         robust cluster(pixwbcode pixcluster)
est store d8

xi: cgmreg  l0708d      centr_tribe       lnpd0  $pix   	$rdpoly0      	i.wbidd if max>2 				& bdeth>50 & bdeth<200,                         robust cluster(pixwbcode pixcluster)
est store d9

** Table 8B - Panel B: Results
******************************
estout  d1 d2 d3 d4 d5 d6 d7 d8 d9, cells(b(star fmt(%9.4f)) se(par) t(fmt(%9.2f)))   ///
       stats(N r2_a, fmt(%9.3f %9.0g) labels(R-squared)) keep(centr_tribe) starlevels(* 0.1 ** 0.05  *** 0.01) style(fixed)




