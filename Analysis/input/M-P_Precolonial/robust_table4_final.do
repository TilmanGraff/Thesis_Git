*******************************************************************************************************************************
** Pre-colonial Ethnic Institutions and Contemporary African Development
** Econometrica 2012
** Stelios Michalopoulos and Elias Papaioannou
*******************************************************************************************************************************


*******************************************************************************************************************************
** Sensitivity Analysis at the Ethnic-country homeland level
*******************************************************************************************************************************
** Table 4: 
** Examining the Role of Other than Institutions Pre-colonial Ethnic Features 
*******************************************************************************************************************************

d, all

** Step A: Using Murdock's Ethnolinguistic Atlas Generate 20 Variables Capturing Various Ethnic Traits
*******************************************************************************************************************************
tab v1
gen gathering=0 if v1!=.
replace gathering=1 if v1>0 & v1!=.
label var gathering "indicator that equals zero if gathering is 0%-5% and 1 if higher (6%-85%)."


tab v2
gen hunting=0 if v2!=.
replace hunting=1 if v2>0 & v2!=.
label var hunting "indicator that equals zero if hunting is 0%-5% and 1 if higher(6%-65%)."


tab v3
gen fishing=0 if v3!=.
replace fishin=1 if v3>0 & v3!=.
label var fishing "indicator that equals zero if fishing is 0%-5% and 1 if higher(6%-85%)."

tab v4
gen anhusb=v4 if v4!=.
label var anhusb "Animal Husbandry variable (0-9 scale)."
**we do not distinguish between type of animal husbandry (pigs, sheep, etc) as described in v40

tab v41
gen milking=0 if v41!=.
replace milking=. if v41==0
replace milking=1 if v41==2
label var milking "indicator on whether domestic animals are milked more often than sporadically."

tab v5
gen agricdep=v5 if v5!=.
label var agricdep "Dependence on agriculture variable (0-9 scale)."

*v28 is also reflecting the intensity of agriculture 
tab v28
gen agricdepalt=0 if v28!=.
replace agricdepalt=. if v28==0
replace agricdepalt=0 if v28==1
replace agricdepalt=1 if v28==2
replace agricdepalt=2 if v28==3
replace agricdepalt=3 if v28==5
replace agricdepalt=4 if v28==6
label var agricdepalt "alternative index of dependence on agriculture (0-4)."


**v42 also reflects intensity on agriculture in binary setting but there is no variation 

tab v8
gen polygyny=0 if v8!=.
replace polygyny=. if v8==0
replace polygyny=1 if v8==2 | v8==4 | v8==5
label var polygyny "indicator that equals one if polygyny is present and zero if not."


tab v9
gen polygynyalt=0 if v9!=.
replace polygynyalt=1 if v9==2
label var polygynyal "alternative indicator for polygyny (as in Fenske)."


tab v15
gen clans=0 if v15!=.
replace clans=1 if v15==6 & v15!=.
replace clans=. if v15==0
label var clans "indicator for clan communities (commuity marriage organization)."


tab v16
gen clansalt=0 if v16!=.
replace clansalt=1 if v16==1 
replace clansalt=1 if v16==2 
replace clansalt=. if v16==0 
label var clansalt "alternative indicator for clan communities (commuity marriage organization)."


tab v30
gen settlements=0 if v30!=.
replace settlements=v30 if v30!=. 
replace settlements=. if v30==0
label var settlements "variable reflecting the type of settlement patterns."


gen compset=0 if v30!=.
replace compset=. if v30==0
replace compset=1 if v30==7
replace compset=1 if v30==8
label var compset "indicator for compact and complex settlements. (zeros indicate nomadic/sedentary)."



tab v32
gen locjuris=0 if v32!=.
replace locjuris=. if v32==0
replace locjuris=v32-2 if v32!=. & v32!=0
label var locjuris "jurisdictional hierarchy at the local level; equals 2, 3, or 4"


tab v43
gen pater=0 if v43!=.
replace pater=. if v43==0
replace pater=1 if v43==1
label var pater "dummy that equals one if there are patrilineal descent types."


** not enoought data on the next set of variables that reflect sex differences in leather, pottery, baot building, hunting, fishing
** not enoought data on the next set of variables that reflect age of occupational specialization in leather, weaving, etc

tab v66
gen class=v66-1
replace class=. if v66==0
label var class "class startification index (0-5 range)."

gen classdummy=class
replace classdummy=1 if class==1 | class==2 | class==3 | class==4 
label var classdummy "indicator for stratified societies (zero=egalitarian). as in Gennaioli-Rainer ."

tab v72
gen elections=0 if v72!=.
replace elections=. if v72==0
replace elections=1 if v72==6
label var elections "indicator on whether there are elections for the local headman."

tab v70
gen slavery=0 if v70!=.
replace slavery=. if v70==0
replace slavery=1 if v70>1 & v70!=.
label var slavery "indicator for presence of slavery. as in Fenske."

tab v74
gen property=0 if v74!=.
replace property=. if v74==0
replace property=1 if v74>1 & v74!=.
label var property "indictaor for presence of some form or property rights. as in Fenske."

***********************************************************************************************************************************************
** Step 2: For each of the Generated Variables Run one Simple Model and one Model Also Including the Jurisdictional Hierarhy Index
** in all specifications we control for country fixed effects and log population density (at the country-ethnic homeland level)
***********************************************************************************************************************************************

** Summary Statistics (not reported)

set more off

sum 	gathering hunting fishing anhusb milking agricdep   agricdepalt         ///
	polygyny polygynyalt clans   settlements compset locjuris    		/// 
    	pater class classdummy elections slavery property



** Generate variable indicating number of ethnicity-country regions in each country
bysort wbcode: egen obs=count(gr) 


foreach var of varlist  gathering hunting fishing anhusb milking agricdep  agricdepalt {

	xi: cgmreg lnlights_nozeros                    `var'      lnpdmean00s          i.wbcode                  	if	obs>1		,   robust cluster(cluster wbcode)
		est store a_`var'	
	}


foreach var of varlist  gathering hunting fishing anhusb milking agricdep  agricdepalt {
 	
	xi: cgmreg lnlights_nozeros        centr_tribe `var'      lnpdmean00s 		i.wbcode                  	if	obs>1 	,   robust cluster(cluster wbcode)
		est store b_`var'
	
	}



foreach var of varlist  polygyny polygynyalt clans settlements compset locjuris  {

	xi: cgmreg lnlights_nozeros                    `var'      lnpdmean00s          i.wbcode    			if	obs>1   ,   robust cluster(cluster wbcode)
    		est store a_`var'
}



foreach var of varlist  polygyny polygynyalt clans   settlements compset locjuris  {

	
	xi: cgmreg lnlights_nozeros    centr_tribe     `var'     lnpdmean00s i.wbcode                  			if   obs>1 ,   robust cluster(cluster wbcode)
    		est store b_`var'
	
}




foreach var of varlist  pater class classdummy elections slavery property  {

	xi: cgmreg lnlights_nozeros                    `var'      lnpdmean00s          i.wbcode         if	obs>1  ,   robust cluster(cluster wbcode)
    		est store a_`var'

		
}


foreach var of varlist  pater class classdummy elections slavery property  {

	
	xi: cgmreg lnlights_nozeros    centr_tribe     `var'     lnpdmean00s i.wbcode                  if  	 obs>1 ,   robust cluster(cluster wbcode)
    		est store b_`var'

	
}





** Table 4 - Results
*********************

** Specification A
******************
estout  a_gathering   a_hunting a_fishing a_anhusb a_milking a_agricdep  a_agricdepalt , cells(b(star fmt(%9.4f)) se(par) t(fmt(%9.2f)))   ///
       stats(r2_a N, fmt(%9.3f %9.0g) labels(R-squared)) keep(gathering  gathering hunting fishing anhusb milking agricdep agricdepalt) starlevels(* 0.1 ** 0.05  *** 0.01 )   style(fixed)

estout  a_polygyny a_polygynyalt a_clans   a_settlements a_compset a_locjuris , cells(b(star fmt(%9.4f)) se(par) t(fmt(%9.2f)))   ///
       stats(r2_a N, fmt(%9.3f %9.0g) labels(R-squared)) keep(polygyny polygynyalt clans settlements compset locjuris ) starlevels(* 0.1 ** 0.05  *** 0.01 )   style(fixed)


estout a_pater a_class a_classdummy a_elections a_slavery a_property , cells(b(star fmt(%9.4f)) se(par) t(fmt(%9.2f)))   ///
       stats(r2_a N, fmt(%9.3f %9.0g) labels(R-squared)) keep(pater class classdummy elections slavery property) starlevels(* 0.1 ** 0.05  *** 0.01 )   style(fixed)


** Specification B
******************
estout  b_gathering   b_hunting b_fishing b_anhusb b_milking b_agricdep b_agricdepalt   , cells(b(star fmt(%9.4f)) se(par) t(fmt(%9.2f)))   ///
       stats(r2_a N, fmt(%9.3f %9.0g) labels(R-squared)) keep(centr_tribe gathering  gathering hunting fishing anhusb milking agricdep agricdepalt) starlevels(* 0.1 ** 0.05  *** 0.01 )   style(fixed)

estout   b_polygyny b_polygynyalt b_clans   b_settlements b_compset b_locjuris   , cells(b(star fmt(%9.4f)) se(par) t(fmt(%9.2f)))   ///
       stats(r2_a N, fmt(%9.3f %9.0g) labels(R-squared)) keep(centr_tribe polygyny polygynyalt clans   settlements compset locjuris ) starlevels(* 0.1 ** 0.05  *** 0.01 )   style(fixed)

estout  b_pater b_class b_classdummy b_elections b_slavery b_property    , cells(b(star fmt(%9.4f)) se(par) t(fmt(%9.2f)))   ///
       stats(r2_a N, fmt(%9.3f %9.0g) labels(R-squared)) keep(centr_tribe pater class classdummy elections slavery property ) starlevels(* 0.1 ** 0.05  *** 0.01 )   style(fixed)












