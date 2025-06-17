**********************
* Prelims
**********************

cd "/Users/tilmangraff/Documents/GitHub/Thesis_Git"

local outpath "/Users/tilmangraff/Dropbox (Harvard University)/Apps/Overleaf/Spatial Inefficiencies/tables/2024-03-02"


loc geoctrls "altitude landsuit temp precip growingdays malaria biomes1 biomes4 biomes5 biomes6 biomes8 biomes10 biomes11 biomes12 biomes13 biomes14 biomes2_3 x x_2 x_3 x_4 y y_2 y_3 y_4 harbor25 river25 lake25 i.ccode iscapital isborder"

foreach outcome in "amenity_pc" "amenity" "i_stat" "Lambda" {
	
	
	
	
foreach type in "imm" "imm_10p" "L_mob" "L_mob_10p" "mob" "mob_10p"{
	

	if "`type'" == "imm"{
	  loc type_addon = "_imm"
	  loc lambda_sub = "\text{imm}"
	  loc lambda_sup = ""
	}
	if "`type'" == "imm_10p"{
	  loc type_addon = "_imm_10p"
	  loc lambda_sub = "\text{imm}"
	  loc lambda_sup = "10\%"
	}
	if "`type'" == "L_mob"{
	  loc type_addon = "_mob"
	  loc lambda_sub = "\text{mob}"
	  loc lambda_sup = ""
	}
	if "`type'" == "L_mob_10p"{
	  loc type_addon = "_mob_10p"
	  loc lambda_sub = "\text{mob}"
	  loc lambda_sup = "10\%"
	}
	if "`type'" == "mob"{
	  loc type_addon = "_mob"
	  loc lambda_sub = "\text{mob}"
	  loc lambda_sup = ""
	}
	if "`type'" == "mob_10p"{
	  loc type_addon = "_mob_10p"
	  loc lambda_sub = "\text{mob}"
	  loc lambda_sup = "10\%"
	}



	if "`outcome'" == "amenity_pc"{
		loc title "Amenities per capita (z-scored)"
	}
	if "`outcome'" == "amenity"{
		loc title "Amenities (z-scored)"
	}
	if "`outcome'" == "Lambda"{
		loc title "Infrastructure discrimination $\Lambda^{`lambda_sup'}_{`lambda_sub'}$ (z-scored)"
	}
	if "`outcome'" == "i_stat"{
		loc title "Ex-ante road density (z-scored)"
	}
	




foreach simcontrols in "Yes"{

use "./Analysis/input/maingrid.dta", clear
drop zLambda_mob*



drop *_forR 
capture confirm variable zi_stat_`type'
	if !_rc{
// drop zi_stat_`type'
// gen zi_stat_`type' = zi_stat_`type'_pc

	}
	capture confirm variable z`outcome'_`type'
	if !_rc{

if "`simcontrols'" == "Yes"{
  loc simctrls "pop_stat rugg lights"
  loc sim_addon = ""
}
else {
  loc simctrls ""
  loc sim_addon = "_nosims"
}

**********************
* Rail regs
**********************

loc col = 1
foreach xvar in "railkm" "placebokm" "railkm_military" "railkm_mining"{
  qui reg z`outcome'_`type' `xvar'_50 `geoctrls' `simctrls', vce(cluster cluster)
  eststo modr`col'
  estadd local fe "Yes": modr`col'
  estadd local gctrls "Yes": modr`col'
  estadd local sctrls "`simcontrols'": modr`col'

  local col = `col' + 1

}

gen dist10 = .
gen dist20 = .
gen dist30 = .
gen dist40 = .

foreach rtype in "rail" "placebo"{

  foreach dist in "10" "20" "30" "40"{
    replace dist`dist' = `rtype'_d`dist'
  }

  reg z`outcome'_`type' dist10 dist20 dist30 dist40 `geoctrls' `simctrls', vce(cluster cluster)
  eststo modr`col'
  estadd local fe "Yes": modr`col'
  estadd local gctrls "Yes": modr`col'
  estadd local sctrls "`simcontrols'": modr`col'

  local col = `col' + 1

}


esttab modr* using "`outpath'/rails_`outcome'`sim_addon'`type_addon'.tex", se replace keep(railkm_50 placebokm_50 railkm_military_50 railkm_mining_50 dist10 dist20 dist30 dist40) stats(fe gctrls sctrls N r2, labels("Country FE" "Geography Controls" "Simulation Controls" "N" "R2")) coeflabels(railkm_50 "50 KM of Colonial Railroads" placebokm_50 "50 KM of Colonial Placebo Railroads" railkm_military_50 "50 KM of Colonial Railroads for Military Purposes" railkm_mining_50 "50 KM of Colonial Railroads for Mining Purposes" dist10 "$<$10KM to railroad" dist20 "10-20KM to railroad" dist30 "20-30KM to railroad" dist40 "30-40KM to railroad") nomtitle collabels(none) mgroups("`title'" "Real" "Placebo", pattern(1 0 0 0 1 1)  ///
prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) nonotes substitute(\_ _) star(* 0.1 ** 0.05 *** 0.01)

eststo clear


**********************
* Aid regs
**********************

foreach xvar_stub in "dis" "dis_transp" "num" "num_transp"{
  gen `xvar_stub' = .
}

loc col = 1
loc xvars = ""
foreach donor in "wb" "china"{
  foreach xvar_stub in "dis" "dis_transp" "num" "num_transp"{

    replace `xvar_stub' = `donor'_`xvar_stub'
    loc xvar "`xvar_stub'"

    qui reg z`outcome'_`type' `xvar' `geoctrls' `simctrls', vce(cluster cluster)
    eststo moda`col'
    estadd local fe "Yes": moda`col'
    estadd local gctrls "Yes": moda`col'
    estadd local sctrls "`simcontrols'": moda`col'

    local col = `col' + 1
    loc xvars "`xvars' `xvar'"

  }
}

esttab moda* using "`outpath'/aid_`outcome'`sim_addon'`type_addon'.tex", se replace keep(`xvars') stats(fe gctrls sctrls N r2, labels("Country FE" "Geography Controls" "Simulation Controls" "N" "R2")) coeflabels(dis "Total disbursements (mil \\$)" dis_transp "Total transport sector disbursements (mil \\$)" num "Number of projects" num_transp "Number of transport sector disbursements") nomtitle collabels(none) mgroups("$\Lambda$: World Bank" "$\Lambda$: China", pattern(1 0 0 0 1 0 0 0)  ///
prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) nonotes substitute(\_ _) star(* 0.1 ** 0.05 *** 0.01)

eststo clear

// **********************
// * Leaders regs
// **********************
//
// loc col = 1
// loc xvars = ""
// foreach ifstub in "1" "!iscapital"{
//   foreach xvar in "years_in_power" "ever_in_power"{
//
//     qui reg z`outcome'_`type' `xvar' `geoctrls' `simctrls' if `ifstub', vce(cluster cluster)
//     eststo mod`col'
//     estadd local fe "Yes": mod`col'
//     estadd local gctrls "Yes": mod`col'
//     estadd local sctrls "`simcontrols'": mod`col'
//
//     local col = `col' + 1
//     loc xvars "`xvars' `xvar'"
//
//   }
// }
//
// ** adding ethnicity regressions here
//
// use "./Analysis/input/grid_ethn.dta", clear
// capture confirm variable z`outcome'_`type'
// 	if !_rc{
// 		capture confirm variable zi_stat_`type'
// 	if !_rc{
// drop zi_stat_`type'
// gen zi_stat_`type' = zi_stat_`type'_pc
// 	}
// loc etctrls = "altitude landsuit temp precip growingdays malaria biomes1 biomes4 biomes5 biomes6 biomes8 biomes10 biomes11 biomes12 biomes13 biomes14 biomes2_3 i.ccode borderdist1 seadist1 larea iscapital"
//
// foreach xvar in "dis" "ex" "etwar"{
//
//   qui reg z`outcome'_`type' `xvar' `etctrls' `simctrls', vce(cluster eth_cluster)
//   eststo mod`col'
//   estadd local fe "Yes": mod`col'
//   estadd local gctrls "Yes": mod`col'
//   estadd local sctrls "`simcontrols'": mod`col'
//
//   local col = `col' + 1
//   loc xvars "`xvars' `xvar'"
//
// }
//
// esttab mod* using "`outpath'/leaders_`outcome'`sim_addon'`type_addon'.tex", se replace keep(`xvars') stats(fe gctrls sctrls N r2, labels("Country FE" "Geography Controls" "Simulation Controls" "N" "R2")) coeflabels(years_in_power "\vspace{0.2ex}\\Total years in power" ever_in_power "Ever in power dummy" dis "Discriminated against" ex "Excluded from government" etwar "Involved in ethnic war") nomtitle collabels(none) mgroups("$\Lambda$: entire sample" "$\Lambda$: excluding capitals" "$\Lambda$: ethnic homeland level", pattern(1 0 1 0 1 0 0)  ///
// prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) nonotes substitute(\_ _) star(* 0.1 ** 0.05 *** 0.01)
// 	}
// eststo clear

}
}
}
}

**********************
* Descriptives Table
**********************

use "./Analysis/input/maingrid.dta", clear
loc col = 1
gen pop1e5 = pop_stat / 100000

loc geoctrls_balance "altitude landsuit temp precip growingdays malaria harbor25 river25 lake25 iscapital isborder"
loc geoctrls "altitude landsuit temp precip growingdays malaria biomes1 biomes4 biomes5 biomes6 biomes8 biomes10 biomes11 biomes12 biomes13 biomes14 biomes2_3 x x_2 x_3 x_4 y y_2 y_3 y_4 harbor25 river25 lake25 i.ccode iscapital isborder"

loc simctrls_balance "pop1e5 rugg lights"
gen zLambda_amenities = zamenity_mob
gen zLambda_amenities_pc = zamenity_pc_mob

foreach type in "imm" "imm_10p" "L_mob" "L_mob_10p" "amenities" "amenity_pc"{

	qui reg zLambda_`type' `simctrls_balance' `geoctrls' zi_stat_imm, vce(cluster cluster)
	
	if "`type'" == "imm"{
		foreach var in `simctrls_balance' `geoctrls_balance'  "zi_stat_imm"{
			if abs(_b[`var'] / _se[`var']) > 1.96{
				loc this_dir = sign(_b[`var'])
			}
			else{
				loc this_dir = 0
			}
			loc dir_descr "`dir_descr'  `this_dir'"
		}
	}
	
	eststo mod`col'
	estadd local gctrls "Yes": mod`col'
	estadd local fe "Yes": mod`col'
	local col = `col' + 1
	
}


esttab mod* using "`outpath'/descriptive_table.tex", se replace nonotes keep(`simctrls_balance' `geoctrls_balance' zi_stat_imm) stats(fe gctrls N r2, labels("Country FE" "Remaining Controls" "N" "R2")) coeflabels(pop1e5 "Population (in 100,000)" rugg "Ruggedness" lights "Night lights" altitude "Altitude" landsuit "Agr. suitability index" temp "Temperature" precip "Precipitation" growingdays "Yearly growing days" malaria "Malaria prevalence" harbor25 "$<25$ KM from suitable harbor" river25 "$<25$ KM from navigable river" lake25 "$<25$ KM from navigable lake" iscapital "National capital" isborder "At national border" zi_stat_imm "Road density (z-scored)") mtitles("$\Lambda_{\text{imm}}$" "$\Lambda_\text{imm}^{\text{10\%}}$" "$\Lambda_{\text{mob}}$" "$\Lambda_{\text{mob}}^{\text{10\%}}$" "Amenities" "Amenities (pc)") noconstant substitute(\_ _) star(* 0.1 ** 0.05 *** 0.01)


di "`dir_descr'"



**********************
* Balance Table
**********************



use "./Analysis/input/maingrid.dta", clear
loc col = 1
gen pop1e5 = pop_stat / 100000

keep if anyrail | anyplacebo

loc nrows wordcount("`simctrls_balance' `geoctrls_balance'")
loc ncols 4
di `nrows'


eststo clear
est drop _all
gen xa = 1
gen ya = 1
if `nrows' > 1 {
	forvalues x1 = 1/`ncols' {
		eststo col`x1': reg xa ya
	}
}
local varcount = 1 // looping through variables
local count = 1  // row with main effects
local countse = `count'+1 // row with SEs
local countfdr = `countse'+1 // row with FDR q-values
local varlabels ""
local statnames ""

esttab col*

loc varcount = 1

foreach var in `simctrls_balance' `geoctrls_balance'  "zi_stat_imm" {
	
	local thisdir : word `varcount' of `dir_descr'
	
	summ `var' if anyrail 
	estadd local thisstat`count' = string(`r(mean)', "%9.2f") : col1
	estadd local thisstat`countse' = "(" + string(`r(sd)', "%9.2f") + ")" : col1
	
	summ `var' if anyplacebo 
	estadd local thisstat`count' = string(`r(mean)', "%9.2f") : col2
	estadd local thisstat`countse' = "(" + string(`r(sd)', "%9.2f") + ")" : col2
	
	qui ttest `var', by(anyrail)
	estadd local thisstat`count' = string(`r(p)', "%9.2f") : col3
	if `r(p)' < 0.05{
		loc mydir = `r(t)'
	}
	else{
		loc mydir = 0
	}
	
	
	local biasdir = `mydir' * `thisdir'
	
	if `biasdir' < 0{
		estadd local thisstat`count' = "Against" : col4
	}
	if `biasdir' > 0{
		estadd local thisstat`count' = "\textbf{In favour}" : col4
	}
	if `biasdir' == 0{
		estadd local thisstat`count' = "?" : col4
	}
	
	local statnames "`statnames' thisstat`count' thisstat`countse'"

	local count = `count' + 2
	local countse = `count' + 1
	local ++varcount
	
	di "`var'"
	di `biasdir'
	
}

esttab col* using "`outpath'/balance_rail.tex", se replace compress cells(none) booktabs nonotes stats(`statnames', labels("Population (in 100,000)" " "  "Ruggedness" " " "Night lights" " " "Altitude" " " "Agr. suitability index"  " " "Temperature" " "  "Precipitation"  " " "Yearly growing days" " " "Malaria prevalence" " "   "$<25$ KM from suitable harbor" " " "$<25$ KM from navigable river" " " "$<25$ KM from navigable lake" " " "National capital" " " "At national border" " " "Road density (z-scored)" " ")) mtitles("Rail" "Placebo" "p-value" "Bias (imm.)") noconstant substitute(\_ _) star(* 0.1 ** 0.05 *** 0.01)

