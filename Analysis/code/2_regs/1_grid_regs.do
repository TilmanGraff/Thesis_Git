**********************
* Prelims
**********************

cd "/Users/tilmangraff/Documents/GitHub/Thesis_Git"

local outpath "/Users/tilmangraff/Dropbox (Harvard University)/Apps/Overleaf/Spatial Inefficiencies/tables/2023-09-26"


loc geoctrls "altitude landsuit temp precip growingdays malaria biomes1 biomes4 biomes5 biomes6 biomes8 biomes10 biomes11 biomes12 biomes13 biomes14 biomes2_3 x x_2 x_3 x_4 y y_2 y_3 y_4 harbor25 river25 lake25 i.ccode iscapital isborder"

foreach type in "base" "10perc"{

if "`type'" == "10perc"{
  loc type_addon = "_10perc"
}
else{
  loc type_addon = ""
}


foreach simcontrols in "No" "Yes"{

use "./Analysis/input/maingrid.dta", clear

if "`simcontrols'" == "Yes"{
  loc simctrls "pop rugg lights"
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
  qui reg zzeta_`type' `xvar'_50 `geoctrls' `simctrls', vce(cluster cluster)
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
    replace dist`dist' = `rtype'`dist'
  }

  reg zzeta_`type' dist10 dist20 dist30 dist40 `geoctrls' `simctrls', vce(cluster cluster)
  eststo modr`col'
  estadd local fe "Yes": modr`col'
  estadd local gctrls "Yes": modr`col'
  estadd local sctrls "`simcontrols'": modr`col'

  local col = `col' + 1

}


esttab modr* using "`outpath'/rails`sim_addon'`type_addon'.tex", se replace keep(railkm_50 placebokm_50 railkm_military_50 railkm_mining_50 dist10 dist20 dist30 dist40) stats(fe gctrls sctrls N r2, labels("Country FE" "Geography Controls" "Simulation Controls" "N" "R2")) coeflabels(railkm_50 "50 KM of Colonial Railroads" placebokm_50 "50 KM of Colonial Placebo Railroads" railkm_military_50 "50 KM of Colonial Railroads for Military Purposes" railkm_mining_50 "50 KM of Colonial Railroads for Mining Purposes" dist10 "$<$10KM to railroad" dist20 "10-20KM to railroad" dist30 "20-30KM to railroad" dist40 "30-40KM to railroad") nomtitle collabels(none) mgroups("Infrastructure discrimination $\Tilde{\Lambda}$ (z-scored)" "Real" "Placebo", pattern(1 0 0 0 1 1)  ///
prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) nonotes

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

    qui reg zzeta_`type' `xvar' `geoctrls' `simctrls', vce(cluster cluster)
    eststo moda`col'
    estadd local fe "Yes": moda`col'
    estadd local gctrls "Yes": moda`col'
    estadd local sctrls "`simcontrols'": moda`col'

    local col = `col' + 1
    loc xvars "`xvars' `xvar'"

  }
}

esttab moda* using "`outpath'/aid`sim_addon'`type_addon'.tex", se replace keep(`xvars') stats(fe gctrls sctrls N r2, labels("Country FE" "Geography Controls" "Simulation Controls" "N" "R2")) coeflabels(dis "Total disbursements (mil \\$)" dis_transp "Total transport sector disbursements (mil \\$)" num "Number of projects" num_transp "Number of transport sector disbursements") nomtitle collabels(none) mgroups("$\Tilde{\Lambda}$: World Bank" "$\Tilde{\Lambda}$: China", pattern(1 0 0 0 1 0 0 0)  ///
prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) nonotes

eststo clear

**********************
* Leaders regs
**********************

loc col = 1
loc xvars = ""
foreach ifstub in "1" "!iscapital"{
  foreach xvar in "years_in_power" "ever_in_power"{

    qui reg zzeta_`type' `xvar' `geoctrls' `simctrls' if `ifstub', vce(cluster cluster)
    eststo mod`col'
    estadd local fe "Yes": mod`col'
    estadd local gctrls "Yes": mod`col'
    estadd local sctrls "`simcontrols'": mod`col'

    local col = `col' + 1
    loc xvars "`xvars' `xvar'"

  }
}

** adding ethnicity regressions here

use "./Analysis/input/grid_ethn.dta", clear
loc etctrls = "altitude landsuit temp precip growingdays malaria biomes1 biomes4 biomes5 biomes6 biomes8 biomes10 biomes11 biomes12 biomes13 biomes14 biomes2_3 i.ccode borderdist1 seadist1 larea iscapital"

foreach xvar in "dis" "ex" "etwar"{

  qui reg zzeta_`type' `xvar' `etctrls' `simctrls', vce(cluster eth_cluster)
  eststo mod`col'
  estadd local fe "Yes": mod`col'
  estadd local gctrls "Yes": mod`col'
  estadd local sctrls "`simcontrols'": mod`col'

  local col = `col' + 1
  loc xvars "`xvars' `xvar'"

}

esttab mod* using "`outpath'/leaders`sim_addon'`type_addon'.tex", se replace keep(`xvars') stats(fe gctrls sctrls N r2, labels("Country FE" "Geography Controls" "Simulation Controls" "N" "R2")) coeflabels(years_in_power "\vspace{0.2ex}\\Total years in power" ever_in_power "Ever in power dummy" dis "Discriminated against" ex "Excluded from government" etwar "Involved in ethnic war") nomtitle collabels(none) mgroups("$\Tilde{\Lambda}$: entire sample" "$\Tilde{\Lambda}$: excluding capitals" "$\Tilde{\Lambda}$: ethnic homeland level", pattern(1 0 1 0 1 0 0)  ///
prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) nonotes

eststo clear

}
}