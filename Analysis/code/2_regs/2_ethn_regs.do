**********************
* Prelims
**********************

cd "/Users/tilmangraff/Documents/GitHub/Thesis_Git"

local outpath "/Users/tilmangraff/Dropbox (Harvard University)/Apps/Overleaf/Spatial Inefficiencies/tables/2022-10-07"

use "./Analysis/input/grid_ethn.dta", clear

loc geoctrls "altitude landsuit temp precip growingdays malaria biomes1 biomes4 biomes5 biomes6 biomes8 biomes10 biomes11 biomes12 biomes13 biomes14 biomes2_3 x x_2 x_3 x_4 y y_2 y_3 y_4 harbor25 river25 lake25 i.ccode iscapital isborder"

loc simctrls "pop rugg lights distinct_good"
xxx
**********************
* Rail regs
**********************

loc col = 1
foreach xvar in "railkm" "placebokm" "railkm_military" "railkm_mining"{
  qui reg zzeta `xvar'_50 `geoctrls' `simctrls', vce(cluster cluster)
  eststo mod`col'
  estadd local fe "Yes": mod`col'
  estadd local gctrls "Yes": mod`col'
  estadd local sctrls "Yes": mod`col'

  local col = `col' + 1

}

esttab mod* using "`outpath'/rails.tex", se replace keep(railkm_50 placebokm_50 railkm_military_50 railkm_mining_50) stats(fe gctrls sctrls N r2, labels("Country FE" "Geography Controls" "Simulation Controls" "N" "R2")) coeflabels(railkm_50 "50 KM of Colonial Railroads" placebokm_50 "50 KM of Colonial Placebo Railroads" railkm_military_50 "50 KM of Colonial Railroads for Military Purposes" railkm_mining_50 "50 KM of Colonial Railroads for Mining Purposes") nomtitle collabels(none) mgroups("Infrastructure discrimination $\Lambda$ (z-scored)", pattern(1 0 0 0)  ///
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

    qui reg zzeta `xvar' `geoctrls' `simctrls', vce(cluster cluster)
    eststo mod`col'
    estadd local fe "Yes": mod`col'
    estadd local gctrls "Yes": mod`col'
    estadd local sctrls "Yes": mod`col'

    local col = `col' + 1
    loc xvars "`xvars' `xvar'"

  }
}

esttab mod* using "`outpath'/aid.tex", se replace keep(`xvars') stats(fe gctrls sctrls N r2, labels("Country FE" "Geography Controls" "Simulation Controls" "N" "R2")) coeflabels(dis "Total disbursements (mil \\$)" dis_transp "Total transport sector disbursements (mil \\$)" num "Number of projects" num_transp "Number of transport sector disbursements") nomtitle collabels(none) mgroups("$\Lambda$: World Bank" "$\Lambda$: China", pattern(1 0 0 0 1 0 0 0)  ///
prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) nonotes

eststo clear

**********************
* Leaders regs
**********************

loc col = 1
loc xvars = ""
foreach ifstub in "1" "!iscapital"{
  foreach xvar in "years_in_power" "ever_in_power"{

    qui reg zzeta `xvar' `geoctrls' `simctrls' if `ifstub', vce(cluster cluster)
    eststo mod`col'
    estadd local fe "Yes": mod`col'
    estadd local gctrls "Yes": mod`col'
    estadd local sctrls "Yes": mod`col'

    local col = `col' + 1
    loc xvars "`xvars' `xvar'"

  }
}

esttab mod* using "`outpath'/leaders.tex", se replace keep(`xvars') stats(fe gctrls sctrls N r2, labels("Country FE" "Geography Controls" "Simulation Controls" "N" "R2")) coeflabels(years_in_power "Total years in power" ever_in_power "Ever in power dummy") nomtitle collabels(none) mgroups("$\Lambda$: entire sample" "$\Lambda$: excluding capitals", pattern(1 0 1 0)  ///
prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) nonotes
