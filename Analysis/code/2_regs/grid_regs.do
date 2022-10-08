**********************
* Prelims
**********************

cd "/Users/tilmangraff/Documents/GitHub/Thesis_Git"

local outpath "/Users/tilmangraff/Dropbox (Harvard University)/Apps/Overleaf/Spatial Inefficiencies/tables"

use "./Analysis/input/maingrid.dta", clear

loc geoctrls "altitude landsuit temp precip growingdays malaria biomes1 biomes4 biomes5 biomes6 biomes8 biomes10 biomes11 biomes12 biomes13 biomes14 biomes2_3 x x_2 x_3 x_4 y y_2 y_3 y_4 harbor25 river25 lake25 i.ccode iscapital isborder"

loc simctrls "pop rugg lights distinct_good"

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

esttab mod* using "`outpath'/firsttest.tex", se replace keep(railkm_50 placebokm_50 railkm_military_50 railkm_mining_50) stats(fe gctrls sctrls N r2, labels("Country FE" "Geography Controls" "Simulation Controls" "N" "R2")) coeflabels(railkm_50 "50 KM of Colonial Railroads" placebokm_50 "50 KM of Colonial Placebo Railroads" railkm_military_50 "50 KM of Colonial Railroads for Military Purposes" railkm_mining_50 "50 KM of Colonial Railroads for Mining Purposes")
