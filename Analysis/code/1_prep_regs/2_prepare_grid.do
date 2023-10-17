**********************
* Prelims
**********************

cd "/Users/tilmangraff/Documents/GitHub/Thesis_Git"

import delim "Analysis/input/opt_loc.csv", clear


**********************
* Some basic cleaning
**********************

drop if region == "NA"

drop if zeta_base == "NA"
destring x y region subregion lights num_landpixels gridarea pop_dens pop_sd lights_sd rugg altitude landsuit temp precip growingdays malaria harbor alternative_lights lights_raw un_code pop p_stat p_opt util_stat util_opt c_stat c_opt i_change_* dma* zet* util_*, replace
encode country, gen(ccode)

foreach type in "base" "10perc" "base_old"{
  summ zeta_`type'
  gen zzeta_`type' = (zeta_`type'-`r(mean)')/`r(sd)'
  replace fma_`type' = "." if fma_`type' == "NA"
  destring fma_`type', replace
}


****** add polynomials
forval i = 2/4{
  foreach var in "x" "y"{
    gen `var'_`i' = `var'^`i'
  }
}



tempfile raw
save `raw'

**********************
* Prepare other variables
**********************

import delim "Analysis/temp/clusters.csv", clear
tempfile clusters
save `clusters'

import delim "Analysis/temp/henderson_controls.csv", clear
tempfile contrls
save `contrls'

import delim "Analysis/temp/ID_capitals.csv", clear
tempfile capitals
save `capitals'

import delim "Analysis/temp/leaders.csv", clear varn(1)
tempfile leaders
save `leaders'

import delim "Analysis/temp/raildf.csv", clear
tempfile raildf
save `raildf'

import delim "Analysis/temp/grid_ids_aid.csv", clear
tempfile aid
save `aid'

import delim "Analysis/temp/ethndf.csv", clear
replace tribeid = "." if tribeid == "NA"
replace tribename = "." if tribeid == "NA"
encode tribename, gen(tribe_name)
drop tribename
destring tribeid, replace
drop v1
tempfile ethn
save `ethn'


import delim "Analysis/temp/borders.csv", clear
drop rownumber country
gen isborder = border != 8
tempfile border
save `border'


**********************
* Merge
**********************

use `raw', clear

foreach mergeset in "capitals" "clusters" "leaders" "raildf" "aid" "contrls" "ethn" "border"{
  merge 1:1 id using ``mergeset''
  drop if _m == 2
  drop _m
}

**********************
* Some further manipulations
**********************

foreach var in "railkm" "placebokm" "railkm_military" "railkm_mining"{
  gen `var'_50 = `var' / 50
}



foreach type in "rail" "placebo"{
  gen aux = 0
  foreach dist in "10" "20" "30" "40"{

    gen `type'`dist' = dist2`type' < `dist' & aux == 0
    replace aux = 1 if dist2`type' < `dist'

  }
  drop aux
  gen any`type' = `type'km > 0
}

gen ever_in_power = years_in_power > 0

**********************
* Export grid
**********************

save "./Analysis/input/maingrid.dta", replace
