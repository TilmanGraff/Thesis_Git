**********************
* Prelims
**********************

cd "/Users/tilmangraff/Documents/GitHub/Thesis_Git"

import delim "Analysis/input/opt_loc.csv", clear

loc types ""base_old" "mob" "mob_10p" "imm" "imm_10p""

**********************
* Some basic cleaning
**********************


destring x y region subregion lights num_landpixels gridarea pop_dens pop_sd lights_sd rugg altitude landsuit temp precip growingdays malaria harbor alternative_lights lights_raw un_code pop* p_stat p_opt util_stat util_opt c_stat c_opt i_change_* i_stat* is_* i_fraction* fma* util_* amenit* pop_opt* c_* , replace ig("NA")
encode country, gen(ccode)


tempfile grid 
save `grid'

**********************
* Briefly compute country totals
**********************

foreach type in `types'{
	
	gen welfare_stat_`type' = pop_stat * util_stat_`type'
	gen welfare_opt_`type' = pop_stat * util_opt_`type'
	
}

replace region = 0 if region == .


preserve


collapse (sum) welfare_*, by(region)
keep if region == 2
gen country = "Africa"
tempfile africa 
save `africa'

restore 

collapse (sum) welfare_* (mean) region, by(country)
append using `africa'

*drop if welfare_stat_base_old == 0

foreach type in `types'{
	gen Lambda_`type' = welfare_opt_`type' / welfare_stat_`type'
}

foreach var of varlist Lambda_mo*{
	gen `var'_CE = `var'^(1/0.7)
}

foreach var of varlist Lambda_*{
	gen `var'_barchart = (`var' - 1)*100
}

keep region *_barchart country

export delim "./Analysis/input/country_means.csv", replace

**********************
* Focus on Africa now
**********************

use `grid', clear



drop if region == .
keep if region == 2

**********************
* Compute Lambdas
**********************

foreach type in `types'{
	gen Lambda_`type' = util_opt_`type' / util_stat_`type' 
}

* Define mobility Lambda based on population movements
gen Lambda_L_mob = pop_opt_mob / pop_stat
gen Lambda_L_mob_10p = pop_opt_mob_10p / pop_stat


**********************
* 
**********************


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

*destring railscon* placebocon*, replace ig("NA")

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
* adjust i_change
**********************

foreach var of varlist i_change* i_stat*{
	replace `var' = `var' / border
	gen `var'_pc = `var' / pop_stat
}

drop amenity_base_old amenity_mob_10p amenity_imm amenity_imm_10p



**********************
* Normalise amenities to per capita
**********************

gen amenity_pc = amenity_mob / pop_stat

qui summ amenity_pc
gen zamenity_pc = (amenity_pc - `r(mean)')/`r(sd)'
gen zamenity_pc_mob = zamenity_pc

qui summ amenity_mob 
gen zamenity_mob = (amenity_mob - `r(mean)')/`r(sd)'

foreach measure in "Lambda" "fma" "i_stat"{
	foreach type in `types'{
		foreach innertype in "" "_L" "_nghbs"{
			foreach outertype in "" "_pc"{
			  capture confirm variable `measure'`innertype'_`type'`outertype'
			  if !_rc{
				count if `measure'`innertype'_`type'`outertype' != .
				if `r(N)' > 0{
				gen aux = `measure'`innertype'_`type'`outertype'
				  summ aux, d 
				  replace aux = r(p99) if aux > r(p99)
				  replace aux = r(p1) if aux < r(p1)
				  summ aux
				  gen z`measure'`innertype'_`type'`outertype' = (aux-`r(mean)')/`r(sd)'
				  drop aux
				  summ `measure'`innertype'_`type'`outertype', d
				  gen z`measure'`innertype'_`type'`outertype'_forR = (`measure'`innertype'_`type'`outertype' - `r(mean)')/`r(sd)'
				  replace z`measure'`innertype'_`type'`outertype'_forR = . if `measure'`innertype'_`type'`outertype' > r(p95) | `measure'`innertype'_`type'`outertype' < r(p5)
				}
			}
		  }
		}
	}
}

foreach type in `types'{
	summ i_stat_imm, d
	gen zi_change_`type' = (i_change_`type' - `r(mean)')/(`r(sd)')
	gen zi_opt_`type' = (i_stat_imm + i_change_`type' - `r(mean)')/(`r(sd)')
}

**********************
* Some further manipulations
**********************

foreach var in "railkm" "placebokm" "railkm_military" "railkm_mining"{
  gen `var'_50 = `var' / 50
}



foreach type in "rail" "placebo"{
  gen aux = 0
  forvalues dist = 10(10)500 {
    gen `type'_d`dist' = dist2`type' < `dist' & aux == 0
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
