**********************
* Prelims
**********************

cd "/Users/tilmangraff/Documents/GitHub/Thesis_Git"

use "./Analysis/input/maingrid.dta", clear
loc types ""mob" "mob_10p" "imm" "imm_10p""

**********************
* Some basic cleaning
**********************

drop if tribeid == .

replace pop_opt_imm = pop_stat
replace pop_opt_imm_10p = pop_stat


foreach type in `types'{
    gen w_stat_`type' = util_stat_`type' * pop_stat
    gen w_opt_`type' = util_opt_`type' * pop_opt_`type'
}

foreach type in "mob" "mob_10p"{
	replace w_stat_`type' = pop_stat
    replace w_opt_`type' = pop_opt_`type'
}

drop *_pc
ll
collapse (sum)  w_stat_* w_opt_* i_stat* i_change*  pop* lights gridarea years_in_power wb_dis wb_commitments wb_num china_dis china_num railkm placebokm railkm_military railkm_mining dist2rail dist2placebo iscapital (mean) fma* rugg altitude landsuit temp precip growingdays malaria biomes7_9 distc harbor25 river25 lake25 biomes1 biomes4 biomes5 biomes6 biomes8 biomes10 biomes11 biomes12 biomes13 biomes14 biomes2_3, by(ccode tribe_name)

replace iscapital = iscapital > 0

foreach type in `types'{
	gen Lambda_`type' = w_opt_`type' / w_stat_`type' 
}

* Define mobility Lambda based on population movements
gen Lambda_L_mob = pop_opt_mob / pop_stat
gen Lambda_L_mob_10p = pop_opt_mob_10p / pop_stat

foreach var of varlist i_change* i_stat*{
	gen `var'_pc = `var' / pop_stat
}


foreach measure in "Lambda" "i_stat" "i_change" "fma"{
	foreach type in `types'{
		foreach innertype in "" "_L" "_nghbs"{
			foreach outertype in "" "_pc"{
			  capture confirm variable `measure'`innertype'_`type'`outertype'
			  if !_rc{
				  summ `measure'`innertype'_`type'`outertype', d 
				  replace `measure'`innertype'_`type'`outertype' = r(p99) if `measure'`innertype'_`type'`outertype' > r(p99)
				  replace `measure'`innertype'_`type'`outertype' = r(p1) if `measure'`innertype'_`type'`outertype' < r(p1)
				  summ `measure'`innertype'_`type'`outertype'
				  gen z`measure'`innertype'_`type'`outertype' = (`measure'`innertype'_`type'`outertype'-`r(mean)')/`r(sd)'
			  }
		}
	}
}
}


decode tribe_name, gen(ethn_name)
decode ccode, gen(country)

***
* include EPR stuff
***
preserve
*import delim "./Analysis/temp/mich_epr.csv", clear

use "/Users/tilmangraff/Documents/GitHub/Thesis_Git/Analysis/input/Ethnic_Data_Michalopoulos/aer_all2013.dta", clear

ren name ethn_name

keep ethn_name ccountry borderdist1 seadist1 coastal diamondd fneigh lnpop60 all      allm allmd allmm allmmd durm  petroleum city1400 cluster split //etwar ewar_hist war dis aut pw sds ex
ren ccou country 


gen eth_cluster_string = country + cluster
encode eth_cluster_string, gen(eth_cluster)
drop cluster



***** really annoying string merge 

replace country = "Cote-dIvoire" if country == "C�te d'Ivoire" // within string!
replace country = "Democratic-Republic-of-the-Congo" if country == "Congo, Dem. Rep."
replace country = "Egypt" if country == "Egypt, Arab Rep."
replace country = "Equatorial-Guinea" if country == "Equatorial Guinea"
replace country = "Sierra-Leone" if country == "Sierra Leone"
replace country = "South-Africa" if country == "South Africa"
replace country = "United-Republic-of-Tanzania" if country == "Tanzania"
replace country = "Central-African-Republic" if country == "Central African Republic"
replace country = "Burkina-Faso" if country == "Burkina Faso"
replace country = "Congo" if country == "Congo, Rep."

* two south sudanese tribes are duplicates in the data, keep a random one:
set seed 2345
gen rand = runiform()

sort rand
bys ethn_name country: gen x = _n
keep if x == 1
drop rand x



tempfile acled
save `acled'

restore



preserve 

import delim "./Analysis/temp/mich_epr.csv", clear

keep ndis dis ex ethn_name etwar ewar_hist war country

* two south sudanese tribes are duplicates in the data, keep a random one:
set seed 2345
gen rand = runiform()

sort rand
bys ethn_name country: gen x = _n
keep if x == 1
drop rand x


tempfile epr
save `epr'


restore


merge 1:1 ethn_name country using `acled'
keep if _m == 3
drop _m

merge 1:1 ethn_name country using `epr'
keep if _m == 3
drop _m

gen larea = log(gridarea)

save "./Analysis/input/grid_ethn.dta", replace
