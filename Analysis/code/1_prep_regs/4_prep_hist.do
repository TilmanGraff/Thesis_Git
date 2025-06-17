***********
* Prelims
***********

cd "/Users/tilmangraff/Documents/GitHub/Thesis_Git"


***********
* Import different years and append
***********


import delim using "./Build/output/ken_hist/centroids.csv", clear
tempfile base
save `base'

forvalues year = 1960/2012{
	
	if fileexists("/Users/tilmangraff/Documents/GitHub/Thesis_Git/Build/output/ken_hist/hist_outcomes/outcomes_`year'.csv"){
		
		import delim using "/Users/tilmangraff/Documents/GitHub/Thesis_Git/Build/output/ken_hist/hist_outcomes/outcomes_`year'.csv", clear
		
		gen year = `year'
		
		
		reshape wide util* l_*, i(id) j(year)
		merge 1:1 id using `base', nogen
		tempfile base
		save `base'
	}
	
}

keep distname util* l_*
reshape long util_stat util_opt util_10p l_statmob l_optmob l_10pmob, i(distname) j(year)

***********
* Merge with expenditure data
***********

merge 1:1 distname year using "./Analysis/input/kenya_roads_exp.dta"
drop _m


gen Lambda_imm = util_opt / util_stat
gen Lambda_imm_10p = util_10p / util_stat

gen Lambda_L_mob = l_optmob / l_statmob
gen Lambda_L_mob_10p = l_10pmob / l_statmob

***********
* Within each year, z-score
***********

foreach type in "L_mob" "imm"{
	foreach type2 in "" "_10p"{
		gen zLambda_`type'`type2' = .
			forvalues year = 1960/2012{
				
				count if Lambda_`type'`type2' != . & year == `year'
				if `r(N)' > 0{
					summ Lambda_`type'`type2' if year == `year'
					replace zLambda_`type'`type2' = (Lambda_`type'`type2' - `r(mean)')/`r(sd)' if year == `year'
				}
				
			}
	}
}

***********
* xtset and export
***********

encode distname_1979, gen(dist_id)
xtset dist_id year 

***********
* compute baseyears
***********

gen baseyear = year if zLambda_imm != .
replace baseyear = L.baseyear if baseyear >= .

preserve
collapse (sum) droadexp_tot droadexp_country, by(baseyear dist_id)
gen exp_share_agg = droadexp_tot / droadexp_country
tempfile aggs 
save `aggs'
restore

merge n:1 baseyear dist_id using `aggs', nogen
gen exp_dens_share_agg = 100*  exp_share_agg / pop1962_share


save "./Analysis/input/ken_hist.dta", replace



