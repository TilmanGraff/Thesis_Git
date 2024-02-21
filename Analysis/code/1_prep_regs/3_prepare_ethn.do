**********************
* Prelims
**********************

cd "/Users/tilmangraff/Documents/GitHub/Thesis_Git"

use "./Analysis/input/maingrid.dta", clear

**********************
* Some basic cleaning
**********************

drop if tribeid == .

foreach type in "base_old" "imm" "imm_10p"{
    gen w_stat_`type' = util_stat_`type' * pop
    gen w_opt_`type' = util_opt_`type' * pop
}

foreach type in "mob"{
	gen w_stat_`type' = pop
    gen w_opt_`type' = pop_opt_`type'
}

collapse (sum) w_stat_* w_opt_* pop lights gridarea years_in_power wb_dis wb_commitments wb_num china_dis china_num railkm placebokm railkm_military railkm_mining i_change_base dist2rail dist2placebo iscapital (mean) rugg altitude landsuit temp precip growingdays malaria biomes7_9 distc harbor25 river25 lake25 biomes1 biomes4 biomes5 biomes6 biomes8 biomes10 biomes11 biomes12 biomes13 biomes14 biomes2_3, by(ccode tribe_name)

replace iscapital = iscapital > 0

foreach type in "base_old" "imm" "imm_10p" "mob"{
    gen zeta_`type' = w_opt_`type' / w_stat_`type'
    drop w_opt_`type' w_stat_`type'

    summ zeta_`type', d
    gen zzeta_`type' = (zeta_`type'-`r(mean)')/`r(sd)'
}

ren zzeta_mob zzeta_mob_L

decode tribe_name, gen(ethn_name)
decode ccode, gen(country)

***
* include EPR stuff
***
preserve
import delim "./Analysis/temp/mich_epr.csv", clear
keep ethn_name country borderdist1 seadist1 coastal fneigh lnpop60 ndis etwar ewar_hist war dis pw aut sds ex cluster

gen eth_cluster_string = country + cluster
encode eth_cluster_string, gen(eth_cluster)
drop cluster

* two south sudanese tribes are duplicates in the data, keep a random one:
set seed 2345
gen rand = runiform()

sort rand
bys ethn_name country: gen x = _n
keep if x == 1
drop rand

tempfile epr
save `epr'

restore

merge 1:1 ethn_name country using `epr'
drop _m
gen larea = log(gridarea)

save "./Analysis/input/grid_ethn.dta", replace
