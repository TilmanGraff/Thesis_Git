**********************
* Prelims
**********************

cd "/Users/tilmangraff/Documents/GitHub/Thesis_Git"

use "./Analysis/input/maingrid.dta", clear

**********************
* Some basic cleaning
**********************

drop if tribeid == .

gen w_stat = util_stat * pop
gen w_opt = util_opt * pop


collapse (sum) w_stat w_opt pop lights gridarea years_in_power wb_dis wb_commitments wb_num china_dis china_num railkm placebokm railkm_military railkm_mining i_change dist2rail dist2placebo (mean) rugg altitude landsuit temp precip growingdays malaria biomes7_9 distc harbor25 river25 lake25 biomes1 biomes4 biomes5 biomes6 biomes8 biomes10 biomes11 biomes12 biomes13 biomes14 biomes2_3, by(ccode tribe_name)

gen zeta = w_opt / w_stat
drop w_opt w_stat
