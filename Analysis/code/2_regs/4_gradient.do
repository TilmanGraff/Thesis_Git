**********************
* Gradient regressions
**********************

cd "/Users/tilmangraff/Documents/GitHub/Thesis_Git"
sort 
postutil clear 

local outpath "/Users/tilmangraff/Dropbox (Harvard University)/Apps/Overleaf/Spatial Inefficiencies/tables/2024-02-17"


loc geoctrls "altitude landsuit temp precip growingdays malaria biomes1 biomes4 biomes5 biomes6 biomes8 biomes10 biomes11 biomes12 biomes13 biomes14 biomes2_3 x x_2 x_3 x_4 y y_2 y_3 y_4 harbor25 river25 lake25 i.ccode iscapital isborder"

loc simctrls "pop_stat rugg lights"

postfile gradient str8 type dist str16 outcome b se str8 railtype using "/Users/tilmangraff/Documents/GitHub/Thesis_Git/Analysis/temp/railgradient.dta", replace

*keep if dist2rail < 200

foreach outcome in "Lambda" "i_stat" "i_opt"{

loc type "imm"

foreach railtype in "rail" "placebo"{

if "`outcome'" == "i_change" || "`outcome'" == "i_stat"{
	loc type `type'
}

 reg z`outcome'_`type' `railtype'_d10-`railtype'_d100 //`geoctrls' `simctrls', vce(cluster cluster)
//  reg z`outcome'_`type' placebo_d10 placebo_d20 placebo_d30 placebo_d40 placebo_d50 placebo_d60 placebo_d70 placebo_d80 placebo_d90 placebo_d100 //`geoctrls' `simctrls', vce(cluster cluster)
// pp
forvalues x = 10(10)100{
	
	post gradient ("`type'") (`x') ("`outcome'") (_b[`railtype'_d`x']) (_se[`railtype'_d`x']) ("`railtype'")
}
di "`outcome'"
}
}
postclose gradient

use "/Users/tilmangraff/Documents/GitHub/Thesis_Git/Analysis/temp/railgradient.dta", clear
