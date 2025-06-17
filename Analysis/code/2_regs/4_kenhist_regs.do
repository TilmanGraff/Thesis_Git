***********
* Prelims
***********

cd "/Users/tilmangraff/Documents/GitHub/Thesis_Git"

use "./Analysis/input/ken_hist.dta", clear

local outpath "/Users/tilmangraff/Dropbox (Harvard University)/Apps/Overleaf/Spatial Inefficiencies/tables/2024-03-24"


loc fullctrl_t "pop1962_t area_t urbrate1962_t earnings_t wage_employment_t value_cashcrops_t MomKam_t border_t dist2nairobi_t"


loc geoctrls "altitude landsuit temp precip growingdays malaria biomes1 biomes4 biomes5 biomes6 biomes8 biomes10 biomes11 biomes12 biomes13 biomes14 biomes2_3 x x_2 x_3 x_4 y y_2 y_3 y_4 harbor25 river25 lake25 i.ccode iscapital isborder"

loc simctrls "pop_stat rugg lights"

foreach type in "imm" "L_mob"{
	foreach type2 in "" "_10p"{
		
		use "./Analysis/input/ken_hist.dta", clear

		if "`type'" == "L_mob"{
			loc typetex = "mob"
		}
		
		if "`type'" == "imm"{
			loc typetex = "imm"
		}

		loc col = 1
		loc xvars = ""
		loc xvars_intct = ""

		if "`type2'" == "_10p"{
			loc type2superscript "^{10\%}"
		}
		else{
			loc type2superscript ""
		}

		foreach dem in "0" "1"{
			
			gen zLambda_`type'`type2'`dem' = zLambda_`type'`type2' if multiparty == `dem'

			reghdfe exp_dens_share zLambda_`type'`type2'`dem' president `fullctrl_t', a(dist_id year) cluster(dist_id)

			eststo mod`col'
			estadd local dfe "Yes": mod`col'
			estadd local kenfe "Yes": mod`col'
			estadd local fe " ": mod`col'
			estadd local gctrls " ": mod`col'
			estadd local sctrls " ": mod`col'

			local col = `col' + 1
			loc xvars "`xvars' zLambda_`type'`type2'`dem'"
			loc xvars_intct "`xvars_intct' zLambda_`type'`type2'`dem'"
		}
		
		foreach dem in "0" "1"{
		
			use "./Analysis/input/ken_hist.dta", clear
			gen zLambda_`type'`type2'`dem' = zLambda_`type'`type2' if multiparty == `dem'

			gen zLambda_p = zLambda_`type'`type2'`dem' * president

			reghdfe exp_dens_share zLambda_`type'`type2'`dem' zLambda_p president `fullctrl_t', a(dist_id year) cluster(dist_id)
			
			eststo mod`col'
			estadd local dfe "Yes": mod`col'
			estadd local kenfe "Yes": mod`col'
			estadd local fe " ": mod`col'
			estadd local gctrls " ": mod`col'
			estadd local sctrls " ": mod`col'

			local col = `col' + 1
			
			
		}
		loc xvars_intct "`xvars_intct' zLambda_p president"


		use "./Analysis/input/maingrid.dta", clear
		gen lyears = log(1 + years_in_power)

		foreach xvar in "ever_in_power" "lyears" {

		reg zLambda_`type'`type2' `xvar' `geoctrls' `simctrls', vce(cluster cluster)
		
		eststo mod`col'
		estadd local fe "Yes": mod`col'
		estadd local gctrls "Yes": mod`col'
		estadd local sctrls "Yes": mod`col'

		local col = `col' + 1
		loc xvars "`xvars' `xvar'"

		}

		esttab mod5 mod6 mod1 mod2  using "`outpath'/favouritism_`type'`type2'.tex", se replace keep(`xvars') stats(dfe kenfe fe gctrls  N r2, labels("Year and District FE"  "(Demographic, political, economic, geographic) $\times$ trend" "Country FE" "Geography Controls" "N" "R2")) coeflabels(zLambda_`type'`type2'0 "\emph{Panel B: Kenyan road expenditure over time} \\ $\Lambda_\text{`typetex'}`type2superscript'$ (non-democracy)" zLambda_`type'`type2'1 "$\Lambda_\text{`typetex'}`type2superscript'$ (democracy)" lyears "log(1 + Total years in power)" ever_in_power "\vspace{0.2ex}\emph{Panel A: entire sample}\\Ever in power dummy") nomtitle collabels(none) mgroups("Discrimination $\Lambda_\text{`typetex'}`type2superscript'$" "Relative road expenditure", pattern(1 0 1 0)  ///
		prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) nonotes substitute(\_ _) star(* 0.1 ** 0.05 *** 0.01) order(ever_in_power lyears zLambda_`type'`type2'0 zLambda_`type'`type2'1)
		
		
		esttab mod1 mod2 mod3 mod4 using "`outpath'/favouritism_`type'`type2'_interaction.tex", se replace keep(`xvars_intct') stats(dfe kenfe N r2, labels("Year and District FE"  "(Demographic, political, economic, geographic) $\times$ trend" "N" "R2")) coeflabels(zLambda_`type'`type2'0 "\emph{Panel A: Kenyan road expenditure over time} \\ $\Lambda_\text{`typetex'}`type2superscript'$ (non-democracy)" zLambda_`type'`type2'1 "$\Lambda_\text{`typetex'}`type2superscript'$ (democracy)" president "Coethnic district" zLambda_p "$\Lambda_\text{`typetex'}`type2superscript' \times$ Coethnic district") nomtitle collabels(none) mgroups("Relative road expenditure", pattern(1 0 0 0)  ///
		prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) nonotes substitute(\_ _) star(* 0.1 ** 0.05 *** 0.01) order(zLambda_`type'`type2'0 zLambda_`type'`type2'1 president zLambda_p)

		eststo clear
	}
}
