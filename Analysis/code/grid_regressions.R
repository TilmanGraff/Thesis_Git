
library("stargazer", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("sandwich", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("lmtest", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("multiwayvcov", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("AER", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("ivpack", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")

rm(list=ls(pattern="mod")) # this removes all models from the RStudio environment

opt_loc <- read.csv("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/temp/opt_loc_with_raildist.csv")
borders <- read.csv("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/temp/borders.csv")
aid <- read.csv(file="/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/temp/grid_ids_aid.csv")
epr <- read.csv(file="/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/temp/opt_loc_epr.csv")
buffer <- read.csv(file="/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/temp/ID_bufferKM.csv")
henderson_controls <- read.csv(file="/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/temp/henderson_controls.csv")
years_in_power <- read.csv("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/temp/ID_years_in_power.csv")
capitals <- read.csv("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/temp/ID_capitals.csv")
missions <- read.csv("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/temp/ID_missions.csv")
iv_ida <- read.csv("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/temp/ID_iv_ida.csv")

opt_loc <- merge(opt_loc, borders, by=c("country", "rownumber"))
opt_loc <- merge(opt_loc, aid, by="ID")
opt_loc <- merge(opt_loc, epr, by="ID")
opt_loc <- merge(opt_loc, buffer, by="ID")
opt_loc <- merge(opt_loc, henderson_controls, by="ID")
opt_loc <- merge(opt_loc, years_in_power, by="ID")
opt_loc <- merge(opt_loc, capitals, by="ID")
opt_loc <- merge(opt_loc, missions, by="ID")
opt_loc <- merge(opt_loc, iv_ida, by="ID")


jedwab_countries <- c("Angola", "Benin", "Guinea-Bissau", "Botswana", "Burkina-Faso", "Burundi", "Cameroon", "Central-African-Republic", "Chad", "Congo", "Democratic-Republic-of-the-Congo", "Djibouti", "Equatorial-Guinea", "Eritrea", "Ethiopia", "Gabon", "Gambia", "Ghana", "Guinea", "Cote-dIvoire", "Kenya", "Liberia", "Malawi", "Mali", "Mauritania", "Mozambique", "Namibia", "Niger", "Nigeria", "Rwanda", "Senegal", "Sierra-Leone", "Somalia", "Sudan", "South-Sudan", "United-Republic-of-Tanzania", "Togo", "Uganda", "Zambia", "Zimbabwe", "South-Africa")

jedwab_countries_via_all_rails <- c("Angola", "Benin", "Botswana", "Burkina-Faso", "Burundi", "Cameroon", "Congo", "Democratic-Republic-of-the-Congo", "Djibouti", "Equatorial-Guinea", "Eritrea", "Ethiopia", "Ghana", "Guinea", "Cote-dIvoire", "Kenya", "Liberia", "Malawi", "Mali", "Mozambique", "Namibia", "Nigeria", "Senegal", "Sierra-Leone", "Somalia", "Sudan", "South-Sudan", "United-Republic-of-Tanzania", "Togo", "Uganda", "Zambia", "Zimbabwe", "South-Africa")

#opt_loc <- opt_loc[!is.na(opt_loc$RailKM) & !is.na(opt_loc$PlaceboKM) & !is.na(opt_loc$cluster),]
#opt_loc <- opt_loc[opt_loc$country %in% jedwab_countries_via_all_rails,]
#opt_loc <- opt_loc[opt_loc$country != "South-Africa",]


opt_loc$x_2 <- opt_loc$x^2
opt_loc$x_3 <- opt_loc$x^3
opt_loc$x_4 <- opt_loc$x^4

opt_loc$y_2 <- opt_loc$y^2
opt_loc$y_3 <- opt_loc$y^3
opt_loc$y_4 <- opt_loc$y^4

opt_loc$smaller_ten <- opt_loc$dist2rail <= 10
opt_loc$ten_twenty <- opt_loc$dist2rail > 10 & opt_loc$dist2rail <= 20
opt_loc$twenty_thirty <- opt_loc$dist2rail > 20 & opt_loc$dist2rail <= 30
opt_loc$thirty_fourty <- opt_loc$dist2rail > 30 & opt_loc$dist2rail <= 40

opt_loc$smaller_ten_p <- opt_loc$dist2placebo <= 10
opt_loc$ten_twenty_p <- opt_loc$dist2placebo > 10 & opt_loc$dist2placebo <= 20
opt_loc$twenty_thirty_p <- opt_loc$dist2placebo > 20 & opt_loc$dist2placebo <= 30
opt_loc$thirty_fourty_p <- opt_loc$dist2placebo > 30 & opt_loc$dist2placebo <= 40

opt_loc$border_cell <- opt_loc$border < 8

opt_loc$wb_dis_transp_incompl <- opt_loc$wb_dis_transp - opt_loc$wb_dis_transp_compl
opt_loc$wb_num_transp_incompl <- opt_loc$wb_num_transp - opt_loc$wb_num_transp_compl

opt_loc$china_dis_transp_incompl <- opt_loc$china_dis_transp - opt_loc$china_dis_transp_compl
opt_loc$china_num_transp_incompl <- opt_loc$china_num_transp - opt_loc$china_num_transp_compl

opt_loc$wb_dis_oth_compl <- opt_loc$wb_dis_compl - opt_loc$wb_dis_transp_compl


# opt_loc$wb_other_disbursements <- opt_loc$wb_disbursements-opt_loc$wb_transport_disbursements
# opt_loc$any_disbursements <- as.numeric(opt_loc$wb_disbursements>0)
# opt_loc$wb_placebo_project <- as.numeric(opt_loc$wb_disbursements==0 & opt_loc$wb_commitments!=0 )
# opt_loc$is_mission <- opt_loc$missions>0
# opt_loc$number_other_wb_projects <- opt_loc$number_wb_projects-opt_loc$number_wb_projects_transport
# opt_loc$number_wb_projects_transport_completed <- opt_loc$number_wb_projects_transport-opt_loc$number_wb_transport_projects_incompleted
# opt_loc$china_transport_aid_incompleted <- opt_loc$china_transport_aid - opt_loc$china_transport_aid_completed
# opt_loc$number_china_other_projects <- opt_loc$number_china_projects - opt_loc$number_china_projects_transport


opt_loc$status <- factor(opt_loc$status, levels=c("DISCRIMINATED", "POWERLESS", "IRRELEVANT", "JUNIOR PARTNER", "SENIOR PARTNER", "DOMINANT", "MONOPOLY"))
opt_loc$epr_scale <- as.numeric(opt_loc$status)
opt_loc$discriminated <- as.numeric(opt_loc$epr_scale == 1)
opt_loc$participation <- as.numeric(opt_loc$epr_scale > 3)
opt_loc$monopoly <- as.numeric(opt_loc$epr_scale == 7)

opt_loc$not_buffer_KM <- opt_loc$RailKM - opt_loc$bufferKM

opt_loc$is_mission <- as.numeric(opt_loc$missions > 0)
opt_loc$is_cath_mission <- as.numeric(opt_loc$missions_cath > 0)
opt_loc$is_birthplace <- as.numeric(opt_loc$years_in_power>0)


# if you want logs

# for(var in colnames(opt_loc)[grepl("dis_", colnames(opt_loc))]){
#   opt_loc[,var] <- log(0.01+opt_loc[,var])
# }

#opt_loc <- opt_loc[opt_loc$capital != 1,]
# opt_loc <- opt_loc[opt_loc$is_birthplace != 1,]

#opt_loc <- opt_loc[opt_loc$wb_dis < quantile(opt_loc$wb_dis, .99),]
#opt_loc <- opt_loc[opt_loc$china_dis < quantile(opt_loc$china_dis, .99),]

#########################################
# BASIC CORRELATIONS FOR APPENDIX
#########################################

mod.APP.1 <- lm(zeta~altitude+temp+landsuit+malaria+biomes1+biomes2_3+biomes4+biomes5+biomes6+biomes7_9+biomes8+biomes10+biomes11+biomes12+biomes13+biomes14+harbor25+river25+lake25+growingdays+precip+border+x+x_2+x_3+x_4+y+y_2+y_3+y_4, data=opt_loc)
mod.APP.2 <- lm(zeta~factor(country)+altitude+temp+landsuit+malaria+biomes1+biomes2_3+biomes4+biomes5+biomes6+biomes7_9+biomes8+biomes10+biomes11+biomes12+biomes13+biomes14+harbor25+river25+lake25+growingdays+precip+border+x+x_2+x_3+x_4+y+y_2+y_3+y_4, data=opt_loc)
mod.APP.3 <- lm(zeta~factor(country)+altitude+temp+landsuit+malaria+biomes1+biomes2_3+biomes4+biomes5+biomes6+biomes7_9+biomes8+biomes10+biomes11+biomes12+biomes13+biomes14+harbor25+river25+lake25+growingdays+precip+border+x+x_2+x_3+x_4+y+y_2+y_3+y_4+rugg+lights+pop+urban, data=opt_loc)



#########################################
# RAILROADS
#########################################

###################
# RailKM
# # #
# mod.1 <- lm(zeta~RailKM, data=opt_loc)
# mod.2 <- lm(zeta~RailKM+factor(country), data=opt_loc)
# mod.3 <- lm(zeta~RailKM+factor(country)+altitude+temp+landsuit+malaria+biomes1+biomes2_3+biomes4+biomes5+biomes6+biomes7_9+biomes8+biomes10+biomes11+biomes12+biomes13+biomes14+harbor25+river25+lake25+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+border, data=opt_loc)
# mod.4 <- lm(zeta~RailKM+factor(country)+altitude+temp+landsuit+malaria+biomes1+biomes2_3+biomes4+biomes5+biomes6+biomes7_9+biomes8+biomes10+biomes11+biomes12+biomes13+biomes14+harbor25+river25+lake25+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+rugg+lights+pop+urban+border, data=opt_loc)
#
# mod.5 <- lm(zeta~PlaceboKM, data=opt_loc)
# mod.6 <- lm(zeta~PlaceboKM+factor(country), data=opt_loc)
# mod.7 <- lm(zeta~PlaceboKM+factor(country)+altitude+temp+landsuit+malaria+biomes1+biomes2_3+biomes4+biomes5+biomes6+biomes7_9+biomes8+biomes10+biomes11+biomes12+biomes13+biomes14+harbor25+river25+lake25+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+border, data=opt_loc)
# mod.8 <- lm(zeta~PlaceboKM+factor(country)+altitude+temp+landsuit+malaria+biomes1+biomes2_3+biomes4+biomes5+biomes6+biomes7_9+biomes8+biomes10+biomes11+biomes12+biomes13+biomes14+harbor25+river25+lake25+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+rugg+lights+pop+urban+border, data=opt_loc)
# # #
# # covariate_labels <- c("KM of Colonial Railroads", "KM of Colonial Placebo Railroads")


###################
# Distance blocks
#
# # #
# data_small <- opt_loc[opt_loc$country %in% jedwab_countries_via_all_rails,]
#
# #mod.1 <- lm(zeta~smaller_ten+ten_twenty+twenty_thirty+thirty_fourty, data=opt_loc)
# mod.2 <- lm(zeta~smaller_ten+ten_twenty+twenty_thirty+thirty_fourty+factor(country), data=opt_loc)
# mod.3 <- lm(zeta~smaller_ten+ten_twenty+twenty_thirty+thirty_fourty+factor(country)+altitude+temp+landsuit+malaria+biomes1+biomes2_3+biomes4+biomes5+biomes6+biomes7_9+biomes8+biomes10+biomes11+biomes12+biomes13+biomes14+harbor25+river25+lake25+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+border, data=opt_loc)
# mod.4 <- lm(zeta~smaller_ten+ten_twenty+twenty_thirty+thirty_fourty+factor(country)+altitude+temp+landsuit+malaria+biomes1+biomes2_3+biomes4+biomes5+biomes6+biomes7_9+biomes8+biomes10+biomes11+biomes12+biomes13+biomes14+harbor25+river25+lake25+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+rugg+lights+urban+pop+border, data=opt_loc)
#
# #mod.5 <- lm(zeta~smaller_ten_p+ten_twenty_p+twenty_thirty_p+thirty_fourty_p, data=opt_loc)
# mod.6 <- lm(zeta~smaller_ten_p+ten_twenty_p+twenty_thirty_p+thirty_fourty_p+factor(country), data=opt_loc)
# mod.7 <- lm(zeta~smaller_ten_p+ten_twenty_p+twenty_thirty_p+thirty_fourty_p+factor(country)+altitude+temp+landsuit+malaria+biomes1+biomes2_3+biomes4+biomes5+biomes6+biomes7_9+biomes8+biomes10+biomes11+biomes12+biomes13+biomes14+harbor25+river25+lake25+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+border, data=opt_loc)
# mod.8 <- lm(zeta~smaller_ten_p+ten_twenty_p+twenty_thirty_p+thirty_fourty_p+factor(country)+altitude+temp+landsuit+malaria+biomes1+biomes2_3+biomes4+biomes5+biomes6+biomes7_9+biomes8+biomes10+biomes11+biomes12+biomes13+biomes14+harbor25+river25+lake25+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+rugg+lights+urban+pop+border, data=opt_loc)
#
# mod.9.1.small <- lm(zeta~smaller_ten+ten_twenty+twenty_thirty+thirty_fourty+factor(country)+altitude+temp+landsuit+malaria+biomes1+biomes2_3+biomes4+biomes5+biomes6+biomes7_9+biomes8+biomes10+biomes11+biomes12+biomes13+biomes14+harbor25+river25+lake25+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+rugg+lights+urban+pop+border_cell, data=data_small)
# mod.9.2.small <- lm(zeta~smaller_ten_p+ten_twenty_p+twenty_thirty_p+thirty_fourty_p+factor(country)+altitude+temp+landsuit+malaria+biomes1+biomes2_3+biomes4+biomes5+biomes6+biomes7_9+biomes8+biomes10+biomes11+biomes12+biomes13+biomes14+harbor25+river25+lake25+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+rugg+lights+urban+pop+border, data=data_small)

# covariate_labels <- c("$<10$ KM to Colonial Railroad", "$10-20$ KM to Colonial Railroad", "$20-30$ KM to Colonial Railroad", "$30-40$ KM to Colonial Railroad", "$<10$ KM to Colonial Placebo Railroad", "$10-20$ KM to Colonial Placebo Railroad", "$20-30$ KM to Colonial Placebo Railroad", "$30-40$ KM to Colonial Placebo Railroad")
#

# Preamble
###################
# log_distance

# mod.1 <- lm(zeta~(dist2rail), data=opt_loc)
# mod.2 <- lm(zeta~(dist2rail)+factor(country), data=opt_loc)
# mod.3 <- lm(zeta~(dist2rail)+factor(country)+altitude+temp+landsuit+malaria+biomes1+biomes2_3+biomes4+biomes5+biomes6+biomes7_9+biomes8+biomes10+biomes11+biomes12+biomes13+biomes14+harbor25+river25+lake25+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4, data=opt_loc)
#
# mod.4 <- lm(zeta~(dist2placebo), data=opt_loc)
# mod.5 <- lm(zeta~(dist2placebo)+factor(country), data=opt_loc)
# mod.6 <- lm(zeta~(dist2placebo)+factor(country)+altitude+temp+landsuit+malaria+biomes1+biomes2_3+biomes4+biomes5+biomes6+biomes7_9+biomes8+biomes10+biomes11+biomes12+biomes13+biomes14+harbor25+river25+lake25+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4, data=opt_loc)
# End Preamble

###################
# Heterogeneous Rails
# data_small <- opt_loc[opt_loc$isnode ==0,]
# #
# mod.1 <- lm(zeta~RailKM_military+factor(country)+altitude+temp+landsuit+malaria+biomes1+biomes2_3+biomes4+biomes5+biomes6+biomes7_9+biomes8+biomes10+biomes11+biomes12+biomes13+biomes14+harbor25+river25+lake25+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+border_cell, data=opt_loc)
# mod.2 <- lm(zeta~RailKM_military+factor(country)+altitude+temp+landsuit+malaria+biomes1+biomes2_3+biomes4+biomes5+biomes6+biomes7_9+biomes8+biomes10+biomes11+biomes12+biomes13+biomes14+harbor25+river25+lake25+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+rugg+lights+pop+border_cell, data=opt_loc)
#
# mod.3 <- lm(zeta~RailKM_mining+factor(country)+altitude+temp+landsuit+malaria+biomes1+biomes2_3+biomes4+biomes5+biomes6+biomes7_9+biomes8+biomes10+biomes11+biomes12+biomes13+biomes14+harbor25+river25+lake25+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+border_cell, data=opt_loc)
# mod.4 <- lm(zeta~RailKM_mining+factor(country)+altitude+temp+landsuit+malaria+biomes1+biomes2_3+biomes4+biomes5+biomes6+biomes7_9+biomes8+biomes10+biomes11+biomes12+biomes13+biomes14+harbor25+river25+lake25+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+rugg+lights+pop+border_cell, data=opt_loc)
#
# mod.5 <- lm(zeta~RailKM_military+RailKM_mining+factor(country)+altitude+temp+landsuit+malaria+biomes1+biomes2_3+biomes4+biomes5+biomes6+biomes7_9+biomes8+biomes10+biomes11+biomes12+biomes13+biomes14+harbor25+river25+lake25+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+border_cell, data=opt_loc)
# mod.6 <- lm(zeta~RailKM_military+RailKM_mining+factor(country)+altitude+temp+landsuit+malaria+biomes1+biomes2_3+biomes4+biomes5+biomes6+biomes7_9+biomes8+biomes10+biomes11+biomes12+biomes13+biomes14+harbor25+river25+lake25+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+rugg+lights+pop+border_cell, data=opt_loc)
# mod.7.small <- lm(zeta~bufferKM+factor(country)+altitude+temp+landsuit+malaria+biomes1+biomes2_3+biomes4+biomes5+biomes6+biomes7_9+biomes8+biomes10+biomes11+biomes12+biomes13+biomes14+harbor25+river25+lake25+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+rugg+lights+pop+urban+border_cell, data=data_small)
# mod.8.small <- lm(zeta~not_buffer_KM+factor(country)+altitude+temp+landsuit+malaria+biomes1+biomes2_3+biomes4+biomes5+biomes6+biomes7_9+biomes8+biomes10+biomes11+biomes12+biomes13+biomes14+harbor25+river25+lake25+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+rugg+lights+pop+urban+border_cell, data=data_small)

#
# covariate_labels <- c("KM of Colonial Rails for Military Purposes", "KM of Colonial Rails for Mining Purposes")


# Preamble
###################
# IV stuff
# opt_loc <- opt_loc[opt_loc$isnode ==0,]
# opt_loc$smaller_20 <- opt_loc$dist2rail <= 20
# opt_loc$emst_40 <- as.numeric(opt_loc$dist2emst <= 40)
# opt_loc$rail_dummy <- as.numeric(opt_loc$RailKM > 0)
#
# mod.1 <- lm(zeta~smaller_20+factor(country)+altitude+temp+landsuit+malaria+biomes1+biomes2_3+biomes4+biomes5+biomes6+biomes7_9+biomes8+biomes10+biomes11+biomes12+biomes13+biomes14+harbor25+river25+lake25+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+rugg+lights+pop+border_cell, data=opt_loc)
# mod.2 <- lm(zeta~RailKM+RailKM*emst_40+emst_40+factor(country)+altitude+temp+landsuit+malaria+biomes1+biomes2_3+biomes4+biomes5+biomes6+biomes7_9+biomes8+biomes10+biomes11+biomes12+biomes13+biomes14+harbor25+river25+lake25+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+rugg+lights+pop+border_cell, data=opt_loc)

# mod.IV <- ivreg(zeta~smaller_20+factor(country)+altitude+temp+landsuit+malaria+biomes1+biomes2_3+biomes4+biomes5+biomes6+biomes7_9+biomes8+biomes10+biomes11+biomes12+biomes13+biomes14+harbor25+river25+lake25+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+border_cell+rugg+lights+pop | . - smaller_20 + emst_40, data=opt_loc)

# # # all of this does not work. My idea being maybe that being close to the emst somehow indicates that this is a smartly positioned railway. Those will obviously not be re-arranged?
#
#
# covariate_labels <- c("KM of Colonial Rails for Military Purposes")
# End Preamble

# Preamble
#########################################
# OLD // ETHNIC POWER RELATIONS // OLD!!!!
#########################################

# mod.1 <- lm(zeta~discriminated+factor(country)+altitude+temp+landsuit+malaria+biomes1+biomes2_3+biomes4+biomes5+biomes6+biomes7_9+biomes8+biomes10+biomes11+biomes12+biomes13+biomes14+harbor25+river25+lake25+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+rugg+lights+pop+urban+border_cell, data=opt_loc)
# mod.2 <- lm(zeta~monopoly+factor(country)+altitude+temp+landsuit+malaria+biomes1+biomes2_3+biomes4+biomes5+biomes6+biomes7_9+biomes8+biomes10+biomes11+biomes12+biomes13+biomes14+harbor25+river25+lake25+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+rugg+lights+pop+urban+border_cell, data=opt_loc)
# mod.3 <- lm(zeta~hist_discr+factor(country)+altitude+temp+landsuit+malaria+biomes1+biomes2_3+biomes4+biomes5+biomes6+biomes7_9+biomes8+biomes10+biomes11+biomes12+biomes13+biomes14+harbor25+river25+lake25+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+rugg+lights+pop+urban+border_cell, data=opt_loc)
# mod.4 <- lm(zeta~hist_monop+factor(country)+altitude+temp+landsuit+malaria+biomes1+biomes2_3+biomes4+biomes5+biomes6+biomes7_9+biomes8+biomes10+biomes11+biomes12+biomes13+biomes14+harbor25+river25+lake25+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+rugg+lights+pop+urban+border_cell, data=opt_loc)
# End Preamble

#########################################
# AID WORLDBANK
#########################################

# opt_loc$wb_dis_compl <- log(opt_loc$wb_dis_compl+0.01)
# opt_loc$wb_dis_transp_compl <- log(opt_loc$wb_dis_transp_compl+0.01)


# mod.1a <- lm(zeta~wb_dis_compl+factor(country)+altitude+temp+landsuit+malaria+biomes1+biomes2_3+biomes4+biomes5+biomes6+biomes7_9+biomes8+biomes10+biomes11+biomes12+biomes13+biomes14+harbor25+river25+lake25+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+border_cell, data=opt_loc)
# mod.1b <- lm(zeta~wb_dis_compl+factor(country)+altitude+temp+landsuit+malaria+biomes1+biomes2_3+biomes4+biomes5+biomes6+biomes7_9+biomes8+biomes10+biomes11+biomes12+biomes13+biomes14+harbor25+river25+lake25+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+rugg+lights+pop+urban+border_cell, data=opt_loc)
# mod.2a <- lm(zeta~wb_dis_transp_compl+factor(country)+altitude+temp+landsuit+malaria+biomes1+biomes2_3+biomes4+biomes5+biomes6+biomes7_9+biomes8+biomes10+biomes11+biomes12+biomes13+biomes14+harbor25+river25+lake25+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+border_cell, data=opt_loc)
# mod.2b <- lm(zeta~wb_dis_transp_compl+factor(country)+altitude+temp+landsuit+malaria+biomes1+biomes2_3+biomes4+biomes5+biomes6+biomes7_9+biomes8+biomes10+biomes11+biomes12+biomes13+biomes14+harbor25+river25+lake25+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+rugg+lights+pop+urban+border_cell, data=opt_loc)
# mod.3a <- lm(zeta~wb_num_compl+factor(country)+altitude+temp+landsuit+malaria+biomes1+biomes2_3+biomes4+biomes5+biomes6+biomes7_9+biomes8+biomes10+biomes11+biomes12+biomes13+biomes14+harbor25+river25+lake25+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+border_cell, data=opt_loc)
# mod.3b <- lm(zeta~wb_num_compl+factor(country)+altitude+temp+landsuit+malaria+biomes1+biomes2_3+biomes4+biomes5+biomes6+biomes7_9+biomes8+biomes10+biomes11+biomes12+biomes13+biomes14+harbor25+river25+lake25+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+rugg+lights+pop+urban+border_cell, data=opt_loc)
# mod.4a <- lm(zeta~wb_num_transp_compl+factor(country)+altitude+temp+landsuit+malaria+biomes1+biomes2_3+biomes4+biomes5+biomes6+biomes7_9+biomes8+biomes10+biomes11+biomes12+biomes13+biomes14+harbor25+river25+lake25+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+border_cell, data=opt_loc)
# mod.4b <- lm(zeta~wb_num_transp_compl+factor(country)+altitude+temp+landsuit+malaria+biomes1+biomes2_3+biomes4+biomes5+biomes6+biomes7_9+biomes8+biomes10+biomes11+biomes12+biomes13+biomes14+harbor25+river25+lake25+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+rugg+lights+pop+urban+border_cell, data=opt_loc)

# Pre 2002 transport projects
###

# mod.1a <- lm(zeta~wb_dis_old+factor(country)+altitude+temp+landsuit+malaria+biomes1+biomes2_3+biomes4+biomes5+biomes6+biomes7_9+biomes8+biomes10+biomes11+biomes12+biomes13+biomes14+harbor25+river25+lake25+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+border_cell, data=opt_loc)
# mod.1b <- lm(zeta~wb_dis_old+factor(country)+altitude+temp+landsuit+malaria+biomes1+biomes2_3+biomes4+biomes5+biomes6+biomes7_9+biomes8+biomes10+biomes11+biomes12+biomes13+biomes14+harbor25+river25+lake25+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+rugg+lights+pop+urban+border_cell, data=opt_loc)
# mod.2a <- lm(zeta~wb_dis_transp_old+factor(country)+altitude+temp+landsuit+malaria+biomes1+biomes2_3+biomes4+biomes5+biomes6+biomes7_9+biomes8+biomes10+biomes11+biomes12+biomes13+biomes14+harbor25+river25+lake25+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+border_cell, data=opt_loc)
# mod.2b <- lm(zeta~wb_dis_transp_old+factor(country)+altitude+temp+landsuit+malaria+biomes1+biomes2_3+biomes4+biomes5+biomes6+biomes7_9+biomes8+biomes10+biomes11+biomes12+biomes13+biomes14+harbor25+river25+lake25+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+rugg+lights+pop+urban+border_cell, data=opt_loc)
# mod.3a <- lm(zeta~wb_num_old+factor(country)+altitude+temp+landsuit+malaria+biomes1+biomes2_3+biomes4+biomes5+biomes6+biomes7_9+biomes8+biomes10+biomes11+biomes12+biomes13+biomes14+harbor25+river25+lake25+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+border_cell, data=opt_loc)
# mod.3b <- lm(zeta~wb_num_old+factor(country)+altitude+temp+landsuit+malaria+biomes1+biomes2_3+biomes4+biomes5+biomes6+biomes7_9+biomes8+biomes10+biomes11+biomes12+biomes13+biomes14+harbor25+river25+lake25+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+rugg+lights+pop+urban+border_cell, data=opt_loc)
# mod.4a <- lm(zeta~wb_num_transp_old+factor(country)+altitude+temp+landsuit+malaria+biomes1+biomes2_3+biomes4+biomes5+biomes6+biomes7_9+biomes8+biomes10+biomes11+biomes12+biomes13+biomes14+harbor25+river25+lake25+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+border_cell, data=opt_loc)
# mod.4b <- lm(zeta~wb_num_transp_old+factor(country)+altitude+temp+landsuit+malaria+biomes1+biomes2_3+biomes4+biomes5+biomes6+biomes7_9+biomes8+biomes10+biomes11+biomes12+biomes13+biomes14+harbor25+river25+lake25+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+rugg+lights+pop+urban+border_cell, data=opt_loc)


#########################################
# AID CHINA
#########################################

# mod.1a <- lm(zeta~china_dis_compl+factor(country)+altitude+temp+landsuit+malaria+biomes1+biomes2_3+biomes4+biomes5+biomes6+biomes7_9+biomes8+biomes10+biomes11+biomes12+biomes13+biomes14+harbor25+river25+lake25+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+border_cell, data=opt_loc)
# mod.1b <- lm(zeta~china_dis_compl+factor(country)+altitude+temp+landsuit+malaria+biomes1+biomes2_3+biomes4+biomes5+biomes6+biomes7_9+biomes8+biomes10+biomes11+biomes12+biomes13+biomes14+harbor25+river25+lake25+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+rugg+lights+pop+urban+border_cell, data=opt_loc)
# mod.2a <- lm(zeta~china_dis_transp_compl+factor(country)+altitude+temp+landsuit+malaria+biomes1+biomes2_3+biomes4+biomes5+biomes6+biomes7_9+biomes8+biomes10+biomes11+biomes12+biomes13+biomes14+harbor25+river25+lake25+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+border_cell, data=opt_loc)
# mod.2b <- lm(zeta~china_dis_transp_compl+factor(country)+altitude+temp+landsuit+malaria+biomes1+biomes2_3+biomes4+biomes5+biomes6+biomes7_9+biomes8+biomes10+biomes11+biomes12+biomes13+biomes14+harbor25+river25+lake25+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+rugg+lights+pop+urban+border_cell, data=opt_loc)
# mod.3a <- lm(zeta~china_num_compl+factor(country)+altitude+temp+landsuit+malaria+biomes1+biomes2_3+biomes4+biomes5+biomes6+biomes7_9+biomes8+biomes10+biomes11+biomes12+biomes13+biomes14+harbor25+river25+lake25+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+border_cell, data=opt_loc)
# mod.3b <- lm(zeta~china_num_compl+factor(country)+altitude+temp+landsuit+malaria+biomes1+biomes2_3+biomes4+biomes5+biomes6+biomes7_9+biomes8+biomes10+biomes11+biomes12+biomes13+biomes14+harbor25+river25+lake25+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+rugg+lights+pop+urban+border_cell, data=opt_loc)
# mod.4a <- lm(zeta~china_num_transp_compl+factor(country)+altitude+temp+landsuit+malaria+biomes1+biomes2_3+biomes4+biomes5+biomes6+biomes7_9+biomes8+biomes10+biomes11+biomes12+biomes13+biomes14+harbor25+river25+lake25+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+border_cell, data=opt_loc)
# mod.4b <- lm(zeta~china_num_transp_compl+factor(country)+altitude+temp+landsuit+malaria+biomes1+biomes2_3+biomes4+biomes5+biomes6+biomes7_9+biomes8+biomes10+biomes11+biomes12+biomes13+biomes14+harbor25+river25+lake25+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+rugg+lights+pop+urban+border_cell, data=opt_loc)
#

####################
# AID Specifications
####################

#
#
# mod.1a.IV <- ivreg(zeta~wb_dis_compl+RailKM+factor(country)+altitude+temp+landsuit+malaria+biomes1+biomes2_3+biomes4+biomes5+biomes6+biomes7_9+biomes8+biomes10+biomes11+biomes12+biomes13+biomes14+harbor25+river25+lake25+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+border_cell | . -wb_dis_compl + is_mission, data=opt_loc)
# mod.1b.IV <- ivreg(zeta~wb_dis_compl+RailKM+factor(country)+altitude+temp+landsuit+malaria+biomes1+biomes2_3+biomes4+biomes5+biomes6+biomes7_9+biomes8+biomes10+biomes11+biomes12+biomes13+biomes14+harbor25+river25+lake25+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+rugg+lights+pop+urban+border_cell | . -wb_dis_compl + is_mission, data=opt_loc)
# mod.2a.IV <- ivreg(zeta~wb_dis_transp_compl+RailKM+factor(country)+altitude+temp+landsuit+malaria+biomes1+biomes2_3+biomes4+biomes5+biomes6+biomes7_9+biomes8+biomes10+biomes11+biomes12+biomes13+biomes14+harbor25+river25+lake25+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+border_cell | . -wb_dis_transp_compl + is_mission, data=opt_loc)
# mod.2b.IV <- ivreg(zeta~wb_dis_transp_compl+RailKM+factor(country)+altitude+temp+landsuit+malaria+biomes1+biomes2_3+biomes4+biomes5+biomes6+biomes7_9+biomes8+biomes10+biomes11+biomes12+biomes13+biomes14+harbor25+river25+lake25+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+rugg+lights+pop+urban+border_cell | . -wb_dis_transp_compl + is_mission, data=opt_loc)
# mod.3a.IV <- ivreg(zeta~wb_num_compl+RailKM+factor(country)+altitude+temp+landsuit+malaria+biomes1+biomes2_3+biomes4+biomes5+biomes6+biomes7_9+biomes8+biomes10+biomes11+biomes12+biomes13+biomes14+harbor25+river25+lake25+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+border_cell | . -wb_num_compl + is_mission, data=opt_loc)
# mod.3b.IV <- ivreg(zeta~wb_num_compl+RailKM+factor(country)+altitude+temp+landsuit+malaria+biomes1+biomes2_3+biomes4+biomes5+biomes6+biomes7_9+biomes8+biomes10+biomes11+biomes12+biomes13+biomes14+harbor25+river25+lake25+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+rugg+lights+pop+urban+border_cell | . -wb_num_compl + is_mission, data=opt_loc)
# mod.4a.IV <- ivreg(zeta~wb_num_transp_compl+RailKM+factor(country)+altitude+temp+landsuit+malaria+biomes1+biomes2_3+biomes4+biomes5+biomes6+biomes7_9+biomes8+biomes10+biomes11+biomes12+biomes13+biomes14+harbor25+river25+lake25+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+border_cell | . -wb_num_transp_compl + is_mission, data=opt_loc)
# mod.4b.IV <- ivreg(zeta~wb_num_transp_compl+RailKM+factor(country)+altitude+temp+landsuit+malaria+biomes1+biomes2_3+biomes4+biomes5+biomes6+biomes7_9+biomes8+biomes10+biomes11+biomes12+biomes13+biomes14+harbor25+river25+lake25+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+rugg+lights+pop+urban+border_cell | . -wb_num_transp_compl + is_mission, data=opt_loc)


# first stages
#
# mod.1a <- lm(wb_dis_compl~is_mission+factor(country)+RailKM+altitude+temp+landsuit+malaria+biomes1+biomes2_3+biomes4+biomes5+biomes6+biomes7_9+biomes8+biomes10+biomes11+biomes12+biomes13+biomes14+harbor25+river25+lake25+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+border_cell, data=opt_loc)
# mod.1b <- lm(wb_dis_compl~is_mission+factor(country)+RailKM+altitude+temp+landsuit+malaria+biomes1+biomes2_3+biomes4+biomes5+biomes6+biomes7_9+biomes8+biomes10+biomes11+biomes12+biomes13+biomes14+harbor25+river25+lake25+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+rugg+lights+pop+urban+border_cell, data=opt_loc)
# mod.2a <- lm(wb_dis_transp_compl~is_mission+factor(country)+RailKM+altitude+temp+landsuit+malaria+biomes1+biomes2_3+biomes4+biomes5+biomes6+biomes7_9+biomes8+biomes10+biomes11+biomes12+biomes13+biomes14+harbor25+river25+lake25+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+border_cell, data=opt_loc)
# mod.2b <- lm(wb_dis_transp_compl~is_mission+factor(country)+RailKM+altitude+temp+landsuit+malaria+biomes1+biomes2_3+biomes4+biomes5+biomes6+biomes7_9+biomes8+biomes10+biomes11+biomes12+biomes13+biomes14+harbor25+river25+lake25+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+rugg+lights+pop+urban+border_cell, data=opt_loc)
# mod.3a <- lm(wb_num_compl~is_mission+factor(country)+RailKM+altitude+temp+landsuit+malaria+biomes1+biomes2_3+biomes4+biomes5+biomes6+biomes7_9+biomes8+biomes10+biomes11+biomes12+biomes13+biomes14+harbor25+river25+lake25+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+border_cell, data=opt_loc)
# mod.3b <- lm(wb_num_compl~is_mission+factor(country)+RailKM+altitude+temp+landsuit+malaria+biomes1+biomes2_3+biomes4+biomes5+biomes6+biomes7_9+biomes8+biomes10+biomes11+biomes12+biomes13+biomes14+harbor25+river25+lake25+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+rugg+lights+pop+urban+border_cell, data=opt_loc)
# mod.4a <- lm(wb_num_transp_compl~is_mission+factor(country)+RailKM+altitude+temp+landsuit+malaria+biomes1+biomes2_3+biomes4+biomes5+biomes6+biomes7_9+biomes8+biomes10+biomes11+biomes12+biomes13+biomes14+harbor25+river25+lake25+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+border_cell, data=opt_loc)
# mod.4b <- lm(wb_num_transp_compl~is_mission+factor(country)+RailKM+altitude+temp+landsuit+malaria+biomes1+biomes2_3+biomes4+biomes5+biomes6+biomes7_9+biomes8+biomes10+biomes11+biomes12+biomes13+biomes14+harbor25+river25+lake25+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+rugg+lights+pop+urban+border_cell, data=opt_loc)


#######################################################
#### Display results
######################################################

mo_list = list() # this puts all models in a list for stargazer
for(i in ls(pattern="mod.")){
  if(!grepl("IV", i)){
  mo_list[[paste(i)]] <- get(i)
} else{
  mo_list[[paste(i)]] <- cluster.robust.se(get(i), opt_loc$cluster)
  #mo_list[[paste(i)]] <- get(i)
}
}

se_list = list() # this puts the respective White standard errors in a list for stargazer
for(i in ls(pattern="mod.")){
  current <- get(i)
  if(!grepl("small", i)){
  #se_list[[paste(i)]] <- sqrt(diag(vcovHC(current, "HC1"))) # this is HAC-robust
  if(grepl("IV", i)){
    se_list[[paste(i)]] <- cluster.robust.se(current, opt_loc$cluster)[,2] # this clusters and is HAC-robust
  } else{
      se_list[[paste(i)]] <- sqrt(diag(cluster.vcov(current, cluster=opt_loc$cluster))) # this clusters and is HAC-robust
    }
} else{
  if(grepl("IV", i)){
    se_list[[paste(i)]] <- cluster.robust.se(current, data_small$cluster)[,2] # this clusters and is HAC-robust
  } else{
      se_list[[paste(i)]] <- sqrt(diag(cluster.vcov(current, cluster=data_small$cluster))) # this clusters and is HAC-robust
    }
}
}

country <- "Country FE" # this puts the respective Controls in a list for stargazer
geog <- "Geographic controls"
sim_controls <- "Simulation controls"
keeplist <- vector()
keeplist <- c(keeplist, c("wb", "china"))

for(i in ls(pattern="mod.")){
  current <- get(i)
  if(any(grepl("factor", names(current$coefficients)))){
    country <- c(country, "Yes")
  } else{
    country <- c(country, "")
  }

  if("malaria" %in% names(current$coefficients)){
    geog <- c(geog, "Yes")
  } else{
    geog <- c(geog, "")
  }
  if("lights" %in% names(current$coefficients)){
    sim_controls <- c(sim_controls, "Yes")
  } else{
    sim_controls <- c(sim_controls, "")
  }
  keeplist <- c(keeplist, names(current$coefficients)[2])
}
control_list = list(country, geog, sim_controls)
control_list_app <- list(country)

if(!grepl("APP", names(mo_list))){
  stargazer(mo_list, se=se_list, type="latex", keep = keeplist, p.auto=TRUE, t.auto=TRUE, add.lines=control_list, keep.stat=c("rsq", "n"), dep.var.labels.include=F)
} else{
  stargazer(mo_list, se=se_list, type="latex", p.auto=TRUE, t.auto=TRUE, add.lines=control_list_app, omit=c("factor", "Constant"), keep.stat=c("rsq", "n"), dep.var.labels.include=F)
}
