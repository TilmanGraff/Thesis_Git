
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

opt_loc <- merge(opt_loc, borders, by=c("country", "rownumber"))
opt_loc <- merge(opt_loc, aid, by="ID")
opt_loc <- merge(opt_loc, epr, by="ID")
opt_loc <- merge(opt_loc, buffer, by="ID")


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

opt_loc$worldbank_other_disbursements <- opt_loc$worldbank_disbursements-opt_loc$worldbank_transport_disbursements
opt_loc$any_disbursements <- as.numeric(opt_loc$worldbank_disbursements>0)
opt_loc$placebo_project <- as.numeric(opt_loc$worldbank_disbursements==0 & opt_loc$worldbank_commitments!=0 )

opt_loc$status <- factor(opt_loc$status, levels=c("DISCRIMINATED", "POWERLESS", "IRRELEVANT", "JUNIOR PARTNER", "SENIOR PARTNER", "DOMINANT", "MONOPOLY"))
opt_loc$epr_scale <- as.numeric(opt_loc$status)
opt_loc$discriminated <- as.numeric(opt_loc$epr_scale == 1)
opt_loc$participation <- as.numeric(opt_loc$epr_scale > 3)
opt_loc$monopoly <- as.numeric(opt_loc$epr_scale == 7)

opt_loc$not_buffer_KM <- opt_loc$RailKM - opt_loc$bufferKM

#########################################
# RAILROADS
#########################################

###################
# RailKM
#
# mod.1 <- lm(zeta~RailKM, data=opt_loc)
# mod.2 <- lm(zeta~RailKM+factor(country), data=opt_loc)
# mod.3 <- lm(zeta~RailKM+factor(country)+altitude+temp+landsuit+malaria+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+border, data=opt_loc)
# mod.4 <- lm(zeta~RailKM+factor(country)+altitude+temp+landsuit+malaria+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+rugg+lights+pop+urban+border, data=opt_loc)

# mod.5 <- lm(zeta~PlaceboKM, data=opt_loc)
# mod.6 <- lm(zeta~PlaceboKM+factor(country), data=opt_loc)
# mod.7 <- lm(zeta~PlaceboKM+factor(country)+altitude+temp+landsuit+malaria+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+border, data=opt_loc)
# mod.8 <- lm(zeta~PlaceboKM+factor(country)+altitude+temp+landsuit+malaria+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+rugg+lights+pop+urban+border, data=opt_loc)
# #
# covariate_labels <- c("KM of Colonial Railroads", "KM of Colonial Placebo Railroads")


###################
# Distance blocks

#
# data_small <- opt_loc[opt_loc$country %in% jedwab_countries_via_all_rails,]
#
# #mod.1 <- lm(zeta~smaller_ten+ten_twenty+twenty_thirty+thirty_fourty, data=opt_loc)
# mod.2 <- lm(zeta~smaller_ten+ten_twenty+twenty_thirty+thirty_fourty+factor(country), data=opt_loc)
# mod.3 <- lm(zeta~smaller_ten+ten_twenty+twenty_thirty+thirty_fourty+factor(country)+altitude+temp+landsuit+malaria+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+border, data=opt_loc)
# mod.4 <- lm(zeta~smaller_ten+ten_twenty+twenty_thirty+thirty_fourty+factor(country)+altitude+temp+landsuit+malaria+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+rugg+lights+urban+pop+border, data=opt_loc)
#
# #mod.5 <- lm(zeta~smaller_ten_p+ten_twenty_p+twenty_thirty_p+thirty_fourty_p, data=opt_loc)
# mod.6 <- lm(zeta~smaller_ten_p+ten_twenty_p+twenty_thirty_p+thirty_fourty_p+factor(country), data=opt_loc)
# mod.7 <- lm(zeta~smaller_ten_p+ten_twenty_p+twenty_thirty_p+thirty_fourty_p+factor(country)+altitude+temp+landsuit+malaria+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+border, data=opt_loc)
# mod.8 <- lm(zeta~smaller_ten_p+ten_twenty_p+twenty_thirty_p+thirty_fourty_p+factor(country)+altitude+temp+landsuit+malaria+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+rugg+lights+urban+pop+border, data=opt_loc)
#
# mod.9.1.small <- lm(zeta~smaller_ten+ten_twenty+twenty_thirty+thirty_fourty+factor(country)+altitude+temp+landsuit+malaria+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+rugg+lights+urban+pop+border_cell, data=data_small)
# mod.9.2.small <- lm(zeta~smaller_ten_p+ten_twenty_p+twenty_thirty_p+thirty_fourty_p+factor(country)+altitude+temp+landsuit+malaria+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+rugg+lights+urban+pop+border, data=data_small)
#
# covariate_labels <- c("$<10$ KM to Colonial Railroad", "$10-20$ KM to Colonial Railroad", "$20-30$ KM to Colonial Railroad", "$30-40$ KM to Colonial Railroad", "$<10$ KM to Colonial Placebo Railroad", "$10-20$ KM to Colonial Placebo Railroad", "$20-30$ KM to Colonial Placebo Railroad", "$30-40$ KM to Colonial Placebo Railroad")
#

###################
# log_distance

# mod.1 <- lm(zeta~(dist2rail), data=opt_loc)
# mod.2 <- lm(zeta~(dist2rail)+factor(country), data=opt_loc)
# mod.3 <- lm(zeta~(dist2rail)+factor(country)+altitude+temp+landsuit+malaria+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4, data=opt_loc)
#
# mod.4 <- lm(zeta~(dist2placebo), data=opt_loc)
# mod.5 <- lm(zeta~(dist2placebo)+factor(country), data=opt_loc)
# mod.6 <- lm(zeta~(dist2placebo)+factor(country)+altitude+temp+landsuit+malaria+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4, data=opt_loc)


###################
# Heterogeneous Rails
data_small <- opt_loc[opt_loc$isnode ==0,]
#
mod.1 <- lm(zeta~RailKM_military+factor(country)+altitude+temp+landsuit+malaria+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+border_cell, data=opt_loc)
mod.2 <- lm(zeta~RailKM_military+factor(country)+altitude+temp+landsuit+malaria+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+rugg+lights+pop+border_cell, data=opt_loc)

mod.3 <- lm(zeta~RailKM_mining+factor(country)+altitude+temp+landsuit+malaria+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+border_cell, data=opt_loc)
mod.4 <- lm(zeta~RailKM_mining+factor(country)+altitude+temp+landsuit+malaria+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+rugg+lights+pop+border_cell, data=opt_loc)

mod.5 <- lm(zeta~RailKM_military+RailKM_mining+factor(country)+altitude+temp+landsuit+malaria+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+border_cell, data=opt_loc)
mod.6 <- lm(zeta~RailKM_military+RailKM_mining+factor(country)+altitude+temp+landsuit+malaria+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+rugg+lights+pop+border_cell, data=opt_loc)
mod.7.small <- lm(zeta~bufferKM+factor(country)+altitude+temp+landsuit+malaria+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+rugg+lights+pop+urban+border_cell, data=data_small)
mod.8.small <- lm(zeta~not_buffer_KM+factor(country)+altitude+temp+landsuit+malaria+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+rugg+lights+pop+urban+border_cell, data=data_small)

#
# covariate_labels <- c("KM of Colonial Rails for Military Purposes", "KM of Colonial Rails for Mining Purposes")


###################
# BufferKM
# opt_loc <- opt_loc[opt_loc$isnode ==0,]
# mod.1 <- lm(zeta~bufferKM+not_buffer_KM, data=opt_loc)
# mod.2 <- lm(zeta~bufferKM+not_buffer_KM+factor(country), data=opt_loc)
# mod.3 <- lm(zeta~bufferKM+not_buffer_KM+factor(country)+altitude+temp+landsuit+malaria+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+border, data=opt_loc)
# mod.4 <- lm(zeta~bufferKM+not_buffer_KM+factor(country)+altitude+temp+landsuit+malaria+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+rugg+lights+pop+urban+border, data=opt_loc)


###################
# IV stuff
# opt_loc <- opt_loc[opt_loc$isnode ==0,]
# opt_loc$smaller_20 <- opt_loc$dist2rail <= 20
# opt_loc$emst_40 <- as.numeric(opt_loc$dist2emst <= 40)
# opt_loc$rail_dummy <- as.numeric(opt_loc$RailKM > 0)
#
# mod.1 <- lm(zeta~smaller_20+factor(country)+altitude+temp+landsuit+malaria+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+rugg+lights+pop+border_cell, data=opt_loc)
# mod.2 <- lm(zeta~RailKM+RailKM*emst_40+emst_40+factor(country)+altitude+temp+landsuit+malaria+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+rugg+lights+pop+border_cell, data=opt_loc)

# mod.IV <- ivreg(zeta~smaller_20+factor(country)+altitude+temp+landsuit+malaria+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+border_cell+rugg+lights+pop | . - smaller_20 + emst_40, data=opt_loc)

# # # all of this does not work. My idea being maybe that being close to the emst somehow indicates that this is a smartly positioned railway. Those will obviously not be re-arranged?
#
#
# covariate_labels <- c("KM of Colonial Rails for Military Purposes")

#########################################
# ETHNIC POWER RELATIONS
#########################################
#
# mod.1 <- lm(zeta~participation+factor(country)+altitude+temp+landsuit+malaria+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+rugg+lights+pop+urban+border_cell, data=opt_loc)
# mod.2 <- lm(zeta~discriminated+factor(country)+altitude+temp+landsuit+malaria+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+rugg+lights+pop+urban+border_cell, data=opt_loc)
# mod.3 <- lm(zeta~monopoly+factor(country)+altitude+temp+landsuit+malaria+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+rugg+lights+pop+urban+border_cell, data=opt_loc)
# mod.4 <- lm(zeta~epr_scale+factor(country)+altitude+temp+landsuit+malaria+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+rugg+lights+pop+urban+border_cell, data=opt_loc)

#########################################
# AID
#########################################

# mod.1 <- lm(zeta~worldbank_disbursements+factor(country)+altitude+temp+landsuit+malaria+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+rugg+lights+pop+urban+border_cell, data=opt_loc)
# mod.2 <- lm(zeta~worldbank_transport_disbursements+factor(country)+altitude+temp+landsuit+malaria+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+rugg+lights+pop+urban+border_cell, data=opt_loc)
# mod.3 <- lm(zeta~worldbank_transport_disbursements_completed+factor(country)+altitude+temp+landsuit+malaria+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+rugg+lights+pop+urban+border_cell, data=opt_loc)
# mod.4 <- lm(zeta~worldbank_other_disbursements+worldbank_transport_disbursements_completed+factor(country)+altitude+temp+landsuit+malaria+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+rugg+lights+pop+urban+border_cell, data=opt_loc)
# mod.5 <- lm(zeta~any_disbursements+factor(country)+altitude+temp+landsuit+malaria+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+rugg+lights+pop+urban+border_cell, data=opt_loc)
# mod.6 <- lm(zeta~placebo_project+factor(country)+altitude+temp+landsuit+malaria+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+rugg+lights+pop+urban+border_cell, data=opt_loc)
# mod.7 <- lm(zeta~number_wb_projects+factor(country)+altitude+temp+landsuit+malaria+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+rugg+lights+pop+urban+border_cell, data=opt_loc)

# mod.5 <- lm(worldbank_disbursements~zeta+factor(country)+altitude+temp+landsuit+malaria+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+rugg+lights+pop+urban+border_cell, data=opt_loc)
# mod.6 <- lm(worldbank_transport_disbursements~zeta+factor(country)+altitude+temp+landsuit+malaria+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+rugg+lights+pop+urban+border_cell, data=opt_loc)
# mod.7 <- lm(worldbank_transport_disbursements_completed~zeta+factor(country)+altitude+temp+landsuit+malaria+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+rugg+lights+pop+urban+border_cell, data=opt_loc)
# #

###################
# AID IV stuff
#
# mod.5 <- lm(worldbank_disbursements~zeta+factor(country)+altitude+temp+landsuit+malaria+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+rugg+lights+pop+urban+border_cell, data=opt_loc)
# mod.6 <- lm(worldbank_transport_disbursements~zeta+factor(country)+altitude+temp+landsuit+malaria+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+rugg+lights+pop+urban+border_cell, data=opt_loc)
# mod.7 <- lm(worldbank_transport_disbursements_completed~zeta+factor(country)+altitude+temp+landsuit+malaria+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+rugg+lights+pop+urban+border_cell, data=opt_loc)
#
# mod.IV1 <- ivreg(worldbank_disbursements~zeta+factor(country)+altitude+temp+landsuit+malaria+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+rugg+lights+pop+urban+border_cell | . - zeta + smaller_ten, data=opt_loc)
# mod.IV2 <- ivreg(worldbank_transport_disbursements~zeta+factor(country)+altitude+temp+landsuit+malaria+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+rugg+lights+pop+urban+border_cell | . - zeta + smaller_ten, data=opt_loc)
# mod.IV3 <- ivreg(worldbank_transport_disbursements_completed~zeta+factor(country)+altitude+temp+landsuit+malaria+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+rugg+lights+pop+urban+border_cell | . - zeta + smaller_ten, data=opt_loc)

#######################################################
#### Display results
######################################################

mo_list = list() # this puts all models in a list for stargazer
for(i in ls(pattern="mod.")){
  if(!grepl("IV", i)){
  mo_list[[paste(i)]] <- get(i)
} else{
  mo_list[[paste(i)]] <- cluster.robust.se(get(i), opt_loc$cluster)
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

}
control_list = list(country, geog, sim_controls)


stargazer(mo_list, se=se_list, type="latex",  keep = c("buffer", "participation", "monopoly", "discriminated", "epr", "zeta","worldbank", "wb", "disbursement","ail", "placebo", "KM", "ten", "thirty", "smaller", "emst"), p.auto=TRUE, t.auto=TRUE, add.lines=control_list, keep.stat=c("rsq", "n"), order=c(1,3,4,2), dep.var.labels.include=F)
