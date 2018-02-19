
library("stargazer", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("sandwich", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("lmtest", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("multiwayvcov", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("AER", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("ivpack", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")

rm(list=ls(pattern="mod")) # this removes all models from the RStudio environment

opt_loc <- read.csv("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/temp/opt_loc_with_raildist.csv")
borders <- read.csv("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/temp/borders.csv")

opt_loc <- merge(opt_loc, borders, by=c("country", "rownumber"))

jedwab_countries <- c("Angola", "Benin", "Guinea-Bissau", "Botswana", "Burkina-Faso", "Burundi", "Cameroon", "Central-African-Republic", "Chad", "Congo", "Democratic-Republic-of-the-Congo", "Djibouti", "Equatorial-Guinea", "Eritrea", "Ethiopia", "Gabon", "Gambia", "Ghana", "Guinea", "Cote-dIvoire", "Kenya", "Liberia", "Malawi", "Mali", "Mauritania", "Mozambique", "Namibia", "Niger", "Nigeria", "Rwanda", "Senegal", "Sierra-Leone", "Somalia", "Sudan", "South-Sudan", "United-Republic-of-Tanzania", "Togo", "Uganda", "Zambia", "Zimbabwe", "South-Africa")

jedwab_countries_via_all_rails <- c("Angola", "Benin", "Botswana", "Burkina-Faso", "Burundi", "Cameroon", "Congo", "Democratic-Republic-of-the-Congo", "Djibouti", "Equatorial-Guinea", "Eritrea", "Ethiopia", "Ghana", "Guinea", "Cote-dIvoire", "Kenya", "Liberia", "Malawi", "Mali", "Mozambique", "Namibia", "Nigeria", "Senegal", "Sierra-Leone", "Somalia", "Sudan", "South-Sudan", "United-Republic-of-Tanzania", "Togo", "Uganda", "Zambia", "Zimbabwe", "South-Africa")

opt_loc <- opt_loc[!is.na(opt_loc$RailKM) & !is.na(opt_loc$PlaceboKM) & !is.na(opt_loc$cluster),]
opt_loc <- opt_loc[opt_loc$country %in% jedwab_countries_via_all_rails,]
#opt_loc <- opt_loc[opt_loc$country != "South-Africa",]


opt_loc$x_2 <- opt_loc$x^2
opt_loc$x_3 <- opt_loc$x^3
opt_loc$x_4 <- opt_loc$x^4

opt_loc$y_2 <- opt_loc$y^2
opt_loc$y_3 <- opt_loc$y^3
opt_loc$y_4 <- opt_loc$y^4

opt_loc$border_cell <- opt_loc$border < 8

###################
# RailKM
#
# mod.1 <- lm(zeta~RailKM, data=opt_loc)
# mod.2 <- lm(zeta~RailKM+factor(country), data=opt_loc)
# mod.3 <- lm(zeta~RailKM+factor(country)+altitude+temp+landsuit+malaria+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+border, data=opt_loc)
# mod.4 <- lm(zeta~RailKM+factor(country)+altitude+temp+landsuit+malaria+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+rugg+lights+pop+urban+border, data=opt_loc)
#
# mod.5 <- lm(zeta~PlaceboKM, data=opt_loc)
# mod.6 <- lm(zeta~PlaceboKM+factor(country), data=opt_loc)
# mod.7 <- lm(zeta~PlaceboKM+factor(country)+altitude+temp+landsuit+malaria+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+border, data=opt_loc)
# mod.8 <- lm(zeta~PlaceboKM+factor(country)+altitude+temp+landsuit+malaria+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+rugg+lights+pop+urban+border, data=opt_loc)
#
# covariate_labels <- c("KM of Colonial Railroads", "KM of Colonial Placebo Railroads")


###################
# Distance blocks
# opt_loc$smaller_ten <- opt_loc$dist2rail <= 10
# opt_loc$ten_twenty <- opt_loc$dist2rail > 10 & opt_loc$dist2rail <= 20
# opt_loc$twenty_thirty <- opt_loc$dist2rail > 20 & opt_loc$dist2rail <= 30
# opt_loc$thirty_fourty <- opt_loc$dist2rail > 30 & opt_loc$dist2rail <= 40
#
# opt_loc$smaller_ten_p <- opt_loc$dist2placebo <= 10
# opt_loc$ten_twenty_p <- opt_loc$dist2placebo > 10 & opt_loc$dist2placebo <= 20
# opt_loc$twenty_thirty_p <- opt_loc$dist2placebo > 20 & opt_loc$dist2placebo <= 30
# opt_loc$thirty_fourty_p <- opt_loc$dist2placebo > 30 & opt_loc$dist2placebo <= 40
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
# mod.9.1.small <- lm(zeta~smaller_ten+ten_twenty+twenty_thirty+thirty_fourty+factor(country)+altitude+temp+landsuit+malaria+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+rugg+lights+urban+pop+border, data=data_small)
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
#
#
# mod.1 <- lm(zeta~RailKM_military+factor(country)+altitude+temp+landsuit+malaria+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+border, data=opt_loc)
# mod.2 <- lm(zeta~RailKM_military+factor(country)+altitude+temp+landsuit+malaria+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+rugg+lights+pop+border, data=opt_loc)
#
# mod.3 <- lm(zeta~RailKM_mining+factor(country)+altitude+temp+landsuit+malaria+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+border, data=opt_loc)
# mod.4 <- lm(zeta~RailKM_mining+factor(country)+altitude+temp+landsuit+malaria+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+rugg+lights+pop+border, data=opt_loc)
#
# mod.5 <- lm(zeta~RailKM_military+RailKM_mining+factor(country)+altitude+temp+landsuit+malaria+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+border, data=opt_loc)
# mod.6 <- lm(zeta~RailKM_military+RailKM_mining+factor(country)+altitude+temp+landsuit+malaria+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+rugg+lights+pop+border, data=opt_loc)
#
# covariate_labels <- c("KM of Colonial Rails for Military Purposes", "KM of Colonial Rails for Mining Purposes")


###################
# IV stuff
opt_loc <- opt_loc[opt_loc$isnode ==0,]
opt_loc$smaller_20 <- opt_loc$dist2rail <= 20
opt_loc$emst_40 <- opt_loc$dist2emst <= 100

mod.OLS <- lm(zeta~RailKM+factor(country)+altitude+temp+landsuit+malaria+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+rugg+lights+pop+border, data=opt_loc)
mod.IV <- ivreg(zeta~RailKM+factor(country)+altitude+temp+landsuit+malaria+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+border+rugg+lights+pop | . - RailKM + emst_40, data=opt_loc)

# # all of this does not work. My idea being maybe that being close to the emst somehow indicates that this is a smartly positioned railway. Those will obviously not be re-arranged?


covariate_labels <- c("KM of Colonial Rails for Military Purposes")

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


stargazer(mo_list, se=se_list, type="text",  keep = c("ail", "placebo", "KM", "ten", "thirty", "smaller, emst"), p.auto=TRUE, t.auto=TRUE, add.lines=control_list, keep.stat=c("rsq", "n"), covariate.labels = covariate_labels)
