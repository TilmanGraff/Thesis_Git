library("stargazer", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("sandwich", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("lmtest", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("multiwayvcov", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("AER", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("ivpack", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")

rm(list=ls(pattern="mod")) # this removes all models from the RStudio environment

epr <- read.csv("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/temp/mich_epr.csv")
acled <- read.csv("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/temp/mich_acled.csv")
prec <- read.csv("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/temp/mich_prec.csv")

colnames(epr)[colnames(epr)=="dis"] <- "discr"

opt_loc <- read.csv("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/temp/opt_loc_with_raildist.csv")
borders <- read.csv("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/temp/borders.csv")
aid <- read.csv(file="/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/temp/grid_ids_aid.csv")
buffer <- read.csv(file="/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/temp/ID_bufferKM.csv")
henderson_controls <- read.csv(file="/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/temp/henderson_controls.csv")
years_in_power <- read.csv("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/temp/ID_years_in_power.csv")
capitals <- read.csv("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/temp/ID_capitals.csv")
polity <- read.csv("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/temp/ID_polity.csv")

opt_loc <- merge(opt_loc, borders, by=c("country", "rownumber"))
opt_loc <- merge(opt_loc, aid, by="ID")
opt_loc <- merge(opt_loc, buffer, by="ID")
opt_loc <- merge(opt_loc, henderson_controls, by="ID")
opt_loc <- merge(opt_loc, years_in_power, by="ID")
opt_loc <- merge(opt_loc, capitals, by="ID")
opt_loc <- merge(opt_loc, polity, by="ID")

opt_loc$border_cell <- opt_loc$border < 8



#########################################
# EPR
#########################################
#
# data_small <- epr
# active_data_set <- acled
# active_data_set$ever_in_power <- as.numeric(active_data_set$years_in_power>0)
#
# #
# # active_data_set <- active_data_set[active_data_set$capital!=1,]
# # data_small <- data_small[data_small$capital!=1,]
#
# # data_small <- merge(epr, acled[,c("tuple_ID", "centr_tribe", "compact_complex_sett", "class")], by="tuple_ID")
# # data_small$centr_tribe1 <- data_small$centr_tribe
# # data_small$centr_tribe1[data_small$centr_tribe1==4] <- 3
# # data_small$decentralised <- data_small$centr_tribe < 2
# #
#
#
# mod.1a.small <- lm(zeta~discr+factor(wbcode)+altitude+temp+landsuit+malaria+biomes1+biomes2_3+biomes4+biomes5+biomes6+biomes7_9+biomes8+biomes10+biomes11+biomes12+biomes13+biomes14+growingdays+precip+seadist1+borderdist1+lnkm2split, data=data_small)
# mod.1b.small <- lm(zeta~discr+factor(wbcode)+altitude+temp+landsuit+malaria+biomes1+biomes2_3+biomes4+biomes5+biomes6+biomes7_9+biomes8+biomes10+biomes11+biomes12+biomes13+biomes14+growingdays+precip+seadist1+borderdist1+lnkm2split+lights+rugg+pop, data=data_small)
#
# mod.2a.small <- lm(zeta~ex+factor(wbcode)+altitude+temp+landsuit+malaria+biomes1+biomes2_3+biomes4+biomes5+biomes6+biomes7_9+biomes8+biomes10+biomes11+biomes12+biomes13+biomes14+growingdays+precip+seadist1+borderdist1+lnkm2split, data=data_small)
# mod.2b.small <- lm(zeta~ex+factor(wbcode)+altitude+temp+landsuit+malaria+biomes1+biomes2_3+biomes4+biomes5+biomes6+biomes7_9+biomes8+biomes10+biomes11+biomes12+biomes13+biomes14+growingdays+precip+seadist1+borderdist1+lnkm2split+lights+rugg+pop, data=data_small)
#
# mod.3a.small <- lm(zeta~etwar+factor(wbcode)+altitude+temp+landsuit+malaria+biomes1+biomes2_3+biomes4+biomes5+biomes6+biomes7_9+biomes8+biomes10+biomes11+biomes12+biomes13+biomes14+growingdays+precip+seadist1+borderdist1+lnkm2split, data=data_small)
# mod.3b.small <- lm(zeta~etwar+factor(wbcode)+altitude+temp+landsuit+malaria+biomes1+biomes2_3+biomes4+biomes5+biomes6+biomes7_9+biomes8+biomes10+biomes11+biomes12+biomes13+biomes14+growingdays+precip+seadist1+borderdist1+lnkm2split+lights+rugg+pop, data=data_small)
#
# mod.4a <- lm(zeta~split10pc+factor(wbcode)+altitude+temp+landsuit+malaria+biomes1+biomes2_3+biomes4+biomes5+biomes6+biomes7_9+biomes8+biomes10+biomes11+biomes12+biomes13+biomes14+growingdays+precip+seadist1+borderdist1+lnkm2split, data=active_data_set)
# mod.4b <- lm(zeta~split10pc+factor(wbcode)+altitude+temp+landsuit+malaria+biomes1+biomes2_3+biomes4+biomes5+biomes6+biomes7_9+biomes8+biomes10+biomes11+biomes12+biomes13+biomes14+growingdays+precip+seadist1+borderdist1+lnkm2split+lights+rugg+pop, data=active_data_set)

#########################################
# ACLED and Prec
#########################################
#
# active_data_set <- acled # has both prec and acled in it and more controls
# active_data_set$centr_tribe1 <- active_data_set$centr_tribe
# active_data_set$centr_tribe1[active_data_set$centr_tribe1==4] <- 3
# active_data_set$decentralised <- active_data_set$centr_tribe < 2
#
# # mod.1 <- lm(zeta~centr_tribe+factor(wbcode)+altitude+temp+landsuit+malaria+biomes1+biomes2_3+biomes4+biomes5+biomes6+biomes7_9+biomes8+biomes10+biomes11+biomes12+biomes13+biomes14+growingdays+precip+seadist1+borderdist1+lnkm2split, data=active_data_set)
# # mod.2 <- lm(zeta~centr_tribe+factor(wbcode)+altitude+temp+landsuit+malaria+biomes1+biomes2_3+biomes4+biomes5+biomes6+biomes7_9+biomes8+biomes10+biomes11+biomes12+biomes13+biomes14+growingdays+precip+seadist1+borderdist1+lnkm2split+lights+rugg+pop, data=active_data_set)
# #
# mod.3 <- lm(zeta~decentralised+factor(wbcode)+altitude+temp+landsuit+malaria+biomes1+biomes2_3+biomes4+biomes5+biomes6+biomes7_9+biomes8+biomes10+biomes11+biomes12+biomes13+biomes14+growingdays+precip+seadist1+borderdist1+lnkm2split, data=active_data_set)
# mod.4 <- lm(zeta~decentralised+factor(wbcode)+altitude+temp+landsuit+malaria+biomes1+biomes2_3+biomes4+biomes5+biomes6+biomes7_9+biomes8+biomes10+biomes11+biomes12+biomes13+biomes14+growingdays+precip+seadist1+borderdist1+lnkm2split+lights+rugg+pop, data=active_data_set)
# #
# mod.5 <- lm(zeta~class+factor(wbcode)+altitude+temp+landsuit+malaria+biomes1+biomes2_3+biomes4+biomes5+biomes6+biomes7_9+biomes8+biomes10+biomes11+biomes12+biomes13+biomes14+growingdays+precip+seadist1+borderdist1+lnkm2split, data=active_data_set)
# mod.6 <- lm(zeta~class+factor(wbcode)+altitude+temp+landsuit+malaria+biomes1+biomes2_3+biomes4+biomes5+biomes6+biomes7_9+biomes8+biomes10+biomes11+biomes12+biomes13+biomes14+growingdays+precip+seadist1+borderdist1+lnkm2split+lights+rugg+pop, data=active_data_set)
#
# mod.7 <- lm(zeta~compact_complex_sett+factor(wbcode)+altitude+temp+landsuit+malaria+biomes1+biomes2_3+biomes4+biomes5+biomes6+biomes7_9+biomes8+biomes10+biomes11+biomes12+biomes13+biomes14+growingdays+precip+seadist1+borderdist1+lnkm2split, data=active_data_set)
# mod.8 <- lm(zeta~compact_complex_sett+factor(wbcode)+altitude+temp+landsuit+malaria+biomes1+biomes2_3+biomes4+biomes5+biomes6+biomes7_9+biomes8+biomes10+biomes11+biomes12+biomes13+biomes14+growingdays+precip+seadist1+borderdist1+lnkm2split+lights+rugg+pop, data=active_data_set)
#
# mod.9a <- lm(zeta~split5pc+factor(wbcode)+altitude+temp+landsuit+malaria+biomes1+biomes2_3+biomes4+biomes5+biomes6+biomes7_9+biomes8+biomes10+biomes11+biomes12+biomes13+biomes14+growingdays+precip+seadist1+borderdist1+lnkm2split, data=active_data_set)
# mod.9b <- lm(zeta~split5pc+factor(wbcode)+altitude+temp+landsuit+malaria+biomes1+biomes2_3+biomes4+biomes5+biomes6+biomes7_9+biomes8+biomes10+biomes11+biomes12+biomes13+biomes14+growingdays+precip+seadist1+borderdist1+lnkm2split+lights+rugg+pop, data=active_data_set)


#########################################
# Years in Power Ethnic
#########################################

# active_data_set <- acled
# active_data_set$ever_in_power <- as.numeric(active_data_set$years_in_power>0)
# active_data_set$polity <- as.numeric(active_data_set$polity>0.5)
# active_data_set$years_in_power_polity <- active_data_set$years_in_power*active_data_set$polity
#
# data_small <- active_data_set[active_data_set$capital != 1,]
#
#
# mod.1a <- lm(zeta~years_in_power+factor(wbcode)+altitude+temp+landsuit+malaria+biomes1+biomes2_3+biomes4+biomes5+biomes6+biomes7_9+biomes8+biomes10+biomes11+biomes12+biomes13+biomes14+growingdays+precip+seadist1+borderdist1+lnkm2split, data=active_data_set)
# mod.1b <- lm(zeta~years_in_power+factor(wbcode)+altitude+temp+landsuit+malaria+biomes1+biomes2_3+biomes4+biomes5+biomes6+biomes7_9+biomes8+biomes10+biomes11+biomes12+biomes13+biomes14+growingdays+precip+seadist1+borderdist1+lnkm2split+lights+rugg+pop, data=active_data_set)
# mod.1c <- lm(zeta~years_in_power+years_in_power_polity+factor(wbcode)+altitude+temp+landsuit+malaria+biomes1+biomes2_3+biomes4+biomes5+biomes6+biomes7_9+biomes8+biomes10+biomes11+biomes12+biomes13+biomes14+growingdays+precip+seadist1+borderdist1+lnkm2split+lights+rugg+pop, data=active_data_set)
#
# mod.2a <- lm(zeta~ever_in_power+factor(wbcode)+altitude+temp+landsuit+malaria+biomes1+biomes2_3+biomes4+biomes5+biomes6+biomes7_9+biomes8+biomes10+biomes11+biomes12+biomes13+biomes14+growingdays+precip+seadist1+borderdist1+lnkm2split, data=active_data_set)
# mod.2b <- lm(zeta~ever_in_power+factor(wbcode)+altitude+temp+landsuit+malaria+biomes1+biomes2_3+biomes4+biomes5+biomes6+biomes7_9+biomes8+biomes10+biomes11+biomes12+biomes13+biomes14+growingdays+precip+seadist1+borderdist1+lnkm2split+lights+rugg+pop, data=active_data_set)
#
# #
# #
# mod.3a.small <- lm(zeta~years_in_power+factor(wbcode)+altitude+temp+landsuit+malaria+biomes1+biomes2_3+biomes4+biomes5+biomes6+biomes7_9+biomes8+biomes10+biomes11+biomes12+biomes13+biomes14+growingdays+precip+seadist1+borderdist1+lnkm2split+lights+rugg+pop, data=data_small)
# mod.3b.small <- lm(zeta~years_in_power+years_in_power_polity+factor(wbcode)+altitude+temp+landsuit+malaria+biomes1+biomes2_3+biomes4+biomes5+biomes6+biomes7_9+biomes8+biomes10+biomes11+biomes12+biomes13+biomes14+growingdays+precip+seadist1+borderdist1+lnkm2split+lights+rugg+pop, data=data_small)
# mod.3c.small <- lm(zeta~ever_in_power+factor(wbcode)+altitude+temp+landsuit+malaria+biomes1+biomes2_3+biomes4+biomes5+biomes6+biomes7_9+biomes8+biomes10+biomes11+biomes12+biomes13+biomes14+growingdays+precip+seadist1+borderdist1+lnkm2split+lights+rugg+pop, data=data_small)

#########################################
# Years in Power Grid
#########################################


data_full <- opt_loc[opt_loc$country != "Western-Sahara",]
data_full$ever_in_power <- as.numeric(data_full$years_in_power>0)
data_full$polity <- as.numeric(data_full$polity>0.5)
data_full$years_in_power_polity <- data_full$years_in_power*data_full$polity

data_small <- data_full[data_full$capital != 1,]

mod.1a.full <- lm(zeta~years_in_power+factor(country)+altitude+temp+landsuit+malaria+biomes1+biomes2_3+biomes4+biomes5+biomes6+biomes7_9+biomes8+biomes10+biomes11+biomes12+biomes13+biomes14+harbor25+river25+lake25+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+border_cell, data=data_full)
mod.1b.full <- lm(zeta~years_in_power+factor(country)+altitude+temp+landsuit+malaria+biomes1+biomes2_3+biomes4+biomes5+biomes6+biomes7_9+biomes8+biomes10+biomes11+biomes12+biomes13+biomes14+harbor25+river25+lake25+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+rugg+lights+pop+border_cell, data=data_full)
mod.1c.full <- lm(zeta~years_in_power+years_in_power_polity+factor(country)+altitude+temp+landsuit+malaria+biomes1+biomes2_3+biomes4+biomes5+biomes6+biomes7_9+biomes8+biomes10+biomes11+biomes12+biomes13+biomes14+harbor25+river25+lake25+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+rugg+lights+pop+border_cell, data=data_full)

mod.2a.full <- lm(zeta~ever_in_power+factor(country)+altitude+temp+landsuit+malaria+biomes1+biomes2_3+biomes4+biomes5+biomes6+biomes7_9+biomes8+biomes10+biomes11+biomes12+biomes13+biomes14+harbor25+river25+lake25+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+border_cell, data=data_full)
mod.2b.full <- lm(zeta~ever_in_power+factor(country)+altitude+temp+landsuit+malaria+biomes1+biomes2_3+biomes4+biomes5+biomes6+biomes7_9+biomes8+biomes10+biomes11+biomes12+biomes13+biomes14+harbor25+river25+lake25+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+rugg+lights+pop+border_cell, data=data_full)

mod.3a.full.small <- lm(zeta~years_in_power+factor(country)+altitude+temp+landsuit+malaria+biomes1+biomes2_3+biomes4+biomes5+biomes6+biomes7_9+biomes8+biomes10+biomes11+biomes12+biomes13+biomes14+harbor25+river25+lake25+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+rugg+lights+pop+border_cell, data=data_small)
mod.3b.full.small <- lm(zeta~years_in_power+years_in_power_polity+factor(country)+altitude+temp+landsuit+malaria+biomes1+biomes2_3+biomes4+biomes5+biomes6+biomes7_9+biomes8+biomes10+biomes11+biomes12+biomes13+biomes14+harbor25+river25+lake25+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+rugg+lights+pop+border_cell, data=data_small)
mod.3c.full.small <- lm(zeta~ever_in_power+factor(country)+altitude+temp+landsuit+malaria+biomes1+biomes2_3+biomes4+biomes5+biomes6+biomes7_9+biomes8+biomes10+biomes11+biomes12+biomes13+biomes14+harbor25+river25+lake25+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+rugg+lights+pop+border_cell, data=data_small)

#######################################################
#### Display results
######################################################

mo_list = list() # this puts all models in a list for stargazer
for(i in ls(pattern="mod.")){
  if(!grepl("IV", i)){
  mo_list[[paste(i)]] <- get(i)
} else{
  mo_list[[paste(i)]] <- cluster.robust.se(get(i), active_data_set$cluster)
}
}

se_list = list() # this puts the respective White standard errors in a list for stargazer
for(i in ls(pattern="mod.")){
  current <- get(i)
  if(grepl("small", i)){
  if(grepl("full", i)){
    if(grepl("IV", i)){
      se_list[[paste(i)]] <- cluster.robust.se(current, data_small$cluster)[,2] # this clusters and is HAC-robust
    } else{
        se_list[[paste(i)]] <- sqrt(diag(cluster.vcov(current, cluster=data_small$cluster))) # this clusters and is HAC-robust
      }
  } else{
  if(grepl("IV", i)){
    se_list[[paste(i)]] <- cluster.robust.se(current, data_small$cluster)[,2] # this clusters and is HAC-robust
  } else{
      se_list[[paste(i)]] <- sqrt(diag(cluster.vcov(current, cluster=cbind(data_small$cluster, data_small$wbcode)))) # this clusters and is HAC-robust
    }
}
} else{
  if(grepl("full", i)){
  if(grepl("IV", i)){
    se_list[[paste(i)]] <- cluster.robust.se(current, data_full$cluster)[,2] # this clusters and is HAC-robust
  } else{
      se_list[[paste(i)]] <- sqrt(diag(cluster.vcov(current, cluster=data_full$cluster))) # this clusters and is HAC-robust
    }
} else{
  if(grepl("IV", i)){
    se_list[[paste(i)]] <- cluster.robust.se(current, active_data_set$cluster)[,2] # this clusters and is HAC-robust
  } else{
      se_list[[paste(i)]] <- sqrt(diag(cluster.vcov(current, cluster=cbind(active_data_set$cluster, active_data_set$wbcode)))) # this clusters and is HAC-robust
    }
}
}
}

mde_list = list() # this puts the respective White standard errors in a list for stargazer
for(i in ls(pattern="mod.")){
    mde_list[[paste(i)]] <- se_list[[paste(i)]] * 2.8
}

country <- "Country FE" # this puts the respective Controls in a list for stargazer
geog <- "Geographic controls"
sim_controls <- "Simulation controls"

keeplist <- vector()
keeplist <- c(keeplist, "polity")

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

stargazer(mo_list, se=se_list, type="text", t=mde_list,  keep = keeplist, p.auto=TRUE, t.auto=TRUE, add.lines=control_list, keep.stat=c("rsq", "n"), report="vc*s", dep.var.labels.include=F)
