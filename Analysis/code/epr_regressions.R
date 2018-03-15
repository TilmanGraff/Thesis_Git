library("stargazer", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("sandwich", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("lmtest", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("multiwayvcov", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("AER", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("MASS", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("ivpack", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")

rm(list=ls(pattern="mod")) # this removes all models from the RStudio environment


opt_ethn <- read.csv("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/temp/opt_ethn.csv")
mich_epr <- read.csv("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/temp/mich_epr.csv")
mich_epr <- merge(mich_epr, opt_ethn[,c("country", "group", "status")], by=c("group", "country"), all.x=T)

mich_epr$x_2 <- mich_epr$x^2
mich_epr$x_3 <- mich_epr$x^3
mich_epr$x_4 <- mich_epr$x^4

mich_epr$y_2 <- mich_epr$y^2
mich_epr$y_3 <- mich_epr$y^3
mich_epr$y_4 <- mich_epr$y^4

mich_epr$status <- factor(mich_epr$status, levels=c("DISCRIMINATED", "POWERLESS", "IRRELEVANT", "JUNIOR PARTNER", "SENIOR PARTNER", "DOMINANT", "MONOPOLY"))
mich_epr$epr_scale <- as.numeric(mich_epr$status)
mich_epr$discriminated <- as.numeric(mich_epr$epr_scale == 1)
mich_epr$participation <- as.numeric(mich_epr$epr_scale > 3)
mich_epr$monopoly <- as.numeric(mich_epr$epr_scale == 7)

## models

mod.1 <- lm(zeta~discriminated+factor(country)+lnkm2split+altitude+temp+landsuit+malaria+growingdays+x+x_2+x_3+x_4+y+y_2+y_3+y_4+pop+lights+rugg+urban, data=mich_epr)
mod.2 <- lm(zeta~monopoly+factor(country)+lnkm2split+altitude+temp+landsuit+malaria+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+pop+lights+rugg+urban, data=mich_epr)
mod.3 <- lm(zeta~participation+factor(country)+lnkm2split+altitude+temp+landsuit+malaria+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+pop+lights+rugg+urban, data=mich_epr)
mod.4 <- lm(zeta~epr_scale+factor(country)+lnkm2split+altitude+temp+landsuit+malaria+growingdays+precip+x+x_2+x_3+x_4+y+y_2+y_3+y_4+pop+lights+rugg+urban, data=mich_epr)

#######################################################
#### Display results
######################################################

mo_list = list() # this puts all models in a list for stargazer
for(i in ls(pattern="mod.")){
  if(!grepl("IV", i)){
  mo_list[[paste(i)]] <- get(i)
} else{
  mo_list[[paste(i)]] <- cluster.robust.se(get(i), opt_acled$country)
}
}

se_list = list() # this puts the respective White standard errors in a list for stargazer
for(i in ls(pattern="mod.")){
  current <- get(i)
  if(!grepl("small", i)){
  #se_list[[paste(i)]] <- sqrt(diag(vcovHC(current, "HC1"))) # this is HAC-robust
  if(grepl("IV", i)){
    #se_list[[paste(i)]] <- cluster.robust.se(current, opt_acled$country)[,2] # this clusters and is HAC-robust
    se_list[[paste(i)]] <- robust.se(current)[,2] # this is HAC-robust

  } else{
      se_list[[paste(i)]] <- sqrt(diag(cluster.vcov(current, cluster=mich_epr$tuple_ID))) # this clusters and is HAC-robust
    }
} else{
  if(grepl("IV", i)){
    se_list[[paste(i)]] <- cluster.robust.se(current, data_small$country)[,2] # this clusters and is HAC-robust
  } else{
      se_list[[paste(i)]] <- sqrt(diag(cluster.vcov(current, cluster=data_small$tuple_ID))) # this clusters and is HAC-robust
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


stargazer(mo_list, se=se_list, type="text",  keep = c("dis", "zeta", "epr", "part", "monopoly"), p.auto=TRUE, t.auto=TRUE, add.lines=control_list, keep.stat=c("n"))
