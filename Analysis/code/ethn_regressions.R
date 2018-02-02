
library("stargazer", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("sandwich", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("lmtest", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("multiwayvcov", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("AER", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("MASS", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")

opt_acled <- read.csv("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/temp/opt_acled.csv")

opt_acled$x_2 <- opt_acled$x^2
opt_acled$x_3 <- opt_acled$x^3
opt_acled$x_4 <- opt_acled$x^4

opt_acled$y_2 <- opt_acled$y^2
opt_acled$y_3 <- opt_acled$y^3
opt_acled$y_4 <- opt_acled$y^4


############
# Zeta on split
############
#
# mod.1 <- lm(zeta~split10pc+factor(country), data=opt_acled)
# mod.2 <- lm(zeta~split10pc+factor(country)+log(km2split)+altitude+temp+landsuit+malaria+growingdays+precip+lakedum+riverdum+petroleum+diamondd+x+x_2+x_3+x_4+y+y_2+y_3+y_4, data=opt_acled)
# mod.3 <- lm(zeta~split10pc+factor(country)+log(km2split)+altitude+temp+landsuit+malaria+growingdays+precip+lakedum+riverdum+petroleum+diamondd+x+x_2+x_3+x_4+y+y_2+y_3+y_4+pop+lights+rugg+urban, data=opt_acled)
#
#
# covariate_labels <- c("Split Dummy")

# Really nothing there!


############
# Violence on Zeta
############

opt_acled$all_d <- opt_acled$all > 0
opt_acled$fatal_d <- opt_acled$fatal > 0
opt_acled$vio_d <- opt_acled$vio > 0
opt_acled$riots_d <- opt_acled$riots > 0

mod.1.1 <- glm.nb(all~zeta+factor(country)+log(km2split)+altitude+temp+landsuit+malaria+growingdays+precip+lakedum+riverdum+petroleum+diamondd+split5pc+x+x_2+x_3+x_4+y+y_2+y_3+y_4+pop+lights+rugg+urban, data=opt_acled)
#mod.1.2 <- lm(all_d~zeta+factor(country)+log(km2split)+altitude+temp+landsuit+malaria+growingdays+precip+lakedum+riverdum+petroleum+diamondd+split5pc+x+x_2+x_3+x_4+y+y_2+y_3+y_4+pop+lights+rugg+urban, data=opt_acled)

mod.2.1 <- glm.nb(battles~zeta+factor(country)+log(km2split)+altitude+temp+landsuit+malaria+growingdays+precip+lakedum+riverdum+petroleum+diamondd+split5pc+x+x_2+x_3+x_4+y+y_2+y_3+y_4+pop+lights+rugg+urban, data=opt_acled)
#mod.2.2 <- lm(fatal_d~zeta+factor(country)+log(km2split)+altitude+temp+landsuit+malaria+growingdays+precip+lakedum+riverdum+petroleum+diamondd+split5pc+x+x_2+x_3+x_4+y+y_2+y_3+y_4+pop+lights+rugg+urban, data=opt_acled)

mod.3.1 <- glm.nb(vio~zeta+factor(country)+log(km2split)+altitude+temp+landsuit+malaria+growingdays+precip+lakedum+riverdum+petroleum+diamondd+split5pc+x+x_2+x_3+x_4+y+y_2+y_3+y_4+pop+lights+rugg+urban, data=opt_acled)
#mod.3.2 <- lm(vio_d~zeta+factor(country)+log(km2split)+altitude+temp+landsuit+malaria+growingdays+precip+lakedum+riverdum+petroleum+diamondd+split5pc+x+x_2+x_3+x_4+y+y_2+y_3+y_4+pop+lights+rugg+urban, data=opt_acled)

mod.4.1 <- glm.nb(riots~zeta+factor(country)+log(km2split)+altitude+temp+landsuit+malaria+growingdays+precip+lakedum+riverdum+petroleum+diamondd+split5pc+x+x_2+x_3+x_4+y+y_2+y_3+y_4+pop+lights+rugg+urban, data=opt_acled)
#mod.4.2 <- lm(riots_d~zeta+factor(country)+log(km2split)+altitude+temp+landsuit+malaria+growingdays+precip+lakedum+riverdum+petroleum+diamondd+split5pc+x+x_2+x_3+x_4+y+y_2+y_3+y_4+pop+lights+rugg+urban, data=opt_acled)


covariate_labels <- c("Zeta")


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
    se_list[[paste(i)]] <- cluster.robust.se(current, opt_acled$cluster)[,2] # this clusters and is HAC-robust
  } else{
      se_list[[paste(i)]] <- sqrt(diag(cluster.vcov(current, cluster=opt_acled$country))) # this clusters and is HAC-robust
    }
} else{
  if(grepl("IV", i)){
    se_list[[paste(i)]] <- cluster.robust.se(current, data_small$country)[,2] # this clusters and is HAC-robust
  } else{
      se_list[[paste(i)]] <- sqrt(diag(cluster.vcov(current, cluster=data_small$country))) # this clusters and is HAC-robust
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


stargazer(mo_list, se=se_list, type="text",  keep = c("ail", "placebo", "KM", "ten", "thirty", "smaller", "zeta"), p.auto=TRUE, t.auto=TRUE, add.lines=control_list, keep.stat=c("n"), covariate.labels = covariate_labels)
