
library("stargazer", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("sandwich", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("lmtest", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")

opt_loc <- read.csv("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/temp/opt_loc_with_raildist.csv")

opt

opt_loc$smaller_ten <- opt_loc$dist2rail <= 10
opt_loc$ten_twenty <- opt_loc$dist2rail > 10 & opt_loc$dist2rail <= 20
opt_loc$twenty_thirty <- opt_loc$dist2rail > 20 & opt_loc$dist2rail <= 30
opt_loc$thirty_fourty <- opt_loc$dist2rail > 30 & opt_loc$dist2rail <= 40



mod.1 <- lm(zeta~smaller_ten+ten_twenty+twenty_thirty+thirty_fourty, data=opt_loc)
mod.2 <- lm(zeta~smaller_ten+ten_twenty+twenty_thirty+thirty_fourty+factor(country), data=opt_loc)
mod.3 <- lm(zeta~smaller_ten+ten_twenty+twenty_thirty+thirty_fourty+factor(country)+rugg+altitude+temp+landsuit+malaria+growingdays+precip+urban, data=opt_loc)

mod.4 <- lm(zeta~(dist2rail), data=opt_loc)
mod.5 <- lm(zeta~(dist2rail)+factor(country), data=opt_loc)
mod.6 <- lm(zeta~(dist2rail)+factor(country)+rugg+altitude+temp+landsuit+malaria+growingdays+precip+urban, data=opt_loc)


stargazer(mod.1, mod.2, mod.3, mod.4, mod.5, mod.6, se=list(sqrt(diag(vcovHC(mod.1, "HC1"))), sqrt(diag(vcovHC(mod.2, "HC1"))), sqrt(diag(vcovHC(mod.3, "HC1"))), sqrt(diag(vcovHC(mod.4, "HC1"))), sqrt(diag(vcovHC(mod.5, "HC1"))), sqrt(diag(vcovHC(mod.6, "HC1")))), type="text", keep = c("_", "ail"), p.auto=TRUE, t.auto=TRUE, add.lines = list(c("Country FE", "", "Yes", "Yes", "", "Yes", "Yes"), c("Geography Controls", "", "", "Yes", "", "Yes", "Yes")))
