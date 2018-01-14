
library("stargazer", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")

opt_loc <- read.csv("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/temp/opt_loc_with_raildist")


mod.1 <- lm(zeta~log(dist2rail)+factor(country), data=opt_loc)
mod.2 <- lm(I_change~log(dist2rail)+factor(country), data=opt_loc)
mod.3 <- lm(zeta~log(dist2rail)+factor(country)+rugg+altitude+temp+landsuit+malaria+growingdays+pop_dens+precip, data=opt_loc[opt_loc$dist2rail < 200,])
mod.4 <- lm(I_change~log(dist2rail)+factor(country)+rugg+altitude+temp+landsuit+malaria+growingdays+pop_dens+precip, data=opt_loc[opt_loc$dist2rail < 200,])


stargazer(mod.1, mod.2, mod.3, mod.4, type="text", omit = c("factor", "Constant"))
