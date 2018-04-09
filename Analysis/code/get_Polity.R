
library("countrycode", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")

# Loading in data
opt_loc <- read.csv("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/input/opt_loc.csv")
pol_all <- read.csv("~/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/input/Polity4.csv", sep=";")
pol <- pol_all[pol_all$year==2016,c("country", "polity")]

pol[pol$country=="Libya", "polity"] <- (-7)
pol[pol$country=="South Sudan", "polity"] <- (0)

pol$wbcode <- countrycode(pol$country, origin = "country.name", destination = "wb")
opt_loc$wbcode <- countrycode(opt_loc$country, origin = "country.name", destination = "wb")

opt_loc <- merge(opt_loc, pol[,c("wbcode", "polity")], by="wbcode", all.x=T)

opt_loc$polity <- (opt_loc$polity+10)/20

write.csv(opt_loc[,c("ID", "polity")], file="/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/temp/ID_polity.csv", row.names = FALSE)
