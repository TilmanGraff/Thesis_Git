library("countrycode", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")


# I believe that all of this is actually bogus
#####################


opt_loc <- read.csv("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/temp/opt_loc_with_raildist.csv")
aid <- read.csv(file="/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/temp/grid_ids_aid.csv")

opt_loc <- merge(opt_loc, aid, by="ID")

opt_loc$wb_num_oth <- opt_loc$wb_num-opt_loc$wb_num_transp


## FIRST: obtain country-relevant project fraction
for(country in unique(opt_loc$country)){

  countryset <- opt_loc[opt_loc$country==country,]

  total_country_other_projects <- sum(countryset$wb_num_oth)

  for(i in 1:nrow(countryset)){
    opt_loc[opt_loc$ID == countryset$ID[i], "projectfraction"] <- opt_loc[opt_loc$ID == countryset$ID[i], "wb_num_oth"] / total_country_other_projects
  }

}

opt_loc[is.na(opt_loc$projectfraction), "projectfraction"] <- 0
opt_loc$wbcode <- countrycode(opt_loc$country, origin = "country.name", destination="wb")


## SECOND: interact this with countrywide propensity to get aid.

country <- c("Angola", "Cameroon", "Congo, DR", "Djibouti", "Egypt", "Equatorial Guinea", "Ghana", "Nigeria", "Sudan", "Senegal", "Zimbabwe")
crossing <- c(2005, 2008, 2006, 2007, 1995, 1998, 2009, 2008, 2008, 1991, 1992)
df <- data.frame(country, crossing)
df$wbcode <- countrycode(df$country, origin = "country.name", destination="wb")
opt_loc <- merge(opt_loc, df[,c("wbcode", "crossing")], by="wbcode", all.x=T)

opt_loc$years_crossed <- 2017 - opt_loc$crossing

opt_loc$iv_ida <- opt_loc$projectfraction*opt_loc$years_crossed

###


write.csv(opt_loc[,c("ID", "iv_ida")], file="/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/temp/ID_iv_ida.csv", row.names = FALSE)
