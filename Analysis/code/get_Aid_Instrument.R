
opt_loc <- read.csv("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/temp/opt_loc_with_raildist.csv")
aid <- read.csv(file="/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/temp/grid_ids_aid.csv")

opt_loc <- merge(opt_loc, aid, by="ID")

opt_loc$number_other_wb_projects <- opt_loc$number_wb_projects-opt_loc$number_wb_projects_transport


## FIRST: obtain country-relevant project fraction
for(country in unique(opt_loc$country)){

  countryset <- opt_loc[opt_loc$country==country,]

  total_country_other_projects <- sum(countryset$number_other_wb_projects)

  for(i in 1:nrow(countryset)){
    opt_loc[opt_loc$ID == countryset$ID[i], "projectfraction"] <- opt_loc[opt_loc$ID == countryset$ID[i], "number_other_wb_projects"] / total_country_other_projects
  }

}

opt_loc[is.na(opt_loc$projectfraction), "projectfraction"] <- 0


## SECOND: interact this with countrywide propensity to get aid.

#...
