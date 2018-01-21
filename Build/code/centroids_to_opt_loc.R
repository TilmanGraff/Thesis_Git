
centroids <- read.csv("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Build/temp/centroids.csv")

# Gather country names
country_table <- as.data.frame(table(centroids$country))
country_names <- paste(country_table[country_table$Freq != 0,"Var1"])

countryfiles <- NA

for (country in country_names[1]){

  if(file.exists(paste("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Build/output/Network_outcomes/", country, "_outcomes.csv", sep = ""))){

    countryfile <- read.csv(paste("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Build/output/Network_outcomes/", country, "_outcomes.csv", sep = ""))

    I_initial <- read.csv(paste("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Build/temp/I/I_", country, ".csv", sep = ""))

    I_opt <- read.csv(paste("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Build/output/Optimised_Networks/", country, ".csv", sep = ""),  header=FALSE)

    countryfile$I_change <- rowSums(I_opt - I_initial) / 2

    countryfile$country <- country

    if(length(countryfiles)==1){
      countryfiles <- countryfile
    } else{
      countryfiles <- rbind(countryfiles, countryfile)
    }

  }

}

opt_loc <- merge(centroids, countryfiles, by=c("country", "rownumber"), all.x=T)
opt_loc$zeta <- opt_loc$util_opt / opt_loc$util_stat



write.csv(opt_loc, file="/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/input/opt_loc.csv", row.names = FALSE)
