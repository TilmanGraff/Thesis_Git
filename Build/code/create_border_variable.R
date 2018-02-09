
countries <- read.csv("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Build/temp/country_names.csv")

borders <- NA

for(i in 1:length(countries$x)){
  country <- countries$x[i]
    if(file.exists(paste("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Build/temp/adj/adj_", country, ".csv", sep = ""))){
      adj <- read.csv(paste("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Build/temp/adj/adj_", country, ".csv", sep = ""))

      if(is.na(borders)){
        borders <- as.data.frame(cbind(rownumber=1:length(adj), country=as.character(country), border=rowSums(adj)))
      } else{
        borders <- rbind(borders, cbind(rownumber=1:length(adj), country=as.character(country), border=rowSums(adj)))
      }



    }


}

write.csv(borders, file="/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/temp/borders.csv", row.names = FALSE)
