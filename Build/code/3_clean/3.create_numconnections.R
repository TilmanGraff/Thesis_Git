# Uses adj matrices to define border variable, which is then merged in the Analysis folder
# do not confuse this with the borderregion variable. This is just about how many connections a gridcell has! (whether its at a coast etc!)

countries <- read.csv("./Build/temp/country_names.csv")

borders <- NA

for(i in 1:length(countries$x)){
  country <- countries$x[i]
    if(file.exists(paste("./Build/temp/adj/adj_", country, ".csv", sep = ""))){
      adj <- read.csv(paste("./Build/temp/adj/adj_", country, ".csv", sep = ""))

      if(is.na(borders)){
        borders <- as.data.frame(cbind(rownumber=1:length(adj), country=as.character(country), border=rowSums(adj)))
      } else{
        borders <- rbind(borders, cbind(rownumber=1:length(adj), country=as.character(country), border=rowSums(adj)))
      }



    }


}

write.csv(borders, file="./Analysis/temp/borders.csv", row.names = FALSE)
