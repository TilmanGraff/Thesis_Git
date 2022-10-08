# Uses adj matrices to define border variable, which is then merged in the Analysis folder
# do not confuse this with the borderregion variable. This is just about how many connections a gridcell has! (whether its at a coast etc!)

countries <- read.csv("./Build/temp/country_names.csv")

borders <- NA

for(i in 1:length(countries$x)){
  country <- countries$x[i]
    if(file.exists(paste0("./Build/output/Network_outcomes/", country, "_outcomes.csv"))){
      adjraw <- read.csv(paste0("./Build/temp/adj/adj_", country, ".csv"))
      prodraw <- read.csv(paste0("./Build/temp/productivities/productivities_", country, ".csv"))
      countryfile <- read.csv(paste0("./Build/output/Network_outcomes/", country, "_outcomes.csv"))

      athome = 1 - countryfile$abroad

      adj = adjraw[athome==1,athome==1]
      prod = prodraw[athome==1,]

      if(is.na(borders)){
        borders <- as.data.frame(cbind(rownumber=1:length(adj), country=as.character(country), border=rowSums(adj), ID = countryfile[athome==1,"ID"], distinct_good = as.numeric(prod$V6 == 0)))
      } else{
        borders <- rbind(borders, cbind(rownumber=1:length(adj), country=as.character(country), border=rowSums(adj), ID = countryfile[athome==1,"ID"], distinct_good = as.numeric(prod$V6 == 0)))
      }



    }
    print(country)

}

write.csv(borders, file="./Analysis/temp/borders.csv", row.names = FALSE)
