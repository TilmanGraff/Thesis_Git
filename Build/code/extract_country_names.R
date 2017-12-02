
centroids <- read.csv("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Build/temp/centroids.csv")
centroids <- centroids[centroids$region==2,]
country_table <- as.data.frame(table(centroids$country))
country_names <- paste(country_table[country_table$Freq != 0,"Var1"])

write.csv(country_names, file="/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Build/temp/country_names.csv", row.names = FALSE)
