# Minor file to create a table of country names needed for Matlab parts


centroids <- read.csv("./Build/temp/centroids.csv")
centroids <- centroids[centroids$region==2,]
country_table <- as.data.frame(table(centroids$country))
country_names <- paste(country_table[country_table$Freq != 0,"Var1"])

write.csv(country_names, file="./Build/temp/country_names.csv", row.names = FALSE)
