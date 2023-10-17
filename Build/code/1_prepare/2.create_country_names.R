# Minor file to create a table of country names needed for Matlab parts


centroids <- read.csv("./Build/temp/centroids.csv")
centroids <- centroids[centroids$region==2,]
country_table <- as.data.frame(table(centroids$country))
country_names <- paste(country_table[country_table$Freq != 0,"Var1"])

extra_countries = c("Germany", "Japan", "China", "United-States")

write.csv(c(country_names, extra_countries), file="./Build/temp/country_names.csv", row.names = FALSE)
