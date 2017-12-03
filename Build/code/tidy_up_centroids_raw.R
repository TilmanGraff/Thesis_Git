
# This code cleans up the untidy centroids_raw.csv file, which came straight out of the messes of manual QGIS
#########

library("plyr", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")

centroids <- read.csv("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Build/input/Centroids_raw.csv")

centroids <- as.data.frame(centroids)

centroids$x <- 0.5*(centroids$left + centroids$right)
centroids$y <- 0.5*(centroids$top + centroids$bottom)

centroids <- centroids[,!names(centroids) %in% c("top", "bottom", "left", "right", "meany", "meanx", "meanriv", "meanonriv", "meandistc", "meanlakebi", "meanland", "meanpop201", "meanx_shif", "meany_shif", "count", "AREA", "meancoasta")]

centroids <- rename(centroids, c("Pop_Sum" = "pop", "Pop_sdv" = "pop_sd", "Lights_sum" = "lights_raw", "Lights_sdv" = "lights_sd", "meanrad_no" = "alternative_lights", "meanrugged" = "rugg", "meanelv" = "altitude", "meanlandsu" = "landsuit", "meantmp" = "temp", "meanprecip" = "precip", "meanharbor" = "harbor", "meangrowda" = "growingdays", "meanmalari" = "malaria", "UN" = "un_code", "NAME" = "country", "REGION" = "region", "SUBREGION" = "subregion", "meannum_la" = "num_landpixels"))

centroids$ID <- as.numeric(rownames(centroids))

centroids <- centroids[,c("ID", "x", "y", "country", "region", "subregion", "pop", "lights_raw", "num_landpixels", "pop_sd", "lights_sd", "rugg", "altitude", "landsuit", "temp", "precip", "growingdays", "malaria", "harbor", "alternative_lights", "un_code")]

# This identifies a desert
# centroids$desert = centroids$landsuit < 0.005

# # This does a light transformation as in Henderson et al (page 20)
# centroids$lights_raw <- centroids$lights_raw / 4 # this is because lights are a meaned variable and I initially had summed them together (there are always four Henderson cells in one of mine)
# centroids$lights <- centroids$lights_raw / centroids$num_landpixels
# centroids[centroids$lights == 0 & !is.na(centroids$lights), "lights"] <- 0.0033995

# This tidies up country names
centroids$country <- gsub("Libyan Arab Jamahiriya", "Libya", centroids$country)
centroids$country <- gsub(" ", "-", centroids$country)
centroids$country <- gsub("'", "", centroids$country)

write.csv(centroids, "/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Build/temp/centroids_wrong_lights.csv", row.names = FALSE)
