
# This code cleans up the untidy centroids_raw.csv file, which came straight out of the messes of manual QGIS
#########

library("plyr", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")

centroids <- read.csv("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Build/input/Centroids_raw.csv")

centroids <- as.data.frame(centroids)

centroids$x <- 0.5*(centroids$left + centroids$right)
centroids$y <- 0.5*(centroids$top + centroids$bottom)

centroids <- centroids[,!names(centroids) %in% c("top", "bottom", "left", "right", "meany", "meanx", "meanriv", "meanonriv", "meandistc", "meanlakebi", "meanland", "meanpop201", "meannum_la", "meanx_shif", "meany_shif", "count", "AREA", "meancoasta")]

centroids <- rename(centroids, c("Pop_Sum" = "pop", "Pop_sdv" = "pop_sd", "Lights_sum" = "lights", "Lights_sdv" = "lights_sd", "meanrad_no" = "alternative_lights", "meanrugged" = "rugg", "meanelv" = "altitude", "meanlandsu" = "landsuit", "meantmp" = "temp", "meanprecip" = "precip", "meanharbor" = "harbor", "meangrowda" = "growingdays", "meanmalari" = "malaria", "UN" = "un_code", "NAME" = "country", "REGION" = "region", "SUBREGION" = "subregion"))

centroids <- centroids[,c("x", "y", "country", "region", "subregion", "pop", "lights", "pop_sd", "lights_sd", "rugg", "altitude", "landsuit", "temp", "precip", "growingdays", "malaria", "harbor", "alternative_lights", "un_code")]


write.csv(centroids, "/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Build/temp/centroids.csv")
