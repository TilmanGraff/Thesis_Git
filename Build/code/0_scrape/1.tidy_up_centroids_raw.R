
# Cleans up the untidy centroids_raw.csv file, which came straight out of the messes of manual QGIS
#########

require("plyr")
require("geosphere")

centroids <- read.csv("./Build/input/Centroids_raw.csv")

centroids <- as.data.frame(centroids)

centroids$x <- 0.5*(centroids$left + centroids$right)
centroids$y <- 0.5*(centroids$top + centroids$bottom)

# This defines gridarea
centroids$gridarea <- NA

for(i in 1:nrow(centroids)){
  df <- data.frame(long = c(centroids[i, "left"], centroids[i, "left"], centroids[i, "right"], centroids[i, "right"]), lat=c(centroids[i, "top"], centroids[i, "bottom"], centroids[i, "bottom"], centroids[i, "top"]))
  centroids[i, "gridarea"] <- areaPolygon(df) / (1000^2)
}


centroids <- centroids[,!names(centroids) %in% c("top", "bottom", "left", "right", "meany", "meanx", "meanriv", "meanonriv", "meandistc", "meanlakebi", "meanland", "meanpop201", "meanx_shif", "meany_shif", "count", "AREA", "meancoasta")]

centroids <- rename(centroids, c("Pop_Sum" = "pop", "Pop_sdv" = "pop_sd", "Lights_sum" = "lights_raw", "Lights_sdv" = "lights_sd", "meanrad_no" = "alternative_lights", "meanrugged" = "rugg", "meanelv" = "altitude", "meanlandsu" = "landsuit", "meantmp" = "temp", "meanprecip" = "precip", "meanharbor" = "harbor", "meangrowda" = "growingdays", "meanmalari" = "malaria", "UN" = "un_code", "NAME" = "country", "REGION" = "region", "SUBREGION" = "subregion", "meannum_la" = "num_landpixels"))

# This defines unique cell IDs
centroids$ID <- as.numeric(rownames(centroids))

# This defines population density
centroids$pop_dens <- centroids$pop / centroids$gridarea

centroids <- centroids[,c("ID", "x", "y", "country", "region", "subregion", "pop", "lights_raw", "num_landpixels", "gridarea", "pop_dens", "pop_sd", "lights_sd", "rugg", "altitude", "landsuit", "temp", "precip", "growingdays", "malaria", "harbor", "alternative_lights", "un_code")]

# This tidies up country names
centroids$country <- gsub("Libyan Arab Jamahiriya", "Libya", centroids$country)
centroids$country <- gsub(" ", "-", centroids$country)
centroids$country <- gsub("'", "", centroids$country)

write.csv(centroids, "./Build/temp/centroids_wrong_lights.csv", row.names = FALSE)
