library("plyr", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")


henderson <- read.csv("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Build/input/Henderson_et_al_2018/data1_regs_only_lights.csv")

henderson <- henderson[,c("x", "y", "lrad2010clip_pl_c")]

henderson <- rename(henderson, c("lrad2010clip_pl_c" = "loglights"))
henderson$lights <- exp(henderson$loglights)

centroids <- read.csv("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Build/temp/centroids.csv")

ref_centroids <- centroids[,c("ID", "x", "y")]

henderson <- merge(henderson, ref_centroids, by=c("x", "y"), all.x=T)

ref_centroids$mean_lights <- NA

for(id in ref_centroids$ID){

  henderson[ref_centroids[ref_centroids$ID == id, "x"]-henderson[, "x"] <= 0.25 & ref_centroids[ref_centroids$ID == id, "x"]-henderson[, "x"] >=0 & ref_centroids[ref_centroids$ID == id, "y"]-henderson[, "y"] <= 0.25 & ref_centroids[ref_centroids$ID == id, "y"]-henderson[, "y"] >= 0,"ID"] <- id

}

for(id in ref_centroids$ID){
  ref_centroids[ref_centroids$ID == id, "mean_lights"] <- mean(henderson[henderson$ID == id & !is.na(henderson$ID),"lights"])
}

ref_centroids[ref_centroids$mean_lights=="NaN","mean_lights"] <- NA

centroids <- read.csv("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Build/temp/centroids_wrong_lights.csv")

centroids <- merge(centroids, ref_centroids, by="ID", all.x=T)
centroids$loglights <- log(centroids$mean_lights)
centroids <- rename(centroids, c("x.x" = "x", "y.x" = "y", "mean_lights" = "lights"))
centroids <- centroids[,c("ID", "x", "y", "country", "region", "subregion", "pop", "lights", "loglights", "num_landpixels", "pop_sd", "lights_sd", "rugg", "altitude", "landsuit", "temp", "precip", "growingdays", "malaria", "harbor", "alternative_lights", "lights_raw", "un_code")]

write.csv(centroids, "/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Build/temp/centroids.csv", row.names = FALSE)
