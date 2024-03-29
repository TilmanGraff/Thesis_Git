
# Goes from wrong_lights to correct_lights by replacing the old geographical centroids lights data

require("plyr")


henderson <- read.csv("/Users/tilmangraff/Dropbox (Harvard University)/spin-inputs/input/Henderson_et_al_2018/data1_regs_only_lights.csv")
centroids <- read.csv("./Build/temp/centroids_wrong_lights.csv")


henderson <- henderson[,c("x", "y", "lrad2010clip_pl_c")]

henderson <- rename(henderson, c("lrad2010clip_pl_c" = "loglights"))
henderson$lights <- exp(henderson$loglights)

centroids <- read.csv("./Build/temp/centroids_wrong_lights.csv")

ref_centroids <- centroids[,c("ID", "x", "y")]

henderson <- merge(henderson, ref_centroids, by=c("x", "y"), all.x=T)

ref_centroids$mean_lights <- NA

for(id in ref_centroids$ID){

  henderson[ref_centroids[ref_centroids$ID == id, "x"]-henderson[, "x"] <= 0.25 & ref_centroids[ref_centroids$ID == id, "x"]-henderson[, "x"] >=0 & ref_centroids[ref_centroids$ID == id, "y"]-henderson[, "y"] <= 0.25 & ref_centroids[ref_centroids$ID == id, "y"]-henderson[, "y"] >= 0,"ID"] <- id

}

for(id in ref_centroids$ID){
  ref_centroids[ref_centroids$ID == id, "mean_lights"] <- mean(henderson[henderson$ID == id & !is.na(henderson$ID),"lights"])
}

# Drop all cells with NaN henderson lights (80% of these are over greenland -- still, this drops about 1000 observations!)
ref_centroids[ref_centroids$mean_lights=="NaN","mean_lights"] <- NA
ref_centroids <- ref_centroids[!is.na(ref_centroids$mean_lights),]


centroids <- merge(centroids, ref_centroids, by="ID", all.x=T)
centroids$loglights <- log(centroids$mean_lights)
centroids <- rename(centroids, c("x.x" = "x", "y.x" = "y", "mean_lights" = "lights"))
centroids <- centroids[,c("ID", "x", "y", "country", "region", "subregion", "pop", "lights", "num_landpixels", "gridarea", "pop_dens", "pop_sd", "lights_sd", "rugg", "altitude", "landsuit", "temp", "precip", "growingdays", "malaria", "harbor", "alternative_lights", "lights_raw", "un_code")]

centroids <- centroids[!is.na(centroids$lights),]

# I now prevent any circuliarity by calling this centroids_noSSudan
#write.csv(format(centroids, scientific=F), "./Build/temp/centroids_noSSudan.csv", row.names = FALSE)


#! extra countries 


henderson <- read.csv("/Users/tilmangraff/Dropbox (Harvard University)/spin-inputs/input/Henderson_et_al_2018/data1_regs_only_lights.csv")
centroids <- read.csv("./Build/temp/excents_wrong_lights.csv")


henderson <- henderson[,c("x", "y", "lrad2010clip_pl_c")]

henderson <- rename(henderson, c("lrad2010clip_pl_c" = "loglights"))
henderson$lights <- exp(henderson$loglights)

centroids <- read.csv("./Build/temp/excents_wrong_lights.csv")

ref_centroids <- centroids[,c("ID", "x", "y", "country")]

#henderson <- merge(henderson, ref_centroids, by=c("x", "y"), all.x=T)

ref_centroids$mean_lights <- NA

for(i in 1:nrow(ref_centroids)){

  # if(ref_centroids[i,"country"] %in% c("United-States", "China")){
nghbs = henderson[ref_centroids[i, "x"]-henderson[, "x"] <= 0.25 & ref_centroids[i, "x"]-henderson[, "x"] >=0 & ref_centroids[i, "y"]-henderson[, "y"] <= 0.25 & ref_centroids[i, "y"]-henderson[, "y"] >= 0,]
  # } else{
  #   nghbs = henderson[ref_centroids[i, "x"]-henderson[, "x"] <= 0.25 & ref_centroids[i, "x"]-henderson[, "x"] >=0 & ref_centroids[i, "y"]-henderson[, "y"] <= 0.25 & ref_centroids[i, "y"]-henderson[, "y"] >= 0,]
  
  # }
  
  ref_centroids[i, "mean_lights"] = mean(nghbs$lights, na.rm = T)

}

# Drop all cells with NaN henderson lights (80% of these are over greenland -- still, this drops about 1000 observations!)
ref_centroids[ref_centroids$mean_lights=="NaN","mean_lights"] <- NA
ref_centroids <- ref_centroids[!is.na(ref_centroids$mean_lights),]


centroids <- merge(centroids, ref_centroids, by="ID", all.x=T)
centroids$loglights <- log(centroids$mean_lights)
centroids <- rename(centroids, c("x.x" = "x", "y.x" = "y", "mean_lights" = "lights", "country.x" = "country"))
centroids <- centroids[,c("ID", "x", "y", "country", "region", "subregion", "pop", "lights", "num_landpixels", "gridarea", "pop_dens", "pop_sd", "lights_sd", "rugg", "altitude", "landsuit", "temp", "precip", "growingdays", "malaria", "harbor", "alternative_lights", "lights_raw", "un_code")]

centroids <- centroids[!is.na(centroids$lights),]

centroids = centroids[centroids$x > -125,]
write.csv(format(centroids, scientific=F), "./Build/temp/excentroids.csv", row.names = FALSE)