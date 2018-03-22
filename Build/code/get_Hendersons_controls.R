
henderson <- read.csv("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Build/input/Henderson_et_al_2018/data1_regs_only_biomes.csv")

opt_loc <- read.csv("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/temp/opt_loc_with_raildist.csv")
ref_centroids <- opt_loc[,c("ID", "x", "y")]

#henderson <- merge(henderson, ref_centroids, by=c("x", "y"), all.x=T)

for(id in ref_centroids$ID){

  henderson[ref_centroids[ref_centroids$ID == id, "x"]-henderson[, "x"] <= 0.25 & ref_centroids[ref_centroids$ID == id, "x"]-henderson[, "x"] >=0 & ref_centroids[ref_centroids$ID == id, "y"]-henderson[, "y"] <= 0.25 & ref_centroids[ref_centroids$ID == id, "y"]-henderson[, "y"] >= 0,"ID"] <- id

}

for(var in colnames(henderson)[grepl("bio", colnames(henderson)) | grepl("distc", colnames(henderson)) | grepl("25", colnames(henderson))]){
  if(!(var  %in% c("biomes2", "biomes3", "biomes7", "biomes9"))){
  ref_centroids[, paste(var)] <- NA

    for(id in ref_centroids$ID){
      ref_centroids[ref_centroids$ID == id, paste(var)] <- (mean(henderson[henderson$ID == id & !is.na(henderson$ID),paste(var)]))
    }
  }
}

for(i in 1:nrow(ref_centroids)){
  if(0.5 %in% ref_centroids[i,grepl("bio", colnames(ref_centroids))]){
    j=1
    while(j<20){
      if(ref_centroids[i,grepl("bio", colnames(ref_centroids))][j] == 0.5){
        ref_centroids[i,grepl("bio", colnames(ref_centroids))][j] <- 1
        j <- 21
      } else{
        j <- j+1
      }
    }
  }
}

ref_centroids[,grepl("bio", colnames(ref_centroids)) | grepl("25", colnames(ref_centroids))] <- round(ref_centroids[,grepl("bio", colnames(ref_centroids)) | grepl("25", colnames(ref_centroids))])


write.csv(ref_centroids[,grepl("bio", colnames(ref_centroids)) | grepl("distc", colnames(ref_centroids)) | grepl("25", colnames(ref_centroids)) | grepl("ID", colnames(ref_centroids))], "/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/temp/henderson_controls.csv", row.names = FALSE)
