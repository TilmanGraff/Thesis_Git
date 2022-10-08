# This file solely has the purpose to minimally perturb those few aid coordinates which are exactly on the border between two grid cells. As I only want to do this once (to have consistent results), I do this in a separate file:


# WORLDBANK
###########
aid_loc <- read.csv("./Analysis/input/AidData_WorldBank/data/locations.csv")
aid_loc <- aid_loc[grepl("Africa", aid_loc$gazetteer_adm_name),]

for(i in 1:nrow(aid_loc)){
  if(aid_loc$latitude[i] %% 0.5 ==0){
    aid_loc$latitude[i] <- aid_loc$latitude[i] + max(min(rnorm(1) / 100, 0.5), -0.5)
  }

  if(aid_loc$longitude[i] %% 0.5 ==0){
    aid_loc$longitude[i] <- aid_loc$longitude[i] + max(min(rnorm(1) / 100, 0.5), -0.5)
  }

}


write.csv(aid_loc, file="/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/input/AidData_WorldBank/data/locations_perturbed.csv", row.names = FALSE)

# CHINA
###########

aid_loc_China <- read.csv("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/input/AidData_China/locations.csv", sep=";")
aid_loc_China$latitude <- as.numeric(gsub("\\,", ".", aid_loc_China$latitude))
aid_loc_China$longitude <- as.numeric(gsub("\\,", ".", aid_loc_China$longitude))


for(i in 1:nrow(aid_loc_China)){
  if(aid_loc_China$latitude[i] %% 0.5 ==0){
    aid_loc_China$latitude[i] <- aid_loc_China$latitude[i] + max(min(rnorm(1) / 100, 0.5), -0.5)
  }

  if(aid_loc_China$longitude[i] %% 0.5 ==0){
    aid_loc_China$longitude[i] <- aid_loc_China$longitude[i] + max(min(rnorm(1) / 100, 0.5), -0.5)
  }

}

write.csv(aid_loc_China, file="/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/input/AidData_China/locations_perturbed.csv", row.names = FALSE)
