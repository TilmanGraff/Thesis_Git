
# This file merges the rownames from OSRM calculations to the general centroids file for future reference
# It then creates productivity matrices for every country with enough locations



centroids <- read.csv("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Build/temp/centroids.csv")
centroids <- centroids[centroids$region==2,]

#########
# A) Merge the rosettastone to get clear rownames
#########


filenames <- list.files(path="/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Build/temp/rosettastones", full.names = T)

rosetta <- read.csv(filenames[1]) # initialises

for(fileID in 2:length(filenames)){
  rosetta <- rbind(rosetta, read.csv(filenames[fileID])) # adds all other rosettastones
}


centroids <- centroids[,!grepl("rownumber", colnames(centroids))] # deletes rownumbers from existing centroids file

centroids_merged <- merge(centroids, rosetta, by="ID", all.x=T) # merges the two

centroids_merged$rownumber <- as.numeric(centroids_merged$rownumber) #cleans data format

###############################
###############################

#########
# B) Write productivity matrices
#########

# Delete existing matrices

file.remove(list.files(path="/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Build/temp/productivities", full.names = T))

# Define parameters

N = 3         # Number of goods
alpha = 0.7     # Production function parameter


# Gather country names
country_table <- as.data.frame(table(centroids[centroids$region ==2, "country"]))
country_names <- paste(country_table[country_table$Freq != 0,"Var1"])

centroids_merged$good_produced <- NA

for (country in country_names){

  J = nrow(centroids_merged[centroids_merged$country==country, ])

  if(J >= N){
    centroids_merged[centroids_merged$country==country, "good_produced"] <- pmin(rank(-centroids_merged[centroids_merged$country==country, "pop"]), N) # assign goods 1 to (N-1) to the (N-1) biggest locations, and then assign good N to all other locations

    prod <- matrix(0, nrow = J, ncol = N)

    case_centroids <- centroids_merged[centroids_merged$country == country, ]

    for(i in 1:J){
      prod[case_centroids[i, "rownumber"], case_centroids[i, "good_produced"]] <- (case_centroids[i, "lights"] * case_centroids[i, "num_landpixels"]) / (case_centroids[i, "pop"]^alpha) # simple inverse production function
      if(case_centroids[i, "pop"]==0){
        prod[case_centroids[i, "rownumber"], case_centroids[i, "good_produced"]] <- 0 # no infinite productivities
      }
    }
    write.csv(prod, file=paste("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Build/temp/productivities/productivities_", country, ".csv", sep=""), row.names = FALSE)
  }

}

# Note that I have only done rownumbers for countries which have undergone the OSRM treatment. Hence, these are for now only the African countries. Centroids are still for entire world, but only the African ones do have rownumbers.

write.csv(format(centroids_merged, scientific=F), "/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Build/temp/centroids.csv", row.names = FALSE)
