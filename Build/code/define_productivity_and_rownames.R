
# This file merges the rownames from OSRM calculations to the general centroids file for future reference
# It then creates productivity matrices for every country with enough locations
# It also slightly amends the pop_dens variable to account for gridcells that are partially covered by water



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
# B) Adjust pop_dens for area over land
#########

centroids_merged$pop_dens <- centroids_merged$pop / (centroids_merged$gridarea * centroids_merged$num_landpixels / 900)



###############################
###############################

#########
# C) Write productivity matrices
#########

# Delete existing matrices

file.remove(list.files(path="/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Build/temp/productivities", full.names = T))

# Gather country names
country_table <- as.data.frame(table(centroids[centroids$region ==2, "country"]))
country_names <- paste(country_table[country_table$Freq != 0,"Var1"])

alpha = 0.7 # Production function parameter


#########
    # EITHER C1) Productivity by (N-1) biggest cities (as in FS2017)
#########

# Define parameters
#
# N = 3         # Number of goods
# alpha = 0.7     # Production function parameter
#
#
# centroids_merged$good_produced <- NA
#
# for (country in country_names){
#
#   J = nrow(centroids_merged[centroids_merged$country==country, ])
#
#   if(J >= N){
#     centroids_merged[centroids_merged$country==country, "good_produced"] <- pmin(rank(-centroids_merged[centroids_merged$country==country, "pop"]), N) # assign goods 1 to (N-1) to the (N-1) biggest locations, and then assign good N to all other locations
#
#     prod <- matrix(0, nrow = J, ncol = N)
#
#     case_centroids <- centroids_merged[centroids_merged$country == country, ]
#
#     for(i in 1:J){
#       prod[case_centroids[i, "rownumber"], case_centroids[i, "good_produced"]] <- (case_centroids[i, "lights"] * case_centroids[i, "num_landpixels"]) / (case_centroids[i, "pop"]^alpha) # simple inverse production function
#       if(case_centroids[i, "pop"]==0){
#         prod[case_centroids[i, "rownumber"], case_centroids[i, "good_produced"]] <- 0 # no infinite productivities
#       }
#     }
#     write.csv(prod, file=paste("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Build/temp/productivities/productivities_", country, ".csv", sep=""), row.names = FALSE)
#   }
#
# }

#########
    # OR C2) Defining cities from WorldBank Data and split between urban / non urban
#########


centroids_merged$urban <- 1

# Imports World Bank Data on global Urbanisation Shares
urb_rates <- read.csv("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Build/input/Worldbank_Urbanisation.csv")
urb_rates <- urb_rates[,c("Country.Name", "X2016..YR2016.")]
colnames(urb_rates) <- c("country", "urb_rate")
urb_rates$urb_rate <- as.numeric(paste(urb_rates$urb_rate))

urb_rates$country <- gsub(" ", "-", urb_rates$country)
urb_rates$country <- gsub("'", "", urb_rates$country)

# Some data-mining
urb_rates$country <- gsub("Cabo-Verde", "Cape-Verde", urb_rates$country)
urb_rates$country <- gsub("Congo,-Rep.", "Congo", urb_rates$country)
urb_rates$country <- gsub("Congo,-Dem.-Rep.", "Democratic-Republic-of-the-Congo", urb_rates$country)
urb_rates$country <- gsub("Egypt,-Arab-Rep.", "Egypt", urb_rates$country)
urb_rates$country <- gsub("Gambia,-The", "Gambia", urb_rates$country)
urb_rates$country <- gsub("Tanzania", "United-Republic-of-Tanzania", urb_rates$country)

# Now, for every country:
for(country in country_names){

  # obtain a target urbanisation rate from World Bank Data
  target_rate <- urb_rates[urb_rates$country == country, "urb_rate"]
  if(!(country %in% urb_rates$country)){
    target_rate <- 42.0 # if no data available, use the overall share from Africa (42%) -- three cases
  }
  if(is.na(target_rate)){
    target_rate <- 42.0 # if no data available, use the overall share from Africa (42%) -- three cases
  }

  target_urb_pop <- (target_rate / 100) * sum(centroids_merged[centroids_merged$country == country, "pop"]) # how much urban population should we have in each country
  current_urb_pop <- sum(centroids_merged[centroids_merged$country == country & centroids_merged$urban == 1, "pop"]) # starting value of urban pop if every place was urban


  i = 1
  while(current_urb_pop > target_urb_pop){ # now, as long as this is too high, gradually drop the least densely populated regions first
    centroids_merged[centroids_merged$country == country, "urban"] <- centroids_merged[centroids_merged$country == country, "pop_dens"] > quantile(centroids_merged[centroids_merged$country == country, "pop_dens"], i/nrow(centroids_merged[centroids_merged$country == country,]))

    # with these dropped, compare anew
    current_urb_pop <- sum(centroids_merged[centroids_merged$country == country & centroids_merged$urban == 1, "pop"])

    # if it's still too high, drop a further percentile of density
    i = i+1

  } # otherwise end the iterations


  # Second step: Assign productivity levels to urban vs non-urban regions

  J = nrow(centroids_merged[centroids_merged$country==country, ])
  prod <- matrix(0, nrow = J, ncol = 2) # and safe them in a productivity matrix
  case_centroids <- centroids_merged[centroids_merged$country == country, ]

  for(i in 1:J){
    if(case_centroids[i, "urban"]==1){
      prod[case_centroids[i, "rownumber"], 2] <- (case_centroids[i, "lights"] * case_centroids[i, "num_landpixels"]) / (case_centroids[i, "pop"]^alpha) # simple inverse production function
    } else{
      prod[case_centroids[i, "rownumber"], 1] <- (case_centroids[i, "lights"] * case_centroids[i, "num_landpixels"]) / (case_centroids[i, "pop"]^alpha) # simple inverse production function
    }
    if(case_centroids[i, "pop"]==0){
      prod[case_centroids[i, "rownumber"], ] <- 0 # no infinite productivities
    }

  }

  write.csv(prod, file=paste("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Build/temp/productivities/productivities_", country, ".csv", sep=""), row.names = FALSE)

}

# Note that I have only done rownumbers for countries which have undergone the OSRM treatment. Hence, these are for now only the African countries. Centroids are still for entire world, but only the African ones do have rownumbers.




write.csv(format(centroids_merged, scientific=F), "/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Build/temp/centroids.csv", row.names = FALSE)
