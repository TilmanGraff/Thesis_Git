
# This file merges the rownames from OSRM calculations to the general centroids file for future reference
# It then creates productivity matrices for every country with enough locations
# It also slightly amends the pop_dens variable to account for gridcells that are partially covered by water

centroids <- read.csv("./Build/temp/centroids.csv")
ports = read.csv("./Build/temp/ports.csv")[,c("ID", "isport")]


#########
# A) Merge the rosettastone to get clear rownames
#########

#
# filenames <- list.files(path="./Build/temp/rosettastones", full.names = T)
#
# rosetta <- read.csv(filenames[1]) # initialises
#
# for(fileID in 2:length(filenames)){
#   rosetta <- rbind(rosetta, read.csv(filenames[fileID])) # adds all other rosettastones
# }
#
#
# centroids <- centroids[,!grepl("rownumber", colnames(centroids))] # deletes rownumbers from existing centroids file
#
# centroids <- merge(centroids, rosetta, by="ID", all.x=T) # merges the two
#
# centroids$rownumber <- as.numeric(centroids$rownumber) #cleans data format

###############################
###############################

#########
# B) Adjust pop_dens for area over land
#########




###############################
###############################

#########
# C) Write productivity matrices
#########

# Delete existing matrices

file.remove(list.files(path="./Build/temp/productivities", full.names = T))

# Gather country names
country_table <- as.data.frame(table(centroids[centroids$region ==2, "country"]))
country_names <- paste(country_table[country_table$Freq != 0,"Var1"])

alpha = 0.7 # Production function parameter


#########
# EITHER C1) Productivity by (N-1) biggest cities (as in FS2017)
#########

# Define parameters

N = 6         # Number of goods: 4 to the four biggest domestic grids, 1 rest-of-the-world good to 3 largest abroad borderregions plus all ports, 1 agricultural good.
alpha = 0.7     # Production function parameter



for (country in country_names){
  if(file.exists(paste("./Build/temp/raw_from_OSRM/adj/adj_", country, ".csv", sep=""))){

  centroids = read.csv(paste0("./Build/temp/borderregions/", country, "_borderregion.csv"))
  centroids$pop_dens <- centroids$pop / (centroids$gridarea * centroids$num_landpixels / 900)
  centroids = merge(centroids, ports, by="ID", all.x=T)


  J = nrow(centroids)
  N_eff = min(N, J)

  if(J >= N_eff){

    centroids[, "good_produced"] = 6
    centroids[centroids$abroad == 1, "good_produced"] = ifelse(rank(-centroids[centroids$abroad==1,"pop"]) <= 3, 5, 6)
    centroids[centroids$isport == 1, "good_produced"] = 5

    centroids[, "good_produced_temp"] = NA

    centroids[centroids$abroad==0, "good_produced_temp"] <- rank(-centroids[centroids$abroad == 0, "pop"])

    centroids[which(centroids$good_produced_temp <= 4),"good_produced"] = centroids[which(centroids$good_produced_temp <= 4),"good_produced_temp"]

    #centroids[, "good_produced"] <- pmin(rank(-centroids[, "pop"]), N_eff) # assign goods 1 to (N-1) to the (N-1) biggest locations, and then assign good N to all other locations

    prod <- matrix(0, nrow = J, ncol = N_eff)

    case_centroids <- centroids

    for(i in 1:J){
      prod[i, case_centroids[i, "good_produced"]] <- (case_centroids[i, "lights"] * case_centroids[i, "num_landpixels"]) / (case_centroids[i, "pop"]^alpha) # simple inverse production function
      if(case_centroids[i, "pop"]==0){
        prod[i, case_centroids[i, "good_produced"]] <- 0 # no infinite productivities
      }
    }
    write.csv(prod, file=paste("./Build/temp/productivities/productivities_", country, ".csv", sep=""), row.names = FALSE)
  }

}
}
#########
# OR C2) Defining cities from WorldBank Data and split between urban / non urban
#########

#
# centroids$urban <- 1
#
# # Imports World Bank Data on global Urbanisation Shares
# urb_rates <- read.csv("./Build/input/Worldbank_Urbanisation.csv")
# urb_rates <- urb_rates[,c("Country.Name", "X2016..YR2016.")]
# colnames(urb_rates) <- c("country", "urb_rate")
# urb_rates$urb_rate <- as.numeric(paste(urb_rates$urb_rate))
#
# urb_rates$country <- gsub(" ", "-", urb_rates$country)
# urb_rates$country <- gsub("'", "", urb_rates$country)
#
# # Some data-mining
# urb_rates$country <- gsub("Cabo-Verde", "Cape-Verde", urb_rates$country)
# urb_rates$country <- gsub("Congo,-Rep.", "Congo", urb_rates$country)
# urb_rates$country <- gsub("Congo,-Dem.-Rep.", "Democratic-Republic-of-the-Congo", urb_rates$country)
# urb_rates$country <- gsub("Egypt,-Arab-Rep.", "Egypt", urb_rates$country)
# urb_rates$country <- gsub("Gambia,-The", "Gambia", urb_rates$country)
# urb_rates$country <- gsub("Tanzania", "United-Republic-of-Tanzania", urb_rates$country)
#
# # Now, for every country:
# for(country in country_names){
#
#   # obtain a target urbanisation rate from World Bank Data
#   target_rate <- urb_rates[urb_rates$country == country, "urb_rate"]
#   if(!(country %in% urb_rates$country)){
#     target_rate <- 42.0 # if no data available, use the overall share from Africa (42%) -- three cases
#   }
#   if(is.na(target_rate)){
#     target_rate <- 42.0 # if no data available, use the overall share from Africa (42%) -- three cases
#   }
#
#   target_urb_pop <- (target_rate / 100) * sum(centroids[centroids$country == country, "pop"]) # how much urban population should we have in each country
#   current_urb_pop <- sum(centroids[centroids$country == country & centroids$urban == 1, "pop"]) # starting value of urban pop if every place was urban
#
#
#   i = 1
#   while(current_urb_pop > target_urb_pop){ # now, as long as this is too high, gradually drop the least densely populated regions first
#     centroids[centroids$country == country, "urban"] <- centroids[centroids$country == country, "pop_dens"] > quantile(centroids[centroids$country == country, "pop_dens"], i/nrow(centroids[centroids$country == country,]))
#
#     # with these dropped, compare anew
#     current_urb_pop <- sum(centroids[centroids$country == country & centroids$urban == 1, "pop"])
#
#     # if it's still too high, drop a further percentile of density
#     i = i+1
#
#   } # otherwise end the iterations
#
#
#   # Second step: Assign productivity levels to urban vs non-urban regions
#
#   J = nrow(centroids[centroids$country==country, ])
#   prod <- matrix(0, nrow = J, ncol = 2) # and safe them in a productivity matrix
#   case_centroids <- centroids[centroids$country == country, ]
#
#   for(i in 1:J){
#     if(case_centroids[i, "urban"]==1){
#       prod[case_centroids[i, "rownumber"], 2] <- (case_centroids[i, "lights"] * case_centroids[i, "num_landpixels"]) / (case_centroids[i, "pop"]^alpha) # simple inverse production function
#     } else{
#       prod[case_centroids[i, "rownumber"], 1] <- (case_centroids[i, "lights"] * case_centroids[i, "num_landpixels"]) / (case_centroids[i, "pop"]^alpha) # simple inverse production function
#     }
#     if(case_centroids[i, "pop"]==0){
#       prod[case_centroids[i, "rownumber"], ] <- 0 # no infinite productivities
#     }
#
#   }
#
#   write.csv(prod, file=paste("./Build/temp/productivities/productivities_", country, ".csv", sep=""), row.names = FALSE)
#
# }

# Note that I have only done rownumbers for countries which have undergone the OSRM treatment. Hence, these are for now only the African countries. Centroids are still for entire world, but only the African ones do have rownumbers.




# write.csv(format(centroids, scientific=F), "./Build/temp/centroids.csv", row.names = FALSE)
