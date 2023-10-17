
# Creating regions of each country including border regions of the countries around them
# Note that I define a "border region" as containing an entire country PLUS its surrioundings. So even the heartland of a country is part of that country's border region.

require("osrm")
require("Imap")
require("gepaf")
require("rgeos")
require("raster")

# Import
centroids <- read.csv("./Build/temp/centroids.csv")

# I create blank borderregion vectors, these will collect IDs of cells that are in their border region
for(un_code in unique(centroids$un_code)){

  assign(paste0("borderregion_", un_code), vector())

}

# Looping over centroids
for(id in centroids$ID){

  thiscentroid = centroids[centroids$ID==id,]
  region = centroids[(centroids$x - thiscentroid$x)^2 + (centroids$y - thiscentroid$y)^2 <= 1.45,] # finding all centroids within 1.45 degrees around them. This cut off is chosen to include two grid cells diagonal but not three grid cells straight in the vicinity

  for(un_code in unique(region$un_code)){ # I see which countries are represented in this larger vicinity. The idea here is to realise that this is symmetric. If gridcell A from Angola has gridcell B from Namibia in its vicinity, that means that gridcell A should be added to the borderregions of Angola and Namibia.

    x = c(get(paste0("borderregion_", un_code)), id) # x is the borderregion vector
    assign(paste0("borderregion_", un_code), as.numeric(x)) # append the vector by current id and assign back to object

  }


}

# from just a vector collection of ids, merge in lat-lons etc to get working datasets for adj-dist-spead

for(un_code in unique(centroids$un_code)){

  df1 = data.frame("ID" = get(paste0("borderregion_", un_code)))
  df = merge(df1, centroids, by="ID", all.x = T)
  country = paste(centroids[centroids$un_code==un_code,"country"][1])
  df$abroad = as.numeric(df$un_code != un_code)

  write.csv(df, file=paste0("./Build/temp/borderregions/", country, "_borderregion.csv"))

}

#! for the extra regions:


# Import
centroids <- read.csv("./Build/temp/excentroids.csv")


# I create blank borderregion vectors, these will collect IDs of cells that are in their border region
for(un_code in unique(centroids$un_code)){

  assign(paste0("borderregion_", un_code), vector())

}

# Looping over centroids
for(id in centroids$ID){

  thiscentroid = centroids[centroids$ID==id,]
  region = centroids[(centroids$x - thiscentroid$x)^2 + (centroids$y - thiscentroid$y)^2 <= 1.45,] # finding all centroids within 1.45 degrees around them. This cut off is chosen to include two grid cells diagonal but not three grid cells straight in the vicinity

  for(un_code in unique(region$un_code)){ # I see which countries are represented in this larger vicinity. The idea here is to realise that this is symmetric. If gridcell A from Angola has gridcell B from Namibia in its vicinity, that means that gridcell A should be added to the borderregions of Angola and Namibia.

    x = c(get(paste0("borderregion_", un_code)), id) # x is the borderregion vector
    assign(paste0("borderregion_", un_code), as.numeric(x)) # append the vector by current id and assign back to object

  }


}

# from just a vector collection of ids, merge in lat-lons etc to get working datasets for adj-dist-spead

for(un_code in unique(centroids$un_code)){

  df1 = data.frame("ID" = get(paste0("borderregion_", un_code)))
  df = merge(df1, centroids, by="ID", all.x = T)
  country = paste(centroids[centroids$un_code==un_code,"country"][1])
  df$abroad = as.numeric(df$un_code != un_code)

  write.csv(df, file=paste0("./Build/temp/borderregions/", country, "_borderregion.csv"))

}


