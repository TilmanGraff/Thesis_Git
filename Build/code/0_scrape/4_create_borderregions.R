
# Creating regions of each country including border regions of the countries around them

require("osrm")
require("Imap")
require("gepaf")
require("rgeos")
require("raster")

centroids <- read.csv("./Build/temp/centroids.csv")

for(un_code in unique(centroids$un_code)){

  assign(paste0("borderregion_", un_code), vector())

}

for(id in centroids$ID){

  thiscentroid = centroids[centroids$ID==id,]
  region = centroids[(centroids$x - thiscentroid$x)^2 + (centroids$y - thiscentroid$y)^2 <= 1.45,]

  for(un_code in unique(region$un_code)){

    x = c(get(paste0("borderregion_", un_code)), id)
    assign(paste0("borderregion_", un_code), as.numeric(x))

  }


}

for(un_code in unique(centroids$un_code)){

  df1 = data.frame("ID" = get(paste0("borderregion_", un_code)))
  df = merge(df1, centroids, by="ID", all.x = T)
  country = paste(centroids[centroids$un_code==un_code,"country"][1])

  write.csv(df, file=paste0("./Build/temp/borderregions/", country, "_borderregion.csv"))

}
