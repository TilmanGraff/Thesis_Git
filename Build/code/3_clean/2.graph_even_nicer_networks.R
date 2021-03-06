
# This file

require("osrm")
require("Imap")
require("gepaf")
require("rgeos")
require("rgdal")
require("scales")

setwd("/Users/tilmangraff/Documents/GitHub/Thesis_Git")

delete_all = 0

if(delete_all == 1){

  file.remove(list.files("./Build/output/R_maps/", full.names=T))

}

centroids <- read.csv("./Build/temp/centroids.csv")

# Gather country names
country_table <- as.data.frame(table(centroids$country))
country_names <- paste(country_table[country_table$Freq != 0,"Var1"])

polys = readOGR("./Build/output/African Borders/AfricanBorders.shp")

###############

df = read.csv("./Analysis/input/opt_loc.csv")
palette <- colorRamp(c('blue4', 'blue', 'gold1','darkorange1'))

zeta_to_col = function(zeta){
  #zeta_scaled = (zeta - min(df$zeta, na.rm=T)) / (max(df$zeta, na.rm=T) - min(df$zeta, na.rm=T))
  rgbvector = palette(ecdf(df$zeta)(zeta))
  col = rgb(rgbvector[1,1], rgbvector[1,2], rgbvector[1,3], maxColorValue = 255)
  return(col)
}


# For every country, calculate matrices
for(country in country_names){

  if(paste0(country, "_outcomes.csv") %in% list.files("Build/output/Network_outcomes/") & !(paste0(country, "_opt.pdf") %in% list.files("./Build/output/R_maps/"))){

    # Import
    case_poly = polys[polys$NAME == country,]
    I_stat = read.csv(paste0("./Build/temp/speed/speed_", country, ".csv"), header = T)
    I_opt = read.csv(paste0("./Build/output/Optimised_Networks/", country, ".csv"), header = F)
    outcomes = read.csv(paste0("./Build/output/Network_outcomes/", country, "_outcomes.csv"))
    rosetta = read.csv(paste0("./Build/temp/rosettastones/rosetta_", country, ".csv"))
    prod = read.csv(paste0("./Build/temp/productivities/productivities_", country, ".csv"))

    # Scale
    # I_opt_scaled = 13*(sqrt(I_opt-4) / max(sqrt(I_opt)))
    # I_stat_scaled = 13*(sqrt(I_stat-4) / max(sqrt(I_opt)))

    I_opt_scaled = 11*((I_opt-4)^0.8 / max((I_opt)^0.8))
    I_stat_scaled = 11*((I_stat-4)^0.8 / max((I_opt)^0.8))

    outcomes$pop_scaled = 5*((outcomes$pop)^0.4 / max((outcomes[outcomes$abroad == 0, "pop"]))^0.4)
    outcomes[outcomes$abroad == 1,"color"] = alpha("grey", .5)

    outcomes$different_good = 0
    outcomes[outcomes$ID %in% rosetta[which(rowSums(prod[,-6]) != 0),"ID"],"different_good"] = 1

    for(graph in c("stat", "opt")){

    # Scale
    if(graph == "opt"){
      outcomes[outcomes$abroad == 0,"color"] = sapply(outcomes[outcomes$abroad == 0,"c_opt"]/outcomes[outcomes$abroad == 0,"c_stat"], zeta_to_col)
    }else{
      outcomes[outcomes$abroad == 0,"color"] = "dodgerblue4"
    }

    pdf(file=paste("./Build/output/R_maps/", country, "_", graph, ".pdf", sep=""), width = 11, height = 11)

    plot(outcomes$x, outcomes$y, main=country, bty = "n", pch = ifelse(outcomes$abroad == 0, 19, 1), axes=F, ylab="", xlab="", asp=1, type = "n") # if you want to plot it

    points(outcomes[outcomes$abroad==1,]$x, outcomes[outcomes$abroad==1,]$y, pch =21, bg=outcomes[outcomes$abroad==1,]$color, col=ifelse(outcomes[outcomes$abroad==1,]$different_good==1,"black", NA), cex = outcomes[outcomes$abroad==1,]$pop_scaled)

    plot(case_poly, add=T, col = alpha("lightskyblue1", 1), border = NA)


    for(i in 1:nrow(outcomes)){
      converted_i = rosetta[outcomes[i,"ID"] == rosetta$ID,"rownumber"]
      for(j in i:nrow(outcomes)){
        converted_j = rosetta[outcomes[j,"ID"] == rosetta$ID,"rownumber"]
        is_abroad = outcomes[converted_i,"abroad"] | outcomes[converted_j,"abroad"]
        if(I_stat[converted_i,converted_j] != 0){
          points(c(outcomes[converted_i,"x"], outcomes[converted_j,"x"]), c(outcomes[converted_i,"y"], outcomes[converted_j,"y"]), type = "l", col = ifelse(is_abroad, alpha("grey", .2), alpha("dodgerblue", .8)),
          lwd = get(paste0("I_", graph, "_scaled"))[converted_i, converted_j])
        }
      }
    }

    #points(outcomes$x, outcomes$y, pch = ifelse(outcomes$abroad == 0, 19, 19), col=outcomes$color, cex = outcomes$pop_scaled)
    points(outcomes[outcomes$abroad==0,]$x, outcomes[outcomes$abroad==0,]$y, pch =21, bg=outcomes[outcomes$abroad==0,]$color, col=ifelse(outcomes[outcomes$abroad==0,]$different_good==1,"white",NA), lwd=2, cex = outcomes[outcomes$abroad==0,]$pop_scaled)

    dev.off()



}

print(country)

}

}
