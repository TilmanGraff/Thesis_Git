# This file

require("osrm")
require("Imap")
require("gepaf")
require("rgeos")
require("rgdal")
require("scales")
require("viridis")
require("classInt")


setwd("/Users/tilmangraff/Documents/GitHub/Thesis_Git")


grid <- readOGR("./Analysis/input/grid_shapefile/grid.shp")
opt_loc <- read.csv("./Analysis/input/opt_loc.csv")

grid@data <- merge(grid@data, opt_loc, by = "ID")


delete_all <- 0

if (delete_all == 1) {
  file.remove(list.files("./Build/output/R_maps/", full.names = T))
}

centroids <- read.csv("./Build/temp/centroids.csv")

# Gather country names
countries = read.csv("./Build/temp/country_names.csv")

polys = readOGR("/Users/tilmangraff/Dropbox (Harvard University)/spin-inputs/input/World_Countries/TM_WORLD_BORDERS-0.3.shp")
polys$NAME = gsub(" ", "-", polys$NAME)

###############

df <- read.csv("./Analysis/input/opt_loc.csv")
palette <- colorRamp(c("blue4", "blue", "gold1", "darkorange1"))

zeta_to_col <- function(zeta) {
  # zeta_scaled = (zeta - min(df$zeta, na.rm=T)) / (max(df$zeta, na.rm=T) - min(df$zeta, na.rm=T))
  rgbvector <- palette(ecdf(df$zeta)(zeta))
  col <- rgb(rgbvector[1, 1], rgbvector[1, 2], rgbvector[1, 3], maxColorValue = 255)
  return(col)
}






# For every country, calculate matrices

runs <- data.frame("names" = c("base_old", "mob", "imm"), "paths" = c("2023-07-17_111951_base", "2024-02-18_040636_mobile_fp_final", "2024-02-20_140808_imm_fp_final"))
extra_countries = c("United-States", "Japan", "China", "Germany")

for (path in runs$paths) {

  # try again with the universal palette
if(!grepl("mob", path)){
  palette <- rocket
  breaks_qt <- classIntervals(df$util_opt_imm / df$util_stat_imm, n = 19, style = "quantile")
  br <- breaks_qt$brks
  offs <- 0.0000001
  br[1] <- br[1] - offs
  br[length(br)] <- br[length(br)] + offs
  # grid$zeta_bracket <- cut(df$util_opt_imm / df$util_stat, br)
  mbr <- vector()
  for (i in 1:19) {
    mbr[i] <- mean(c(br[i], br[i + 1]))
  }

  whichlabels <- seq(1, 20, 3)
}else{
  palette <- viridis
  breaks_qt <- classIntervals(df$pop_opt / df$pop_stat, n = 19, style = "quantile", na.rm = T)
  br <- breaks_qt$brks
  offs <- 0.0000001
  br[1] <- br[1] - offs
  br[length(br)] <- br[length(br)] + offs
  # grid$zeta_bracket <- cut(df$util_opt_imm / df$util_stat, br)
  mbr <- vector()
  for (i in 1:19) {
    mbr[i] <- mean(c(br[i], br[i + 1]))
  }

  whichlabels <- seq(1, 20, 3)
}



  outfolder <- paste0("./Build/output/R_maps/", path)
  infolder <- paste0("./Build/output/", path)
  dir.create(outfolder)

  for (country in countries$x) {
    if (paste0(country, "_outcomes.csv") %in% list.files(paste0(infolder, "/Network_outcomes/")) & !(paste0(country, "_opt.pdf") %in% list.files(outfolder))) {

      for(type in c("", "_10p")){
      # Import
      case_poly = polys[polys$NAME == country,]
      I_stat <- read.csv(paste0("./Build/temp/speed/speed_", country, ".csv"), header = T)
      I_opt <- read.csv(paste0(infolder, "/Optimised_Networks/", country, type, ".csv"), header = F)
      outcomes <- read.csv(paste0(infolder, "/Network_outcomes/", country, "_outcomes.csv"))
      rosetta <- read.csv(paste0("./Build/temp/rosettastones/rosetta_", country, ".csv"))
      prod <- read.csv(paste0("./Build/temp/productivities/productivities_", country, ".csv"))

      # Scale
      # I_opt_scaled = 13*(sqrt(I_opt-4) / max(sqrt(I_opt)))
      # I_stat_scaled = 13*(sqrt(I_stat-4) / max(sqrt(I_opt)))

      if(country %in% extra_countries){
        I_opt_scaled <- 5 * ((I_opt - 4)^0.8 / max((I_opt)^0.8))
        I_stat_scaled <- 5 * ((I_stat - 4)^0.8 / max((I_opt)^0.8))
      }else{
        I_opt_scaled <- 11 * ((I_opt - 4)^0.8 / max((I_opt)^0.8))
        I_stat_scaled <- 11 * ((I_stat - 4)^0.8 / max((I_opt)^0.8))
      }
      

      outcomes[outcomes$abroad == 1, "color"] <- alpha("grey", .5)

      outcomes$different_good <- 0
      outcomes[outcomes$ID %in% rosetta[which(rowSums(prod[, -6]) != 0), "ID"], "different_good"] <- 1
if(!grepl("mob", path)){
      outcomes$zeta <- outcomes[,paste0("util_opt", type)] / outcomes$util_stat
      outcomes$pop = read.csv(paste0("./Build/temp/borderregions/", country, "_borderregion", ".csv"))$pop
      outcomes$pop_stat = outcomes$pop
      outcomes$pop_opt = outcomes$pop
      outcomes$pop_opt_10p = outcomes$pop
}else{
      outcomes$zeta <- outcomes[,paste0("pop_opt", type)] / outcomes$pop_stat
     

}
      outcomes$zetacol <- cut(outcomes$zeta, br)

      for (graph in c("stat", "opt")) {
        if(!(graph == "stat" & type == "_10p")){
        # Scale
        if (graph == "opt") {
          outcomes$pop_scaled <- 5 * ((outcomes[,paste0("pop_opt", type)])^0.4 / max((outcomes[outcomes$abroad == 0, "pop_stat"]))^0.4)
          outcomes[outcomes$abroad == 0, "color"] <- palette(20)[outcomes[outcomes$abroad == 0, "zetacol"]]
        } else {
          outcomes$pop_scaled <- 5 * ((outcomes[,paste0("pop_stat")])^0.4 / max((outcomes[outcomes$abroad == 0, "pop_stat"]))^0.4)
          outcomes[outcomes$abroad == 0, "color"] <- "dodgerblue4"
        }

        pdf(file = paste(outfolder, "/", country, "_", graph, type, ".pdf", sep = ""), width = 11, height = 11)

        plot(outcomes$x, outcomes$y, main = country, bty = "n", pch = ifelse(outcomes$abroad == 0, 19, 1), axes = F, ylab = "", xlab = "", asp = 1, type = "n") # if you want to plot it

        points(outcomes[outcomes$abroad == 1, ]$x, outcomes[outcomes$abroad == 1, ]$y, pch = 21, bg = outcomes[outcomes$abroad == 1, ]$color, col = ifelse(outcomes[outcomes$abroad == 1, ]$different_good == 1, "black", NA), cex = outcomes[outcomes$abroad == 1, ]$pop_scaled)

        plot(case_poly, add = T, col = alpha("lightskyblue1", 0.3), border = NA)


        for (i in 1:nrow(outcomes)) {
          converted_i <- rosetta[outcomes[i, "ID"] == rosetta$ID, "rownumber"]
          for (j in i:nrow(outcomes)) {
            converted_j <- rosetta[outcomes[j, "ID"] == rosetta$ID, "rownumber"]
            is_abroad <- outcomes[converted_i, "abroad"] | outcomes[converted_j, "abroad"]
            if (I_stat[converted_i, converted_j] != 0) {
              points(c(outcomes[converted_i, "x"], outcomes[converted_j, "x"]), c(outcomes[converted_i, "y"], outcomes[converted_j, "y"]),
                type = "l", col = ifelse(is_abroad, alpha("grey", .2), alpha("dodgerblue", .8)),
                lwd = get(paste0("I_", graph, "_scaled"))[converted_i, converted_j]
              )
            }
          }
        }

        # points(outcomes$x, outcomes$y, pch = ifelse(outcomes$abroad == 0, 19, 19), col=outcomes$color, cex = outcomes$pop_scaled)
        points(outcomes[outcomes$abroad == 0, ]$x, outcomes[outcomes$abroad == 0, ]$y, pch = 21, bg = outcomes[outcomes$abroad == 0, ]$color, col = ifelse(outcomes[outcomes$abroad == 0, ]$different_good == 1, "white", NA), lwd = 2, cex = outcomes[outcomes$abroad == 0, ]$pop_scaled)

        # adding a color key just for the DRC so we don't crowd the other graphs
        if ((country == "Democratic-Republic-of-the-Congo") & graph == "opt" & type != "_10p") {
          lgd_ <- rep(NA, 19)
          lgd_[whichlabels] <- rev(round(mbr[whichlabels], 3))
          legend(
            x = 12.21455, y = 5.381389,
            legend = lgd_,
            fill = rev(palette(20)),
            border = NA,
            y.intersp = 0.5,
            cex = 1.3, text.font = 1, bty = "n"
          )
        }
        if ((country == "Burkina-Faso") & graph == "opt" & type != "_10p") {
          lgd_ <- rep(NA, 19)
          lgd_[whichlabels] <- rev(round(mbr[whichlabels], 3))
          legend(
            x = -6.61455, y = 15.681389,
            legend = lgd_,
            fill = rev(palette(20)),
            border = NA,
            y.intersp = 0.5,
            cex = 1.3, text.font = 1, bty = "n"
          )
        }

        dev.off()
      }
      }
      }

      print(country)
    }
  }
}
