
# Cleans up the untidy centroids_raw.csv file, which came straight out of the messes of manual QGIS
#########

require("plyr")
require("geosphere")

henderson <- read.csv("/Users/tilmangraff/Dropbox (Harvard University)/spin-inputs/input/Henderson_et_al_2018/data1_regs_only_lights.csv")


centroidsraw <- read.csv("/Users/tilmangraff/Dropbox (Harvard University)/spin-inputs/input/Centroids_raw.csv")

centroidsraw <- as.data.frame(centroidsraw)
centroidsraw$x <- 0.5*(centroidsraw$left + centroidsraw$right)
centroidsraw$y <- 0.5*(centroidsraw$top + centroidsraw$bottom)


extra_countries = c("Germany", "Japan", "China", "United States")
big_countries = c("China", "United-States")


# cut out alaska and hawaii
centroidsraw = centroidsraw[centroidsraw$x > -125,]
centroidsraw$x_alt = centroidsraw$x + runif(1, -0.1, 0.1)
centroidsraw$y_alt = centroidsraw$y + runif(1, -0.1, 0.1)

centroidsraw <- rename(centroidsraw, c("Pop_Sum" = "pop", "Pop_sdv" = "pop_sd", "Lights_sum" = "lights_raw", "Lights_sdv" = "lights_sd", "meanrad_no" = "alternative_lights", "meanrugged" = "rugg", "meanelv" = "altitude", "meanlandsu" = "landsuit", "meantmp" = "temp", "meanprecip" = "precip", "meanharbor" = "harbor", "meangrowda" = "growingdays", "meanmalari" = "malaria", "UN" = "un_code", "NAME" = "country", "REGION" = "region", "SUBREGION" = "subregion", "meannum_la" = "num_landpixels"))

centroidsraw = centroidsraw[,c("country", "x_alt", "y_alt", "x", "y", "top", "left", "pop", "rugg", "num_landpixels")]

# This defines unique cell IDs
centroidsraw$ID <- as.numeric(rownames(centroidsraw))


centroids <- centroidsraw[centroidsraw$country %in% extra_countries,]

# This tidies up country names
centroids$country <- gsub(" ", "-", centroids$country)
centroids$country <- gsub("'", "", centroids$country)


#* now, create border regions

for(c in unique(centroids$country)){

tcentraw = centroids[centroids$country == c,]
borderregion = tcentraw

for(i in 1:nrow(tcentraw)){
    thisone = tcentraw[i,]

    region = centroidsraw[(centroidsraw$x - thisone$x)^2 + (centroidsraw$y - thisone$y)^2 <= 1.45 & !(centroidsraw$ID %in% borderregion$ID) ,]
    if(nrow(region) > 0){
        borderregion = rbind(borderregion, region)
    }

}

borderregion$lights = NA

if(c %in% big_countries){
    big_region = borderregion[borderregion$left %% 1 == 0 & borderregion$top %% 1 == 0,]
    for(i in 1:nrow(big_region)){
        thisone = big_region[i,]

        big_region[i,"lights"] = mean(exp(henderson[abs(henderson$y_alt - thisone$y) <= 0.5 & abs(henderson$x_alt - thisone$x) <=0.5,"lrad2010clip_pl_c"]))

        nghbs = centroidsraw[abs(centroidsraw$x_alt - thisone$x) <= 0.5 & abs(centroidsraw$y_alt - thisone$y) <= 0.5,]
        nghbs[is.na(nghbs$num_landpixels),"num_landpixels"] = 0
        
        big_region[i,"rugg"] = mean(nghbs$rugg, na.rm = T)
        big_region[i,"pop"] = sum(nghbs$pop)
        big_region[i,"num_landpixels"] = sum(nghbs$num_landpixels)
        big_region[i,"gridarea"] = areaPolygon(cbind(c(thisone$left, thisone$left+1, thisone$left+1, thisone$left), c(thisone$top, thisone$top, thisone$top-1, thisone$top-1))) / (1000^2)

    }

    big_region$abroad = as.numeric(big_region$country != c)
    big_region = big_region[!is.na(big_region$rugg) & !is.na(big_region$lights),]

      write.csv(big_region, file=paste0("./Build/temp/borderregions/", c, "_borderregion.csv"))


} else{

    for(i in 1:nrow(borderregion)){
        thisone = borderregion[i,]

        borderregion[i,"lights"] = mean(exp(henderson[abs(henderson$y_alt - thisone$y) <= 0.25 & abs(henderson$x_alt - thisone$x) <=0.25,"lrad2010clip_pl_c"]))
        borderregion[i,"gridarea"] = areaPolygon(cbind(c(thisone$left, thisone$left+1, thisone$left+1, thisone$left), c(thisone$top, thisone$top, thisone$top-1, thisone$top-1))) / (1000^2)

    }

    borderregion$abroad = as.numeric(borderregion$country != c)
    borderregion = borderregion[!is.na(borderregion$rugg) & !is.na(borderregion$lights),]

    write.csv(borderregion, file=paste0("./Build/temp/borderregions/", c, "_borderregion.csv"))

}

print(c)

}


