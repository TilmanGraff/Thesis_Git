
library("rgdal", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("geosphere", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("raster", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("rgeos", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("Imap", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
library("gepaf", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("vegan", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("readstata13", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library(RColorBrewer)
library(classInt)

# Loading in data
opt_loc <- read.csv("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/input/opt_loc.csv")

ethn_all <- readOGR("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/input/Murdock_EA_2011_vkZ/Murdock_EA_2011_vkZ.shp")

ethn <- ethn_all[which(!grepl("UNINHABITED", ethn_all@data$NAME) & (!is.na(ethn_all@data$TRIBE_CODE)) & (!is.na(ethn_all@data$NAME)) & ethn_all@data$NAME!="<NA>"),]

# tagging ethn_IDs for each grid cell
opt_loc$ethn_ID <- NA

for(i in 1:(length(ethn))){
  merger <- (point.in.polygon(opt_loc$x, opt_loc$y, ethn@polygons[[i]]@Polygons[[1]]@coords[,1], ethn@polygons[[i]]@Polygons[[1]]@coords[,2]))

  opt_loc[merger==1,"ethn_ID"] <- ethn@data[i,"TRIBE_CODE"]
  opt_loc[merger==1,"ethn_NAME"] <- ethn@data[i,"NAME"]
}

# Aggregating over ethnicity-country tuples

opt_ethn <- as.data.frame(unique(opt_loc[,c("country", "ethn_NAME")]))

opt_ethn[,c("tuple_ID", "rugg", "malaria", "pop", "lights", "zeta", "x", "y")] <- NA

opt_ethn$tuple_ID <- 1:nrow(unique(opt_loc[,c("country", "ethn_NAME")]))
row.names(opt_ethn) <- opt_ethn$tuple_ID

for(i in 1:nrow(opt_ethn)){

  subset <- opt_loc[opt_loc$ethn_NAME == opt_ethn$ethn_NAME[i] & opt_loc$country == opt_ethn$country[i] & !is.na(opt_loc$ID), ]
  subset <- subset[!is.na(subset$rownumber),]
  opt_ethn[i, "pop"] <- sum(subset$pop, na.rm=T)
  opt_ethn[i, "rugg"] <- mean(subset$rugg, na.rm=T)
  opt_ethn[i, "malaria"] <- mean(subset$malaria, na.rm=T)
  opt_ethn[i, "lights"] <- mean(subset$lights, na.rm=T)
  opt_ethn[i, "growingdays"] <- mean(subset$growingdays, na.rm=T)
  opt_ethn[i, "temp"] <- mean(subset$temp, na.rm=T)
  opt_ethn[i, "altitude"] <- mean(subset$altitude, na.rm=T)
  opt_ethn[i, "urban"] <- mean(subset$urban, na.rm=T)
  opt_ethn[i, "landsuit"] <- mean(subset$landsuit, na.rm=T)
  opt_ethn[i, "precip"] <- mean(subset$precip, na.rm=T)
  opt_ethn[i, "x"] <- mean(subset$x, na.rm=T)
  opt_ethn[i, "y"] <- mean(subset$y, na.rm=T)
  opt_ethn[i, "first.x"] <- subset[1, "x"]
  opt_ethn[i, "first.y"] <- subset[1, "y"]
  opt_ethn[i, "RailKM"] <- sum(subset$RailKM, na.rm=T)

  if(opt_ethn[i, "pop"] >0 ){
    opt_ethn[i, "zeta_1"] <- sum(subset$pop * subset$zeta, na.rm=T) / opt_ethn[i, "pop"]
  } else{
    opt_ethn[i, "zeta_1"] <- NA
  }


  if(opt_ethn[i, "pop"] >0 ){
    opt_ethn[i, "zeta"] <- (sum(subset$pop * subset$util_opt)  / sum(subset$pop * subset$util_stat))
  } else{
    opt_ethn[i, "zeta"] <- NA
  }

  opt_ethn[i, "strict_mean_zeta"] <- mean(subset$zeta, na.rm=T)

}

opt_ethn <- opt_ethn[!is.na(opt_ethn$ethn_NAME),]


###############
# Michalopoulos EPR data
###############

epr <- read.dta13("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/input/Ethnic_Data_Michalopoulos/epr_main.dta")


# major annoying data mining:

iso <- unique(epr$wbcode)
countryname <- c("Angola", "Burundi", "Benin", "Botswana", "Central-African-Republic", "Cote-dIvoire", "Cameroon", "Democratic-Republic-of-the-Congo", "Congo", "Algeria", "Egypt", "Eritrea", "Ethiopia", "Gabon", "Ghana", "Guinea", "Gambia", "Guinea-Bissau", "Kenya", "Liberia", "Morocco", "Madagascar", "Mali", "Mozambique", "Mauritius", "Malawi", "Namibia", "Niger", "Nigeria", "Rwanda", "Sudan", "Senegal", "Sierra-Leone", "Chad", "Togo", "United-Republic-of-Tanzania", "Uganda", "South-Africa", "Zambia", "Zimbabwe")

epr$country <- NA

for(i in 1:length(iso)){
  epr[epr$wbcode == iso[i], "country"] <- countryname[i]
}

epr_sudan <- epr[epr$country %in% c("South-Sudan", "Sudan"),]

for(i in epr_sudan$name){ # this classifies clearly South Sudan and cleary Sudan ethnicities. I am losing 34 ethnicities who are present on both sides of the border, as the acled did not code this back then.
  if(i %in% opt_ethn[opt_ethn$country=="South-Sudan", "ethn_NAME"] & !(i %in% opt_ethn[opt_ethn$country=="Sudan", "ethn_NAME"])){
    epr[epr$name == i, "country"] <- "South-Sudan"
  } else{
    if(i %in% opt_ethn[opt_ethn$country=="Sudan", "ethn_NAME"] & !(i %in% opt_ethn[opt_ethn$country=="South-Sudan", "ethn_NAME"])){
      epr[epr$name == i, "country"] <- "Sudan"
    } else{
      epr[epr$name == i, "country"] <- NA
    }
  }
}


epr$country <- gsub(" ", "-", epr$country)
epr$country <- gsub("'", "", epr$country)

epr[grepl("dIvoire", epr$country), "country"] <- "Cote-dIvoire"
epr[grepl("Congo,-Dem.", epr$country), "country"] <- "Democratic-Republic-of-the-Congo"
epr[grepl("Congo,-Rep.", epr$country), "country"] <- "Congo"
epr[grepl("Egypt,-Arab-Rep", epr$country), "country"] <- "Egypt"
epr[grepl("Tanzania", epr$country), "country"] <- "United-Republic-of-Tanzania"

epr_merged <- merge(opt_ethn, epr, by.x=c("country", "ethn_NAME"), by.y=c("country", "name"))

write.csv(epr_merged, file="/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/temp/mich_epr.csv", row.names = FALSE)





######################
# Importing ACLED data
######################

acled <- read.dta13("/Users/Tilmanski/Downloads/codes_aer_journal/aer_all2013.dta")

acled_subset <- acled[,c("ccountry", "name", "km2split", "lakedum", "riverdum", "petroleum", "diamondd", "split10pc", "split5pc", "all", "fatal", "allf", "allm", "allmm", "battles", "vio", "riots", "govt", "civilians", "rebels", "mil", "external", "intervention", "outside", "dur", "durm", "durdead", "sum_state_no", "sum_onesided_no", "sum_nonstate_no")]

acled_sudan <- acled_subset[acled_subset$ccountry %in% c("South-Sudan", "Sudan"),]

for(i in acled_sudan$name){ # this classifies clearly South Sudan and cleary Sudan ethnicities. I am losing 34 ethnicities who are present on both sides of the border, as the acled did not code this back then.
  if(i %in% opt_ethn[opt_ethn$country=="South-Sudan", "ethn_NAME"] & !(i %in% opt_ethn[opt_ethn$country=="Sudan", "ethn_NAME"])){
    acled_subset[acled_subset$name == i, "ccountry"] <- "South-Sudan"
  } else{
    if(i %in% opt_ethn[opt_ethn$country=="Sudan", "ethn_NAME"] & !(i %in% opt_ethn[opt_ethn$country=="South-Sudan", "ethn_NAME"])){
      acled_subset[acled_subset$name == i, "ccountry"] <- "Sudan"
    } else{
      acled_subset[acled_subset$name == i, "ccountry"] <- NA
    }
  }
}

acled_subset$ccountry <- gsub(" ", "-", acled_subset$ccountry)
acled_subset$ccountry <- gsub("'", "", acled_subset$ccountry)

acled_subset[grepl("dIvoire", acled_subset$ccountry), "ccountry"] <- "Cote-dIvoire"
acled_subset[grepl("Congo,-Dem.", acled_subset$ccountry), "ccountry"] <- "Democratic-Republic-of-the-Congo"
acled_subset[grepl("Congo,-Rep.", acled_subset$ccountry), "ccountry"] <- "Congo"
acled_subset[grepl("Egypt,-Arab-Rep", acled_subset$ccountry), "ccountry"] <- "Egypt"
acled_subset[grepl("Tanzania", acled_subset$ccountry), "ccountry"] <- "United-Republic-of-Tanzania"

acled_merged <- merge(opt_ethn, acled_subset, by.x=c("country", "ethn_NAME"), by.y=c("ccountry", "name"))

write.csv(acled_merged, file="/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/temp/opt_acled.csv", row.names = FALSE)

##############
# Get railroads on ethn level
##############

rails <- readOGR("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/input/Railroads/Built/Railroads.TAB")

south_african_rails <- readOGR("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/input/Railroads/South_africa.shp")

south_african_rails@data <- rails@data[1:length(south_african_rails),] # this creates an empty databox for south africa (of data which I do not have), in order to merge seemlessly
south_african_rails@data[1] <- NA
south_african_rails@data[2] <- NA
south_african_rails@data[3] <- NA
south_african_rails@data[4] <- NA
south_african_rails@data[5] <- NA
south_african_rails@data[6] <- NA
south_african_rails@data[7] <- NA

south_african_rails <- spTransform(south_african_rails, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")) # gives common CRS to merge seemlessly
rails <- spTransform(rails, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")) # gives common CRS to merge seemlessly

all_rails <- rbind(rails, south_african_rails) # merges

# This gets you a spatial object of ethn_country tuples.
########################################################
ethn_all <- readOGR("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/input/Murdock_EA_2011_vkZ/Murdock_EA_2011_vkZ.shp")

world <- readOGR("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Build/input/World_Countries/TM_WORLD_BORDERS-0.3.shp") # reads in the global country shapefile
africa <- world[world@data$REGION==2,]
ssudan <- readOGR("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Build/input/South-Sudan/ssd_admbnda_adm0_200k_ssnbs_20160114.shp")
ssudan@data <- africa@data[1,]
ssudan@data[1,] <- NA
ssudan@data$NAME <- "South-Sudan"
africa <- rbind(africa, ssudan)
africa@data$zeta <- NA
africa@data$NAME <- gsub(" ", "-", africa@data$NAME)
africa@data$NAME <- gsub("'", "", africa@data$NAME)
africa@data$NAME <- gsub("Libyan-Arab-Jamahiriya", "Libya", africa@data$NAME)
clipped <- gIntersection(ethn_all, africa, byid=T)
clipped$ID <- 1:length(clipped)
acled_merged$map_ID <- NA
for(i in 1:length(clipped)){

  one_polygon <- clipped[which(clipped$ID == i),]
  main_slot <- one_polygon@plotOrder[1]

  merger <- point.in.polygon(acled_merged$first.x, acled_merged$first.y, one_polygon@polygons[[1]]@Polygons[[main_slot]]@coords[,1], one_polygon@polygons[[1]]@Polygons[[main_slot]]@coords[,2])

  if(sum(merger) == 1){
    acled_merged[merger==1,"map_ID"] <- i
  }


}

mappable_dataset <- merge(clipped, acled_merged, by.x="ID", by.y="map_ID")
mappable_dataset <- mappable_dataset[!is.na(mappable_dataset@data$tuple_ID),]

# doing the EMST
###############


nodes <- readOGR("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/input/Railroads/EMST_Lines/nodes.TAB")
#emst <- readOGR("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/input/Railroads/EMST_Lines/EMST.TAB")

# this is all the cities that are mentioned on the south africa map plus pretoria (capital back then)
cape <- c(18.598227, -34.107790)
beaufort_west <- c(22.561545, -32.305843)
bleomfontain <- c(26.087945, -29.105537)
port_elizabeth <- c(25.670227, -33.844601)
johannesburg <- c(28.024229, -26.173970)
pretoria <- c(28.138890, -25.792591)
eendekuil <- c(18.886597, -32.688726)
port_nolloth <- c(16.875627, -29.224785)
de_aar <- c(23.974669, -30.668652)
george <- c(22.500869, -33.997303)
port_alfred <- c(26.867809, -33.624552)
east_london <- c(27.890928, -33.040990)
kimberley <- c(24.733504, -28.766301)
vryburg <- c(24.755151, -26.932349)

sa_nodes <- rbind(cape, beaufort_west, bleomfontain, port_elizabeth, johannesburg, pretoria, eendekuil, port_nolloth, de_aar, george, port_alfred, east_london, kimberley, vryburg)

dta <- as.data.frame(rep(1, nrow(sa_nodes)))
colnames(dta) <- "node"

sa_pts <- SpatialPointsDataFrame(SpatialPoints(sa_nodes, proj4string = CRS(proj4string(nodes))), data=dta)

all_nodes <- rbind(nodes, sa_pts)

dist_matrix <- matrix(0, nrow=length(all_nodes), ncol=length(all_nodes))

for(i in 1:length(all_nodes)){
  for(j in i:length(all_nodes)){

    dist_matrix[i,j] <- gdist(as.numeric(all_nodes@coords[i,1]), as.numeric(all_nodes@coords[i,2]), as.numeric(all_nodes@coords[j,1]), as.numeric(all_nodes@coords[j,2]), units="km")

    dist_matrix[j,i] <- dist_matrix[i,j]

  }
}

connections <- as.data.frame(cbind(2:length(all_nodes), spantree(dist_matrix)$kid))
row.names(connections) <- 1:nrow(connections)

my_emst <- (SpatialLinesDataFrame(SpatialLines(lapply(1:nrow(connections), function(x) Lines(list(Line(rbind(cbind(as.numeric(all_nodes@coords[connections[x,1],1]), as.numeric(all_nodes@coords[connections[x,1],2])), cbind(as.numeric(all_nodes@coords[connections[x,2],1]), as.numeric(all_nodes@coords[connections[x,2],2]))))), paste(x)))), connections))
########################################################
# FINISHED



for(i in 1:length(mappable_dataset)){
  subset_of_one <- mappable_dataset[i,]
  centroid <- gCentroid(subset_of_one)
  dist_Rail <- dist2Line(centroid, all_rails)[1] / 1000
  acled_merged[acled_merged$tuple_ID==subset_of_one@data$tuple_ID, "dist2Rail"] <- dist_Rail
  dist_EMST <- dist2Line(centroid, my_emst)[1] / 1000
  acled_merged[acled_merged$tuple_ID==subset_of_one@data$tuple_ID, "dist2emst"] <- dist_EMST

  if(gIntersects(subset_of_one, all_rails)){
    intersect <- gIntersection(subset_of_one, all_rails)
    if(!("coords" %in% slotNames(intersect))){
    if("lineobj" %in% slotNames(intersect)){
      acled_merged[acled_merged$tuple_ID == mappable_dataset@data$tuple_ID[i], "RailKM"] <- Reduce("+",lapply(1:length(intersect@lineobj@lines), function(j) sum(LineLength(intersect@lineobj@lines[[j]]@Lines[[1]]@coords, longlat = T))))

    } else{
      acled_merged[acled_merged$tuple_ID == mappable_dataset@data$tuple_ID[i], "RailKM"] <-  LineLength(intersect@lines[[1]]@Lines[[1]]@coords, longlat = T)

    }

  }
  } else{
    acled_merged[acled_merged$tuple_ID == mappable_dataset@data$tuple_ID[i], "RailKM"] <- 0
}

if(gIntersects(subset_of_one, my_emst)){
  intersect <- gIntersection(subset_of_one, my_emst)
  if(!("coords" %in% slotNames(intersect))){
  if("lineobj" %in% slotNames(intersect)){
    acled_merged[acled_merged$tuple_ID == mappable_dataset@data$tuple_ID[i], "emstKM"] <- Reduce("+",lapply(1:length(intersect@lineobj@lines), function(j) sum(LineLength(intersect@lineobj@lines[[j]]@Lines[[1]]@coords, longlat = T))))
    print(c(i, "with multiple"))
  } else{
    acled_merged[acled_merged$tuple_ID == mappable_dataset@data$tuple_ID[i], "emstKM"] <-  LineLength(intersect@lines[[1]]@Lines[[1]]@coords, longlat = T)
    print(i)
  }

}
} else{
  acled_merged[acled_merged$tuple_ID == mappable_dataset@data$tuple_ID[i], "emstKM"] <- 0
}

}

write.csv(acled_merged, file="/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/temp/opt_acled.csv", row.names = FALSE)

##############
# Ethnicity heatmap
##############
#
ethn_all <- readOGR("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/input/Murdock_EA_2011_vkZ/Murdock_EA_2011_vkZ.shp")

world <- readOGR("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Build/input/World_Countries/TM_WORLD_BORDERS-0.3.shp") # reads in the global country shapefile
africa <- world[world@data$REGION==2,]
ssudan <- readOGR("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Build/input/South-Sudan/ssd_admbnda_adm0_200k_ssnbs_20160114.shp")
ssudan@data <- africa@data[1,]
ssudan@data[1,] <- NA
ssudan@data$NAME <- "South-Sudan"
africa <- rbind(africa, ssudan)
africa@data$zeta <- NA
africa@data$NAME <- gsub(" ", "-", africa@data$NAME)
africa@data$NAME <- gsub("'", "", africa@data$NAME)
africa@data$NAME <- gsub("Libyan-Arab-Jamahiriya", "Libya", africa@data$NAME)


clipped <- gIntersection(ethn_all, africa, byid=T)

clipped$ID <- 1:length(clipped)

acled_merged$map_ID <- NA
epr_merged$map_ID <- NA

for(i in 1:length(clipped)){

  one_polygon <- clipped[which(clipped$ID == i),]
  main_slot <- one_polygon@plotOrder[1]

  merger.acled <- point.in.polygon(acled_merged$first.x, acled_merged$first.y, one_polygon@polygons[[1]]@Polygons[[main_slot]]@coords[,1], one_polygon@polygons[[1]]@Polygons[[main_slot]]@coords[,2])
  merger.epr <- point.in.polygon(epr_merged$first.x, epr_merged$first.y, one_polygon@polygons[[1]]@Polygons[[main_slot]]@coords[,1], one_polygon@polygons[[1]]@Polygons[[main_slot]]@coords[,2])

  if(sum(merger.acled) == 1){
    acled_merged[merger.acled==1,"map_ID"] <- i
  }


  if(sum(merger.epr) == 1){
      epr_merged[merger.epr==1,"map_ID"] <- i
    }


}

mappable_dataset <- merge(clipped, acled_merged, by.x="ID", by.y="map_ID")
mappable_dataset <- merge(mappable_dataset, epr_merged[,c("map_ID", "dis")], by.x="ID", by.y="map_ID")
mappable_dataset <- mappable_dataset[!is.na(mappable_dataset@data$tuple_ID) & !is.na(mappable_dataset@data$dis),]

my.palette <- brewer.pal(n = 9, name = "OrRd") # for an orange palette


breaks_qt <- classIntervals(mappable_dataset$zeta, n = 8, style = "quantile")
br <- breaks_qt$brks
offs <- 0.0000001
br[1] <- br[1] - offs
br[length(br)] <- br[length(br)] + offs
mappable_dataset$zeta_bracket <- cut(mappable_dataset$zeta, br)

edges <- mappable_dataset@bbox
scale.parameter = 1.1
xshift = 0
yshift = 0

edges[1, ] <- (edges[1, ] - mean(edges[1, ])) * scale.parameter + mean(edges[1,
    ]) + xshift
edges[2, ] <- (edges[2, ] - mean(edges[2, ])) * scale.parameter + mean(edges[2,
    ]) + yshift

max_extend <- max(abs(edges[1,2]-edges[1,1]), abs(edges[2,2]-edges[2,1]))

edges[1,1] <- mean(edges[1,]) - 0.5*max_extend
edges[1,2] <- mean(edges[1,]) + 0.5*max_extend

edges[2,1] <- mean(edges[2,]) - 0.5*max_extend
edges[2,2] <- mean(edges[2,]) + 0.5*max_extend


png(filename=paste("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/output/other_maps/ethnicity_zeta.png", sep=""), width=6, height=6, units = 'in', res = 200)

print(spplot(mappable_dataset, "zeta_bracket", col="transparent", col.regions=my.palette, cuts=8, xlim=edges[1,], ylim=edges[2,]))

dev.off()
