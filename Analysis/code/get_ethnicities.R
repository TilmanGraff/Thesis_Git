
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
    opt_ethn[i, "zeta"] <- sum(subset$pop * subset$zeta, na.rm=T) / opt_ethn[i, "pop"]
  } else{
    opt_ethn[i, "zeta"] <- NA
  }

  opt_ethn[i, "strict_mean_zeta"] <- mean(subset$zeta, na.rm=T)

}

opt_ethn <- opt_ethn[!is.na(opt_ethn$ethn_NAME),]

######################
# Importing EPR data
######################

epr <- read.dta13("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/input/Ethnic_Data_Michalopoulos/epr_main.dta")


# major annoying data mining:

iso <- unique(epr$wbcode)
countryname <- c("Angola", "Burundi", "Benin", "Botswana", "Central-African-Republic", "Cote-dIvoire", "Cameroon", "Democratic-Republic-of-the-Congo", "Congo", "Algeria", "Egypt", "Eritrea", "Ethiopia", "Gabon", "Ghana", "Guinea", "Gambia", "Guinea-Bissau", "Kenya", "Liberia", "Morocco", "Madagascar", "Mali", "Mozambique", "Mauritius", "Malawi", "Namibia", "Niger", "Nigeria", "Rwanda", "Sudan", "Senegal", "Sierra-Leone", "Chad", "Togo", "United-Republic-of-Tanzania", "Uganda", "South-Africa", "Zambia", "Zimbabwe")

epr$country <- NA

for(i in 1:length(iso)){
  epr[epr$wbcode == iso[i], "country"] <- countryname[i]
}


######################
# Importing ACLED data
######################

acled <- read.dta13("/Users/Tilmanski/Downloads/codes_aer_journal/aer_all2013.dta")

acled_subset <- acled[,c("ccountry", "name", "km2split", "lakedum", "riverdum", "petroleum", "diamondd", "split10pc", "split5pc", "all", "fatal", "allf", "allm", "allmm", "battles", "vio", "riots", "govt", "civilians", "rebels", "mil", "external", "intervention", "outside", "dur", "durm", "durdead")]

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
# Ethnicity heatmap
##############

# ethn_all <- readOGR("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/input/Murdock_EA_2011_vkZ/Murdock_EA_2011_vkZ.shp")
#
# world <- readOGR("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Build/input/World_Countries/TM_WORLD_BORDERS-0.3.shp") # reads in the global country shapefile
# africa <- world[world@data$REGION==2,]
# ssudan <- readOGR("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Build/input/South-Sudan/ssd_admbnda_adm0_200k_ssnbs_20160114.shp")
# ssudan@data <- africa@data[1,]
# ssudan@data[1,] <- NA
# ssudan@data$NAME <- "South-Sudan"
# africa <- rbind(africa, ssudan)
# africa@data$zeta <- NA
# africa@data$NAME <- gsub(" ", "-", africa@data$NAME)
# africa@data$NAME <- gsub("'", "", africa@data$NAME)
# africa@data$NAME <- gsub("Libyan-Arab-Jamahiriya", "Libya", africa@data$NAME)
#
#
# clipped <- gIntersection(ethn_all, africa, byid=T)
#
# clipped$ID <- 1:length(clipped)
#
# acled_merged$map_ID <- NA
#
# for(i in 1:length(clipped)){
#
#   one_polygon <- clipped[which(clipped$ID == i),]
#   main_slot <- one_polygon@plotOrder[1]
#
#   merger <- point.in.polygon(acled_merged$first.x, acled_merged$first.y, one_polygon@polygons[[1]]@Polygons[[main_slot]]@coords[,1], one_polygon@polygons[[1]]@Polygons[[main_slot]]@coords[,2])
#
#   if(sum(merger) == 1){
#     acled_merged[merger==1,"map_ID"] <- i
#   }
#
#
# }
#
# mappable_dataset <- merge(clipped, acled_merged, by.x="ID", by.y="map_ID")
# mappable_dataset <- mappable_dataset[!is.na(mappable_dataset@data$tuple_ID),]
#
# my.palette <- brewer.pal(n = 9, name = "OrRd") # for an orange palette
#
#
# breaks_qt <- classIntervals(mappable_dataset$zeta, n = 8, style = "quantile")
# br <- breaks_qt$brks
# offs <- 0.0000001
# br[1] <- br[1] - offs
# br[length(br)] <- br[length(br)] + offs
# mappable_dataset$zeta_bracket <- cut(mappable_dataset$zeta, br)
#
# png(filename=paste("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/output/zeta_heatmaps/ethnicity_zeta.png", sep=""), width=6, height=6, units = 'in', res = 200)
#
# print(spplot(mappable_dataset, "zeta_bracket", col="transparent", col.regions=my.palette, cuts=8))
#
# dev.off()
