library(pdftools)
library("rjson", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("rgdal", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("geosphere", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("raster", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("rgeos", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("Imap", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
library("gepaf", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("vegan", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("countrycode", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")

opt_loc <- read.csv("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/input/opt_loc.csv")

nominatim_osm <- function(address = NULL)
{
     if(suppressWarnings(is.null(address)))
         return(data.frame())
     tryCatch(
         d <- jsonlite::fromJSON(
             gsub('\\@addr\\@', gsub('\\s+', '\\%20', address),
                  'http://nominatim.openstreetmap.org/search/@addr@?format=json&addressdetails=0&limit=1')
         ), error = function(c) return(data.frame())
     )
     if(length(d) == 0) return(data.frame())
     return(data.frame(lon = as.numeric(d$lon), lat = as.numeric(d$lat)))
}


text <- pdf_text("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/input/Leaders.pdf")
text2 <- strsplit(text, "\n")

texts <- c(gsub("\\s{2,}", ",", text2[[1]]), gsub("\\s{2,}", ",", text2[[2]]))

df <- data.frame(lapply(read.delim(textConnection(texts), sep=",", header=F), as.character), stringsAsFactors=FALSE)

leaders <- df[-1,-1]
colnames(leaders) <- leaders[1,]
leaders <- leaders[-1,]

leaders <- leaders[!grepl("office", leaders$`Entered office`),]
leaders <- leaders[!grepl("Table", leaders$Country),]


leaders$Country <- gsub(" ", "-", leaders$Country)
leaders$Country <- gsub("'", "", leaders$Country)

leaders[grepl("Ivoire", leaders$Country), "Country"] <- "Cote-dIvoire"
leaders[grepl("Democratic-Republic-of-Congo", leaders$Country), "Country"] <- "Democratic-Republic-of-the-Congo"
#leaders[grepl("Republic-of-the-Congo", leaders$Country), "Country"] <- "Congo"
leaders[grepl("Egypt,-Arab-Rep", leaders$Country), "Country"] <- "Egypt"
leaders[grepl("Tanzania", leaders$Country), "Country"] <- "United-Republic-of-Tanzania"

leaders$Ethnicity <- toupper(leaders$Ethnicity)

leaders[grepl("TIGRINYA", leaders$Ethnicity), "Ethnicity"] <- "TIGRINYA"
leaders[grepl("KROU", leaders$Ethnicity), "Ethnicity"] <- "KRU"
leaders[grepl("FULA", leaders$Ethnicity), "Ethnicity"] <- "FULA"
leaders[grepl("KABRE", leaders$Ethnicity), "Ethnicity"] <- "KABRE"
leaders[grepl("DIOULA", leaders$Ethnicity), "Ethnicity"] <- "DIULA"
leaders[grepl("BETI", leaders$Ethnicity), "Ethnicity"] <- "BETE"
leaders[grepl("ASANTE", leaders$Ethnicity), "Ethnicity"] <- "ASHANTI"
leaders[grepl("LHOMWE", leaders$Ethnicity), "Ethnicity"] <- "LOMWE"
leaders[grepl("XHOSA", leaders$Ethnicity), "Ethnicity"] <- "XOSA"

leaders$until <- leaders$`Left office`
leaders[grepl("ongoing", leaders$`Left office`), "until"] <- "01.01.2014"
leaders$from <- as.numeric(format(as.Date(as.character(leaders$`Entered office`), format="%d.%m.%Y"), "%Y"))
leaders[is.na(leaders$from),"from"] <- as.numeric(format(as.Date(as.character(leaders[is.na(leaders$from),"Entered office"]), format="%m.%d.%Y"), "%Y"))

leaders$until <- as.numeric(format(as.Date(as.character(leaders$until), format="%d.%m.%Y"), "%Y"))

leaders$years_in_power <- leaders$until-leaders$from

leaders$`ADM2 region` <- gsub("<[^>]+>", "",leaders$`ADM2 region`)
leaders$`ADM1 region` <- gsub("<[^>]+>", "",leaders$`ADM1 region`)


leaders$x <- NA
leaders$y <- NA

for(i in 1:nrow(leaders)){
  lonlat <- nominatim_osm(paste(leaders[i, "ADM2 region"], leaders[i, "Country"], sep=", "))
  if(nrow(lonlat)==0){
    lonlat <- nominatim_osm(paste(leaders[i, "ADM1 region"], leaders[i, "Country"]))
  }
  if(nrow(lonlat)!=0){
  leaders[i,"x"] <- lonlat$lon
  leaders[i,"y"] <- lonlat$lat
  } else{
  leaders[i,"x"] <- NA
  leaders[i,"y"] <- NA
  }
}

# Merge onto opt_loc gridcells

# opt_loc <- read.csv("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/input/opt_loc.csv")
#
# df <- opt_loc[!is.na(opt_loc$zeta),]
# row.names(df) <- 1:nrow(df)
# # back out corner locations from centroid
# df$bl_x <- df$x - 0.25
# df$tl_x <- df$x - 0.25
# df$br_x <- df$x + 0.25
# df$tr_x <- df$x + 0.25
# df$bl_y <- df$y - 0.25
# df$tl_y <- df$y + 0.25
# df$br_y <- df$y - 0.25
# df$tr_y <- df$y + 0.25
#
# polygon_file = SpatialPolygons(lapply(1:nrow(df), function(x) Polygons(list(Polygon( cbind(t(df[x, c("bl_x", "tl_x", "tr_x", "br_x")]), t(df[x, c("bl_y", "tl_y", "tr_y", "br_y")])) )), paste0(x))))
# polygon_dataframe = SpatialPolygonsDataFrame(polygon_file, df)
#
# for(i in 1:length(polygon_dataframe)){
#     merger <- (point.in.polygon(leaders$x, leaders$y, polygon_dataframe@polygons[[i]]@Polygons[[1]]@coords[,1], polygon_dataframe@polygons[[i]]@Polygons[[1]]@coords[,2]))
#
#     subset <- leaders[merger==1,]
#
#     opt_loc[opt_loc$ID==polygon_dataframe@data$ID[i], "years_in_power"] <- sum(subset$years_in_power, na.rm=T)
#
# }

opt_loc$wbcode <- countrycode(opt_loc$country, origin = "country.name", destination="wb")
df <- opt_loc[!is.na(opt_loc$wbcode),]

df$years_in_power <- 0

leaders <- leaders[!is.na(leaders$x),]

for(i in 1:nrow(leaders)){

  country <- countrycode(leaders$Country[i], origin = "country.name", destination = "wb")
  if(!is.na(country)){

    subset <- df[df$wbcode==country,]
    nearest_ID <- NA
    min_dist <- 1000000000
if(nrow(subset)>0){
    for(j in 1:nrow(subset)){
      if(!is.na(subset[j,c("x")]) & !is.na(subset[j,c("y")])){
      dist <- gdist(leaders[i,c("x")], leaders[i,c("y")], subset[j,c("x")], subset[j,c("y")])
      if(!is.na(dist)){
      if(dist < min_dist){
        min_dist <- dist
        nearest_ID <- subset[j,"ID"]
      }
    }
  }
  }
    df[df$ID==nearest_ID,"years_in_power"] <- df[df$ID==nearest_ID,"years_in_power"] + leaders[i,"years_in_power"]
}
}
}

df <- merge(opt_loc, df, by="ID", all.x=T)


#eth_years_in_power <- aggregate(leaders$in_power, by=list(leaders$Ethnicity, leaders$Country), FUN="sum")

#colnames(years_in_power) <- c("ethn_NAME", "country", "years_in_power")

write.csv(df[,c("ID", "years_in_power")], file="/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/temp/ID_years_in_power.csv", row.names = FALSE)
