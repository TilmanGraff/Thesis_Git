

###############
# This file creates spatial heatmaps of variables of interest


file.remove(list.files(path="/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/output/zeta_heatmaps/", full.names = T))

opt_loc <- read.csv("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/input/opt_loc.csv")

country_table <- as.data.frame(table(opt_loc$country))
country_names <- paste(country_table[country_table$Freq != 0,"Var1"])

# Aggregate zetas to country level
national_zeta <- data.frame()
i = 1

for(country in country_names){
  df <- opt_loc[opt_loc$country==country,]

  if(!is.na(df[1, "zeta"])){

    national_zeta[i, "country"] <- country
    national_zeta[i, "zeta"] <- ((sum(df$pop * df$util_opt)  / sum(df$pop * df$util_stat)) - 1) * 100


    i = i+1
  }
}

national_zeta <- national_zeta[order(-national_zeta$zeta),]


####
# 1) Map of all countries by their collective zeta

library(RColorBrewer)
library("rgdal", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")

world <- readOGR("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Build/input/World_Countries/TM_WORLD_BORDERS-0.3.shp") # reads in the global country shapefile

africa <- world[world@data$REGION==2,]


ssudan <- readOGR("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Build/input/South-Sudan/ssd_admbnda_adm0_200k_ssnbs_20160114.shp")

ssudan@data <- africa@data[1,]
ssudan@data[1,] <- NA
ssudan@data$NAME <- "South-Sudan"
ssudan@data$AREA <- 50

africa <- rbind(africa, ssudan)


africa@data$zeta <- NA
africa@data$NAME <- gsub(" ", "-", africa@data$NAME)
africa@data$NAME <- gsub("'", "", africa@data$NAME)
africa@data$NAME <- gsub("Libyan-Arab-Jamahiriya", "Libya", africa@data$NAME)



for(i in 1:nrow(africa@data)){ # this is a rather murky way to merge in the zeta for each country, as the classic merge command somehow throws the file off

  if(africa@data$NAME[i] %in% national_zeta$country){
    africa@data$zeta[i] <- national_zeta[africa@data$NAME[i] == national_zeta$country, "zeta"]
  }

}

# if you want to have your own color scheme
my.palette <- brewer.pal(n = 9, name = "OrRd") # for an orange palette
# display.brewer.all() # shows you available palettes


png(filename=paste("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/output/zeta_heatmaps/African_countries_zeta.png", sep=""))
print(spplot(africa, "zeta", col="transparent", col.regions=my.palette, cuts=8)) # cuts always has to be one less than n in the definition of my.palette
dev.off()

###
# 2) Trying to make this happen within Countries


opt_loc <- read.csv("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/input/opt_loc.csv")

for(country in country_names){
  if(file.exists(paste("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Build/output/Network_outcomes/", country, "_outcomes.csv", sep=""))){

    df <- opt_loc[!is.na(opt_loc$zeta) & opt_loc$country==country,]
    row.names(df) <- df$rownumber
    print(country)

    # back out corner locations from centroid
    df$bl_x <- df$x - 0.25
    df$tl_x <- df$x - 0.25
    df$br_x <- df$x + 0.25
    df$tr_x <- df$x + 0.25

    df$bl_y <- df$y - 0.25
    df$tl_y <- df$y + 0.25
    df$br_y <- df$y - 0.25
    df$tr_y <- df$y + 0.25


    polygon_file = SpatialPolygons(lapply(1:nrow(df), function(x) Polygons(list(Polygon( cbind(t(df[x, c("bl_x", "tl_x", "tr_x", "br_x")]), t(df[x, c("bl_y", "tl_y", "tr_y", "br_y")])) )), paste0(x))))

    polygon_dataframe = SpatialPolygonsDataFrame(polygon_file, df)

    edges <- polygon_dataframe@bbox
    scale.parameter = 1.1
    xshift = 0
    yshift = 0

    edges[1, ] <- (edges[1, ] - mean(edges[1, ])) * scale.parameter + mean(edges[1,
        ]) + xshift
    edges[2, ] <- (edges[2, ] - mean(edges[2, ])) * scale.parameter + mean(edges[2,
        ]) + yshift


     png(filename=paste("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/output/zeta_heatmaps/", country, "_zeta.png", sep=""))

    print(spplot(polygon_dataframe, "zeta", col="transparent", col.regions=my.palette, cuts=8, xlim=edges[1,], ylim=edges[2,], main = country))
     dev.off()
  }
}

###
# 3) for all countries on this smaller scale


df <- opt_loc[!is.na(opt_loc$zeta),]
row.names(df) <- 1:nrow(df)


# back out corner locations from centroid
df$bl_x <- df$x - 0.25
df$tl_x <- df$x - 0.25
df$br_x <- df$x + 0.25
df$tr_x <- df$x + 0.25

df$bl_y <- df$y - 0.25
df$tl_y <- df$y + 0.25
df$br_y <- df$y - 0.25
df$tr_y <- df$y + 0.25


polygon_file = SpatialPolygons(lapply(1:nrow(df), function(x) Polygons(list(Polygon( cbind(t(df[x, c("bl_x", "tl_x", "tr_x", "br_x")]), t(df[x, c("bl_y", "tl_y", "tr_y", "br_y")])) )), paste0(x))))

polygon_dataframe = SpatialPolygonsDataFrame(polygon_file, df)

edges <- polygon_dataframe@bbox
scale.parameter = 1.1
xshift = 0
yshift = 0

edges[1, ] <- (edges[1, ] - mean(edges[1, ])) * scale.parameter + mean(edges[1,
    ]) + xshift
edges[2, ] <- (edges[2, ] - mean(edges[2, ])) * scale.parameter + mean(edges[2,
    ]) + yshift


 png(filename=paste("/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/output/zeta_heatmaps/African_gridcells_zeta.png", sep=""), width=6, height=6, units = 'in', res = 700)

print(spplot(polygon_dataframe, "zeta", col="transparent", col.regions=my.palette, cuts=8, xlim=edges[1,], ylim=edges[2,], main = "Africa"))
 dev.off()
