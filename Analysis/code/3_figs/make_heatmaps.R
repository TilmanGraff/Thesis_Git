

###############
# This file creates spatial heatmaps of variables of interest


# file.remove(list.files(path="/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/output/zeta_heatmaps/", full.names = T))


require("RColorBrewer")
require("rgdal")
require("classInt")
require("sp")
require("viridis")
require("grid")
library(lattice)

grid = readOGR("./Analysis/input/grid_shapefile/grid.shp")
opt_loc = read.csv("./Analysis/input/opt_loc.csv")

grid@data = merge(grid@data, opt_loc, by="ID")

country_table <- as.data.frame(table(opt_loc$country))
country_names <- paste(country_table[country_table$Freq != 0,"Var1"])

# Aggregate zetas to country level
national_zeta <- data.frame()
i = 1

for(country in country_names){
  df <- opt_loc[opt_loc$country==country,]

  if(!is.na(df[1, "zeta"])){

    national_zeta[i, "country"] <- country
    national_zeta[i, "zeta"] <- ((sum(df$pop * df$util_opt)  / sum(df$pop * df$util_stat)))


    i = i+1
  }
}

national_zeta <- national_zeta[order(-national_zeta$zeta),]


####
# 1) Map of all countries by their collective zeta


# world <- readOGR("./Build/input/World_Countries/TM_WORLD_BORDERS-0.3.shp") # reads in the global country shapefile
#
# africa <- world[world@data$REGION==2,]
#
#
# ssudan <- readOGR("./Build/input/South-Sudan/ssd_admbnda_adm0_200k_ssnbs_20160114.shp")
#
# ssudan@data <- africa@data[1,]
# ssudan@data[1,] <- NA
# ssudan@data$NAME <- "South-Sudan"
#
#
# africa <- rbind(africa, ssudan)
#
#
# africa@data$zeta <- NA
# africa@data$NAME <- gsub(" ", "-", africa@data$NAME)
# africa@data$NAME <- gsub("'", "", africa@data$NAME)
# africa@data$NAME <- gsub("Libyan-Arab-Jamahiriya", "Libya", africa@data$NAME)
#
#
#
# writeOGR(obj=africa, dsn="./Build/output/African Borders/AfricanBorders.shp", layer="africa", driver="ESRI Shapefile") # this is in geographical projection

africa = readOGR("./Build/output/African Borders/AfricanBorders.shp")

for(i in 1:nrow(africa@data)){ # this is a rather murky way to merge in the zeta for each country, as the classic merge command somehow throws the file off

  if(africa@data$NAME[i] %in% national_zeta$country){
    africa@data$zeta[i] <- national_zeta[africa@data$NAME[i] == national_zeta$country, "zeta"]
  }

}

# if you want to have your own color scheme
my.palette <- brewer.pal(n = 9, name = "OrRd") # for an orange palette
my.palette = colorRampPalette((c("royalblue2", "palevioletred1", "goldenrod1"))) #"paleturquoise2",
# display.brewer.all() # shows you available palettes
my.palette = colorRampPalette((c("royalblue2", "paleturquoise2", "goldenrod1", "palevioletred1")))
my.palette = rocket

edges <- africa@bbox
scale.parameter = 1
xshift = 0
yshift = 7

edges[1, ] <- (edges[1, ] - mean(edges[1, ])) * scale.parameter + mean(edges[1,
    ]) + xshift
edges[2, ] <- (edges[2, ] - mean(edges[2, ])) * scale.parameter + mean(edges[2,
    ]) + yshift


breaks_qt <- classIntervals(africa$zeta, n = 8, style = "quantile")
br <- breaks_qt$brks
offs <- 0.0000001
br[1] <- br[1] - offs
br[length(br)] <- br[length(br)] + offs
africa$zeta_bracket <- cut(africa$zeta, br)


africa$col = my.palette(9)[(cut(africa$zeta, br, labels = F))]

# png(filename=paste("./Analysis/output/zeta_heatmaps/African_countries_zeta.png", sep=""), width=6, height=6, units = 'in', res=300 )
pdf("./Analysis/output/zeta_heatmaps/African_countries_zeta.pdf", width = 8, height = 8)
#print(spplot(africa, "zeta", col="transparent", col.regions="col", xlim=edges[1,], ylim=edges[2,], colorkey=T, par.settings = list(axis.line = list(col = "transparent")))) # cuts always has to be one less than n in the definition of my.palette
plot(africa, col = africa$col, xlim=edges[1,], ylim=edges[2,])

dev.off()

####
# barchart




###
# 2) Trying to make this happen within Countries

breaks_qt <- classIntervals(grid$zeta, n = 19, style = "quantile")
br <- breaks_qt$brks
offs <- 0.0000001
br[1] <- br[1] - offs
br[length(br)] <- br[length(br)] + offs
grid$zeta_bracket <- cut(grid$zeta, br)
mbr = vector()
for(i in 1:19){
  mbr[i] = mean(c(br[i], br[i+1]))
}

whichlabels = seq(1, 20, 3)

####
# first, entire continent
pdf("./Analysis/output/zeta_heatmaps/African_gridcells_zeta.pdf", width = 8, height = 8)

SP <- spplot(grid, zcol = "zeta_bracket", col="transparent", col.regions=my.palette(20),  par.settings = list(axis.line = list(col = "transparent")),
colorkey = list(
        labels=list(
            at = whichlabels,
            labels = round(mbr[whichlabels], 3)
        ), space = "left", height = .4
    ))
args <- SP$legend$left$args$key

## Prepare list of arguments needed by `legend=` argument (as described in ?xyplot)
legendArgs <- list(fun = draw.colorkey,
                   args = list(key = args),
                   corner = c(0.25,.25))

## Call spplot() again, this time passing in to legend the arguments
## needed to print a color key
print(spplot(grid, zcol = "zeta_bracket", col="transparent", col.regions=my.palette(20),  par.settings = list(axis.line = list(col = "transparent")), colorkey = FALSE,
       legend = list(inside = legendArgs)))

dev.off()


#####
# and now for all countries

for(country in country_names){
  if(file.exists(paste("./Build/output/Network_outcomes/", country, "_outcomes.csv", sep=""))){

    pdf(paste0("./Analysis/output/zeta_heatmaps/", country, "_zeta.pdf"), width = 8, height = 8)
    print(spplot(grid[grid$country == country,], zcol = "zeta_bracket", col="transparent", col.regions=my.palette(20), par.settings = list(axis.line = list(col = "transparent")),
    colorkey = F))
    dev.off()


  }
}



##########
# on ethn level

ethn_all <- readOGR("./Analysis/input/Murdock_EA_2011_vkZ/Murdock_EA_2011_vkZ.shp")
africa = readOGR("./Build/output/African Borders/AfricanBorders.shp")
#africa@data$wbcode <- countrycode(africa@data$ISO3, origin = "iso3c", destination = "wb")

clipped <- gIntersection(ethn_all, africa, byid=T)

ethn_opt = aggregate(grid[,"zeta"], clipped, FUN=mean)


breaks_qt <- classIntervals(ethn_opt$zeta, n = 19, style = "quantile")
br <- breaks_qt$brks
offs <- 0.0000001
br[1] <- br[1] - offs
br[length(br)] <- br[length(br)] + offs
ethn_opt$zeta_bracket <- cut(ethn_opt$zeta, br)
mbr = vector()
for(i in 1:19){
  mbr[i] = mean(c(br[i], br[i+1]))
}

whichlabels = seq(1, 20, 3)


pdf("./Analysis/output/zeta_heatmaps/African_ethn_zeta.pdf", width = 8, height = 8)

SP <- spplot(ethn_opt, zcol = "zeta_bracket", col="transparent", col.regions=my.palette(20),  par.settings = list(axis.line = list(col = "transparent")),
colorkey = list(
        labels=list(
            at = whichlabels,
            labels = round(mbr[whichlabels], 3)
        ), space = "left", height = .4
    ))
args <- SP$legend$left$args$key

## Prepare list of arguments needed by `legend=` argument (as described in ?xyplot)
legendArgs <- list(fun = draw.colorkey,
                   args = list(key = args),
                   corner = c(0.25,.25))

## Call spplot() again, this time passing in to legend the arguments
## needed to print a color key
print(spplot(ethn_opt, zcol = "zeta_bracket", col="transparent", col.regions=my.palette(20),  par.settings = list(axis.line = list(col = "transparent")), colorkey = FALSE,
       legend = list(inside = legendArgs)))

dev.off()


###
# 3) for all countries on this smaller scale

#my.palette <- (brewer.pal(n = 9, name = "OrRd")) # for an orange palette



# df <- opt_loc[!is.na(opt_loc$zeta),]
# row.names(df) <- 1:nrow(df)
#
#
# # back out corner locations from centroid
# df$bl_x <- df$x - 0.25
# df$tl_x <- df$x - 0.25
# df$br_x <- df$x + 0.25
# df$tr_x <- df$x + 0.25
#
# df$bl_y <- df$y - 0.25
# df$tl_y <- df$y + 0.25
# df$br_y <- df$y - 0.25
# df$tr_y <- df$y + 0.25
#
#
# polygon_file = SpatialPolygons(lapply(1:nrow(df), function(x) Polygons(list(Polygon( cbind(t(df[x, c("bl_x", "tl_x", "tr_x", "br_x")]), t(df[x, c("bl_y", "tl_y", "tr_y", "br_y")])) )), paste0(x))))
#
# polygon_dataframe = SpatialPolygonsDataFrame(polygon_file, df)

# edges <- polygon_dataframe@bbox
# scale.parameter = 1.1
# xshift = 0
# yshift = 0
#
# edges[1, ] <- (edges[1, ] - mean(edges[1, ])) * scale.parameter + mean(edges[1,
#     ]) + xshift
# edges[2, ] <- (edges[2, ] - mean(edges[2, ])) * scale.parameter + mean(edges[2,
#     ]) + yshift
#
# breaks_qt <- classIntervals(polygon_dataframe$zeta, n = 9, style = "quantile")
# br <- breaks_qt$brks
# offs <- 0.0000001
# br[1] <- br[1] - offs
# br[length(br)] <- br[length(br)] + offs
# polygon_dataframe$zeta_bracket <- cut(polygon_dataframe$zeta, br)

#
#
#  png(filename=paste("./Analysis/output/zeta_heatmaps/African_gridcells_zeta.png", sep=""), width=6, height=6, units = 'in', res = 300)
#
# print(spplot(polygon_dataframe, "zeta_bracket", col="transparent", col.regions=my.palette, cuts=8, xlim=edges[1,], ylim=edges[2,], par.settings = list(axis.line = list(col = "transparent"))))
#
#  dev.off()
