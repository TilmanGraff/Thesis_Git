

###############
# This file creates spatial heatmaps of variables of interest


# file.remove(list.files(path="/Users/Tilmanski/Documents/UNI/MPhil/Second Year/Thesis_Git/Analysis/output/Lambda_heatmaps/", full.names = T))


require("RColorBrewer")
require("rgdal")
require("classInt")
require("sp")
require("viridis")
require("grid")
library("lattice")
require("rgeos")
require("readstata13")

outpath = paste0("./Analysis/output/Lambda_heatmaps/revision/")
dir.create(outpath)
path = "2024-02-18_040636_mobile_fp_final"
for(version in c("L_mob", "imm_10p", "amenities", "imm", "L_mob_10p")){


      opt_loc = read.dta13("./Analysis/input/maingrid.dta")


      country_table <- as.data.frame(table(opt_loc$country))
      country_names <- paste(country_table[country_table$Freq != 0,"Var1"])

      #opt_loc$Lambda = opt_loc[,paste0("Lambda_", run)]

      grid = readOGR("./Analysis/input/grid_shapefile/grid.shp")
      grid@data = merge(grid@data, opt_loc, by.x="ID", by.y = "id")

if(version == "amenities"){
grid@data[,paste0("Lambda_", version)] = grid$amenities
}
        # # Aggregate Lambdas to country level
        # national_Lambda <- data.frame()
        # i = 1

        # for(country in country_names){
        #   df <- opt_loc[opt_loc$country==country,]

        #   if(!is.na(df[1, "Lambda"])){

        #     national_Lambda[i, "country"] <- country
        #     national_Lambda[i, "Lambda"] <- ((sum(df$pop * df$util_opt)  / sum(df$pop * df$util_stat)))


        #     i = i+1
        #   }
        # }

        # national_Lambda <- national_Lambda[order(-national_Lambda$Lambda),]


        # ####
        # # 1) Map of all countries by their collective Lambda


        # # world <- readOGR("./Build/input/World_Countries/TM_WORLD_BORDERS-0.3.shp") # reads in the global country shapefile
        # #
        # # africa <- world[world@data$REGION==2,]
        # #
        # #
        # # ssudan <- readOGR("./Build/input/South-Sudan/ssd_admbnda_adm0_200k_ssnbs_20160114.shp")
        # #
        # # ssudan@data <- africa@data[1,]
        # # ssudan@data[1,] <- NA
        # # ssudan@data$NAME <- "South-Sudan"
        # #
        # #
        # # africa <- rbind(africa, ssudan)
        # #
        # #
        # # africa@data$Lambda <- NA
        # # africa@data$NAME <- gsub(" ", "-", africa@data$NAME)
        # # africa@data$NAME <- gsub("'", "", africa@data$NAME)
        # # africa@data$NAME <- gsub("Libyan-Arab-Jamahiriya", "Libya", africa@data$NAME)
        # #
        # #
        # #
        # # writeOGR(obj=africa, dsn="./Build/output/African Borders/AfricanBorders.shp", layer="africa", driver="ESRI Shapefile") # this is in geographical projection

        # africa = readOGR("./Build/output/African Borders/AfricanBorders.shp")

        # for(i in 1:nrow(africa@data)){ # this is a rather murky way to merge in the Lambda for each country, as the classic merge command somehow throws the file off

        #   if(africa@data$NAME[i] %in% national_Lambda$country){
        #     africa@data$Lambda[i] <- national_Lambda[africa@data$NAME[i] == national_Lambda$country, "Lambda"]
        #   }

        # }

        # # if you want to have your own color scheme
        # my.palette <- brewer.pal(n = 9, name = "OrRd") # for an orange palette
        # my.palette = colorRampPalette((c("royalblue2", "palevioletred1", "goldenrod1"))) #"paleturquoise2",
        # # display.brewer.all() # shows you available palettes
        # my.palette = colorRampPalette((c("royalblue2", "paleturquoise2", "goldenrod1", "palevioletred1")))
   if(grepl("imm", version)){
  my.palette <- rocket
  }
  if(grepl("mob", version)){
    my.palette <- viridis
  }
    if(grepl("amenities", version)){
    my.palette <- turbo
  }


        # edges <- africa@bbox
        # scale.parameter = 1
        # xshift = 0
        # yshift = 7

        # edges[1, ] <- (edges[1, ] - mean(edges[1, ])) * scale.parameter + mean(edges[1,
        #     ]) + xshift
        # edges[2, ] <- (edges[2, ] - mean(edges[2, ])) * scale.parameter + mean(edges[2,
        #     ]) + yshift


        # breaks_qt <- classIntervals(africa$Lambda, n = 8, style = "quantile")
        # br <- breaks_qt$brks
        # offs <- 0.0000001
        # br[1] <- br[1] - offs
        # br[length(br)] <- br[length(br)] + offs
        # africa$Lambda_bracket <- cut(africa$Lambda, br)


        # africa$col = my.palette(9)[(cut(africa$Lambda, br, labels = F))]

        # # png(filename=paste("./Analysis/output/Lambda_heatmaps/African_countries_Lambda.png", sep=""), width=6, height=6, units = 'in', res=300 )
        # pdf(paste0(outpath, "African_countries_Lambda.pdf"), width = 8, height = 8)
        # #print(spplot(africa, "Lambda", col="transparent", col.regions="col", xlim=edges[1,], ylim=edges[2,], colorkey=T, par.settings = list(axis.line = list(col = "transparent")))) # cuts always has to be one less than n in the definition of my.palette
        # plot(africa, col = africa$col, xlim=edges[1,], ylim=edges[2,])

        # dev.off()

####
# barchart




###
# 2) Trying to make this happen within Countries

        grid@data[grid@data$pop_stat < 10 & !is.na(grid@data$pop_stat),paste0("Lambda_", version)] = NA
        breaks_qt <- classIntervals(grid@data[,paste0("Lambda_", version)], n = 19, style = "quantile")
        br <- breaks_qt$brks
        offs <- 0.0000001
        br[1] <- br[1] - offs
        br[length(br)] <- br[length(br)] + offs
        grid$Lambda_bracket <- cut(grid@data[,paste0("Lambda_", version)], br)
        mbr = vector()
        for(i in 1:19){
          mbr[i] = mean(c(br[i], br[i+1]))
        }

        whichlabels = seq(1, 20, 3)

        ####
        # first, entire continent
        pdf(paste0(outpath, "African_gridcells_", version,  ".pdf"), width = 8, height = 8)

        SP <- spplot(grid, zcol = "Lambda_bracket", col="transparent", col.regions=my.palette(20),  par.settings = list(axis.line = list(col = "transparent")),
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
        print(spplot(grid, zcol = "Lambda_bracket", col="transparent", col.regions=my.palette(20),  par.settings = list(axis.line = list(col = "transparent")), colorkey = FALSE,
              legend = list(inside = legendArgs)))

        dev.off()

        

        #####
        # and now for all countries

        for(country in unique(grid$country)){
          if(file.exists(paste0("./Build/output/", path, "/Network_outcomes/", country, "_outcomes.csv"))){

            pdf(paste0(outpath, country, "_", version, ".pdf"), width = 8, height = 8)
            print(spplot(grid[grid$country == country,], zcol = "Lambda_bracket", col="transparent", col.regions=my.palette(20), par.settings = list(axis.line = list(col = "transparent")),
            colorkey = F))
            dev.off()


          }
        }



        # ##########
        # # on ethn level

        # ethn_all <- readOGR("./Analysis/input/Murdock_EA_2011_vkZ/Murdock_EA_2011_vkZ.shp")
        # africa = readOGR("./Build/output/African Borders/AfricanBorders.shp")
        # #africa@data$wbcode <- countrycode(africa@data$ISO3, origin = "iso3c", destination = "wb")

        # clipped <- gIntersection(ethn_all, africa, byid=T)

        # ethn_opt = aggregate(grid[,"Lambda"], clipped, FUN=mean)


        # breaks_qt <- classIntervals(ethn_opt$Lambda, n = 19, style = "quantile")
        # br <- breaks_qt$brks
        # offs <- 0.0000001
        # br[1] <- br[1] - offs
        # br[length(br)] <- br[length(br)] + offs
        # ethn_opt$Lambda_bracket <- cut(ethn_opt$Lambda, br)
        # mbr = vector()
        # for(i in 1:19){
        #   mbr[i] = mean(c(br[i], br[i+1]))
        # }

        # whichlabels = seq(1, 20, 3)


        # pdf(paste0(outpath, "African_ethn_Lambda.pdf"), width = 8, height = 8)

        # SP <- spplot(ethn_opt, zcol = "Lambda_bracket", col="transparent", col.regions=my.palette(20),  par.settings = list(axis.line = list(col = "transparent")),
        # colorkey = list(
        #         labels=list(
        #             at = whichlabels,
        #             labels = round(mbr[whichlabels], 3)
        #         ), space = "left", height = .4
        #     ))
        # args <- SP$legend$left$args$key

        # ## Prepare list of arguments needed by `legend=` argument (as described in ?xyplot)
        # legendArgs <- list(fun = draw.colorkey,
        #                   args = list(key = args),
        #                   corner = c(0.25,.25))

        # ## Call spplot() again, this time passing in to legend the arguments
        # ## needed to print a color key
        # print(spplot(ethn_opt, zcol = "Lambda_bracket", col="transparent", col.regions=my.palette(20),  par.settings = list(axis.line = list(col = "transparent")), colorkey = FALSE,
        #       legend = list(inside = legendArgs)))

        # dev.off()

}


###
# 3) for all countries on this smaller scale

#my.palette <- (brewer.pal(n = 9, name = "OrRd")) # for an orange palette



# df <- opt_loc[!is.na(opt_loc$Lambda),]
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
# breaks_qt <- classIntervals(polygon_dataframe$Lambda, n = 9, style = "quantile")
# br <- breaks_qt$brks
# offs <- 0.0000001
# br[1] <- br[1] - offs
# br[length(br)] <- br[length(br)] + offs
# polygon_dataframe$Lambda_bracket <- cut(polygon_dataframe$Lambda, br)

#
#
#  png(filename=paste("./Analysis/output/Lambda_heatmaps/African_gridcells_Lambda.png", sep=""), width=6, height=6, units = 'in', res = 300)
#
# print(spplot(polygon_dataframe, "Lambda_bracket", col="transparent", col.regions=my.palette, cuts=8, xlim=edges[1,], ylim=edges[2,], par.settings = list(axis.line = list(col = "transparent"))))
#
#  dev.off()
