require(scales)

sunnmd_df = data.frame("country" = NA, "s_unnamed" = NA, "cor" = NA, "corp" = NA, "withinvar" = NA)
gmaps_df = data.frame("from" = NA, "to" = NA, "dist" = NA, "dur" = NA, "share_unnamed" = NA, "X.gmapdist." = NA,  "X.gmapdur." = NA,    "popfrom" = NA,      "popto" = NA,     "speed" = NA,       "popm" = NA, "country" = NA)

row = 1

country_names = read.csv("./Build/temp/country_names.csv")$x
country_names = country_names[!(country_names %in% c("Germany", "Japan", "China", "United-States"))]

for(country in country_names){
  
  df = read.csv(paste("./Build/temp/osrmbias/bias_", country, ".csv"))

  
  if(nrow(df) > 2){
    case_c <- read.csv(paste0("./Build/temp/borderregions/", country, "_borderregion.csv"))
    
    
    for(i in unique(df$from)){
      df[df$from == i,"popfrom"] = case_c[i,"pop"]
      df[df$to == i,"popto"] = case_c[i,"pop"]
    }
    
    df$speed = df$dist / df$dur 
    
    df$popm =( df$popfrom + df$popto ) / 2
    cor = cor.test(df$popm, df$share_unnamed)
    
    
    
    wmean = weighted.mean(df$share_unnamed, w = df$dist)
    
    if(!is.na(wmean)){
      sunnmd_df[row,] = c(country, wmean, cor$estimate, cor$p.value, sd(df$share_unnamed))
      row = row+1
    }
  
  
  gdf = df[!is.na(df$X.gmapdist.),]
  
  if(nrow(gdf)>0){
    gdf$country = country
    gdf$country = country
    gmaps_df = rbind(gmaps_df, gdf)
  }
  }
}

gmaps_df$speed = gmaps_df$dist/(gmaps_df$dur/60)
gmaps_df$gspeed = (gmaps_df$X.gmapdist./1000) / (gmaps_df$X.gmapdur./3600)

gmaps_df = gmaps_df[gmaps_df$popm>1,]

gmaps_df$lpop = log(gmaps_df$popm)

newx = seq(7,17,by = 0.05)

glm = lm(gspeed~lpop, data = gmaps_df)
gconf_interval <- predict(glm, newdata=data.frame(lpop=newx), interval="confidence", level = 0.95)

olm = lm(speed~lpop, data = gmaps_df)
oconf_interval <- predict(olm, newdata=data.frame(lpop=newx), interval="confidence", level = 0.95)


pdf("./Build/output/osmbias/osm_gmaps_lpop.pdf", width = 8, height = 7)
plot(log(gmaps_df$popm), gmaps_df$speed, bty = "n", xlab = "log Population", ylab = "Speed", col = alpha("grey77", .6), pch = 19, xlim = c(7.5, 15) , ylim = c(0, 100))

legend("bottomright", pch= 19, legend = c("OSM", "Google Maps"), col = c("grey77", "tomato2"), bty = "n")

points(log(gmaps_df$popm), gmaps_df$gspeed, col = alpha("tomato2", .6), pch = 19)

polygon(x = c(newx, rev(newx)), y = c(gconf_interval[,2], rev(gconf_interval[,3])), col = alpha("tomato2", .2), border = NA)
polygon(x = c(newx, rev(newx)), y = c(oconf_interval[,2], rev(oconf_interval[,3])), col = alpha("grey77", .2), border = NA)

abline(glm, col = "tomato2", lwd = 2)
abline(olm, col = "grey77", lwd = 2)

dev.off()

pdf("./Build/output/osmbias/osm_gmaps.pdf", width = 8, height = 7)
plot(gmaps_df$speed, gmaps_df$gspeed, col = alpha("chocolate2", .6), pch = 19, ylim = c(0, 100), xlim = c(0,100), bty = "n", xlab = "OSM Speed" ,ylab = "Google Maps Speed")
abline(a = 0, b= 1, col = "darkred", lty = 2, lwd = 2)
dev.off()

