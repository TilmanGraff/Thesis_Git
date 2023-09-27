
# This file transforms given centroid geometries into adj, dist, and infrastr matrices
# It does so by scraping OSM for Moore neighborhood connections between centroid coordinates

require("osrm")
require("Imap")
require("gepaf")
require("rgeos")
require("rgdal")
require("geosphere")
require("scales")
require("sf")
require("mapsapi")
require("xml2")


gmapskey = "AIzaSyBkvUko68TxI85HTqbFkRrFVLAI2uIDh3Q"
gmapscount = 0

scrape = FALSE

osrmRoute_shareunnamed = function (src, dst, loc, overview = "simplified", exclude, returnclass, 
                                   osrm.server = getOption("osrm.server"), osrm.profile = getOption("osrm.profile")){
  opt <- options(error = NULL)
  on.exit(options(opt), add = TRUE)
  # if (!missing(returnclass)) {
  #   warning("\"returnclass\" is deprecated.", call. = FALSE)
  # }
  url <- base_url(osrm.server, osrm.profile, "route")
  #url <- paste0(osrm.server, "/", osrm.profile, "/", "route")
  #if (missing(loc)) {
    src <- input_route(x = src, id = "src", single = TRUE)
    dst <- input_route(x = dst, id = "dst", single = TRUE)
    id1 <- src$id
    id2 <- dst$id
    oprj <- src$oprj
    coords <- paste0(src$lon, ",", src$lat, ";", dst$lon, 
                     ",", dst$lat)
  # } else {
  #   loc <- input_route(x = loc, single = FALSE)
  #   oprj <- loc$oprj
  #   id1 <- loc$id1
  #   id2 <- loc$id2
  #   coords <- paste0(apply(cbind(loc$lon, loc$lat), MARGIN = 1, 
  #                          FUN = paste0, collapse = ","), collapse = ";")
  # }
  url <- paste0(url, coords, "?alternatives=false&geometries=polyline&steps=true&overview=", 
                tolower(overview), "&generate_hints=false")
  if (!missing(exclude)) {
    url <- paste0(url, "&exclude=", exclude)
  }
  e <- try({
    req_handle <- curl::new_handle(verbose = FALSE)
    curl::handle_setopt(req_handle, useragent = "osrm_R_package")
    r <- curl::curl_fetch_memory(utils::URLencode(url), handle = req_handle)
  }, silent = TRUE)
  if (inherits(e, "try-error")) {
    stop(e, call. = FALSE)
  }
  test_http_error(r)
  res <- RcppSimdJson::fparse(rawToChar(r$content))
  if (overview == FALSE) {
    return(round(c(duration = res$routes$duration/60, distance = res$routes$distance/1000), 
                 2))
  }
  geodf <- googlePolylines::decode(res$routes$geometry)[[1]][, 
                                                             c(2, 1)]
  rcoords <- paste0(geodf$lon, " ", geodf$lat, collapse = ", ")
  rosf <- st_sf(src = id1, dst = id2, duration = res$routes$duration/60, 
                distance = res$routes$distance/1000, geometry = st_as_sfc(paste0("LINESTRING(", 
                                                                                 rcoords, ")")), crs = 4326, row.names = paste(id1, id2, sep = "_"), share_unnamed = weighted.mean(res$routes$legs[[1]]$steps[[1]]$name == "", w = res$routes$legs[[1]]$steps[[1]]$distance))
  if (!is.na(oprj)) {
    rosf <- st_transform(rosf, oprj)
  }
  return(rosf)
}

base_url = function (osrm.server, osrm.profile, query) 
{
  if (osrm.server == "https://routing.openstreetmap.de/") {
    url <- paste0(osrm.server, "routed-", osrm.profile, "/", 
                  query, "/v1/driving/")
  }
  else {
    url <- paste0(osrm.server, query, "/v1/", osrm.profile, 
                  "/")
  }
  return(url)
}

input_route = function (x, id, single = TRUE, all.ids = FALSE) 
{
  oprj <- NA
  if (single) {
    if (is.vector(x)) {
      if (length(x) == 2 & is.numeric(x)) {
        if (x[1] > 180 | x[1] < -180 | x[2] > 90 | x[2] < 
            -90) {
          stop(paste0("longitude is bounded by the interval [-180, 180], ", 
                      "latitude is bounded by the interval [-90, 90]"), 
               call. = FALSE)
        }
        lon <- clean_coord(x[1])
        lat <- clean_coord(x[2])
        return(list(id = id, lon = lon, lat = lat, oprj = oprj))
      }
      else {
        stop(paste0("\"", id, "\" should be a numeric vector of length 2, ", 
                    "i.e., c(lon, lat)."), call. = FALSE)
      }
    }
    if (inherits(x = x, what = c("sfc", "sf"))) {
      oprj <- st_crs(x)
      if (length(st_geometry(x)) > 1) {
        message(paste0("Only the first row/element of \"", 
                       id, "\" is used."))
      }
      if (inherits(x, "sfc")) {
        x <- x[1]
        idx <- id
      }
      else {
        x <- x[1, ]
        idx <- row.names(x)
      }
      if (sf::st_geometry_type(x, by_geometry = FALSE) != 
          "POINT") {
        stop(paste0("\"", id, "\" geometry should be of type POINT."), 
             call. = FALSE)
      }
      x <- sf::st_transform(x = x, crs = 4326)
      coords <- sf::st_coordinates(x)
      lon <- clean_coord(coords[, 1])
      lat <- clean_coord(coords[, 2])
      return(list(id = idx, lon = lon, lat = lat, oprj = oprj))
    }
    if (inherits(x = x, what = c("data.frame", "matrix"))) {
      if (nrow(x) > 1) {
        message(paste0("Only the first row of \"", id, 
                       "\" is used."))
        x <- x[1, , drop = FALSE]
      }
      idx <- row.names(x)
      if (is.null(idx)) {
        idx <- id
      }
      x <- unlist(x)
      if (length(x) == 2 & is.numeric(x)) {
        lon <- clean_coord(x[1])
        lat <- clean_coord(x[2])
        return(list(id = idx, lon = lon, lat = lat, oprj = oprj))
      }
      else {
        stop(paste0("\"", id, "\" should contain coordinates."), 
             call. = FALSE)
      }
    }
    else {
      stop(paste0("\"", id, "\" should be a vector of coordinates, ", 
                  "a data.frame or a matrix ", "of coordinates, an sfc POINT object or an ", 
                  "sf POINT object."), call. = FALSE)
    }
  }
  else {
    if (inherits(x = x, what = c("sfc", "sf"))) {
      oprj <- st_crs(x)
      lx <- length(st_geometry(x))
      if (lx < 2) {
        stop("\"loc\" should have at least 2 rows or elements.", 
             call. = FALSE)
      }
      type <- sf::st_geometry_type(x, by_geometry = FALSE)
      type <- as.character(unique(type))
      if (length(type) > 1 || type != "POINT") {
        stop("\"loc\" geometry should be of type POINT", 
             call. = FALSE)
      }
      if (inherits(x, "sfc")) {
        id1 <- "src"
        id2 <- "dst"
        if (all.ids) {
          rn <- 1:lx
        }
      }
      else {
        rn <- row.names(x)
        id1 <- rn[1]
        id2 <- rn[lx]
      }
      x <- sf::st_transform(x = x, crs = 4326)
      coords <- sf::st_coordinates(x)
      lon <- clean_coord(coords[, 1])
      lat <- clean_coord(coords[, 2])
      if (!all.ids) {
        return(list(id1 = id1, id2 = id2, lon = lon, 
                    lat = lat, oprj = oprj))
      }
      else {
        return(list(id = rn, lon = lon, lat = lat, oprj = oprj))
      }
    }
    if (inherits(x = x, what = c("data.frame", "matrix"))) {
      lx <- nrow(x)
      if (lx < 2) {
        stop("\"loc\" should have at least 2 rows.", 
             call. = FALSE)
      }
      if (ncol(x) == 2 && is.numeric(x[, 1]) && is.numeric(x[, 
                                                             2])) {
        lon <- clean_coord(x[, 1])
        lat <- clean_coord(x[, 2])
        rn <- row.names(x)
        if (is.null(rn)) {
          rn <- 1:lx
        }
        id1 <- rn[1]
        id2 <- rn[lx]
        if (!all.ids) {
          return(list(id1 = id1, id2 = id2, lon = lon, 
                      lat = lat, oprj = oprj))
        }
        else {
          return(list(id = rn, lon = lon, lat = lat, 
                      oprj = oprj))
        }
      }
      else {
        stop(paste0("\"loc\" should contain coordinates."), 
             call. = FALSE)
      }
    }
    else {
      stop(paste0("\"loc\" should be ", "a data.frame or a matrix ", 
                  "of coordinates, an sfc POINT object or an ", 
                  "sf POINT object."), call. = FALSE)
    }
  }
}


clean_coord = function (x) 
{
  format(round(as.numeric(x), 5), scientific = FALSE, justify = "none", 
         trim = TRUE, nsmall = 5, digits = 5)
}


test_http_error = function (r) 
{
  if (r$status_code >= 400) {
    if (substr(r$type, 1, 16) != "application/json") {
      stop(sprintf("OSRM API request failed [%s]", r$status_code), 
           call. = FALSE)
    }
    else {
      rep <- RcppSimdJson::fparse(rawToChar(r$content))
      stop(sprintf("OSRM API request failed [%s]\n%s\n%s", 
                   r$status_code, rep$code, rep$message), call. = FALSE)
    }
  }
  return(NULL)
}




centroids <- read.csv("./Build/temp/centroids.csv")

# Gather country names
country_table <- as.data.frame(table(centroids$country))
country_names <- paste(country_table[country_table$Freq != 0,"Var1"])

polys = readOGR("./Build/output/African Borders/AfricanBorders.shp")

###############

if(scrape){
  
  # For every country, calculate matrices
  for(country in country_names){
    #for(country in "Kenya"){
    if(!file.exists(paste("./Build/temp/osrmbias/bias_", country, ".csv"))){
    countrydf = data.frame("from" = NA, "to" = NA, "dist" = NA, "dur" = NA, "share_unnamed" = NA, "gmapdist", "gmapdur")
    row = 1
    #if(!(paste("dist_", country, ".csv", sep="") %in% list.files("./Build/temp/dist/"))){
    if(TRUE){
      
      case_centroids <- read.csv(paste0("./Build/temp/borderregions/", country, "_borderregion.csv"))
      case_poly = polys[polys$NAME == country,]
      
      # pdf(file=paste("./Build/output/Road_Networks/network_", country, ".pdf", sep=""), width = 8, height = 8)
      
      # plot(case_centroids$x, case_centroids$y, main=country, bty = "n", pch = ifelse(case_centroids$country == country, 19, 1), axes=F, ylab="", xlab="", asp=1, col="grey") # if you want to plot it
      # plot(case_poly, add=T, col = alpha("dodgerblue", .2), border = NA)
      
      n <- nrow(case_centroids)
      
      # this is to create a rosetta stone which translates the (globally unique) cell ID into the rownumber of the square matrices needed for Matlab
      case_centroids$rownumber <- c(1:n)
      
      adj <- matrix(0, nrow = n, ncol = n)
      dist <- matrix(0, nrow = n, ncol = n)
      speed <- matrix(0, nrow = n, ncol = n)
      walked <- matrix(0, nrow = n, ncol = n)
      
      minutes_per_kilometer <- 15
      
      for(i in 1:n){
        
        
        # identifies the nine centroids within the respective circle to find itself plus eight nearest neighbors
        neighbour_ids <- case_centroids[(case_centroids$y-case_centroids$y[i])^2+(case_centroids$x-case_centroids$x[i])^2<=0.5^2+0.5^2,"ID"]
        df <- case_centroids[case_centroids$ID %in% neighbour_ids, c("ID", "x", "y") ]
        colnames(df) <- c("id", "lon", "lat")
        
        # scrapes OSRM for routes to all neighbours excluding itself
        for(a in neighbour_ids[! neighbour_ids %in% case_centroids[i, "ID"]]){
          
          j <- case_centroids[case_centroids$ID == a,"rownumber"]
          if(adj[j,i] == 0){
            
            route = NULL
            attemptcounter = 1
            while(is.null(route) & attemptcounter <= 50){ # I try until I have a non-null route, or 100 times, whichever comes faster
              
              # This is where the action happens, I scrape the route from OSRM
              routecands <- try(osrmRoute_shareunnamed(src=df[df$id==case_centroids$ID[i],2:3], dst=df[df$id==a,2:3], overview = "full"), silent = T)
              if(typeof(routecands) == "list"){
                route = routecands
              }else{
                if(grepl("NoRoute", routecands)){
                  attemptcounter = 1001
                  route = NULL
                }
              }
              
              
              attemptcounter = attemptcounter+1
              # if(attemptcounter > 2){ # now, if I've already tried twice, I begin to sleep for a second to let the server catch its breath
              #   Sys.sleep(1)
              #   print(attemptcounter)
              #   if(attemptcounter == 3 | attemptcounter %% 5 == 0){ # I also check once what the reason for the error was, it it's "NoRoute", I gather this here and immediately jump to the next id. This is to circumvent super-long annoying loops in which I retry 100 times in vain because there is no route. There is still a rare problem that occurs when the server is down first, so attemptcounter goes beyond 3 and then when it comes back it turns out there is no route. Then attemptcounter is already past the moment where it should check what the problem is. Not sure how often this would happen, but worth a think. Maybe do something like if(attemptcounter %% 10 == 0) or something?
              #     message = capture.output(route = osrmRoute_shareunnamed(src=df[df$id==case_centroids$ID[i],], dst=df[df$id==a,], sp=TRUE, overview = "simplified"), type="message") #captures the server's error message
              #     if(grepl("NoRoute", message[2])){ # if NoRoute, sett counter to 101 to end loop
              #       attemptcounter = 1001 # break the while loop
              #     }
              #   }
              # }
            }

            
            
            if(!is.null(route)){
              coords <- st_coordinates(route$geometry)
              # creates the walked distance to the start of the route and from the end of it
              walked[i,j] <- distm(c(df[df$id==case_centroids$ID[i],"lon"], df[df$id==case_centroids$ID[i],"lat"]), c(coords[1,"X"], coords[1,"Y"]))  + distm(c(df[df$id==a,"lon"], df[df$id==a,"lat"]), c(coords[nrow(coords),"X"], coords[nrow(coords),"Y"]))
              
              across_border = case_centroids[case_centroids$ID==df[df$id==case_centroids$ID[i],"id"],"country"] != country | case_centroids[case_centroids$ID==df[df$id==a,"id"],"country"] != country
              
              if(route$duration + walked[i,j]*minutes_per_kilometer < minutes_per_kilometer*(distm(c(df[df$id==case_centroids$ID[i],"lon"], df[df$id==case_centroids$ID[i],"lat"]), c(df[df$id==a,"lon"], df[df$id==a,"lat"])))){ # if walking entire way is longer than proposed route
                
                # creates matrices for every country
                dist[i, j] <- route$distance + walked[i,j]
                speed[i, j] <- dist[i,j] / ((route$duration + walked[i,j]*minutes_per_kilometer) / 60)
                adj[i, j] <- 1
                
                #points(c(df[df$id==case_centroids$ID[i],"lon"], coords[,1], df[df$id==a,"lon"]), c(df[df$id==case_centroids$ID[i],"lat"], coords[,2], df[df$id==a,"lat"]), type='l', col = ifelse(across_border, alpha("grey", .5), "dodgerblue3"), lwd = ifelse(across_border, 1.2, 2)) # if you want to plot it
                

                if(runif(1) < 0.03){
                  gmapscount = gmapscount+1
                  print(paste("GMAPS:", gmapscount))

                  gxml = mp_directions(
                    origin = as.numeric(df[df$id==case_centroids$ID[i],2:3]),
                    destination = as.numeric(df[df$id==a,2:3]),
                    alternatives = FALSE,
                    key = gmapskey,
                    quiet = TRUE)

                  if(grepl("ZERO_RESULTS", xml_text(gxml))){
                    countrydf[row,] = c(i,j,route$distance, route$duration, route$share_unnamed, NA, NA)
                  }else{
                  groute = mp_get_routes(gxml)

                  countrydf[row,] = c(i,j,route$distance, route$duration, route$share_unnamed, groute$distance_m, groute$duration_s)
                  }

                 

            }else{
                              countrydf[row,] = c(i,j,route$distance, route$duration, route$share_unnamed, NA, NA)

            }
            





                row = row + 1
                
              } else{ # if not, you just walk the entire direct line
                dist[i, j] <- distm(c(df[df$id==case_centroids$ID[i],"lon"], df[df$id==case_centroids$ID[i],"lat"]), c(df[df$id==a,"lon"], df[df$id==a,"lat"]))
                speed[i, j] <- 60/minutes_per_kilometer
                adj[i, j] <- 1
                
                #points(c(df[df$id==case_centroids$ID[i],"lon"], df[df$id==a,"lon"]), c(df[df$id==case_centroids$ID[i],"lat"],  df[df$id==a,"lat"]), type='l', col = ifelse(across_border, alpha("grey", .2), alpha("dodgerblue1", .4)), lwd = 1) # if you want to plot it
                
              }
            } else{
              dist[i, j] <- 0
              speed[i, j] <- 0
              adj[i, j] <- 0
              #points(c(df[df$id==case_centroids$ID[i],"lon"], df[df$id==a,"lon"]), c(df[df$id==case_centroids$ID[i],"lat"],  df[df$id==a,"lat"]), type='l', col="blue") # if you want to plot it
            }
          }
        }
        
        print(paste0(round(i/n*100, 2), "% of ", country, " completed"))
        
      }
    
      
      # write.csv(dist, file=paste("./Build/temp/raw_from_OSRM/dist/dist_", country, ".csv", sep=""), row.names = FALSE)
      # write.csv(speed, file=paste("./Build/temp/raw_from_OSRM/speed/speed_", country, ".csv", sep=""), row.names = FALSE)
      # write.csv(adj, file=paste("./Build/temp/raw_from_OSRM/adj/adj_", country, ".csv", sep=""), row.names = FALSE)
      # write.csv(case_centroids[,c("ID", "rownumber")], file=paste("./Build/temp/rosettastones/rosetta_", country, ".csv", sep=""), row.names = FALSE)
      write.csv(countrydf, file=paste("./Build/temp/osrmbias/bias_", country, ".csv"), row.names = FALSE)
      # dev.off()
    }
  }
  }
}

sunnmd_df = data.frame("country" = NA, "s_unnamed" = NA, "cor" = NA, "corp" = NA, "withinvar" = NA)
gmaps_df = data.frame("country" = NA, "dist" = NA, "dur" = NA, "gdist" = NA, "gdur" = NA, "pop" = NA)

row = 1

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
  }

  gdf = df[!is.na(df$X.gmapdist.),]
  gdf$country = country




}



