osrmRoute_shareunnamed = function (src, dst, loc, overview = "simplified", exclude, returnclass, 
          osrm.server = getOption("osrm.server"), osrm.profile = getOption("osrm.profile")) 
{
  opt <- options(error = NULL)
  on.exit(options(opt), add = TRUE)
  if (!missing(returnclass)) {
    warning("\"returnclass\" is deprecated.", call. = FALSE)
  }
  url <- base_url(osrm.server, osrm.profile, "route")
  #url <- paste0(osrm.server, "/", osrm.profile, "/", "route")
  if (missing(loc)) {
    src <- input_route(x = src, id = "src", single = TRUE)
    dst <- input_route(x = dst, id = "dst", single = TRUE)
    id1 <- src$id
    id2 <- dst$id
    oprj <- src$oprj
    coords <- paste0(src$lon, ",", src$lat, ";", dst$lon, 
                     ",", dst$lat)
  }
  else {
    loc <- input_route(x = loc, single = FALSE)
    oprj <- loc$oprj
    id1 <- loc$id1
    id2 <- loc$id2
    coords <- paste0(apply(cbind(loc$lon, loc$lat), MARGIN = 1, 
                           FUN = paste0, collapse = ","), collapse = ";")
  }
  url <- paste0(url, coords, "?alternatives=false&geometries=polyline&steps=false&overview=", 
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
                                                                                 rcoords, ")")), crs = 4326, row.names = paste(id1, 
                                                                                                                               id2, sep = "_"), share_unnamed = weighted.mean(res$waypoints$name == "", w = res$waypoints$distance))
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
