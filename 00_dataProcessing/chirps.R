#' Extract and format CHIRPS data
#'
#' @param startDate starting date of the data extraction
#' @param endDate ending date of the data extraction
#' @coords data.frame with 2 columns (Latitude and Longitude)
#' @raster optional boolean to export results in SpatRast (terra) format
#' @return data.frame or SpatRast
#' @examples
#' chirps(startDate = "2021-05-13", endDate = "2021-09-28", coords = data.frame("x" = c(9.57, 10.55), "y" = c(11.55, 12.43)))

chirps <- function(startDate, endDate, raster = FALSE, coords = NULL){
  dates <- seq.Date(as.Date(startDate, format = "%Y-%m-%d"), as.Date(endDate, format = "%Y-%m-%d"), by = "day")
  year <- unique(format(dates, "%Y"))
  chirps <- terra::rast()
  for (file in list.files('/home/jovyan/agwise/rawData/2_weather/rain_chirps/raw', pattern = paste0(year, collapse = '|'), full.names = TRUE)) {
    terra::add(chirps) <- terra::rast(file)
  }
  names(chirps) <- as.character(format(as.Date(terra::time(chirps)), "%Y%m%d"))
  chirps <- chirps[[as.character(format(dates, format = "%Y%m%d"))]]
  if (raster){
    aoi <- suppressWarnings(terra::vect(sf::st_as_sf(sf::st_as_sfc(sf::st_bbox(c(xmin = min(coords[,1]), xmax = max(coords[,1]), ymax = max(coords[,2]), ymin = min(coords[,2])), crs = sf::st_crs(4326))))))
    chirps <- terra::crop(chirps,aoi)
    return(chirps)
  }
  else {
    w <- data.frame()
    for (pnt in seq(1:nrow(coords))){
      lon <- coords[pnt, 1]
      lat <- coords[pnt, 2]
      z <- terra::extract(chirps,data.frame(lon,lat))
      out <- data.frame("dates" = dates)
      out$X <- lon
      out$Y <- lat
      out$RAIN <- as.vector(t(z[2:length(z)]))
      out <- data.frame("X" = out$X, "Y" = out$Y, "dates" = out$dates,
                      "year" = format(as.Date(out$dates), format = "%Y"),
                      "month" = format(as.Date(out$dates), format = "%m"),
                      "day" = format(as.Date(out$dates), format = "%d"),
                      "RAIN" = out$RAIN)
      w <- rbind(w, out)
    }
    return(w)
  }
}

#' Extract and format CHIRPS data
#'
#' @coords data.frame with 2 columns (Latitude and Longitude)
#' @dates vector with 2 elements (startDate and endDate)
#' @raster optional boolean to export results in raster format
#' @return data.frame
#' @examples
#' chirps.geodata(dates = c(startDate = "2017-05-13", endDate = "2018-09-28"), coords = data.frame("x" = c(9.57, 10.55), "y" = c(11.55, 12.43)))

chirps.geodata <- function(coords = NULL, dates = NULL, raster = NULL){
  # Create output
  out <- coords
  out$location <- paste(out[[2]], out[[1]], sep = "_")
  out <- out[,c(3,2,1)]
  # Construct the timeframe of interest (TOI)
  dates <- seq.Date(as.Date(dates[1], format = "%Y-%m-%d"), as.Date(dates[2], format = "%Y-%m-%d"), by = "day")
  years <- unique(format(dates, "%Y"))
  files <- paste0("~/agwise/rawData/chirps_precipitation_", years, ".nc")
  # Read relevant files into a raster stack
  chirps <- terra::rast()
  for (file in files) {
    # Check if file is locally available; if not download
    if(!file.exists(file)){
      year <- gsub("\\D", "", file)
      download.file(url = paste0("https://data.chc.ucsb.edu/products/CHIRPS-2.0/global_daily/netcdf/p05/chirps-v2.0.", year, ".days_p05.nc"), destfile = file)
    }
    terra::add(chirps) <- terra::rast(file)
  }
  # Rename layers of stack to dates
  names(chirps) <- as.character(format(as.Date(terra::time(chirps)), "%Y%m%d"))
  chirps <- chirps[[as.character(format(dates, format = "%Y%m%d"))]]
  # Subset AOI
  aoi <- suppressWarnings(terra::vect(sf::st_as_sf(sf::st_as_sfc(sf::st_bbox(c(xmin = min(coords[,1]), xmax = max(coords[,1]), ymax = max(coords[,2]), ymin = min(coords[,2])), crs = sf::st_crs(4326))))))
  chirps <- terra::crop(chirps, aoi)
  if (isTRUE(raster)){
    return(chirps)
  }
  else {
    w <- data.frame()
    for (pnt in seq(1:nrow(coords))){
      lon <- coords[pnt, 1]
      lat <- coords[pnt, 2]
      z <- terra::extract(chirps,data.frame(lon,lat))
      out <- data.frame("dates" = dates)
      out$X <- lon
      out$Y <- lat
      out$RAIN <- as.vector(t(z[2:length(z)]))
      out <- data.frame("X" = out$X, "Y" = out$Y, "dates" = out$dates,
                        "year" = format(as.Date(out$dates), format = "%Y"),
                        "month" = format(as.Date(out$dates), format = "%m"),
                        "day" = format(as.Date(out$dates), format = "%d"),
                        "RAIN" = out$RAIN)
      w <- rbind(w, out)
    }
    return(w)
  }
}
