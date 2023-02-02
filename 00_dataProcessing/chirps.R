#' Extract and format CHIRPS data
#'
#' @param startDate starting date of the data extraction
#' @param endDate ending date of the data extraction
#' @df data.frame with 2 columns (LatLong)
#' @raster optional boolean to export results in raster format
#' @return data.frame
#' @examples
#' chirps(startDate = "2021-05-13", endDate = "2021-09-28", df = data.frame("x" = c(9.57, 10.55), "y" = c(11.55, 12.43)))

chirps <- function(startDate, endDate, raster = FALSE, df = NULL){
  dates <- seq.Date(as.Date(startDate, format = "%Y-%m-%d"), as.Date(endDate, format = "%Y-%m-%d"), by = "day")
  year <- unique(format(dates, "%Y"))
  chirps <- terra::rast()
  for (file in list.files('/home/jovyan/agwise/rawData/2_weather/rain_chirps/raw', pattern = paste0(year, collapse = '|'), full.names = TRUE)) {
    terra::add(chirps) <- terra::rast(file)
  }
  names(chirps) <- as.character(format(as.Date(terra::time(chirps)), "%Y%m%d"))
  chirps <- chirps[[as.character(format(dates, format = "%Y%m%d"))]]
  if (raster){
    aoi <- suppressWarnings(terra::vect(sf::st_as_sf(sf::st_as_sfc(sf::st_bbox(c(xmin = min(df[,1]), xmax = max(df[,1]), ymax = max(df[,2]), ymin = min(df[,2])), crs = sf::st_crs(4326))))))
    chirps <- terra::crop(chirps,aoi)
    return(chirps)
  }
  else {
    w <- data.frame()
    for (pnt in seq(1:nrow(df))){
      lon <- df[pnt, 1]
      lat <- df[pnt, 2]
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
