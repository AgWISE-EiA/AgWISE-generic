agera5 <- function(startDate, endDate, coordPoints = NULL, raster = FALSE){
  dates <- seq.Date(as.Date(startDate, format = "%Y-%m-%d"), as.Date(endDate, format = "%Y-%m-%d"), by = "day")
  year <- unique(format(dates, "%Y"))
  wind <- terra::rast()
  temp <- terra::rast()
  tmin <- terra::rast()
  tmax <- terra::rast()
  rhum <- terra::rast()
  srad <- terra::rast()
  for (file in list.files(path = '/home/jovyan/agwise/rawData/', pattern = paste0(year, collapse = '|'), full.names = TRUE)) {
    if (grepl("_wind_", file, fixed = TRUE)){terra::add(wind) <- terra::rast(file)}
    else if (grepl("_temp_", file, fixed = TRUE)){terra::add(temp) <- terra::rast(file)}
    else if (grepl("_tmin_", file, fixed = TRUE)){terra::add(tmin) <- terra::rast(file)}
    else if (grepl("_tmax_", file, fixed = TRUE)){terra::add(tmax) <- terra::rast(file)}
    else if (grepl("_rhum_", file, fixed = TRUE)){terra::add(rhum) <- terra::rast(file)}
    else if (grepl("_srad_", file, fixed = TRUE)){terra::add(srad) <- terra::rast(file)}
  }
  names(wind) <- as.character(format(as.Date(terra::time(wind)), "%Y%m%d"))
  names(temp) <- as.character(format(as.Date(terra::time(temp)), "%Y%m%d"))
  names(tmin) <- as.character(format(as.Date(terra::time(tmin)), "%Y%m%d"))
  names(tmax) <- as.character(format(as.Date(terra::time(tmax)), "%Y%m%d"))
  names(rhum) <- as.character(format(as.Date(terra::time(rhum)), "%Y%m%d"))
  names(srad) <- as.character(format(as.Date(terra::time(srad)), "%Y%m%d"))
  wind <- wind[[as.character(format(dates, format = "%Y%m%d"))]]
  temp <- temp[[as.character(format(dates, format = "%Y%m%d"))]]
  tmin <- tmin[[as.character(format(dates, format = "%Y%m%d"))]]
  tmax <- tmax[[as.character(format(dates, format = "%Y%m%d"))]]
  rhum <- rhum[[as.character(format(dates, format = "%Y%m%d"))]]
  srad <- srad[[as.character(format(dates, format = "%Y%m%d"))]]
  if (raster){
    names(wind) <- as.character(paste0("wind", format(as.Date(terra::time(wind)), "%Y%m%d")))
    names(temp) <- as.character(paste0("temp", format(as.Date(terra::time(temp)), "%Y%m%d")))
    names(tmin) <- as.character(paste0("tmin", format(as.Date(terra::time(tmin)), "%Y%m%d")))
    names(tmax) <- as.character(paste0("tmax", format(as.Date(terra::time(tmax)), "%Y%m%d")))
    names(rhum) <- as.character(paste0("rhum", format(as.Date(terra::time(rhum)), "%Y%m%d")))
    names(srad) <- as.character(paste0("srad", format(as.Date(terra::time(srad)), "%Y%m%d")))
    agera <- c(wind, temp, tmin, tmax, rhum, srad)
    aoi <- suppressWarnings(terra::vect(sf::st_as_sf(sf::st_as_sfc(sf::st_bbox(c(xmin = min(coordPoints[,1]), xmax = max(coordPoints[,1]), ymax = max(coordPoints[,2]), ymin = min(coordPoints[,2])), crs = sf::st_crs(4326))))))
    agera <- terra::crop(agera,aoi)
    return(agera)
  }
  else {
    w <- data.frame()
    for (pnt in seq(1:nrow(coordPoints))){
      lon <- coordPoints[pnt, 1]
      lat <- coordPoints[pnt, 2]
      z.wind <- terra::extract(wind,data.frame(lon,lat))
      z.temp <- terra::extract(temp,data.frame(lon,lat))
      z.tmin <- terra::extract(tmin,data.frame(lon,lat))
      z.tmax <- terra::extract(tmax,data.frame(lon,lat))
      z.rhum <- terra::extract(rhum,data.frame(lon,lat))
      z.srad <- terra::extract(srad,data.frame(lon,lat))
      out <- data.frame("dates" = dates)
      out$X <- lon
      out$Y <- lat
      out$WIND <- as.vector(t(z.wind[2:length(z.wind)]))
      out$TEMP <- as.vector(t(z.temp[2:length(z.temp)]))
      out$TMIN <- as.vector(t(z.tmin[2:length(z.tmin)]))
      out$TMAX <- as.vector(t(z.tmax[2:length(z.tmax)]))
      out$RELH <- as.vector(t(z.rhum[2:length(z.rhum)]))
      out$RADN <- as.vector(t(z.srad[2:length(z.srad)]))
      out <- data.frame("X" = out$X, "Y" = out$Y, "dates" = out$dates,
                      "year" = format(as.Date(out$dates), format = "%Y"),
                      "month" = format(as.Date(out$dates), format = "%m"),
                      "day" = format(as.Date(out$dates), format = "%d"),
                      "WIND" = out$WIND,
                      "TEMP" = out$TEMP-273,
                      "TMIN" = out$TMIN-273,
                      "TMAX" = out$TMAX-273,
                      "RHUM" = out$RELH,
                      "SRAD" = out$RADN*1e-6)
      w <- rbind(w, out)
    }
    return(w)
  }
}
