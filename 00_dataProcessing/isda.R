#' Extract and format iSDA data
#'
#' @coords data.frame with 2 columns (Latitude and Longitude)
#' @raster optional boolean to export results in SpatRast (terra) format
#' @return data.frame or SpatRast
#' @examples
#' isda(coords = data.frame("x" = c(9.57, 10.55), "y" = c(11.55, 12.43)))

isda <- function(coords = NULL, raster = FALSE){
  dep <- c("20"="0..20cm","50"="20..50cm")
  tex <- c("clay_content"="clay_tot_psa","sand_content"="sand_tot_psa","silt_content"="silt_tot_psa", "texture_class"="texture.class")
  phy <- c("bulk_density"="db_od","ph"="ph_h2o", "stone_content"="log.wpg2","nitrogen_total"="log.n_tot_ncs")
  che <- c("carbon_total"="log.c_tot","carbon_organic"="log.oc",
           "phosphorous_extractable"="log.p_mehlich3","potassium_extractable"="log.k_mehlich3",
           "zinc_extractable"="log.zn_mehlich3","magnesium_extractable"="log.mg_mehlich3","calcium_extractable"="log.ca_mehlich3",
           "aluminium_extractable"="log.al_mehlich3","iron_extractable"="log.fe_mehlich3","sulphur_extractable"="log.s_mehlich3",
           "cation_exchange_capacity"="log.ecec.f")
  url <- "~/common_data/isda/raw/"
  aoi <- suppressWarnings(terra::vect(sf::st_as_sf(sf::st_as_sfc(sf::st_bbox(c(xmin = min(coords[,1]), xmax = max(coords[,1]), ymax = max(coords[,2]), ymin = min(coords[,2])), crs = sf::st_crs(4326))))))
  out <- coords
  out$location <- paste(out[[2]], out[[1]], sep = "_")
  out <- out[,c(3,2,1)]
  risda <- terra::rast()
  for (par in c(tex, phy, che)) {
      var <- names(c(tex, phy, che)[c(tex, phy, che) == par])
    for (d in dep) {
      lab <- names(dep[dep == d])
      lyr <- paste("sol",par,"m_30m",d,"2001..2017_v0.13_wgs84.tif",sep = "_")
      tif.cog <- paste0(url,lyr)
      if (raster){
        d <- suppressWarnings(terra::crop(terra::rast(tif.cog), aoi))
        names(d) <- paste0(par, "_", d)
        risda <- c(risda, d)
      }
      vals <- NULL
      for (pnt in seq_len(nrow(coords))) {
        pnt <- coords[pnt,]
        val <- terra::extract(d, data.frame(x = pnt[1], y = pnt[2]))[,2]
        if (par %in% c(che, "log.wpg2")){val <- expm1(val / 10)}
        else if (par == "db_od"){val <- val / 100}
        else if (par == "log.n_tot_ncs"){val <- expm1(val / 100)}
        else if (par == "ph_h2o"){val <- val / 10}
        else val <- val
        vals <- c(vals, val)
      }
      out <- cbind(out, vals)
      colnames(out)[ncol(out)] <- c(paste(var, lab, sep = "_"))
    }
    `%not_in%` <- purrr::negate(`%in%`)
    if (var %not_in% c("bedrock_depth_cm", "texture_class")) {
      fava <- (data.frame(out[,ncol(out) - 1] + out[,ncol(out)]))/2
      colnames(fava) <- var
      out <- cbind(out, fava)
    }    
  }
  if(raster){
    return(risda)
  }
  return(out)
}

#' Extract and format iSDA data
#'
#' @coords data.frame with 2 columns (LatLong)
#' @return data.frame
#' @examples
#' extract clay values for set of coordinates
#' isda.geodata(coords = data.frame("x" = c(9.57, 10.55), "y" = c(11.55, 12.43)))
#' extract clay values for AOI
#' isda.geodata(coords = data.frame("x" = c(9.57, 10.55), "y" = c(11.55, 12.43)), raster = TRUE)

isda.geodata <- function(variable = NULL, depth = NULL, coords = NULL, raster = NULL){
  # Create output
  out <- coords
  out$location <- paste(out[[2]], out[[1]], sep = "_")
  out <- out[,c(3,2,1)]
  # Construct file name
  d <- sub(".*-(\\d+).*", "\\1", depth)
  file <- paste("isda", variable, depth, "v0.13_30s.tif", sep = "_")
  # Check if file is locally available; if not download
  if(!file.exists(paste0("~/agwise/rawData/", file))){
    geodata::soil_af_isda(var = variable, depth = d, path = "~/agwise/rawData/")
  }
  # Read file
  r <- suppressWarnings(terra::rast(paste0("~/agwise/rawData/", file)))
  # Create AOI from coords
  aoi <- suppressWarnings(terra::vect(sf::st_as_sf(sf::st_as_sfc(sf::st_bbox(c(xmin = min(coords[,1]), xmax = max(coords[,1]), ymax = max(coords[,2]), ymin = min(coords[,2])), crs = sf::st_crs(4326))))))
  # Subset AOI
  data <- suppressWarnings(terra::crop(r, aoi))
  if(isTRUE(raster)){
    return(data)
  }
  else {
    vals <- terra::extract(data, coords)[2]
    out <- cbind(out, vals)
    return(out)  
  }
}

