#################################################################################################################
## sourcing required packages 
#################################################################################################################
packages_required <- c("terra", "sf", "rgl", "rgdal", "dplyr", "sp", "plyr", "geodata", "terra", "tidyverse")

# check and install packages that are not yet installed
installed_packages <- packages_required %in% rownames(installed.packages())
if(any(installed_packages == FALSE)){
  install.packages(packages_required[!installed_packages])}

# load required packages
invisible(lapply(packages_required, library, character.only = TRUE))


#################################################################################################################
## function to crop soil layers for AOI
#################################################################################################################


#' Title function to crop the soil layers for the AOI, using the extent of the country read from the shape file
#'
#' @param soilLayer global/Africa layer for a specific soil variable 
#' @param pathOut path to hold the cropped layers
#' @param returnFormat can be either raster or point. When it is set to point, countryCoord has to be provided
#' @param countryCoord a data frame with longitude and latitude columns
#' @param countryName currently works for c("Rwanda", "Ethiopia", "Burundi", "CDR", "Ghana", "Kenya", "Nigeria", "Tanzania") 
#'
#' @return if need be the cropped layer is returned but the main idea is to write  
#'  out the cropped layers in the path as indicated as the pathOut
#'
#' @examples 
#' cropSoil("isda_ca_0-20cm_v0.13_30s.tif", 
#' returnFormat = "raster, 
#' pathIn = "./AgWise/rawData/1_soil/",
#' pathOut = "./AgWise/EiA_Analytics/AgWISE-UseCaseRAB/inputData/soil", 
#' countryName = "Rwanda")
cropSoil <- function(soilLayer, pathIn,  pathOut, returnFormat = c("raster", "point"), countryCoord=NULL, countryName){
  
  if(returnFormat != "raster" & is.null(countryCoord) ){
    return("Please provide a file with the GPS coordinates or set returnFormat to raster")
  }else{
    if(countryName == "Rwanda"){
      country <- "RWA"
    }else if(countryName == "Ethiopia"){
      country <- "ETH"
    }else if(countryName == "Burundi"){
      country <- "BDI"
    }else if(countryName == "CDR"){
      country <- "COD"
    }else if(countryName == "Ghana"){
      country <- "GHA"
    }else if(countryName == "Kenya"){
      country <- "KEN"
    }else if(countryName == "Nigeria"){
      country <- "NGA"
    }else if(countryName == "Tanzania"){
      country <- "TZA"
    }
    
    ## read the relevant gdam and a soil layer to be cropped
    countryShp <- geodata::gadm(country = country, level = 2, path='.')
    readLayer <- terra::rast(paste(pathIn, soilLayer, sep="/"))
    
    ## create a directory to store the cropped / subset soil data
    if (!dir.exists(pathOut)){
      dir.create(pathOut)
    }
    
    ## crop/subset and write out
    if(returnFormat == "raster"){
      croppedLayer <- terra::crop(readLayer, countryShp)
      croppedLayerName <- paste(countryName, soilLayer, sep="_")
      terra::writeRaster(croppedLayer, paste0(pathOut, "/" ,croppedLayerName, sep=""), filetype="GTiff")
      return (croppedLayer)
      
    }else{
      
      croppedLayer <- raster::extract(readLayer, countryCoord[, c("longitude",  "latitude")]) 
      shp_gadm <- raster::extract(countryShp, countryCoord[, c("longitude",  "latitude")]) 
      
      countryCoord <- cbind(countryCoord, croppedLayer)
      countryCoord$country <- shp_gadm$COUNTRY
      countryCoord$province <- shp_gadm$NAME_1
      countryCoord$district <- shp_gadm$NAME_2
      
      soilLayer2 <- gsub(".tif",".RDS", soilLayer)
      subsetLayerName <- paste(countryName, soilLayer2, sep="_")
      saveRDS(countryCoord, paste0(pathOut, "/" ,subsetLayerName, sep=""))
      return(countryCoord)
      
    }

  }
  
}
