#################################################################################################################
## sourcing required packages 
#################################################################################################################
packages_required <- c("terra", "sf", "rgl", "rgdal", "sp", "geodata", "terra", "tidyverse")

# check and install packages that are not yet installed
installed_packages <- packages_required %in% rownames(installed.packages())
if(any(installed_packages == FALSE)){
  install.packages(packages_required[!installed_packages])}

# load required packages
invisible(lapply(packages_required, library, character.only = TRUE))


#################################################################################################################
#################################################################################################################

#' Title cropSoil
#' @description is a function to crop the soil layers for the AOI, using the extent of the country read from the shape file.
#'
#' @param pathIn the path to the gloal soil layers in this case it is "./AgWise/rawData/1_soil/"
#' @param pathOut path to hold the cropped layers
#' @param countryCoord a data frame with longitude and latitude columns. if this is null cropped raser will be returned
#' @param countryName currently works for c("Rwanda", "Ethiopia", "Burundi", "CDR", "Ghana", "Kenya", "Nigeria", "Tanzania") 
#'
#' @return if need be the cropped layer is returned but the main idea is to write  
#'  out the cropped layers in the path as indicated as the pathOut
#'
#' @examples 
#' cropSoil(pathIn = "./AgWise/rawData/1_soil/",
#' pathOut = "./AgWise/EiA_Analytics/AgWISE-UseCaseRAB/inputData/soil",
#' countryCoord = data.frame(longitude = c(29.1, 30.2), latitude = c(1.3, 2.5)),#'  
#' countryName = "Rwanda")
cropSoil <- function(pathIn,  pathOut, countryCoord=NULL, countryName){
  
  
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
  
  ## create a directory to store the cropped / subset soil data
  if (!dir.exists(pathOut)){
    dir.create(pathOut)
  }
  
  
  ## read the relevant gdam and a soil layer to be cropped
  countryShp <- geodata::gadm(country = country, level = 2, path='.')
  
  
  ## read, crop and save 
  listRaster <-list.files(path=pathIn, pattern=".tif$")
  readLayers <- terra::rast(paste(pathIn, listRaster, sep="/"))
  croppedLayer <- terra::crop(readLayers, countryShp)
  
  
  ## get soil organic matter as a funtion of organic carbon
  croppedLayer$`SOM_0-20cm` <- (croppedLayer$`oc_0-20cm` * 2)/10
  croppedLayer$`SOM_20-50cm` <- (croppedLayer$`oc_20-50cm` * 2)/10
  
  
  ##### permanent wilting point ####
  croppedLayer$'PWP_0-20cm' <- (-0.024 * croppedLayer$`sand.tot.psa_0-20cm`/100) + 0.487 *
    croppedLayer$`clay.tot.psa_0-20cm`/100 + 0.006 * croppedLayer$`SOM_0-20cm` + 
    0.005*(croppedLayer$`sand.tot.psa_0-20cm`/100 * croppedLayer$`SOM_0-20cm`) - 
    0.013*(croppedLayer$`clay.tot.psa_0-20cm`/100 * croppedLayer$`SOM_0-20cm`) + 
    0.068*(croppedLayer$`sand.tot.psa_0-20cm`/100 * croppedLayer$`clay.tot.psa_0-20cm`/100 ) + 0.031
  croppedLayer$'PWP_0-20cm' <- (croppedLayer$'PWP_0-20cm' + (0.14 * croppedLayer$'PWP_0-20cm' - 0.02))*100
  
  
  
  croppedLayer$'PWP_20-50cm' <- (-0.024 * croppedLayer$`sand.tot.psa_20-50cm`/100) + 0.487 *
    croppedLayer$`clay.tot.psa_20-50cm`/100 + 0.006 * croppedLayer$`SOM_20-50cm` + 
    0.005*(croppedLayer$`sand.tot.psa_20-50cm`/100 * croppedLayer$`SOM_20-50cm`) - 
    0.013*(croppedLayer$`clay.tot.psa_20-50cm`/100 * croppedLayer$`SOM_20-50cm`) + 
    0.068*(croppedLayer$`sand.tot.psa_20-50cm`/100 * croppedLayer$`clay.tot.psa_20-50cm`/100 ) + 0.031
  croppedLayer$'PWP_20-50cm' <- (croppedLayer$'PWP_20-50cm' + (0.14 * croppedLayer$'PWP_20-50cm' - 0.02))*100
  
  
  
  ##### FC ######
  croppedLayer$'FC_0-20cm' <- -0.251 * croppedLayer$`sand.tot.psa_0-20cm`/100 + 0.195 * 
    croppedLayer$`clay.tot.psa_0-20cm`/100 + 0.011 * croppedLayer$`SOM_0-20cm` + 
    0.006*(croppedLayer$`sand.tot.psa_0-20cm`/100 * croppedLayer$`SOM_0-20cm`) - 
    0.027*(croppedLayer$`clay.tot.psa_0-20cm`/100 * croppedLayer$`SOM_0-20cm`) + 
    0.452*(croppedLayer$`sand.tot.psa_0-20cm`/100 * croppedLayer$`clay.tot.psa_0-20cm`/100) + 0.299
  croppedLayer$'FC_0-20cm' <- (croppedLayer$`FC_0-20cm` + (1.283 * croppedLayer$`FC_0-20cm`^2 - 0.374 * croppedLayer$`FC_0-20cm` - 0.015))*100
  
  
  croppedLayer$'FC_20-50cm' <- -0.251 * croppedLayer$`sand.tot.psa_20-50cm`/100 + 0.195 * 
    croppedLayer$`clay.tot.psa_20-50cm`/100 + 0.011 * croppedLayer$`SOM_20-50cm` + 
    0.006*(croppedLayer$`sand.tot.psa_20-50cm`/100 * croppedLayer$`SOM_20-50cm`) - 
    0.027*(croppedLayer$`clay.tot.psa_20-50cm`/100 * croppedLayer$`SOM_20-50cm`) + 
    0.452*(croppedLayer$`sand.tot.psa_20-50cm`/100 * croppedLayer$`clay.tot.psa_20-50cm`/100) + 0.299
  croppedLayer$'FC_20-50cm' <- (croppedLayer$`FC_20-50cm` + (1.283 * croppedLayer$`FC_20-50cm`^2 - 0.374 * croppedLayer$`FC_20-50cm` - 0.015))*100
  
  
  
  ##### soil water at saturation ######
  
  croppedLayer$'SWS_0-20cm' <- 0.278*(croppedLayer$`sand.tot.psa_0-20cm`/100)+0.034*
    (croppedLayer$`clay.tot.psa_0-20cm`/100)+0.022*croppedLayer$`SOM_0-20cm` -
    0.018*(croppedLayer$`sand.tot.psa_0-20cm`/100*croppedLayer$`SOM_0-20cm`)- 0.027*
    (croppedLayer$`clay.tot.psa_0-20cm`/100*croppedLayer$`SOM_0-20cm`)-
    0.584 * (croppedLayer$`sand.tot.psa_0-20cm`/100*croppedLayer$`clay.tot.psa_0-20cm`/100)+0.078
  croppedLayer$'SWS_0-20cm' <- (croppedLayer$'SWS_0-20cm' +(0.636*croppedLayer$'SWS_0-20cm'-0.107))*100
  croppedLayer$'SWS_0-20cm' <- (croppedLayer$`FC_0-20cm`/100+croppedLayer$`SWS_0-20cm`/100-(0.097*croppedLayer$`sand.tot.psa_0-20cm`/100)+0.043)*100
  
  
  
  croppedLayer$'SWS_20-50cm' <- 0.278*(croppedLayer$`sand.tot.psa_20-50cm`/100)+0.034*
    (croppedLayer$`clay.tot.psa_20-50cm`/100)+0.022*croppedLayer$`SOM_20-50cm` -
    0.018*(croppedLayer$`sand.tot.psa_20-50cm`/100*croppedLayer$`SOM_20-50cm`)- 0.027*
    (croppedLayer$`clay.tot.psa_20-50cm`/100*croppedLayer$`SOM_20-50cm`)-
    0.584 * (croppedLayer$`sand.tot.psa_20-50cm`/100*croppedLayer$`clay.tot.psa_20-50cm`/100)+0.078
  croppedLayer$'SWS_20-50cm' <- (croppedLayer$'SWS_20-50cm' +(0.636*croppedLayer$'SWS_20-50cm'-0.107))*100
  croppedLayer$'SWS_20-50cm' <- (croppedLayer$`FC_20-50cm`/100+croppedLayer$`SWS_20-50cm`/100-(0.097*croppedLayer$`sand.tot.psa_20-50cm`/100)+0.043)*100
  
  
  ### write out the result
  croppedLayerName <- paste(countryName, "iSDA_soils", sep="_")
  terra::writeRaster(croppedLayer, paste0(pathOut, "/" ,croppedLayerName, ".tif", sep=""), filetype="GTiff")
  
  if(!is.null(countryCoord)){
    croppedLayer <- raster::extract(croppedLayer, countryCoord[, c("longitude",  "latitude")]) 
    shp_gadm <- raster::extract(countryShp, countryCoord[, c("longitude",  "latitude")]) 
    Coord_soils <- cbind(countryCoord, croppedLayer)
    Coord_soils$country <- shp_gadm$COUNTRY
    Coord_soils$province <- shp_gadm$NAME_1
    Coord_soils$district <- shp_gadm$NAME_2
    saveRDS(Coord_soils, paste0(pathOut, "/" , croppedLayerName, ".RDS", sep=""))
    return(Coord_soils)
  }else{
    return(croppedLayer)
  }
  
}


