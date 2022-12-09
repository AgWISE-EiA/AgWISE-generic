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
#################################################################################################################

#' Title cropSoil
#' @description is a function to crop the soil layers for the AOI, using the extent of the country read from the shape file.
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







#####################################################################################################
######################################################################################################

#' Title get_soil Hydraulics
#' @description this function computes the soil hydraulics (PWP, SWS and FC) based on sand, clay and OM content and using the 
#' pedo-transfer equation used in harvest choice. A user can decide to process a raster or a GPS readings.
#'
#' @param pathIn is the path where the sand, clay and oc tif layers or RDS files are saved
#' @param countryName 
#' @param pathOut is the path where the soil hydraulics data will be written out
#' @param returnFormat  c("raster", "point")
#'
#' @return depending on the user preference either a raster layer or a data frame will be written out in the pathOUt
#' @export
#'
#' @examples get_soilHydraulics(pathIn = "./AgWise/EiA_Analytics/AgWISE-UseCaseRAB/inputData/soil",
#' countryName = "Rwanda", pathOut = "./AgWise/EiA_Analytics/AgWISE-UseCaseRAB/inputData/soil",
#' returnFormat = "raster")
get_soilHydraulics <- function(pathIn, countryName, pathOut, returnFormat = c("raster", "point")){
  
  if(returnFormat == "point"){
    
    listRDS <-list.files(path=pathIn, pattern=".RDS")
    relevantRDS_20 <- listRDS[c(grep("0-20cm", listRDS))]
    relevantRDS_2050 <- listRDS[c(grep("20-50cm", listRDS))]
    
    sand_20 <- readRDS(paste(pathIn, relevantRDS_20[grep("sand", relevantRDS_20)], sep="/"))
    sand_2050 <- readRDS(paste(pathIn, relevantRDS_2050[grep("sand", relevantRDS_2050)], sep="/"))
    
    sand_20 <- sand_20[, c("country", "province", "district", "longitude","latitude", "sand.tot.psa_0-20cm")]
    sand_2050 <- sand_2050[, c("country", "province", "district", "longitude","latitude", "sand.tot.psa_20-50cm")]
    
    sand_20$location <- paste(sand_20$longitude, sand_20$latitude, sep="_")
    sand_2050$location <- paste(sand_2050$longitude, sand_2050$latitude, sep="_")
    
    
    clay_20 <- readRDS(paste(pathIn, relevantRDS_20[grep("clay", relevantRDS_20)], sep="/"))
    clay_2050 <- readRDS(paste(pathIn, relevantRDS_2050[grep("clay", relevantRDS_2050)], sep="/"))
    clay_20$location <- paste(clay_20$longitude, clay_20$latitude, sep="_")
    clay_2050$location <- paste(clay_2050$longitude, clay_2050$latitude, sep="_")
    clay_20 <- clay_20[, c("location", "clay.tot.psa_0-20cm")]
    clay_2050 <- clay_2050[, c("location", "clay.tot.psa_20-50cm")]
    
    ## om = oc*2 and then changed to percentage
    SOMperc_20 <- readRDS(paste(pathIn, relevantRDS_20[grep("oc", relevantRDS_20)], sep="/"))
    SOMperc_2050 <- readRDS(paste(pathIn, relevantRDS_2050[grep("oc", relevantRDS_2050)], sep="/"))
    
    SOMperc_20$location <- paste(SOMperc_20$longitude, SOMperc_20$latitude, sep="_")
    SOMperc_2050$location <- paste(SOMperc_2050$longitude, SOMperc_2050$latitude, sep="_")
    
    SOMperc_20 <- SOMperc_20[, c("location", "oc_0-20cm")]
    SOMperc_2050 <- SOMperc_2050[, c("location", "oc_20-50cm")]
    
    SOMperc_20$`om_0-20cm` <- (SOMperc_20$`oc_0-20cm` * 2)/10
    SOMperc_2050$`om_20-50cm` <- (SOMperc_2050$`oc_20-50cm`*2)/10
    
   
    dfList_20 <- list(sand_20, clay_20, SOMperc_20)      
    df_20 <- dfList_20 %>% reduce(full_join, by=c("location"))
    
    dfList_2050 <- list(sand_2050, clay_2050, SOMperc_2050)      
    df_2050 <- dfList_2050 %>% reduce(full_join, by=c("location"))
    
    
    
    
    ##### permanent wilting point ####
 
    PWP_20p <- sand_20
    PWP_20p$'PWP_0-20cm' <- (-0.024 * df_20$`sand.tot.psa_0-20cm`/100) + 0.487 * df_20$`clay.tot.psa_0-20cm`/100 + 0.006 * 
      df_20$`om_0-20cm` + 
      0.005*(df_20$`sand.tot.psa_0-20cm`/100 * df_20$`om_0-20cm`) - 0.013*(df_20$`clay.tot.psa_0-20cm`/100 * df_20$`om_0-20cm`) + 
      0.068*(df_20$`sand.tot.psa_0-20cm`/100 * df_20$`clay.tot.psa_0-20cm`/100 ) + 0.031
    PWP_20p$'PWP_0-20cm' <- (PWP_20p$'PWP_0-20cm' + (0.14 * PWP_20p$'PWP_0-20cm' - 0.02))*100
    
    
    PWP_2050p <- sand_2050
    PWP_2050p$'PWP_20-50cm' <- (-0.024 * df_2050$`sand.tot.psa_20-50cm`/100) + 0.487 * df_2050$`clay.tot.psa_20-50cm`/100 + 0.006 * 
      df_2050$`om_20-50cm` + 
      0.005*(df_2050$`sand.tot.psa_20-50cm`/100 * df_2050$`om_20-50cm`) - 0.013*(df_2050$`clay.tot.psa_20-50cm`/100 *
                                                                                   df_2050$`om_20-50cm`) + 
      0.068*(df_2050$`sand.tot.psa_20-50cm`/100 * df_2050$`clay.tot.psa_20-50cm`/100 ) + 0.031
    PWP_2050p$'PWP_20-50cm' <- (PWP_2050p$'PWP_20-50cm' + (0.14 * PWP_2050p$'PWP_20-50cm' - 0.02))*100
    
    
   
    ##### FC ######
    FC_20p <- sand_20
    FC_20p$'FC_0-20cm' <- -0.251 * df_20$`sand.tot.psa_0-20cm`/100 + 0.195 * df_20$`clay.tot.psa_0-20cm`/100 + 0.011 *
      df_20$`om_0-20cm` + 
      0.006*(df_20$`sand.tot.psa_0-20cm`/100 * df_20$`om_0-20cm`) - 0.027*(df_20$`clay.tot.psa_0-20cm`/100 * 
                                                                             df_20$`om_0-20cm`) + 
      0.452*(df_20$`sand.tot.psa_0-20cm`/100 * df_20$`clay.tot.psa_0-20cm`/100) + 0.299
    FC_20p$'FC_0-20cm' <- (FC_20p$'FC_0-20cm' + (1.283 * FC_20p$'FC_0-20cm'^2 - 0.374 * FC_20p$'FC_0-20cm' - 0.015))*100
    
    
    FC_2050p <- sand_2050
    FC_2050p$'FC_20-50cm' <- -0.251 * df_2050$`sand.tot.psa_20-50cm`/100 + 0.195 * df_2050$`clay.tot.psa_20-50cm`/100 + 0.011 *
      df_2050$`om_20-50cm` + 
      0.006*(df_2050$`sand.tot.psa_20-50cm`/100 * df_2050$`om_20-50cm`) - 0.027*(df_2050$`clay.tot.psa_20-50cm`/100 * 
                                                                                   df_2050$`om_20-50cm`) + 
      0.452*(df_2050$`sand.tot.psa_20-50cm`/100 * df_2050$`clay.tot.psa_20-50cm`/100) + 0.299
    FC_2050p$'FC_20-50cm' <- (FC_2050p$'FC_20-50cm' + (1.283 * FC_2050p$'FC_20-50cm'^2 - 0.374 * FC_2050p$'FC_20-50cm' - 0.015))*100
    
    
    
    ##### soil water at saturation ######
    SWS_20p <- sand_20
    SWS_20p$'SWS_0-20cm' <- 0.278*(df_20$`sand.tot.psa_0-20cm`/100)+0.034*(df_20$`clay.tot.psa_0-20cm`/100)+0.022*
      df_20$`om_0-20cm` - 0.018*(df_20$`sand.tot.psa_0-20cm`/100*df_20$`om_0-20cm`)- 0.027*
      (df_20$`clay.tot.psa_0-20cm`/100*df_20$`om_0-20cm`)-
      0.584 * (df_20$`sand.tot.psa_0-20cm`/100*df_20$`clay.tot.psa_0-20cm`/100)+0.078
    
    SWS_20p$B <- (SWS_20p$'SWS_0-20cm' +(0.636*SWS_20p$'SWS_0-20cm'-0.107))*100
    SWS_20p$'SWS_0-20cm' <- (FC_20p$'FC_0-20cm'/100+SWS_20p$B/100-(0.097*df_20$`sand.tot.psa_0-20cm`/100)+0.043)*100
    
    
    SWS_2050p <- sand_2050
    SWS_2050p$'SWS_20-50cm' <- 0.278*(df_2050$`sand.tot.psa_20-50cm`/100)+0.034*(df_2050$`clay.tot.psa_20-50cm`/100)+0.022*
      df_2050$`om_20-50cm` - 0.018*(df_2050$`sand.tot.psa_20-50cm`/100*df_2050$`om_20-50cm`)- 0.027*
      (df_2050$`clay.tot.psa_20-50cm`/100 * df_2050$`om_20-50cm`)-
      0.584 * (df_2050$`sand.tot.psa_20-50cm`/100*df_2050$`clay.tot.psa_20-50cm`/100)+0.078
    SWS_2050p$B <- (SWS_2050p$'SWS_20-50cm' +(0.636*SWS_2050p$'SWS_20-50cm'-0.107))*100
    SWS_2050p$'SWS_20-50cm' <- (FC_2050p$'FC_20-50cm'/100+ SWS_2050p$B/100-(0.097*df_2050$`sand.tot.psa_20-50cm`/100)+0.043)*100
    
    
    
    
    ## create a directory to store the cropped / subset soil data
    if (!dir.exists(pathOut)){
      dir.create(pathOut)
    }

    saveRDS(PWP_20p, paste0(pathOut, "/" , paste(countryName, "PWP_0-20cm_30s.RDS", sep="_"), sep=""))
    saveRDS(PWP_2050p, paste0(pathOut, "/" , paste(countryName, "PWP_20-50cm_30s.RDS", sep="_"), sep=""))
    saveRDS(FC_20p, paste0(pathOut, "/" , paste(countryName, "FC_0-20cm_30s.RDS", sep="_"), sep=""))
    saveRDS(FC_2050p, paste0(pathOut, "/" , paste(countryName, "FC_20-50cm_30s.RDS", sep="_"), sep=""))
    saveRDS(SWS_20p, paste0(pathOut, "/" , paste(countryName, "SWS_0-20cm_30s.RDS", sep="_"), sep=""))
    saveRDS(SWS_2050p, paste0(pathOut, "/" , paste(countryName, "SWS_20-50cm_30s.RDS", sep="_"), sep=""))
    
    
    
  }else if(returnFormat == "raster"){
    ## read the relevant layers
    listRaster <-list.files(path=pathIn, pattern=".tif$")
    relevantRaster_20 <- listRaster[c(grep("0-20cm", listRaster))]
    relevantRaster_2050 <- listRaster[c(grep("20-50cm", listRaster))]
    
    
    sand_20 <- terra::rast(paste(pathIn, relevantRaster_20[grep("sand", relevantRaster_20)], sep="/"))
    sand_2050 <- terra::rast(paste(pathIn, relevantRaster_2050[grep("sand", relevantRaster_2050)], sep="/"))
    
    clay_20 <- terra::rast(paste(pathIn, relevantRaster_20[grep("clay", relevantRaster_20)], sep="/"))
    clay_2050 <- terra::rast(paste(pathIn, relevantRaster_2050[grep("clay", relevantRaster_2050)], sep="/"))
    
    SOCperc_20 <- terra::rast(paste(pathIn, relevantRaster_20[grep("oc", relevantRaster_20)], sep="/"))
    SOMperc_20 <- (SOCperc_20*2)/10
    
    SOCperc_2050 <- terra::rast(paste(pathIn, relevantRaster_2050[grep("oc", relevantRaster_2050)], sep="/"))
    SOMperc_2050 <- (SOCperc_2050*2)/10
    
    
    
    ##### permanent wilting point ####
    PWP_20 <-rast(sand_20)
    PWP_20 <- (-0.024 * sand_20/100) + 0.487 * clay_20/100 + 0.006 * SOMperc_20 + 
      0.005*(sand_20/100 * SOMperc_20) - 0.013*(clay_20/100 * SOMperc_20) + 
      0.068*(sand_20/100 * clay_20/100 ) + 0.031
    PWP_20 <- (PWP_20 + (0.14 * PWP_20 - 0.02))*100
    names(PWP_20) <- "PWP_0-20cm"
    
    
    PWP_2050 <-rast(sand_2050)
    PWP_2050 <- (-0.024 * sand_2050/100) + 0.487 * clay_2050/100 + 0.006 * SOMperc_2050 + 
      0.005*(sand_2050/100 * SOMperc_2050) - 0.013*(clay_2050/100 * SOMperc_2050) + 
      0.068*(sand_2050/100 * clay_2050/100 ) + 0.031
    PWP_2050 <- (PWP_2050 + (0.14 * PWP_2050 - 0.02))*100
    names(PWP_2050) <- "PWP_20-50cm"
    
    
    ##### FC ######
    FC_20 <- rast(sand_20)
    FC_20 <- -0.251 * sand_20/100 + 0.195 * clay_20/100 + 0.011 * SOMperc_20 + 
      0.006*(sand_20/100 * SOMperc_20) - 0.027*(clay_20/100 * SOMperc_20) + 
      0.452*(sand_20/100 * clay_20/100) + 0.299
    FC_20 <- (FC_20 + (1.283 * FC_20^2 - 0.374 * FC_20 - 0.015))*100
    names(FC_20) <- "FC_0-20cm"
    
    FC_2050 <- rast(sand_2050)
    FC_2050 <- -0.251 * sand_2050/100 + 0.195 * clay_2050/100 + 0.011 * SOMperc_2050 + 
      0.006*(sand_2050/100 * SOMperc_2050) - 0.027*(clay_2050/100 * SOMperc_2050) + 
      0.452*(sand_2050/100 * clay_2050/100) + 0.299
    FC_2050 <- (FC_2050 + (1.283 * FC_2050^2 - 0.374 * FC_2050 - 0.015))*100
    names(FC_2050) <- "FC_20-50cm"
    
    
    ##### soil water at saturation ######
    SWS_20_A <- rast(sand_20)
    SWS_20_A <- 0.278*(sand_20/100)+0.034*(clay_20/100)+0.022*SOMperc_20 -
      0.018*(sand_20/100*SOMperc_20)- 0.027*(clay_20/100*SOMperc_20)-
      0.584 * (sand_20/100*clay_20/100)+0.078
    SWS_20_B <- (SWS_20_A +(0.636*SWS_20_A-0.107))*100
    SWS_20 <- (FC_20/100+SWS_20_B/100-(0.097*sand_20/100)+0.043)*100
    names(SWS_20) <- "SWS_0-20cm"
    
    
    SWS_2050_A <- rast(sand_2050)
    SWS_2050_A <- 0.278*(sand_2050/100)+0.034*(clay_2050/100)+0.022*SOMperc_2050 -
      0.018*(sand_2050/100*SOMperc_2050)- 0.027*(clay_2050/100*SOMperc_2050)-
      0.584 * (sand_2050/100*clay_2050/100)+0.078
    SWS_2050_B <- (SWS_2050_A +(0.636*SWS_2050_A-0.107))*100
    SWS_2050 <- (FC_2050/100+SWS_2050_B/100-(0.097*sand_2050/100)+0.043)*100
    names(SWS_2050) <- "SWS_20-50cm"
    
    
    ## create a directory to store the cropped / subset soil data
    if (!dir.exists(pathOut)){
      dir.create(pathOut)
    }
    
    
    terra::writeRaster(PWP_20, paste0(pathOut, "/" ,countryName, "_PWP_0-20cm_30s.tif", sep=""), filetype="GTiff")
    terra::writeRaster(PWP_2050, paste0(pathOut, "/" ,countryName, "_PWP_20-50cm_30s.tif", sep=""), filetype="GTiff")
    
    terra::writeRaster(FC_20, paste0(pathOut, "/" ,countryName, "_FC_0-20cm_30s.tif", sep=""), filetype="GTiff")
    terra::writeRaster(FC_2050, paste0(pathOut, "/" ,countryName, "_FC_20-50cm_30s.tif", sep=""), filetype="GTiff")
    
    terra::writeRaster(SWS_20, paste0(pathOut, "/" ,countryName, "_SWS_0-20cm_30s.tif", sep=""), filetype="GTiff")
    terra::writeRaster(SWS_2050, paste0(pathOut, "/" ,countryName, "_SWS_20-50cm_30s.tif", sep=""), filetype="GTiff")
    
  }
  
  
  
}









