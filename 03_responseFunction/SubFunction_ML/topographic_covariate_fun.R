##############################################################################################
## Create topographic variables
##############################################################################################
# DEM data downloaded - SRTM - "SRTM void filled" from earth explorer
#' Title: function to aggregate data from get_covariates (eg. for rainfall, sum of the daily rainfall over the cropping season)
#' @param path the directory where dem is stored
#' @param dem file name of dem data
#' @param countryName name of country
#' @param pathIn directory where input data are stored
#' @param pathOut the directory for writing the outputs
#' @return
#'        slope : slope
#'        tpi : topographic positioning index
#'        tri : topographic ruggedness index
#' @examples: 
#' create_topo_covariate('/home/jovyan/agwise/EiA_Analytics/AgWISE-UseCaseRAB/inputData/DEM', 
#'                '/home/jovyan/agwise/EiA_Analytics/AgWISE-UseCaseRAB/intermediateOutput', 
#'                    "rwanda_dem.tif", 
#'                   "Rwanda")

##############################################################################################
create_topo_covariate <-function(pathIn, pathOut, dem, countryName
                               ){

  # install missing packages
  packages <- c("terra", "geodata", "rgdal", "dplyr", "sp")

  installed_packages <- packages %in% rownames(installed.packages())
  if (any(installed_packages == FALSE)) {
    install.packages(packages[!installed_packages])
  }

  # packages loading
  invisible(lapply(packages, library, character.only = TRUE))
  
  # specify the country name
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
      
  #get country adm vector
  print(noquote("reading country shapefile"))
  aoi <- geodata::gadm(country = country , level = 1, path = getwd())
  
  #read dem data, project to wgs 1984 geographic & crop raster
  print(noquote("reading and masking input dem"))
  dem <- terra::rast(paste(pathIn, dem, sep = "/")) %>% 
    terra::project(crs(aoi), method = 'bilinear') %>% 
    terra::crop(aoi) %>% terra::mask(aoi)
      
  #calculate the slope, tpi, tri
  print(noquote("calculating slope"))
  slope <-
    terra::terrain(dem, v = 'slope', unit = 'degrees') %>% terra::crop(aoi, mask = T)
  print(noquote("calculating tpi"))
  tpi <- terra::terrain(dem, v = 'TPI') %>% terra::crop(aoi, mask = T)
  print(noquote("calculating tri"))
  tri <- terra::terrain(dem, v = 'TRI') %>% terra::crop(aoi, mask = T)

  # dirName <- paste0(pathOut, '/IntermediateOutput/ResponseFunction/ML_Covariates')
  dirName <- pathOut
  if (!dir.exists(dirName)){
    dir.create(dirName)
  }

  #write the results
  print(noquote("writing outputs"))
    terra::writeRaster(slope,
                filename = paste(dirName,  "slope.tif", sep = "/"),
                filetype = "GTiff", overwrite = T)
    terra::writeRaster(tpi,
                       filename = paste(dirName,  "tpi.tif", sep = "/"),
                       filetype = "GTiff", overwrite = T)
    terra::writeRaster(tri,
                       filename = paste(dirName,  "tri.tif", sep = "/"),
                       filetype = "GTiff", overwrite = T)
}

