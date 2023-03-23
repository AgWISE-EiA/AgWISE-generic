##############################################################################################
## Create topographic variables
##############################################################################################
# DEM data downloaded - SRTM - "SRTM void filled" from earth explorer
#' Title: function to aggregate data from get_covariates (eg. for rainfall, sum of the daily rainfall over the cropping season)
#' @param path the directory where dem is stored
#' @param dem file name of dem data
#' @param countryName name of country
#' @param pathInD directory where dem data are stored
#' @param pathInO directory where observation data are stored
#' @param obs observation data
#' @param col a vector containing the column index of the observation variables used in the function c(ID, long, lat, Crop, season, pl_Date, hv_Date, N, P, K, Yield)
#' @param pathOut the directory for writing the outputs
#' @return a data frame containing the col information & columns corresponding to the topographic parameters :
#'        slope : slope
#'        tpi : topographic positioning index
#'        tri : topographic ruggedness index
#' @return for each parameters, return a raster :
#'        slope : slope
#'        tpi : topographic positioning index
#'        tri : topographic ruggedness index
#'
#' @examples:
#' create_topo_covariate(pathInD = '/home/jovyan/agwise/rawData/7_DEM',pathInO = '/home/jovyan/agwise/EiA_Analytics/AgWISE-UseCaseRAB/03_responseFunction/SubFunction_ML',obs = "FAKE_RAB_Rice_Coordinates.csv",
#'                      col<-c(13,2,3,5,6,11,12,7,8,9,10), pathOut = '/home/jovyan/agwise/EiA_Analytics/AgWISE-UseCaseRAB/inputData/DEM/Rice/ResponseFunction/ML_Covariates',
#'                     dem = "rwanda_dem.tif", countryName = "Rwanda")
#'
##############################################################################################
create_topo_covariate <-function(pathInD, pathInO, obs, col, pathOut, dem, countryName
                               ){
  # Check the installation

  # install missing packages
  packages <- c("terra", "geodata", "rgdal", "dplyr", "sp")

  # installed_packages <- packages %in% rownames(installed.packages())
  # if (any(installed_packages == FALSE)) {
  #   install.packages(packages[!installed_packages])
  # }

  # packages loading
  invisible(lapply(packages, library, character.only = TRUE)) # temporary the time to fix the issue

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

  # 1 Extract the Topographic variables for prediction (Raster) ####

  ## Get country adm vector ####
  print(noquote("reading country shapefile"))
  aoi <- geodata::gadm(country = country , level = 1, path = getwd())

  ## Read dem data, project to wgs 1984 geographic & crop raster ####
  print(noquote("reading and masking input dem"))
  dem <- terra::rast(paste(pathInD, dem, sep = "/")) %>%
    terra::project(crs(aoi), method = 'bilinear') %>%
    terra::crop(aoi) %>% terra::mask(aoi)

  ## resample to the rain data (i.e 0.05 deg)
    r1 <- dem
    res(r1) <- 0.05

  ## calculate the slope, tpi, tri ####
  print(noquote("calculating slope"))
  slope <-
    terra::terrain(dem, v = 'slope', unit = 'degrees') %>% terra::crop(aoi, mask = T)
  slope <- terra::resample(slope, r1, method = "near")
  print(noquote("calculating tpi"))
  tpi <- terra::terrain(dem, v = 'TPI') %>% terra::crop(aoi, mask = T)
  tpi <- terra::resample(tpi, r1, method = "near")
  print(noquote("calculating tri"))
  tri <- terra::terrain(dem, v = 'TRI') %>% terra::crop(aoi, mask = T)
  tri <- terra::resample(tri, r1, method = "near")
  
  dem<-terra::resample(dem, r1, method = "near")

  ## Writting of output ####
  # Check if the directory exists
  dirName <- pathOut
  if (!dir.exists(dirName)){
    dir.create(dirName, recursive=T)
  }

  # write the results
  print(noquote("writing outputs"))
    terra::writeRaster(slope,
                filename = paste(dirName,  paste0(countryName,"_slope.tif"), sep = "/"),
                filetype = "GTiff", overwrite = T)
    terra::writeRaster(tpi,
                       filename = paste(dirName,  paste0(countryName,"_tpi.tif"), sep = "/"),
                       filetype = "GTiff", overwrite = T)
    terra::writeRaster(tri,
                       filename = paste(dirName,  paste0(countryName,"_tri.tif"), sep = "/"),
                       filetype = "GTiff", overwrite = T)

    # 2 Extract the Topographic variables by observation for calibration (DataFrame) ####
    print(noquote("extracting topographic covariates"))

    ## Load and shaping of the ground data ####
    ground <- readRDS(paste(pathInO, obs, sep = "/"))
    ground <- ground[,col]
    names(ground)<-c("ID", "Long", "Lat", "Crop", "Season","Planting", "Harvesting", "N","P","K", "Yield")

    ground$Planting <- as.Date(ground$Planting, "%Y-%m-%d") # Planting date in Date format
    ground$Harvesting <- as.Date(ground$Harvesting, "%Y-%m-%d") # Harvesting date in Date format

    ## Load and read the topographic raster variables
    ground.vect <- terra::vect(ground,geom = c("Long", "Lat"), crs = "epsg:4326")
    topo_cov <- c(dem, slope, tpi, tri)
    topo_calib <- terra::extract(topo_cov, ground.vect, df = T)
    
    ## Shape the output and write ####
    output <- cbind.data.frame(ground, topo_calib[,-1])
    saveRDS(object = output,
            file = paste0(pathOut, '/',countryName,'_ML_Topo_Calibration.RDS'))
}

#create_topo_covariate(pathInD = '/home/jovyan/agwise/rawData/7_DEM',pathInO = '/home/jovyan/agwise/EiA_Analytics/AgWISE-UseCaseRAB/03_responseFunction/SubFunction_ML',obs = "FAKE_RAB_Rice_Coordinates.csv", col<-c(13,2,3,5,6,11,12,7,8,9,10), pathOut = '/home/jovyan/agwise/EiA_Analytics/AgWISE-UseCaseRAB/inputData/DEM/Rice/ResponseFunction/ML_Covariates', dem = "rwanda_dem.tif", countryName = "Rwanda")
