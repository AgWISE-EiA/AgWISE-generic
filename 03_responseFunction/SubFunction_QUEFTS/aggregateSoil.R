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
## Use geodata to get iSDA layers for the whole of Africa. to be done only once b/c the layers will be made avilable local
#################################################################################################################

vars <- c( "Al", "bdr", "clay", "C.tot", "Ca", "db.od", "eCEC.f", "Fe", "K", "Mg", "N.tot", "oc", "P", "pH.H2O", "sand", "silt", "S", "texture", "wpg2", "Zn")

invisible(lapply(vars,       
                 function(i) {
                   afph <- soil_af_isda(i, path="./AgWise/rawData/1_soil/",  depth = 20, quiet=TRUE)
                 }
))



invisible(lapply(vars,       
                 function(i) {
                   afph <- soil_af_isda(i, path="/home/jovyan/AgWise/rawData/1_soil/", depth = 50, quiet=TRUE)
                 }
))

#################################################################################################################
## use geodata to get ISRIC layers for the whole of aggregated at 30cm depth and 1km resolution.
## B, Cu, Mn and Na are not available in iSDA. N, P and Ptot are presented in different units and it might be good to compare the sources
## to be done only once b/c the layers will be made avilable local
#################################################################################################################
vars_isric <- c("B", "Cu", "Mn", "N", "Na", "P", "Ptot") 

invisible(lapply(vars_isric,       
                 function(i) {
                   soil_af_elements(i, path="/home/jovyan/AgWise/rawData/1_soil/", quiet=TRUE)
                 }
))




#################################################################################################################
## crop for the AOI: cropped layers will be available as input data in "pathOut"
#################################################################################################################
## source cropping function
source("./AgWise/EiA_Analytics/AgWISE-UseCaseRAB/03_responseFunction/SubFunction_QUEFTS/aggregateSoil_Functions.R")


## read all soil layers
listRaster <-list.files(path="./AgWise/rawData/1_soil/", pattern=".tif$")



## crop the layers for the country and get raster
lapply(listRaster,       
                 function(i) {
                   cropSoil(i, pathIn = "./AgWise/rawData/1_soil/",
                            pathOut = "./AgWise/EiA_Analytics/AgWISE-UseCaseRAB/inputData/soil", 
                            returnFormat = "raster", countryName = "Rwanda")
                 }
)


## crop for GPS coordinates
Rwanda_coor <- read.csv("AgWise/EiA_Analytics/useCase_RAB/Rice/inputData/RAB_Rice_Coordinates.csv")

lapply(listRaster,       
       function(i) {
         cropSoil(i, pathIn = "./AgWise/rawData/1_soil/",
                  pathOut = "./AgWise/EiA_Analytics/AgWISE-UseCaseRAB/inputData/soil", 
                  returnFormat = "point", countryCoord = Rwanda_coor, countryName = "Rwanda")
       }
)





#####################################################################################################
## pedo-transfer function from harvest choice to get soil hydraulics based on sand, silt, clay and OM in percentage 
######################################################################################################


get_soilHydraulics(pathIn = "./AgWise/EiA_Analytics/AgWISE-UseCaseRAB/inputData/soil",
                   countryName = "Rwanda", pathOut = "./AgWise/EiA_Analytics/AgWISE-UseCaseRAB/inputData/soil",
                   returnFormat = "raster")


get_soilHydraulics(pathIn = "./AgWise/EiA_Analytics/AgWISE-UseCaseRAB/inputData/soil",
                   countryName = "Rwanda", pathOut = "./AgWise/EiA_Analytics/AgWISE-UseCaseRAB/inputData/soil",
                   returnFormat = "point", countryCoord = Rwanda_coor)
