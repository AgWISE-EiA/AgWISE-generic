##############################################################################################
## Run the Aggregate function for rainfall and topographic:
##############################################################################################


# 1. Source the function --------------------------------------------------

source('./agwise/EiA_Analytics/AgWISE-UseCaseRAB/03_responseFunction/SubFunction_ML/aggregate_rain.R')
source('./agwise/EiA_Analytics/AgWISE-UseCaseRAB/03_responseFunction/SubFunction_ML/aggregate_topographic.R')

# 2. Run aggregate_rain --------------------------------------------------
## Calibrate

pathInR<-'./agwise/rawData/2_weather/rain_chirps/raw'
pathInT<-'./agwise/rawData'
pathInO <-"./agwise/EiA_Analytics/AgWISE-UseCaseRAB/inputData/RAB_rice_coordinates_field.RDS"
pathOut <- "./agwise/EiA_Analytics/AgWISE-UseCaseRAB/inputData/Rainfall/Rice/ResponseFunction/ML_Covariates"
outName <- 'Rwanda_Rice'
col<-c(13,1,2,4,7,5,6,10,11,12,9) # ID, long, lat, Crop, season, pl_Date, hv_Date, N, P, K, Yield
thr <- 1


aggregate_rain_calibrate(pathInR, pathInT, pathInO, pathOut, outName, col, thr)

## Predict
pathInR<-'./agwise/rawData/2_weather/rain_chirps/raw'
pathInT<-'./agwise/rawData'
pathInO <- "./agwise/EiA_Analytics/AgWISE-UseCaseRAB/inputData/RAB_plantingDates.xlsx"
pathOut <- './agwise/EiA_Analytics/AgWISE-UseCaseRAB/inputData/Rainfall/Rice' # Should be the same than path when Module 1 codes will be ready
crop <-"Rice"
season <- 1
agroeco <- 'lowland'
thr<-1
sourceR<-'CHIRPS'
sourceT <- 'AgERA'
outName <- 'Rwanda'

shp<-terra::vect('./agwise/rawData/6_country/gadm36_RWA_1.shp')

aggregate_rain_predict(pathInR, pathInT, pathInO, shp, crop, season, agroeco, thr, sourceR, sourceT, outName)


# 3. Run aggregate_topographic --------------------------------------------------
pathInD = './agwise/rawData/7_DEM'
pathInO = './agwise/EiA_Analytics/AgWISE-UseCaseRAB/inputData'
obs = "RAB_rice_coordinates_field.RDS"
col<-c(13,1,2,4,7,5,6,10,11,12,9) # ID, long, lat, Crop, season, pl_Date, hv_Date, N, P, K, Yield
pathOut = './agwise/EiA_Analytics/AgWISE-UseCaseRAB/inputData/DEM/Rice/ResponseFunction/ML_Covariates'
dem = "rwanda_dem.tif"
countryName = "Rwanda"

create_topo_covariate(pathInD,pathInO, obs, col, pathOut, dem, countryName)

