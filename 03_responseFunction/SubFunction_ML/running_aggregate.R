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
pathInO <-"./agwise/EiA_Analytics/AgWISE-UseCaseRAB/03_responseFunction/SubFunction_ML/RAB_potato_2023_shared_format.csv"
pathOut <- "./agwise/EiA_Analytics/AgWISE-UseCaseRAB/inputData/Rainfall/Potato/ResponseFunction/ML_Covariates"
col<-c(1,5,6,10,9,16,17,12,13,14,15) # ID, long, lat, Crop, season, pl_Date, hv_Date, N, P, K, Yield
thr <- 1

aggregate_rain_calibrate(pathInR, pathInT, pathInO, pathOut, col, thr)

## Predict
## To do

# 3. Run aggregate_topographic --------------------------------------------------
pathInD = './agwise/rawData/7_DEM'
pathInO = './agwise/EiA_Analytics/AgWISE-UseCaseRAB/03_responseFunction/SubFunction_ML'
obs = "RAB_potato_2023_shared_format.csv"
col<-c(1,5,6,10,9,16,17,12,13,14,15)
pathOut = './agwise/EiA_Analytics/AgWISE-UseCaseRAB/inputData/DEM/Potato/ResponseFunction/ML_Covariates'
dem = "rwanda_dem.tif"
countryName = "Rwanda"

create_topo_covariate(pathInD,pathInO, obs, col, pathOut, dem, countryName)

