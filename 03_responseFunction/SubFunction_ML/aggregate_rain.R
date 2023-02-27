##############################################################################################
## Aggregate the covariates for rainfall & Temperature:
##############################################################################################
#
#' Title: function to aggregate rainfall & temperature data  (eg.sum of the daily rainfall over the cropping season) 

##############################################################################################
# 1. Aggregate Rainfall & Temperature for calibration -----------------------------------

aggregate_rain_calibrate <- function(pathInR,
                                     pathInT,
                                     pathInO, 
                                     pathOut, 
                                     col, 
                                     thr){
  
  #' @param pathInR the directory where are stored the rainfall data
  #' @param pathInT the directory where are stored the temperature data
  #' @param pathInO the pathway where are stored the ground data (Use Case Data), should be csv with sep=";"
  #' @param pathOut the directory where will be stored the aggregated rainfall data
  #' @param thr the rain-rate threshold for a rainy days given in mm of rain/day (float). Default value is set to 1 mm/day.
  #' @param col a vector containing the column index of the variables used in the function c(ID, long, lat, Crop, season, pl_Date, hv_Date, N, P, K, Yield) 
  #' @return a data frame containing the col information & columns corresponding to the rainfall parameters :
  #'        tr : Total rainfall between pl_Date and hv_Date (mm)
  #'        nrd : Number of rainy days between pl_Date and hv_Date (days)
  #'        di : Average daily rainfall between pl_Date and hv_Date (mm/day)
  #'        tmean : Average tmean temperature between pl_Date and hv_Date
  #'        tmin : Average tmin temperature between pl_Date and hv_Date 
  #'        tmax : Average tmax temperature between pl_Date and hv_Date
  #' @examples:

  
  # Check the installation
  
  # packages required
  packages_required <- c("sf", "rgdal", "terra")
  
  # check and install packages that are not yet installed
  installed_packages <- packages_required %in% rownames(installed.packages())
  if(any(installed_packages == FALSE)){
    install.packages(packages_required[!installed_packages])}
  
  # load required packages
  # invisible(lapply(packages_required, library, character.only = TRUE)) # temporary the time to fix the issue

  # 1 List all the rainfall & temperature and read them ####
  listRasterR<-list.files(path=pathInR, pattern=".nc", full.names = T) # rainfall
  listRasterTmean<-list.files(path=pathInT, pattern="temp", full.names=T) # Mean Temp
  listRasterTmin <-list.files(path=pathInT, pattern="tmin", full.names=T) # Min Temp
  listRasterTmax <-list.files(path=pathInT, pattern="tmax", full.names=T) # Max Temp
  
  
  # 2 Load and shaping of the ground data ####
  ground<-read.csv(pathInO, sep=';', dec='.', header=T)
  ground <- ground[,col] # Select the targeted column
  names(ground)<- c("ID", "Long", "Lat", "Crop", "Season","Planting", "Harvesting", "N","P","K", "Yield")
  
  ground$Planting <- as.Date(ground$Planting, "%Y-%m-%d") # Planting date in Date format
  ground$Harvesting <- as.Date(ground$Harvesting, "%Y-%m-%d") # Harvesting date in Date format
  
  # 3 Compute the median planting and harvesting date over the ground data in case of NA ####
  planting.med.y <- format(as.POSIXlt(median(ground$Planting, na.rm=T)), "%Y")
  planting.med.m <- format(as.POSIXlt(median(ground$Planting, na.rm=T)), "%m")
  planting.med.d <- format(as.POSIXlt(median(ground$Planting, na.rm=T)), "%d")
  
  harvesting.med.y <- format(as.POSIXlt(median(ground$Harvesting, na.rm=T)), "%Y")
  harvesting.med.m <- format(as.POSIXlt(median(ground$Harvesting, na.rm=T)), "%m")
  harvesting.med.d <- format(as.POSIXlt(median(ground$Harvesting, na.rm=T)), "%d")
  
  # 4 Loop on all the ID to calculate the parameters ####
  groundOut <- ground

  for(i in 1:nrow(ground)){
    
  # Extract the information for the i-th row
  print(paste0("Compute TR, DI and NRD for ID ", i))
  groundi<-ground[i,]

  # Test for presence of planting and harvesting date
  if (is.na(groundi$Planting)){
    groundi$Planting <-as.Date(paste0(planting.med.y, '-', planting.med.m, '-',planting.med.d), "%Y-%m-%d")
  } 
  if (is.na(groundi$Harvesting)) {
    groundi$Harvesting <-as.Date(paste0(harvesting.med.y, '-', harvesting.med.m, '-',harvesting.med.d), "%Y-%m-%d")
  }
  
  # Test if the cropping season overlaps two civil year
  yearPi <- format(as.POSIXlt(groundi$Planting), "%Y")
  yearHi <- format(as.POSIXlt(groundi$Harvesting), "%Y")
  
  ## 4.1 Case same year ####
  if (yearPi == yearHi) {
    
    ### 4.1.1 Convert planting Date and harvesting in Julian Day ####
    pl_j <-as.POSIXlt(groundi$Planting)$yday
    hv_j <-as.POSIXlt(groundi$Harvesting)$yday
    
    ### 4.1.2 Read for the corresponding year and date ####
    ## Rainfall
    rasti<-listRasterR[which(grepl(yearPi, listRasterR, fixed=TRUE) == T)]
    rasti <- terra::rast(rasti, lyrs=c(pl_j:hv_j))
    
    ## Mean Temp
    tmean<-listRasterTmean[which(grepl(yearPi, listRasterTmean, fixed=TRUE) == T)]
    tmean <- terra::rast(tmean, lyrs=c(pl_j:hv_j))
    
    ## Min Temp
    tmin<-listRasterTmin[which(grepl(yearPi, listRasterTmin, fixed=TRUE) == T)]
    tmin <- terra::rast(tmin, lyrs=c(pl_j:hv_j))
    
    ## Max Temp
    tmax<-listRasterTmax[which(grepl(yearPi, listRasterTmax, fixed=TRUE) == T)]
    tmax <- terra::rast(tmax, lyrs=c(pl_j:hv_j))
  }
  
  ## 4.2 Case two years ####
  if (yearPi < yearHi) {
    
    ### 4.2.1 Convert planting Date and harvesting in Julian Day ####
    pl_j <-as.POSIXlt(groundi$Planting)$yday
    hv_j <-as.POSIXlt(groundi$Harvesting)$yday
    
    ### 4.2.2 Read for the corresponding years and date ####
    ## Rainfall
    rasti1<-listRasterR[which(grepl(yearPi, listRasterR, fixed=TRUE) == T)]
    rasti1 <- terra::rast(rasti1, lyrs=c(pl_j:terra::nlyr(terra::rast(rasti1))))
    rasti2 <-listRasterR[which(grepl(yearHi, listRasterR, fixed=TRUE) == T)]
    rasti2 <- terra::rast(rasti2, lyrs=c(1:hv_j))
    rasti <- c(rasti1, rasti2)
    
    # Mean Temp 
    tmean1<-listRasterTmean[which(grepl(yearPi, listRasterTmean, fixed=TRUE) == T)]
    tmean1<-terra::rast(tmean1, lyrs=c(pl_j:terra::nlyr(terra::rast(tmean1))))
    tmean2 <-listRasterTmean[which(grepl(yearHi, listRasterTmean, fixed=TRUE) == T)]
    tmean2 <- terra::rast(tmean2, lyrs=c(1:hv_j))
    tmean <- c(tmean1, tmean2)
    
    # Min Temp 
    tmin1<-listRasterTmin[which(grepl(yearPi, listRasterTmin, fixed=TRUE) == T)]
    tmin1<-terra::rast(tmin1, lyrs=c(pl_j:terra::nlyr(terra::rast(tmin1))))
    tmin2 <-listRasterTmin[which(grepl(yearHi, listRasterTmin, fixed=TRUE) == T)]
    tmin2 <- terra::rast(tmin2, lyrs=c(1:hv_j))
    tmin <- c(tmin1, tmin2)
    
    # Max Temp 
    tmax1<-listRasterTmax[which(grepl(yearPi, listRasterTmax, fixed=TRUE) == T)]
    tmax1<-terra::rast(tmax1, lyrs=c(pl_j:terra::nlyr(terra::rast(tmax1))))
    tmax2 <-listRasterTmax[which(grepl(yearHi, listRasterTmax, fixed=TRUE) == T)]
    tmax2 <- terra::rast(tmax2, lyrs=c(1:hv_j))
    tmax <- c(tmax1, tmax2)
  }
  
  ### 4.3 Extract the information for the i-th row ####
  xy <- data.frame(groundi$Long, groundi$Lat)
  raini<-terra::extract(rasti, xy,method='simple', cells=FALSE)
  tmeani<-terra::extract(tmean, xy,method='simple', cells=FALSE)
  tmini<-terra::extract(tmin, xy,method='simple', cells=FALSE)
  tmaxi<-terra::extract(tmax, xy,method='simple', cells=FALSE)
    
  ### 4.4 Compute the rainy season parameters ####
    # Compute the total amount of rainfall
    toti<-sum(raini[c(2:length(raini))])
    groundOut$tr[i]<-toti
    
    # Compute the Daily intensity
    if (yearPi == yearHi){
      dii<-toti/(hv_j-pl_j)
      groundOut$di[i]<-dii
    }
    if (yearPi < yearHi){
      dii<-toti/(hv_j+(365-pl_j))
      groundOut$di[i]<-dii
    }
    
    # Compute the Number of rainy day
    nrdi<- raini[c(2:length(raini))]
    nrdi[nrdi<thr] <- 0
    nrdi[nrdi>=thr]<-1
    nrdi<-sum(nrdi)
    groundOut$nrd[i]<- nrdi
    
    # Compute the average mean temp
    meani<-mean(as.numeric(tmeani[c(2:length(tmeani))]))
    groundOut$tmean[i]<-meani
    
    # Compute the average min temp
    mini<-mean(as.numeric(tmini[c(2:length(tmini))]))
    groundOut$tmin[i]<-mini
    
    # Compute the average max temp
    maxi<-mean(as.numeric(tmaxi[c(2:length(tmaxi))]))
    groundOut$tmax[i]<-maxi
    
  }
  
  print("End of the loop")

  # 5 Writting of output ####
# Check if the directory exists

dirName<-pathOut
if (!dir.exists(dirName)){
  dir.create(dirName, recursive = T)
}
  
  saveRDS(object = groundOut, 
          file=paste0(pathOut, '/ML_Rain_Temp_Calibration.RDS'))
  
  print ("End of the aggregate rain function for prediction ")
}
  

# pathO <- './AgWise/EiA_Analytics/useCase_RAB/Rice/inputData/RAB_Rice_Coordinates.csv'
# 
# # Create a fake dataset for testing
# fake<-read.csv(pathO, sep=',', dec='.', header=T)
# fake$Crop<-'Rice'
# fake$Year<-sample(2000 : 2021, size = nrow(fake), replace = T)
# fake$N<-sample(0 : 200, size = nrow(fake), replace = T)
# fake$P<-sample(0 : 200, size = nrow(fake), replace = T)
# fake$K<-sample(0 : 200, size = nrow(fake), replace = T)
# fake$Yield <-sample(0 : 1500, size = nrow(fake), replace = T)
# fake$pl_Date<-paste0(fake$Year,"-04-01")
# fake$hv_Date<-paste0(fake$Year,"-09-30")
# fake$pl_Date[sample(nrow(fake),110)]<-'NA'
# fake$hv_Date[sample(nrow(fake),110)]<-'NA'
# fake$ID<-paste0(fake$longitude, '-', fake$latitude, '-', fake$Year)
# write.csv(fake, "./AgWise/EiA_Analytics/AgWISE-UseCaseRAB/03_responseFunction/SubFunction_ML/FAKE_RAB_Rice_Coordinates.csv")

# pathInC<-'./agwise/rawData/2_weather/rain_chirps/raw'
# pathInO <-"./agwise/EiA_Analytics/AgWISE-UseCaseRAB/03_responseFunction/SubFunction_ML/RAB_potato_2023_shared_format.csv"
# pathOut <- "./agwise/EiA_Analytics/AgWISE-UseCaseRAB/inputData/Rainfall/Potato/ResponseFunction/ML_Covariates"
# col<-c(1,5,6,10,9,16,17,12,13,14,15) # ID, long, lat, Crop, season, pl_Date, hv_Date, N, P, K, Yield
# thr <- 1
# 
# aggregate_rain_calibrate(pathInC, pathInO, pathOut, col, thr)

# 2. Aggregate Rainfall for prediction  -----------------------------------

aggregate_rain_predict <-function(pathIn,
                    pathInO,
                    pathOut,
                    shp,
                    crop,
                    season,
                    agroeco,
                    thr,
                    source){
  
  #' @param pathInC the directory where are stored the rainfall data
  #' @param pathInO the pathway where are stored the planting data (Use Case Data) - Should have "Crop", "Season", "agroecology", "planting date" and "harvest date" columns
  #' @param pathOut the directory where will be stored the aggregated rainfall data
  #' @param shp a shapefile containing the extent of the study area
  #' @param crop the targeted crop, should be the same that the one in the planting date file
  #' @param season the targeted season (integer), should be the same that the one in the planting date file
  #' @param agroeco the targeted agroecology, should be the same that the one in the planting date file
  #' @param thr the rain-rate threshold for a rainy days given in mm of rain/day (float). Default value is set to 1 mm/day.
  #' @param source the name of the rainfall data sources
  #'  
  #' @return for each parameters, return a raster stack of 3 layers corresponding to the values for each scenario :
  #'        tot_rf : Total rainfall between pl_Date and hv_Date (mm)
  #'        nrd : Number of rainy days between pl_Date and hv_Date (days)
  #'        di : Average daily rainfall between pl_Date and hv_Date (mm/day)
  #' @examples: 
  
# Check the installation

  # packages required
  packages_required <- c( "sf", "rgdal", "raster", "terra","readxl")
  
  # check and install packages that are not yet installed
  installed_packages <- packages_required %in% rownames(installed.packages())
  if(any(installed_packages == FALSE)){
    install.packages(packages_required[!installed_packages])}
  
  # load required packages
  # invisible(lapply(packages_required, library, character.only = TRUE)) # temporary the time to fix the issue

# 1 List all the rainfall and read them ####
listRaster<-list.files(pathInC, pattern=".nc", full.names = T)
listRaster<-listRaster[c(1:length(listRaster)-1)] # to get the last complete year and remove the ongoing one

# 2 Read Observed data and subset the specific information ####
ground<-readxl::read_excel(pathInO)
ground<-subset(ground, ground$Crop == crop & ground$Season == season & ground$agroecology == agroeco)
  
# 3 Convert planting date and harvest date in Julian Day ####
pl_Date<-ground$`planting date`
pl_Date<-as.Date(paste0("1981-",pl_Date), "%Y-%m-%d")
pl_Date<-as.POSIXlt(pl_Date)$yday

hv_Date<-ground$`harvest date`
hv_Date <-as.Date(paste0("1981-",hv_Date), "%Y-%m-%d")
hv_Date<-as.POSIXlt(hv_Date)$yday

# 3 Loop on all the years to calculate the parameters ####
# Initialize empty raster for the storage
tot.out<-terra::rast(listRaster[1], lyrs=1)
tot.out<-terra::crop(tot.out, shp)
tot.out[]<-'NA'
nrd.out<-tot.out
di.out<-tot.out

  ## 3.1 Case same year ###

  if (pl_Date < hv_Date) {
    # Loop on each year
    
    for (i in 1:length(listRaster)){
      
      
      # Read raster
      readLayers<-terra::rast(listRaster[i], lyrs=c(pl_Date:hv_Date))
      
      # Crop the raster !!!!! This should be remove and done by Eduardo
      croppedLayers<-terra::crop(readLayers, shp)
      
      if (i == 1) {
        print(paste0("Compute TR, DI and NRD for year ", i))
        # Compute the total amount of rainfall
        toti<-terra::app(croppedLayers, fun='sum')
        tot.out<-toti
        
        # Compute the Daily intensity
        dii<-toti/(hv_Date-pl_Date)
        di.out<-dii
        
        # Compute the Number of rainy day
        nrdi<- croppedLayers
        nrdi[croppedLayers<thr] <- 0
        nrdi[croppedLayers>=thr]<-1
        nrdi<-terra::app(nrdi, fun='sum')
        nrd.out<- nrdi
      } else {
        print(paste0("Compute TR, DI and NRD for year ", i))
        
        # Compute the total amount of rainfall
        toti<-terra::app(croppedLayers, fun='sum')
        terra::add(tot.out)<-toti
        
        # Compute the Daily intensity
        dii<-toti/(hv_Date-pl_Date)
        terra::add(di.out)<-dii
        
        # Compute the Number of rainy day
        nrdi<- croppedLayers
        nrdi[croppedLayers<thr] <- 0
        nrdi[croppedLayers>=thr]<-1
        nrdi<-terra::app(nrdi, fun='sum')
        terra::add(nrd.out)<- nrdi
      }
    }
  }

## 3.2 Case two years ###

if (pl_Date > hv_Date) {
  # Loop on each year
  
  for (i in 1:(length(listRaster)-1)){
    # Stop of the loop one the second to last year
    
    # Read raster
    readLayers1<-terra::rast(listRaster[i], lyrs=c(pl_Date:terra::nlyr(terra::rast(listRaster[i]))))
    readLayers2<-terra::rast(listRaster[i+1], lyrs=c(1:hv_Date))
    readLayers<-c(rasti1,rasti2)
    
    # Crop the raster !!!!! This should be remove and done by Eduardo
    croppedLayers<-terra::crop(readLayers, shp)
    
    if (i == 1) {
      print(paste0("Compute TR, DI and NRD for year ", i))
      # Compute the total amount of rainfall
      toti<-terra::app(croppedLayers, fun='sum')
      tot.out<-toti
      
      # Compute the Daily intensity
      dii<-toti/(pl_Date-hv_Date)
      di.out<-dii
      
      # Compute the Number of rainy day
      nrdi<- croppedLayers
      nrdi[croppedLayers<thr] <- 0
      nrdi[croppedLayers>=thr]<-1
      nrdi<-terra::app(nrdi, fun='sum')
      nrd.out<- nrdi
    } else {
      print(paste0("Compute TR, DI and NRD for year ", i))
      
      # Compute the total amount of rainfall
      toti<-terra::app(croppedLayers, fun='sum')
      terra::add(tot.out)<-toti
      
      # Compute the Daily intensity
      dii<-toti/(pl_Date-hv_Date)
      terra::add(di.out)<-dii
      
      # Compute the Number of rainy day
      nrdi<- croppedLayers
      nrdi[croppedLayers<thr] <- 0
      nrdi[croppedLayers>=thr]<-1
      nrdi<-terra::app(nrdi, fun='sum')
      terra::add(nrd.out)<- nrdi
    }
  }
}

print("Calculation of TR, DI and NDR completed")
# End of the loop for yearly parameters calculation

# 4 Calculation of the quantiles for scenarios ####
print("Calculation of the quantiles 0.25, 0.50 and 0.75 for scenarios")
tot.q<-quantile(tot.out, probs=c(0.25,0.5, 0.75))
di.q <- quantile(di.out, probs=c(0.25,0.5, 0.75))
nrd.q <-quantile(nrd.out, probs=c(0.25,0.5, 0.75))
print ("Calculation of the quantiles completed")

# 5 Writting of output ####
# Check if the directory exists

dirName<-paste0(pathOut, '/ResponseFunction/ML_Covariates')
if (!dir.exists(dirName)){
  dir.create(dirName, recursive=T)
}

names(tot.q)<-c("tr_below", "tr_normal", "tr_above")
terra::writeRaster(tot.q, paste0(dirName,"/",crop,"_",season, "_", agroeco,"_",source,"_Total_Rainfall_Scenarios.tif"), filetype="GTiff")
names(di.q) <- c("di_below", "di_normal", "di_above")
terra::writeRaster(di.q, paste0(dirName,"/",crop,"_",season, "_", agroeco,"_",source,"_Daily_Intensity_Scenarios.tif"), filetype="GTiff")
names(nrd.q) <- c("nrd_below", "nrd_normal", "nrd_above")
terra::writeRaster(nrd.q, paste0(dirName,"/",crop,"_",season, "_", agroeco,"_",source,"_Number_Of_Rainy_Days_Scenarios.tif"), filetype="GTiff")

print ("End of the aggregate rain function for prediction ")
}

# pathInC<-'./agwise/rawData/2_weather/rain_chirps/raw'
# pathInO <- 'agwise/EiA_Analytics/AgWISE-UseCaseRAB/inputData/plantingDates_2.xlsx'
# pathOut <- './agwise/EiA_Analytics/AgWISE-UseCaseRAB/inputData/Rainfall/Rice' # Should be the same than path when Module 1 codes will be ready
# 
# crop <-"Rice"
# season <- 1
# agroeco <- 'lowland'
# 
# source<-'CHIRPS'
# shp<-terra::vect('./agwise/rawData/6_country/gadm36_RWA_1.shp')
# 
# aggregate_rain_predict(pathInC, pathInO, pathOut, shp, crop, season,agroeco, thr, source)


