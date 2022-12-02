##############################################################################################
## Aggregate the covariates for weather:
##############################################################################################
#
#' Title: function to aggregate data from get_covariates (eg. for rainfall, sum of the daily rainfall over the cropping season) 
#' @param path the directory where are stored the covariates
#' @param pl_Date the planting date given in date format in the form of "YYYY-mm-dd" (from the NOT data for training or from expertise)
#' @param hv_Date the harvesting date given in date format in the form of "YYY-mm-dd"(from the NOT data for training or from expertise)
#' @param thr the rain-rate threshold for a rainy days given in mm of rain/day (float). Default value is set to 1 mm/day.
#'  
#' @return for each scenario, returned a raster stack of 4 parameters :
#'        tot_rf : Total rainfall between pl_Date and hv_Date (mm)
#'        nrd : Number of rainy days between pl_Date and hv_Date (days)
#'        di : Average daily rainfall between pl_Date and hv_Date (mm/day)
#' @examples: 

##############################################################################################
#



aggregate_covariate <-function(path,
                    pathOut,
                    pl_Date,
                    hv_Date,
                    thr){


# 1 List all the rainfall and read them 
listRaster<-list.files(path=path, pattern=".nc", full.names = T)

# 2 Convert pl_Date and hv_Date in Julian Day
pl_Date<-as.Date(paste0("1981-",pl_Date), "%Y-%m-%d")
pl_Date<-as.POSIXlt(pl_Date)$yday

hv_Date <-as.Date(paste0("1981-",hv_Date), "%Y-%m-%d")
hv_Date<-as.POSIXlt(hv_Date)$yday

# 3 Loop on all the years to calculate the parameters
# Initialize empty raster for the storage
tot.out<-rast(listRaster[1], lyrs=1)
tot.out<-terra::crop(tot.out, rwanda)
tot.out[]<-'NA'
nrd.out<-tot.out
di.out<-tot.out

# Loop on each year

for (i in 1:length(listRaster)){
  
  # Read raster
  rasti<-rast(listRaster[i], lyrs=c(pl_Date:hv_Date))
  
  # Crop the raster !!!!! This should be remove and done by Eduardo
  rastic<-terra::crop(rasti, rwanda)
  
  if (i == 1) {
    print(paste0("Compute TR, DI and NRD for year ", i))
    # Compute the total amount of rainfall
    toti<-terra::app(rastic, fun='sum')
    tot.out<-toti
    
    # Compute the Daily intensity
    dii<-toti/(hv_Date-pl_Date)
    di.out<-dii
    
    # Compute the Number of rainy day
    nrdi<- rastic
    nrdi[rastic<thr] <- 0
    nrdi[rastic>=thr]<-1
    nrdi<-terra::app(nrdi, fun='sum')
    nrd.out<- nrdi
  } else {
    print(paste0("Compute TR, DI and NRD for year ", i))
    # Compute the total amount of rainfall
    toti<-terra::app(rastic, fun='sum')
    add(tot.out)<-toti
    
    # Compute the Daily intensity
    dii<-toti/(hv_Date-pl_Date)
    add(di.out)<-dii
    
    # Compute the Number of rainy day
    nrdi<- rastic
    nrdi[rastic<thr] <- 0
    nrdi[rastic>=thr]<-1
    nrdi<-terra::app(nrdi, fun='sum')
    add(nrd.out)<- nrdi
  }
  
}
print("Calculation of TR, DI and NDR completed")
# End of the loop for yearly parameters calculation

# 4 Calculation of the quantiles for scenarios
print("Calculation of the quantiles 0.25, 0.50 and 0.75 for scenarios")
tot.q<-quantile(tot.out, probs=c(0.25,0.5, 0.75))
di.q <- quantile(di.out, probs=c(0.25,0.5, 0.75))
nrd.q <-quantile(nrd.out, probs=c(0.25,0.5, 0.75))
print ("Calculation of the quantiles completed")

# 5 Writting of output
# Check if the directory exists

dirName<-paste0(pathOut, '/IntermediateOutput/ResponseFunction/ML_Covariates')
if (!dir.exists(dirName)){
  dir.create(dirName)
}

names(tot.q)<-c("tr_below", "tr_normal", "tr_above")
terra::writeRaster(tot.q, paste0(dirName,"/Total_Rainfall_Scenarios.tif"), filetype="GTiff")
names(di.q) <- c("di_below", "di_normal", "di_above")
terra::writeRaster(di.q, paste0(dirName,"/Daily_Intensity_Scenarios.tif"), filetype="GTiff")
names(nrd.q) <- c("nrd_below", "nrd_normal", "nrd_above")
terra::writeRaster(nrd.q, paste0(dirName,"/Number_Of_Rainy_Days_Scenarios.tif"), filetype="GTiff")

print ("End of the aggregate covariate function")
}

path<-'./AgWise/rawData/2_weather/rain_chirps/raw'
pathOut <- './AgWise/EiA_Analytics/useCase_RAB/Rice' # Should be the same than path when Module 1 codes will be ready
pl_Date<-"04-01"
hv_Date<-"09-30"
thr<-1

rwanda<-vect('./AgWise/rawData/6_country/gadm36_RWA_1.shp')

aggregate_covariate(path, pathOut, pl_Date, hv_Date, thr)
