#' Extract worldclim data
#'

#' @coords data.frame with 2 columns (Latitude and Longitude)
#' @raster optional boolean to export results in SpatRast (terra) format
#' @res    resolution
#' @var    list of required variables c("tavg", "tmin", "tmax", "prec", "bio",  "elev", "wind", "vapr", "srad")
#' @return SpatRast
#' @examples
#' worldclim(var=c("bio","tmin"),10,raster = TRUE) 
#' worldclim(var=c("elev","tmin"),10,raster = FALSE,coords = data.frame("x" = c(9.57, 10.55), "y" = c(11.55, 12.43))) 



#Extracts worldclim historical data (1970-2000)
worldclim<- function(var, res, raster = TRUE, coords = NULL){
  
  #define function elements and paths
  #downloaded file path
  url<-"datadownload/"
  # url to download from  ##historical climate data (1970-2000)
  url_download<-"https://geodata.ucdavis.edu/climate/worldclim/2_1/base/"
  
  ras.all<-raster::stack()
  #loop through list of required vars
  for (i in var) {
    var<-i
    stopifnot(res %in% c("0.5","2.5", "5", "10", "0.5m","2.5m", "5m", "10m","30", "30s"))
    stopifnot(var %in% c("tavg", "tmin", "tmax", "prec", "bio",  "elev", "wind", "vapr", "srad"))
    res<-ifelse ((grepl("m", res,)), res, paste0(res, "m"))
    if (res=="0.5m" || res=="0.5" || res=="30" || res=="30s") {res<-"30s"} 
    
    
    #file name
    file<-paste0("wc2.1_",res,"_",var)
    
    
    
    #check if file available
    if(!file.exists(paste0(url, file, ".zip"))){    #if not available download, unzip and stack, retun rasterstack
      download.file(paste0(url_download,file,".zip"), paste0(url,file,".zip"), mode="wb")
      suppressWarnings(unzip(paste0(url, file,".zip"), exdir=url, overwrite=FALSE))
    }
    #list files for matching var, res
    if(var == "bio"){
      # 19 bioclimatic variables
      #rasfiles<-list.files(url, pattern =sprintf("wc2.1_%s_%s_%02d.tif", res, var, 1:19) ,full.names=TRUE)
      rasfiles<-file.path(url, sprintf("wc2.1_%s_%s_%d.tif", res, var, 1:19))
    }else if (var == "elev"){
      #rasfiles<-list.files(url, pattern =sprintf("wc2.1_%s_%s_%02d.tif", res, var, 1:12) ,full.names=TRUE)
      rasfiles<-file.path(url, sprintf("wc2.1_%s_%s.tif", res, var))
    }else {
      #rasfiles<-list.files(url, pattern =sprintf("wc2.1_%s_%s_%02d.tif", res, var, 1:12) ,full.names=TRUE)
      rasfiles<-file.path(url, sprintf("wc2.1_%s_%s_%02d.tif", res, var, 1:12))
    }
    #stack all the rasters 
    
    ras <- raster::stack(rasfiles)
    
    
    # and return raster stack for all provided vars
    ras.all <- raster::stack(ras.all,ras)
    
    
  }
  
  
  if (raster) {       #for raster output raster=TRUE
    return(ras.all)
    
  } else{             #for TABLE output raster=False
    df <- terra::extract(ras.all,coords)
    return(df)
    
  }
  
}


## up Next:: update table - rename colnames, append coords col  use loop


# s<-worldclim(var=c("bio","tmin"),10,raster = TRUE) 
# s
# d<-worldclim(var=c("elev","tmin"),10,raster = FALSE,coords = data.frame("x" = c(9.57, 10.55), "y" = c(11.55, 12.43))) 
# View(d)

