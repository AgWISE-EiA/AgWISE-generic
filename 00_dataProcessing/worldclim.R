#' Extract worldclim data
#'

#' @coords data.frame with 2 columns (Latitude and Longitude)
#' @raster optional boolean to export results in SpatRast (terra) format
#' @res    resolution
#' @var    list of required variables c("tavg", "tmin", "tmax", "prec", "bio",  "elev", "wind", "vapr", "srad")
#' @return SpatRast
#' @examples
#' worldclim(var, res, raster = TRUE, coords = NULL)
#' worldclim(var=c("bio","tmin"),10,raster = TRUE) 
#' worldclim(var=c("elev","tmin"),10,raster = FALSE,coords = data.frame("x" = c(9.57, 10.55), "y" = c(11.55, 12.43))) 


#Extracts worldclim historical data (1970-2000)
worldclim<- function(var, res, raster = TRUE, coords = NULL){
  
  #define function elements and paths
  #downloaded file path
  ifelse(!dir.exists(file.path("geodata/")), dir.create(file.path("geodata/")), FALSE)
  url<-"geodata/"
  #url<-"datadownload/"
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
    }
    suppressWarnings(unzip(paste0(url, file,".zip"), exdir=url, overwrite=FALSE))
    #list files for matching var, res
    if(var == "bio"){
      # 19 bioclimatic variables
      rasfiles<-file.path(url, sprintf("wc2.1_%s_%s_%d.tif", res, var, 1:19))
    }else if (var == "elev"){
      rasfiles<-file.path(url, sprintf("wc2.1_%s_%s.tif", res, var))
      
    }else {
      rasfiles<-file.path(url, sprintf("wc2.1_%s_%s_%02d.tif", res, var, 1:12))
    }
    #stack all the rasters 
    ras <- raster::stack(rasfiles)
    
    # and return raster stack for all provided vars
    ras.all <- raster::stack(ras.all,ras)
  }
  
  
  if (raster) {       #for raster output raster=TRUE
    #crop raster for given aoi/ coords bounds
    # if (!is.null(coords)){
    #   aoi <- suppressWarnings(terra::vect(sf::st_as_sf(sf::st_as_sfc(sf::st_bbox(c(xmin = min(coords[,1]), xmax = max(coords[,1]), ymax = max(coords[,2]), ymin = min(coords[,2])), crs = sf::st_crs(4326))))))
    #   # Subset AOI
    #   ras.all <- suppressWarnings(terra::crop(ras.all, aoi))
    # }
    return(ras.all)
    
  } else{             #for TABLE output raster=False
    df <- data.frame()
    for (pnt in seq(1:nrow(coords))){
      lon <- coords[pnt, 1]
      lat <- coords[pnt, 2]
      df1 <- data.frame(terra::extract(ras.all,data.frame(lon,lat)))
      X <- lon
      Y <- lat
      df1<- data.frame(X, Y, df1)
      df <- rbind(df, df1)
    }

    names(df) <- sub(sprintf("wc2.1_%s_", res), "", names(df))
    names(df) <- sub("layer", "elev", names(df))
    return(df)
  }
  
}




###################################################################################################
###################################################################################################

#' Extract worldclim historical monthly data
#'

#' @param startDate starting date of the data extraction
#' @param endDate ending date of the data extraction
#' @coords data.frame with 2 columns (Latitude and Longitude)
#' @raster optional boolean to export results in SpatRast (terra) format
#' @var    list of required variables c( "tmin", "tmax", "prec")
#' @return SpatRast
#' @examples
#' worldclim_monthly(startDate, endDate, var,  raster = TRUE, coords = NULL)
#' worldclim_monthly("2010-01-01", "2013-10-01", "prec", raster = TRUE)
#' worldclim_monthly("2010-01-01", "2013-10-01", "prec", raster = FALSE, coords = data.frame("x" = c(9.57, 10.55), "y" = c(11.55, 12.43)))


#Extracts worldclim historical monthly data (1960-2018)
worldclim_monthly<- function(startDate, endDate, var,  raster = TRUE, coords = NULL){
  #resolution available -  2.5m
  #variables available are tmin,tmax and prec
  #define function elements and paths
  #downloaded file path
  ifelse(!dir.exists(file.path("geodata/")), dir.create(file.path("geodata/")), FALSE)
  url<-"geodata/"
  #url<-"datadownload/"
  # url to download from  ##historical climate data (1960-2018)
  url_download<-"https://geodata.ucdavis.edu/climate/worldclim/2_1/hist/"
  res<-"2.5m"
  var<-as.character(var)
  ras.all<-raster::stack()
  #loop through list of required vars
  for (i in var) {
    var<-i
    
    #check variable 
    stopifnot(var %in% c("tmin", "tmax", "prec"))
    
    #dates
    dates <- seq.Date(as.Date(startDate, format = "%Y-%m-%d"), as.Date(endDate, format = "%Y-%m-%d"), by = "month")
    years <- unique(format(dates, "%Y"))
    months <- unique(format(dates, "%m"))
    year_month<-unique(format(dates, "%Y-%m"))
    
    #check year range for download
    for (i in years) {
      #file name
      if      (i %in% c(format(seq.Date(as.Date("1960",format = "%Y"),as.Date("1969",format = "%Y"), by="year"),"%Y"))){year<-"1960-1969"}
      else if (i %in% c(format(seq.Date(as.Date("1970",format = "%Y"),as.Date("1979",format = "%Y"), by="year"),"%Y"))){year<-"1970-1979"}
      else if (i %in% c(format(seq.Date(as.Date("1980",format = "%Y"),as.Date("1989",format = "%Y"), by="year"),"%Y"))){year<-"1980-1989"}
      else if (i %in% c(format(seq.Date(as.Date("1990",format = "%Y"),as.Date("1999",format = "%Y"), by="year"),"%Y"))){year<-"1990-1999"}
      else if (i %in% c(format(seq.Date(as.Date("2000",format = "%Y"),as.Date("2009",format = "%Y"), by="year"),"%Y"))){year<-"2000-2009"}
      else if (i %in% c(format(seq.Date(as.Date("2010",format = "%Y"),as.Date("2018",format = "%Y"), by="year"),"%Y"))){year<-"2010-2018"}
      
      file<-paste0("wc2.1_",res,"_",var,"_",year,".zip")
      
      #check if file available
      if(!file.exists(paste0(url, file))){    #if not available download, unzip and stack, retun rasterstack
        download.file(paste0(url_download,file), paste0(url,file), mode="wb")
      }
      suppressWarnings(unzip(paste0(url, file), exdir=url, overwrite=FALSE))
      
    }
    
    #list all files for the given months and year 
    rasfiles<-file.path(url, sprintf("wc2.1_%s_%s_%s.tif", res, var, c(year_month)))
    
    #stack all the rasters 
    ras <- raster::stack(rasfiles)
    
    # and return raster stack for all provided vars
    ras.all <- raster::stack(ras.all,ras)
    
    #rename raster layer names
    names(ras.all)<-paste(var, c(year_month))
  
  }
  
  
  if (raster) {       #for raster output raster=TRUE
    #crop raster for given aoi/ coords bounds
    # if (!is.null(coords)){
    #   aoi <- suppressWarnings(terra::vect(sf::st_as_sf(sf::st_as_sfc(sf::st_bbox(c(xmin = min(coords[,1]), xmax = max(coords[,1]), ymax = max(coords[,2]), ymin = min(coords[,2])), crs = sf::st_crs(4326))))))
    #   # Subset AOI
    #   ras.all <- suppressWarnings(terra::crop(ras.all, aoi))
    # }
    
    return(ras.all)
    
  } else{             #for TABLE output raster=False
    df <- data.frame()
    for (pnt in seq(1:nrow(coords))){
      lon <- coords[pnt, 1]
      lat <- coords[pnt, 2]
      df1 <- data.frame(terra::extract(ras.all,data.frame(lon,lat)))
      X <- lon
      Y <- lat
      df1<- data.frame(X, Y, df1)
      df <- rbind(df, df1)
    }
    
    names(df) <- sub(sprintf("wc2.1_%s_", res), "", names(df))
    
    return(df)
  }
  
}



###################################################################################################
###################################################################################################

#' Extract worldclim future data
#'

#' @coords data.frame with 2 columns (Latitude and Longitude)
#' @raster optional boolean to export results in SpatRast (terra) format
#' @res    resolution  can be either 2.5m, 5m or 10m
#' @var    list of required variables c("tmin", "tmax", "prec", "bio")
#' @ssp    Shared Socio-economic Pathways (SSPs): 126, 245, 370 and 585.
#' @model  global climate models (GCMs)
#' @period time periods: 2021-2040, 2041-2060, 2061-2080, and 2081-2100. 
#' @return SpatRast
#' @examples
#'

#Extracts worldclim historical monthly data (1960-2018)
worldclim_future<- function(var, res, ssp, model, period,  raster = TRUE, coords = NULL){
  #define function elements and paths
  #downloaded file path
  ifelse(!dir.exists(file.path("geodata/")), dir.create(file.path("geodata/")), FALSE)
  url<-"geodata/"
  
  # url to download from  ##future climate data
  url_download<-"https://geodata.ucdavis.edu/climate/worldclim/2_1/fut/"
  
  
  #update path for given res
  
  ras.all<-raster::stack()
  #loop through list of required vars
  for (i in var) {
    var<-i
    stopifnot(res %in% c("2.5", "5", "10", "2.5m", "5m", "10m"))
    stopifnot(var %in% c( "tmin", "tmax", "prec", "bio","bioc"))
    stopifnot(ssp %in% c("126","245","370","585"))
    stopifnot(period %in% c("2021-2040", "2041-2060", "2061-2080", "2081-2100"))
    stopifnot(model %in% c("BCC-CSM2-MR","CNRM-CM6-1","CNRM-ESM2-1","CanESM5",
                           "GFDL-ESM4","IPSL-CM6A-LR", "MIROC-ES2L","MIROC6","MRI-ESM2-0"))
    res<-ifelse ((grepl("m", res,)), res, paste0(res, "m"))
    ssp<-paste0("ssp",ssp)
    
    if (var=="bio") {var<-"bioc"}
    
    if (res=="2.5m") {url_download<-paste0(url_download,"2.5m/")}
    else if (res=="5m") {url_download<-paste0(url_download,"5m/")}
    else if (res=="10m") {url_download<-paste0(url_download,"10m/")}
    
    file<-paste0("wc2.1_",res,"_",var,"_",model,"_",ssp,"_",period)
    
    if(!file.exists(paste0(url, file,".zip"))){    #if not available download, unzip and stack, retun rasterstack
      download.file(paste0(url_download,file,".zip"), paste0(url,file,".zip"), mode="wb")
      
    }
    suppressWarnings(unzip(paste0(url, file,".zip"), exdir=url, overwrite=FALSE))
    
    file_url<-paste0(url,"share/spatial03/worldclim/cmip6/7_fut/",res,"/",model,"/",ssp,"/")
    
    rasfiles<-paste0(file_url,file,".tif")
    
    #stack all the rasters 
    
    ras <- raster::stack(rasfiles)
    
    
    # and return raster stack for all provided vars
    ras.all <- raster::stack(ras.all,ras)
    
  }
  
  if (raster) {       #for raster output raster=TRUE
    #crop raster for any given aoi/ coords bounds
    # if (!is.null(coords)){
    #   aoi <- suppressWarnings(terra::vect(sf::st_as_sf(sf::st_as_sfc(sf::st_bbox(c(xmin = min(coords[,1]), xmax = max(coords[,1]), ymax = max(coords[,2]), ymin = min(coords[,2])), crs = sf::st_crs(4326))))))
    #   # Subset AOI
    #   ras.all <- suppressWarnings(terra::crop(ras.all, aoi))
    # }
    return(ras.all)
    
    
  } else{             #for TABLE output raster=False
    df <- data.frame()
    for (pnt in seq(1:nrow(coords))){
      lon <- coords[pnt, 1]
      lat <- coords[pnt, 2]
      df1 <- data.frame(terra::extract(ras.all,data.frame(lon,lat)))
      X <- lon
      Y <- lat
      df1<- data.frame(X, Y, df1)
      df <- rbind(df, df1)
    }
    
    #names(df) <- sub(sprintf("wc2.1_%s_", res), "", names(df))
    
    return(df)
    
    
  }
  
  
  
}