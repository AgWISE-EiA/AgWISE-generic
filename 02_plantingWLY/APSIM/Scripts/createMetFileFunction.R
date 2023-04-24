
#' Title
#' depends on foreach package
#' @param rain 
#' @param max 
#' @param min 
#' @param solar 
#' @param stn 
#' @param filename 
#'
#' @return
#' @export
#'
#' @examples
createMetFile<-function(rain,max, min,solar,stn,filename = NULL){
  
  my_packages <- c("spdep", "rgdal", "maptools", "raster", "plyr", "ggplot2", "rgdal",
                   "dplyr", "cowplot","readxl", "apsimx", "gtools", "foreach","doParallel",
                   "ranger")
  list.of.packages <- my_packages
  new.packages <- list.of.packages[!(my_packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  lapply(my_packages, require, character.only = TRUE)
  
  if(missing(filename)) filename <- "noname.met"
  
  if(!grepl(".met", filename, fixed = TRUE)) stop("filename should end in .met")
  
  pwr <- foreach (i = 1:(nrow(stn))) %do% {data.frame(date = date[1],
                                                        rain = rain[i] ,
                                                        max = max[i],
                                                        min = min[i],
                                                        solar = solar[i])}
  
  foreach (i = 1: length(pwr))%do%{
    names(pwr[[i]])<- c('Date', 'rain', 'maxt', 'mint', 'radn')}
  
  pwr2<-foreach(i = 1:length(pwr))%do%{
    pwr[[i]]%>%
      mutate(day = lubridate::yday(Date))%>%
      mutate(year=format(as.Date(pwr[[i]]$Date, format="%Y/%m/%d"),"%Y"))%>%
      select("year", "day", 'rain', 'maxt', 'mint', 'radn')}
  
  units <- c("()", "()","(mm)","(oC)", "(oC)", "(MJ/m2/day)")
  
  comments <- paste("!data from various areas. retrieved: ", Sys.time())
  
  ## Calculating annual amplitude in mean monthly temperature
  pwr<- foreach(i = 1:length(pwr2))%do%{                   
    attr(pwr2[[i]], "filename") <- filename
    attr(pwr2[[i]], "site") <- paste("site =", sub(".met", "", filename, fixed = TRUE))
    attr(pwr2[[i]], "latitude") <- paste("latitude =",  stn$Latitude[[i]])
    attr(pwr2[[i]], "longitude") <- paste("longitude =", stn$Longitude[[i]])
    attr(pwr2[[i]], "tav") <- paste("tav =", mean(colMeans(pwr2[[i]][,c("maxt","mint")], na.rm=TRUE), na.rm=TRUE))
    attr(pwr2[[i]], "colnames") <- names(pwr2[[i]])
    attr(pwr2[[i]], "units") <- units
    attr(pwr2[[i]], "comments") <- comments
    ## No constants
    class(pwr2[[i]]) <- c("met", "data.frame")
    pwr2[[i]] <- amp_apsim_met(pwr2[[i]])
  }
  
  if(filename != "noname.met"){
    #write_apsim_met(pwr2, wrt.dir = wrt.dir, filename = filename)
  }
  return(invisible(pwr2))
}