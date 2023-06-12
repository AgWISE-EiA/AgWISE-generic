################################################################################################
##SECOND FUNCTION ON PLOTTING USING THE RESULTS OBTAINED FROM apsim.spatial COMMAND##
#results = the results list obtained from apsim.spatial command
#b = the country shapefile e.g "ZM" for Zimbabwe
#wkdir = the directory where station data is saved
#' Title
#'
#' @param stn 
#' @param results 
#' @param b 
#' @param wkdir 
#'
#' @return
#' @export
#'
#' @examples
apsim.plots<- function(stn, results, b, wkdir){
  my_packages <- c("spdep", "rgdal", "maptools", "raster", "plyr", "ggplot2", "rgdal",
                   "dplyr", "cowplot","readxl", "apsimx", "gtools", "foreach","doParallel",
                   "ranger")
  list.of.packages <- my_packages
  new.packages <- list.of.packages[!(my_packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  lapply(my_packages, require, character.only = TRUE)
  
  cores<- detectCores()
  myCluster <- makeCluster(cores -2, # number of cores to use
                           type = "PSOCK") # type of cluster
  registerDoParallel(myCluster)
   setwd(wkdir)
  foreach (i = 1:length(results))%do%{
    results[[i]]$Longitude<-stn$Longitude[[i]]
    results[[i]]$Latitude<-stn$Latitude[[i]]
  }
  
  for (i in 1:10){
  foreach (i = 1:length(results))%do%{ 
    if(lengths(results[i])< 5){
      results[[i]] <- NULL
    }
  }
  }
  ##############################Graphs######################################
  foreach (i = 1:length(results))%do%{
   print(results[[i]]  %>%
   ggplot(aes(x=SimulationID, y=Yield)) +
   geom_point(na.rm=TRUE)+
  ggtitle(paste0("Yield ",i))+
  scale_x_continuous(breaks = seq(1, 365, by = 5))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
   )
   }
  ###########################################################################
  final<- do.call("smartbind", results)
  glimpse(final)
  
  final <- mutate(final, lonlat= paste0(Longitude, "_", Latitude))
  stns<- read.csv("D:/dev_agwise/AgWISE-UseCaseRAB/02_plantingWLY/APSIM/stn.csv")
  
  final<-inner_join(final, stns, by= "lonlat")%>% 
    dplyr::select (-c(X, Longitude.y, Latitude.y))%>%
    rename(Longitude = Longitude.x , Latitude = Latitude.x)
  p_Win <- final  %>% 
    group_by(Longitude, Latitude)%>%
    arrange(desc(Yield)) %>% 
    slice(1:10)%>%
    as.data.frame() 
  
  pd<-final%>%
    group_by(Longitude, Latitude)%>%
    slice(which.max(Yield))%>%
    as.data.frame() 
  
  country<-raster::getData("GADM", country=b, level=0)
  
  print(ggplot()+geom_polygon(data=country, aes(x=long, y=lat), fill = "white")+
          geom_point(data=pd, aes(x=Longitude, y=Latitude, color= SowDate), size = 2))
  
  print(ggplot() +  geom_point(data=pd, aes(x=Longitude, y=Latitude, color=SowDate), size = 2))
  
  print(ggplot()+geom_polygon(data=country, aes(x=long, y=lat), fill = "white")+
          geom_point(data=pd, aes(x=Longitude, y=Latitude, color= Yield), size = 2))
  
  print(ggplot() +  geom_point(data=pd, aes(x=Longitude, y=Latitude, color= Yield), size = 2))
return(final)
}


