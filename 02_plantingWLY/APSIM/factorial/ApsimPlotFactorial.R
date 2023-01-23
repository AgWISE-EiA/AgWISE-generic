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
  setwd(wkdir)

  foreach (i = 1:length(results))%do%{
    results[[i]]$Longitude<-stn$Longitude[[i]]
    results[[i]]$Latitude<-stn$Latitude[[i]]
    results[[i]]$Location<-stn$Location[[i]]
  }
  
  foreach (i = 1:length(results))%do%{ 
    if(lengths(results[i])< 5){
      results[[i]] <- NULL
    }
  }
  
  foreach (i = 1:length(results))%do%{ 
    if(lengths(results[i])< 5){
      results[[i]] <- NULL
    }
  }
  
  foreach (i = 1:length(results))%do%{ 
    if(lengths(results[i])< 5){
      results[[i]] <- NULL
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
  
  p_Win <- final  %>% 
    group_by(Longitude, Latitude)%>%
    arrange(desc(Yield)) %>% 
    slice(1:10)%>%
    as.data.frame() 
  
  pd<-final%>%
    group_by(Longitude, Latitude)%>%
    slice(which.max(Yield))%>%
    as.data.frame() 
  
  country<-getData("GADM", country=b, level=0)
  
  print(ggplot()+geom_polygon(data=country, aes(x=long, y=lat), fill = "white")+
          geom_point(data=pd, aes(x=Longitude, y=Latitude, color= SowDate), size = 2))
  
  print(ggplot() +  geom_point(data=pd, aes(x=Longitude, y=Latitude, color=SowDate), size = 2))
  
  print(ggplot()+geom_polygon(data=country, aes(x=long, y=lat), fill = "white")+
          geom_point(data=pd, aes(x=Longitude, y=Latitude, color= Yield), size = 2))
  
  print(ggplot() +  geom_point(data=pd, aes(x=Longitude, y=Latitude, color= Yield), size = 2))
return(p_Win)
}


