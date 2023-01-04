#' Title
#'
#' @param stn = Dataframe with the latlon data
#' @param results = the results list obtained from apsim.spatial command
#' @param b = the country shape file e.g "ZM" for Zimbabwe
#' @param wkdir = the directory where station data is saved
#'
#' @return The planting dates
#' @export
#'
#' @examples apsim.plots(stn = stn, results=results, b= "RWANDA")

apsim.plots<- function(stn, results, b){
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
##############################Graphs######################################
  foreach (i = 1:length(results))%do%{
    print(results[[i]]  %>%
            ggplot(aes(x=Potato.SowingDate, y=Yield)) +
            geom_point(na.rm=TRUE)+
            ggtitle(paste0("Yield ",i)))
  }
  #######################################################################
  final<- do.call("smartbind", results)
  FinalBound<-final
  colnames(FinalBound)[colnames(FinalBound) == "Date"]  <- "Potato.HarvestingDate"  
  FinalBound$Potato.SowingDate = format(as.Date(FinalBound$Potato.SowingDate), "%d/%m/%Y")
  FinalBound$Potato.HarvestingDate = strptime(FinalBound$Potato.HarvestingDate, "%Y-%m-%d")
  FinalBound$Potato.HarvestingDate = format(FinalBound$Potato.HarvestingDate, "%d/%m/%Y")
  
  finall<-final%>%
    group_by(Longitude, Latitude)%>%
    slice(which.max(Yield))%>%
    as.data.frame()
  
  #country<-raster::getData("GADM", country=b, level=0)
  country<-geodata::gadm(country=b, level=0, path=tempdir())
  
  print(ggplot()+tidyterra::geom_spatvector(data=country, fill = "white")+
          geom_point(data=finall, aes(x=Longitude, y=Latitude, color= Potato.SowingDate), size = 2))
  
  print(ggplot() + geom_point(data=final, aes(x=Longitude, y=Latitude, color= Potato.SowingDate), size = 2))
  
  print(ggplot()+tidyterra::geom_spatvector(data=country, fill = "white")+
          geom_point(data=finall, aes(x=Longitude, y=Latitude, color= Yield), size = 2))
  
  print(ggplot() + geom_point(data=finall, aes(x=Longitude, y=Latitude, color= Yield), size = 2))
return(FinalBound)
}




