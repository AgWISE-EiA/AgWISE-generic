##############Remove unnecessary stations###########################################################################
#Load list of soil files
load(file="D:/dev_agwise/AgWISE-UseCaseRAB/02_plantingWLY/APSIM_SowingWindow/my_list_sol.RData")

foreach (i = 1:length(my_list_sol))%do%{
  my_list_sol[[i]]$Longitude<-stn$Longitude[[i]]
  my_list_sol[[i]]$Latitude<-stn$Latitude[[i]]
}

#Run 10 times
for (i in 1:10){
  foreach (i = 1:length(my_list_sol))%do%{ 
    if(lengths(my_list_sol[i])== 3){
      my_list_sol[[i]] <- NULL
    }
  }
}

final<- foreach (i = 1:length(my_list_sol))%do%{ 
  as.data.frame(cbind(my_list_sol[[i]]$Longitude, my_list_sol[[i]]$Latitude))
}

#######################################################
## sourcing the data 
stn<- read.csv("D:/RwandaData/coordinates_Rwanda.csv")
names(stn)<- c("Longitude", 'Latitude', "Location")

stns<- do.call("smartbind", final)
names(stn)<- c("Longitude", 'Latitude')
library(dplyr)
stns <- mutate(stn, lonlat= paste0(Longitude, "_", Latitude))

write.csv(stns, "D:/dev_agwise/AgWISE-UseCaseRAB/02_plantingWLY/APSIM_SowingWindow/stn.csv")
#####################################################################################################################
 
