setwd("D:/RwandaData")
#######################################################
## sourcing the data 
stn<- read.csv("coordinates_Rwanda.csv")
names(stn)<- c("Longitude", 'Latitude', "Location")

rain<-read.csv("Rainfall.data.coordinates_Rwanda.csv")
rain$Date = seq(as.Date("1981-01-01"), as.Date("2020-12-31"), by="days")
date<- as.data.frame(rain$Date)
rain<-rain[,-1]

max<-read.csv("Tmax.data.coordinates_Rwanda.csv")
max<-max[,-1]

min<-read.csv("Tmin.data.coordinates_Rwanda.csv")
min<-min[,-1]

solar<-read.csv("S.Rad.data.coordinates_Rwanda.csv")
solar<-solar[,-1]

#########################################################
## sourcing function to create met file
setwd("D:/dev_agwise/AgWISE-UseCaseRAB/02_plantingWLY/APSIM/Scripts/")
source('createMetFileFunction.R')

my_list_clm<-createMetFile(rain = rain,max = max,min = min,solar = solar,stn = stn)
save(my_list_clm, file="D:/dev_agwise/AgWISE-UseCaseRAB/02_plantingWLY/APSIM/my_list_clm.RData")
#source soil and climate data

#Write the weather files to a working directory and Edit the weather as per location
foreach (i =1:length(my_list_clm)) %dopar% {
  apsimx::write_apsim_met(my_list_clm[[i]], wrt.dir = "D:/project", filename = paste0('wth_loc_',i,'.met'))}

#########################################################

#Get soil data from iscric
my_list_sol <- foreach (i = 1:nrow(stn)-1) %dopar% {
  tryCatch(apsimx::get_isric_soil_profile(lonlat = c(stn$Longitude[i], stn$Latitude[[i]]), fix = TRUE)
           , error=function(err) NA)
}
save(my_list_sol, file="D:/dev_agwise/AgWISE-UseCaseRAB/02_plantingWLY/APSIM/my_list_sol.RData")



