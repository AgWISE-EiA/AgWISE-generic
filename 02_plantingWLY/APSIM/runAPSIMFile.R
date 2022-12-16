setwd("D:/j")
#######################################################
## sourcing the data 
stn<- read.csv("Rwanda/station.csv")
names(stn)<- c("Longitude", 'Latitude', "Location")

rain<-read.csv("Rwanda/Rainfall.data.coordinates_Rwanda.csv")
rain$Date = seq(as.Date("1981-01-01"), as.Date("2020-12-31"), by="days")
date<- as.data.frame(rain$Date)
rain<-rain[,-1]
  
max<-read.csv("Rwanda/Tmax.data.coordinates_Rwanda.csv")
max<-max[,-1]

min<-read.csv("Rwanda/Tmin.data.coordinates_Rwanda.csv")
min<-min[,-1]

solar<-read.csv("Rwanda/S.Rad.data.coordinates_Rwanda.csv")
solar<-solar[,-1]

#########################################################
## sourcing function to create met file
setwd("D:/dev_agwise/AgWISE-generic/02_plantingWLY/APSIM/")
source('createMetFileFunction.R')

my_list_clm<-createMetFile(rain = rain,max = max,min = min,solar = solar,stn = stn)

#########################################################
## sourcing function to create spatialize apsim
setwd("D:/dev_agwise/AgWISE-generic/02_plantingWLY/APSIM/")
source('apsimSpatialFunction.R')
results<- apsim.spatial(my_list_clm = my_list_clm,
                        wkdir ="D:/project", 
                        crop = "Maize.apsimx", 
                        clck = c("2019-01-01T00:00:00", "2020-12-01T00:00:00"),
                        sd = "1-jan", 
                        ed = "30-dec",
                        variety = "sc501",
                        fert = 200,
                        rep1 ="[Maize].Grain.Total.Wt*10 as Yield" ,
                        rep2 ="[Maize].SowingDate")
 
#########################################################
## sourcing function to create plot

setwd("D:/dev_agwise/AgWISE-generic/02_plantingWLY/APSIM/")
source('ApsimPlotFunction.R')
PlantingDates<-apsim.plots(stn = stn,
            results=results, 
            b= "RWANDA",
            wkdir= "D:/project")

