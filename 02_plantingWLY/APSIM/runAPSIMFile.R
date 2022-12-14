setwd("D:/j")
#########################################################
## sourcing the data 
stn<- read.csv("Rwanda/station.csv")
names(stn)<- c("Longitude", 'Latitude', "Location")
rain<-read.csv("Rwanda/Rainfall.data.coordinates_Rwanda.csv")
rain$Date = seq(as.Date("1981-01-01"), as.Date("2020-12-31"), by="days")

max<-read.csv("Rwanda/Tmax.data.coordinates_Rwanda.csv")
max$Date = seq(as.Date("1981-01-01"), as.Date("2020-12-31"), by="days")

min<-read.csv("Rwanda/Tmin.data.coordinates_Rwanda.csv")
min$Date = seq(as.Date("1981-01-01"), as.Date("2020-12-31"), by="days")

solar<-read.csv("Rwanda/S.Rad.data.coordinates_Rwanda.csv")
solar$Date = seq(as.Date("1981-01-01"), as.Date("2020-12-31"), by="days")

#########################################################
## sourcing function to create met file
setwd("D:/OneDrive - CGIAR/Documents/Script/EiA/APSIM/test")
source('createMetFileFunction.R')

my_list_clm<-createMetFile(rain = rain,max = max,min = min,solar = solar,stn = stn)

#########################################################
## sourcing function to create spatialize apsim
setwd("D:/OneDrive - CGIAR/Documents/Script/EiA/APSIM/test")
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

setwd("D:/OneDrive - CGIAR/Documents/Script/EiA/APSIM/test")
source('ApsimPlotFunction.R')
PlantingDates<-apsim.plots(stn = stn,
            results=results, 
            b= "RWANDA",
            wkdir= "D:/project")

