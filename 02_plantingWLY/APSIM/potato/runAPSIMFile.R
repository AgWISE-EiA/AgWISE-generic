#'This is the main or index file for running APSIM simulations.
#'This file sources functions for preparing, processing, and visualizing data.
#'The core is a call to 'apsim.spatial' for running apsim simulations.
#'The simulations are provided in a list and run in parallel.
#'

#TODO implement automated data sourcing system from module 1.
#TODO Rename - suggests it runs single APSIM file. But runs spatial suite. 

setwd("D:/dev_agwise/AgWISE-generic/02_plantingWLY/APSIM/")
#######################################################
## sourcing the data 
stn<- read.csv("Rwanda_dummy_data/station.csv")
names(stn)<- c("Longitude", 'Latitude', "Location")

rain<-read.csv("Rwanda_dummy_data/Rainfall.data.coordinates_Rwanda.csv")
rain$Date = seq(as.Date("1981-01-01"), as.Date("2020-12-31"), by="days")
date<- as.data.frame(rain$Date)
rain<-rain[,-1]
  
max<-read.csv("Rwanda_dummy_data/Tmax.data.coordinates_Rwanda.csv")
max<-max[,-1]

min<-read.csv("Rwanda_dummy_data/Tmin.data.coordinates_Rwanda.csv")
min<-min[,-1]

solar<-read.csv("Rwanda_dummy_data/S.Rad.data.coordinates_Rwanda.csv")
solar<-solar[,-1]

#########################################################
## sourcing function to create met file
setwd("D:/dev_agwise/AgWISE-generic/02_plantingWLY/APSIM/potato")
source('createMetFileFunction.R')

my_list_clm<-createMetFile(rain = rain,max = max,min = min,solar = solar,stn = stn)

#########################################################
## sourcing function to create spatialize apsim
#TODO clarify output format of this (suppose it's a list).
setwd("D:/dev_agwise/AgWISE-generic/02_plantingWLY/APSIM/potato")
source('apsimSpatialFunction.R')
results <- apsim.spatial(my_list_clm = my_list_clm,
                        wkdir ="D:/potato/project",
                        crop = "potato.apsimx", 
                        clck = c("2019-01-01T00:00:00", "2020-12-01T00:00:00"),
                        sd = "1-jan", 
                        ed = "30-dec",
                        variety = "Karaka",
                        rep1 = "[Potato].Tuber.Total.Wt*10 as Yield")
 
#########################################################
## sourcing function to create plot

setwd("D:/dev_agwise/AgWISE-generic/02_plantingWLY/APSIM/potato")
source('ApsimPlotFunction.R')
PlantingDates<-apsim.plots(stn = stn,
            results=results, 
            b= "RWANDA")

