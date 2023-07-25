#Downloads the soil and prepare the weather data
source("D:/dev_agwise/AgWISE-UseCaseRAB/02_plantingWLY/APSIM/Scripts/GetSoilandWeather.R")

stn<- read.csv("D:/RwandaData/coordinates_Rwanda.csv")
names(stn)<- c("Longitude", 'Latitude', "Location")
#########################################################


## Load list of met files
setwd("D:/dev_agwise/AgWISE-UseCaseRAB/02_plantingWLY/APSIM/")
load(file="D:/dev_agwise/AgWISE-UseCaseRAB/02_plantingWLY/APSIM/my_list_clm.RData")


#Load list of soil files
load(file="D:/dev_agwise/AgWISE-UseCaseRAB/02_plantingWLY/APSIM/my_list_sol.RData")

# ##########################################################################################################
# #Fix the soil
# setwd("D:/dev_agwise/AgWISE-UseCaseRAB/02_plantingWLY/APSIM/Scripts/")
# source('FixSoil.R')
# 
# foreach (i =1:length(my_list_sol)) %dopar% {
#   tryCatch(fix_apsimx_soil_profile(my_list_sol[[i]]), error=function(err) NA)
# }

#########################################################
## sourcing function to create spatialize apsim
setwd("D:/dev_agwise/AgWISE-UseCaseRAB/02_plantingWLY/APSIM/Scripts/")
source('aspimSpatialFactorial.R')

SimFebMar<- apsimSpatialFactorial(scfl = "D:/dev_agwise/AgWISE-UseCaseRAB/02_plantingWLY/APSIM/Maize_Factorial/",
                                  my_list_clm = my_list_clm,
                                  wkdir ="D:/project", 
                                  crop = "MaizeFactorialFebMar.apsimx", 
                                  clck = c("1981-01-01T00:00:00", "2020-12-31T00:00:00"),
                                  variety = "Late",
                                  rep1 ="[Maize].Grain.Total.Wt*10 as Yield" ,
                                  rep2 ="[Maize].SowingDate",
                                  ppln = 5.3)

SimAugSep<- apsimSpatialFactorial(scfl = "D:/dev_agwise/AgWISE-UseCaseRAB/02_plantingWLY/APSIM/Maize_Factorial/",
                                  my_list_clm = my_list_clm,
                                  wkdir ="D:/project", 
                                  crop = "MaizeFactorialAugSep.apsimx", 
                                  clck = c("1981-01-01T00:00:00", "2020-12-31T00:00:00"),
                                  variety = "Late",
                                  rep1 ="[Maize].Grain.Total.Wt*10 as Yield" ,
                                  rep2 ="[Maize].SowingDate",
                                  ppln = 5.3)

## sourcing function to run the spatialized apsim
setwd("D:/dev_agwise/AgWISE-UseCaseRAB/02_plantingWLY/APSIM/Scripts/")
source('RunSim.R')
resultsFebMar<-my_list_sim(crop = "MaizeFactorialFebMar.apsimx",
                           my_list_clm = my_list_clm, 
                           extd.dir = "D:/project", 
                           stn = stn,
                           my_list_soil = my_list_sol[[2]])


################################################
resultsAugSep<-my_list_sim(crop = "MaizeFactorialAugSep.apsimx",
                           my_list_clm = my_list_clm, 
                           extd.dir = "D:/project", 
                           stn = stn,
                           my_list_soil = my_list_sol[[2]]) 

save(resultsFebMar, file="D:/dev_agwise/AgWISE-UseCaseRAB/02_plantingWLY/APSIM/Maize_Results/season1_outputLA/resultsFebMar.RData")
save(resultsAugSep, file="D:/dev_agwise/AgWISE-UseCaseRAB/02_plantingWLY/APSIM/Maize_Results/season2_outputLA/resultsAugSep.RData")
###########################################################################################################################

##############Here is the post processing Section
load(file="D:/dev_agwise/AgWISE-UseCaseRAB/02_plantingWLY/APSIM/Maize_Results/season1_outputLA/resultsFebMar.RData")
load(file="D:/dev_agwise/AgWISE-UseCaseRAB/02_plantingWLY/APSIM/Maize_Results/season2_outputLA/resultsAugSep.RData")

#####################################################################################################
#source("D:/dev_agwise/AgWISE-UseCaseRAB/02_plantingWLY/APSIM/Scripts/RemoveUnnecessaryStn.R")

#########################################################
## sourcing function to create plot
setwd("D:/dev_agwise/AgWISE-UseCaseRAB/02_plantingWLY/APSIM/Scripts/")
source('ApsimPlotFactorial.R')

PlantingDatesFebMar<-apsim.plots(stn = stn,
                                 results=resultsFebMar, 
                                 b= "RWANDA",
                                 wkdir= "D:/project")

PlantingDatesAugSep<-apsim.plots(stn = stn,
                                 results=resultsAugSep, 
                                 b= "RWANDA",
                                 wkdir= "D:/project")


#You can also choose to save all the files together as opposed to a list
save(PlantingDatesFebMar, file="D:/dev_agwise/AgWISE-UseCaseRAB/02_plantingWLY/APSIM/OutputData/APSIM_MZ_SHT_S1.RData")
save(PlantingDatesAugSep, file="D:/dev_agwise/AgWISE-UseCaseRAB/02_plantingWLY/APSIM/OutputData/APSIM_MZ_SHT_S2.RData")


#HERE########################################################
## sourcing function to create spatialize apsim
setwd("D:/dev_agwise/AgWISE-UseCaseRAB/02_plantingWLY/APSIM/Scripts/")
source('aspimSpatialFactorial.R')

SimFebMar<- apsimSpatialFactorial(scfl = "D:/dev_agwise/AgWISE-UseCaseRAB/02_plantingWLY/APSIM/Maize_Factorial/",
                                  my_list_clm = my_list_clm,
                                  wkdir ="D:/project", 
                                  crop = "MaizeFactorialFebMar.apsimx", 
                                  clck = c("1981-01-01T00:00:00", "2020-12-31T00:00:00"),
                                  variety = "Mid",
                                  rep1 ="[Maize].Grain.Total.Wt*10 as Yield" ,
                                  rep2 ="[Maize].SowingDate",
                                  ppln = 5.3)

SimAugSep<- apsimSpatialFactorial(scfl = "D:/dev_agwise/AgWISE-UseCaseRAB/02_plantingWLY/APSIM/Maize_Factorial/",
                                  my_list_clm = my_list_clm,
                                  wkdir ="D:/project", 
                                  crop = "MaizeFactorialAugSep.apsimx", 
                                  clck = c("1981-01-01T00:00:00", "2020-12-31T00:00:00"),
                                  variety = "Mid",
                                  rep1 ="[Maize].Grain.Total.Wt*10 as Yield" ,
                                  rep2 ="[Maize].SowingDate",
                                  ppln = 5.3)

## sourcing function to run the spatialized apsim
setwd("D:/dev_agwise/AgWISE-UseCaseRAB/02_plantingWLY/APSIM/Scripts/")
source('RunSim.R')
resultsFebMar<-my_list_sim(crop = "MaizeFactorialFebMar.apsimx",
                           my_list_clm = my_list_clm, 
                           extd.dir = "D:/project", 
                           stn = stn,
                           my_list_soil = my_list_sol[[2]])

resultsAugSep<-my_list_sim(crop = "MaizeFactorialAugSep.apsimx",
                           my_list_clm = my_list_clm, 
                           extd.dir = "D:/project", 
                           stn = stn,
                           my_list_soil = my_list_sol[[2]]) 

save(resultsFebMar, file="D:/dev_agwise/AgWISE-UseCaseRAB/02_plantingWLY/APSIM/Maize_Results/season1_outputHA/resultsFebMar.RData")
save(resultsAugSep, file="D:/dev_agwise/AgWISE-UseCaseRAB/02_plantingWLY/APSIM/Maize_Results/season2_outputHA/resultsAugSep.RData")
###########################################################################################################################


##############Here is the post processing Section
load(file="D:/dev_agwise/AgWISE-UseCaseRAB/02_plantingWLY/APSIM/Maize_Results/season1_outputHA/resultsFebMar.RData")
load(file="D:/dev_agwise/AgWISE-UseCaseRAB/02_plantingWLY/APSIM/Maize_Results/season2_outputHA/resultsAugSep.RData")

#####################################################################################################
#source("D:/dev_agwise/AgWISE-UseCaseRAB/02_plantingWLY/APSIM/Scripts/RemoveUnnecessaryStn.R")

#########################################################
## sourcing function to create plot
setwd("D:/dev_agwise/AgWISE-UseCaseRAB/02_plantingWLY/APSIM/Scripts/")
source('ApsimPlotFactorial.R')

PlantingDatesFebMar<-apsim.plots(stn = stn,
                                 results=resultsFebMar, 
                                 b= "RWANDA",
                                 wkdir= "D:/project")

PlantingDatesAugSep<-apsim.plots(stn = stn,
                                 results=resultsAugSep, 
                                 b= "RWANDA",
                                 wkdir= "D:/project")


#You can also choose to save all the files together as opposed to a list
save(PlantingDatesFebMar, file="D:/dev_agwise/AgWISE-UseCaseRAB/02_plantingWLY/APSIM/OutputData/APSIM_MZ_LNG_S1.RData")
save(PlantingDatesAugSep, file="D:/dev_agwise/AgWISE-UseCaseRAB/02_plantingWLY/APSIM/OutputData/APSIM_MZ_LNG_S2.RData")

#########################################################
## sourcing function to create spatialize apsim
setwd("D:/dev_agwise/AgWISE-UseCaseRAB/02_plantingWLY/APSIM/Scripts/")
source('aspimSpatialFactorial.R')

SimFebMar<- apsimSpatialFactorial(scfl = "D:/dev_agwise/AgWISE-UseCaseRAB/02_plantingWLY/APSIM/Maize_Factorial/",
                                  my_list_clm = my_list_clm,
                                  wkdir ="D:/project", 
                                  crop = "MaizeFactorialFebMar.apsimx", 
                                  clck = c("1981-01-01T00:00:00", "2020-12-31T00:00:00"),
                                  variety = "Early",
                                  rep1 ="[Maize].Grain.Total.Wt*10 as Yield" ,
                                  rep2 ="[Maize].SowingDate",
                                  ppln = 5.3)

SimAugSep<- apsimSpatialFactorial(scfl = "D:/dev_agwise/AgWISE-UseCaseRAB/02_plantingWLY/APSIM/Maize_Factorial/",
                                  my_list_clm = my_list_clm,
                                  wkdir ="D:/project", 
                                  crop = "MaizeFactorialAugSep.apsimx", 
                                  clck = c("1981-01-01T00:00:00", "2020-12-31T00:00:00"),
                                  variety = "Early",
                                  rep1 ="[Maize].Grain.Total.Wt*10 as Yield" ,
                                  rep2 ="[Maize].SowingDate",
                                  ppln = 5.3)

## sourcing function to run the spatialized apsim
setwd("D:/dev_agwise/AgWISE-UseCaseRAB/02_plantingWLY/APSIM/Scripts/")
source('RunSim.R')
resultsFebMar<-my_list_sim(crop = "MaizeFactorialFebMar.apsimx",
                           my_list_clm = my_list_clm, 
                           extd.dir = "D:/project", 
                           stn = stn,
                           my_list_soil = my_list_sol[[2]])

resultsAugSep<-my_list_sim(crop = "MaizeFactorialAugSep.apsimx",
                           my_list_clm = my_list_clm, 
                           extd.dir = "D:/project", 
                           stn = stn,
                           my_list_soil = my_list_sol[[2]]) 

save(resultsFebMar, file="D:/dev_agwise/AgWISE-UseCaseRAB/02_plantingWLY/APSIM/Maize_Results/season1_outputMA/resultsFebMar.RData")
save(resultsAugSep, file="D:/dev_agwise/AgWISE-UseCaseRAB/02_plantingWLY/APSIM/Maize_Results/season2_outputMA/resultsAugSep.RData")
###########################################################################################################################


##############Here is the post processing Section
load(file="D:/dev_agwise/AgWISE-UseCaseRAB/02_plantingWLY/APSIM/Maize_Results/season1_outputMA/resultsFebMar.RData")
load(file="D:/dev_agwise/AgWISE-UseCaseRAB/02_plantingWLY/APSIM/Maize_Results/season2_outputMA/resultsAugSep.RData")

#####################################################################################################
#source("D:/dev_agwise/AgWISE-UseCaseRAB/02_plantingWLY/APSIM/Scripts/RemoveUnnecessaryStn.R")

#########################################################
## sourcing function to create plot
setwd("D:/dev_agwise/AgWISE-UseCaseRAB/02_plantingWLY/APSIM/Scripts/")
source('ApsimPlotFactorial.R')

PlantingDatesFebMar<-apsim.plots(stn = stn,
                                 results=resultsFebMar, 
                                 b= "RWANDA",
                                 wkdir= "D:/project")

PlantingDatesAugSep<-apsim.plots(stn = stn,
                                 results=resultsAugSep, 
                                 b= "RWANDA",
                                 wkdir= "D:/project")


#You can also choose to save all the files together as opposed to a list
save(PlantingDatesFebMar, file="D:/dev_agwise/AgWISE-UseCaseRAB/02_plantingWLY/APSIM/OutputData/APSIM_MZ_MED_S1.RData")
save(PlantingDatesAugSep, file="D:/dev_agwise/AgWISE-UseCaseRAB/02_plantingWLY/APSIM/OutputData/APSIM_MZ_MED_S2.RData")

