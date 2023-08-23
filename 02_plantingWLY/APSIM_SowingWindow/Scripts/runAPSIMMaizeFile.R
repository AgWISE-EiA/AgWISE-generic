#Downloads the soil and prepare the weather data
source("D:/dev_agwise/AgWISE-UseCaseRAB/02_plantingWLY/APSIM_SowingWindow/Scripts/GetSoilandWeather.R")

stn<- read.csv("D:/RwandaData/coordinates_Rwanda.csv")
names(stn)<- c("Longitude", 'Latitude', "Location")
#########################################################


## Load list of met files
setwd("D:/dev_agwise/AgWISE-UseCaseRAB/02_plantingWLY/APSIM_SowingWindow/")
load(file="D:/dev_agwise/AgWISE-UseCaseRAB/02_plantingWLY/APSIM_SowingWindow/my_list_clm.RData")


#Load list of soil files
load(file="D:/dev_agwise/AgWISE-UseCaseRAB/02_plantingWLY/APSIM_SowingWindow/my_list_sol.RData")

# ##########################################################################################################
# #Fix the soil
# setwd("D:/dev_agwise/AgWISE-UseCaseRAB/02_plantingWLY/APSIM_SowingWindow/Scripts/")
# source('FixSoil.R')
# 
# foreach (i =1:length(my_list_sol)) %dopar% {
#   tryCatch(fix_apsimx_soil_profile(my_list_sol[[i]]), error=function(err) NA)
# }

#########################################################
## sourcing function to create spatialize apsim
setwd("D:/dev_agwise/AgWISE-UseCaseRAB/02_plantingWLY/APSIM_SowingWindow/Scripts/")
source('aspimSpatialFactorial.R')

Simulation<- apsimSpatialFactorial(wkdir ="D:/project", 
                                scfl = "D:/dev_agwise/AgWISE-UseCaseRAB/02_plantingWLY/APSIM_SowingWindow/Maize_Factorial/",
                                my_list_clm = my_list_clm,
                                crop = "MaizeFactorial.apsimx", 
                                clck = c("2018-10-01T00:00:00", "2019-12-01T00:00:00"),
                                sd = "01-feb", 
                                ed = "31-mar",
                                variety = "Early",
                                SowingDepth = 50,
                                RowSpacing = 750,
                                ppln = 5.3,
                                MinESW = 100,
                                MinRain = 25,
                                RainDays = 5,
                                rep1 ="[Maize].SowingDate as SowDate",
                                rep2 ="[Maize].Grain.Total.Wt*10 as Yield")

## sourcing function to run the spatialized apsim
setwd("D:/dev_agwise/AgWISE-UseCaseRAB/02_plantingWLY/APSIM_SowingWindow/Scripts/")
source('RunSim.R')
results<-my_list_sim(crop = "MaizeFactorial.apsimx",
                           my_list_clm = my_list_clm, 
                           extd.dir = "D:/project", 
                           stn = stn,
                           my_list_soil = my_list_sol[[3]])

save(results, file="D:/dev_agwise/AgWISE-UseCaseRAB/02_plantingWLY/APSIM_SowingWindow/Maize_Results/season1_outputLA/results.RData")
###########################################################################################################################

##############Here is the post processing Section
load(file="D:/dev_agwise/AgWISE-UseCaseRAB/02_plantingWLY/APSIM_SowingWindow/Maize_Results/season1_outputLA/results.RData")

#####################################################################################################
#source("D:/dev_agwise/AgWISE-UseCaseRAB/02_plantingWLY/APSIM/Scripts/RemoveUnnecessaryStn.R")

#########################################################
## sourcing function to create plot
setwd("D:/dev_agwise/AgWISE-UseCaseRAB/02_plantingWLY/APSIM_SowingWindow/Scripts/")
source('ApsimPlotFactorial.R')

PlantingDates<-apsim.plots(stn = stn,
                                 results=results, 
                                 b= "RWANDA",
                                 wkdir= "D:/project")

#You can also choose to save all the files together as opposed to a list
save(PlantingDates, file="D:/dev_agwise/AgWISE-UseCaseRAB/02_plantingWLY/APSIM_SowingWindow/OutputData/APSIM_MZ_SHT_S1.RData")
