packages <- c("reshape2", "lmerTest", "emmeans",  "lme4", "predictmeans", "topmodel", "tidyverse", 
              "plyr", "geosphere", "rgdal", "sf", "plotly", "insight","ggplot2", "lubridate",
              "hrbrthemes")

installed_packages <- packages %in% rownames(installed.packages())
if(any(installed_packages == FALSE)){
  install.packages(packages[!installed_packages])}

suppressWarnings(suppressPackageStartupMessages(invisible(lapply(packages, library, character.only = TRUE))))

###############################################################################################
## read data
###############################################################################################
DSSAT_PT_SHT_S2 <- readRDS("D:/dev_agwise/AgWISE-UseCaseRAB/02_plantingWLY/DSSAT/WLY_Outputs/RW_DSSAT_Potato_SHORT_S2.rds")
DSSAT_PT_MED_S2 <- readRDS("D:/dev_agwise/AgWISE-UseCaseRAB/02_plantingWLY/DSSAT/WLY_Outputs/RW_DSSAT_Potato_MEDIUM_S2.rds")
DSSAT_PT_LNG_S2 <- readRDS("D:/dev_agwise/AgWISE-UseCaseRAB/02_plantingWLY/DSSAT/WLY_Outputs/RW_DSSAT_Potato_LONG_S2.rds")


##############################################################################################
##  preparing data for plotting
##############################################################################################

## add sowing year
DSSAT_PT_SHT_S2$SowYear <- DSSAT_PT_SHT_S2$Sowing.year
DSSAT_PT_MED_S2$SowYear <- DSSAT_PT_MED_S2$Sowing.year
DSSAT_PT_LNG_S2$SowYear <- DSSAT_PT_LNG_S2$Sowing.year


## indicate season incase we want to combine the two seasons data
DSSAT_PT_SHT_S2$season <- "S2"
DSSAT_PT_MED_S2$season <- "S2"
DSSAT_PT_LNG_S2$season <- "S2"


## get sowing dates to order the leevls in ggplot
unique(DSSAT_PT_SHT_S2$SowDate)
unique(DSSAT_PT_MED_S2$SowDate)
unique(DSSAT_PT_LNG_S2$SowDate)

S1_SowDate <- c("01-Jan","08-jan","15-jan", "22-jan", "29-jan",
                "05-Feb", "12-feb", "19-feb", "26-feb",
                "05-Mar", "12-mar", "19-mar", "26-mar",
                "02-Apr", "09-apr", "16-apr", "23-apr", "30-apr")

S2_SowDate <- c("01-Jul","08-Jul", "15-Jul", "29-Jul",
                "05-Aug", "12-Aug", "19-Aug", "26-Aug",
                "02-Sep", "09-Sep" ,"16-Sep", "23-Sep", "30-Sep",
                "07-Oct", "14-Oct", "21-Oct", "28-Oct")


## Maize simulation does not have location, get it from 
RAB_Location <- read.csv("D:/RwandaData/coordinates_Rwanda.csv")
RAB_Location$lonlat <- paste(RAB_Location$Lon, RAB_Location$Lat, sep="_")
RAB_Location$Location <- RAB_Location$Municipality

DSSAT_PT_SHT_S2$lonlat <- paste(DSSAT_PT_SHT_S2$Lon, DSSAT_PT_SHT_S2$Lat, sep="_")
DSSAT_PT_MED_S2$lonlat  <- paste(DSSAT_PT_MED_S2$Lon, DSSAT_PT_MED_S2$Lat, sep="_")
DSSAT_PT_LNG_S2$lonlat  <- paste(DSSAT_PT_MED_S2$Lon, DSSAT_PT_MED_S2$Lat, sep="_")

DSSAT_PT_SHT_S2 <- merge(DSSAT_PT_SHT_S2, RAB_Location[, c("lonlat", "Location")], by="lonlat")
DSSAT_PT_MED_S2 <- merge(DSSAT_PT_MED_S2, RAB_Location[, c("lonlat", "Location")], by="lonlat")
DSSAT_PT_LNG_S2 <- merge(DSSAT_PT_LNG_S2, RAB_Location[, c("lonlat", "Location")], by="lonlat")


## map: remove points in water?: lon = 29.2 and lat = c(>-2.3, <-1.8)
RWA <- suppressWarnings(suppressMessages(readOGR(dsn = "D:/dev_agwise/AgWISE-UseCaseRAB/02_plantingWLY/APSIM/shapefiles/", layer="gadm36_RWA_2")))
AEZ <- suppressWarnings(suppressMessages(readOGR(dsn="D:/dev_agwise/AgWISE-UseCaseRAB/02_plantingWLY/APSIM/shapefiles/",  layer="AEZ_DEM_Dissolve")))
RW_aez <- suppressWarnings(suppressMessages(spTransform(AEZ, CRS( "+proj=longlat +ellps=WGS84 +datum=WGS84"))))
RWA_sf <- suppressMessages(st_as_sf(RWA))
RW_aez_sf <- suppressMessages(st_as_sf(RW_aez))

# unique(DSSAT_PT_MED_S2$Sow.Date)
# mapData <- DSSAT_PT_MED_S2[DSSAT_PT_MED_S2$Sow.Date == "08-Jul" & DSSAT_PT_MED_S2$SowYear == 2005, ]
# ggplot() +
#   # geom_sf(data = RW_aez_sf, aes(fill=as.factor(AEZs_no))) +
#   geom_sf(data = RWA_sf, aes(fill=as.factor(NAME_1))) +
#   geom_point(data = mapData, aes(Lon, Lat))+
#   theme_bw()+
#   theme(legend.position = "none")

DSSAT_PT_SHT_S2$index <- c(1:nrow(DSSAT_PT_SHT_S2))
DSSAT_PT_MED_S2$index <- c(1:nrow(DSSAT_PT_MED_S2))
DSSAT_PT_LNG_S2$index <- c(1:nrow(DSSAT_PT_LNG_S2))

W_data_S <- DSSAT_PT_SHT_S2[DSSAT_PT_SHT_S2$Longitude < 29.2 & 
                              DSSAT_PT_SHT_S2$Latitude < -1.8 & DSSAT_PT_SHT_S2$Latitude > -2.4, ]
W_data_M <- DSSAT_PT_MED_S2[DSSAT_PT_MED_S2$Longitude < 29.2 & 
                              DSSAT_PT_MED_S2$Latitude < -1.8 & DSSAT_PT_MED_S2$Latitude > -2.4, ]
W_data_L <- DSSAT_PT_LNG_S2[DSSAT_PT_LNG_S2$Longitude < 29.2 & 
                              DSSAT_PT_LNG_S2$Latitude < -1.8 & DSSAT_PT_LNG_S2$Latitude > -2.4, ]

DSSAT_PT_SHT_S2 <- droplevels(DSSAT_PT_SHT_S2[!DSSAT_PT_SHT_S2$index %in% W_data_S$index, ])
DSSAT_PT_MED_S2 <- droplevels(DSSAT_PT_MED_S2[!DSSAT_PT_MED_S2$index %in% W_data_M$index, ])
DSSAT_PT_LNG_S2 <- droplevels(DSSAT_PT_LNG_S2[!DSSAT_PT_LNG_S2$index %in% W_data_L$index, ])

### creating the season 2 data across the three cultivars

DSSAT_PT_SHT_S2$cultivar <- "Short"
DSSAT_PT_MED_S2$cultivar <- "Medium"
DSSAT_PT_LNG_S2$cultivar <- "Long"

DSSAT_PT_S2 <- rbind(DSSAT_PT_SHT_S2, DSSAT_PT_MED_S2, DSSAT_PT_LNG_S2)
DSSAT_PT_S2<-subset(DSSAT_PT_S2, Sow.Date %in%  c("01-Jul","08-Jul", "15-Jul", "29-Jul",
                                                  "05-Aug", "12-Aug", "19-Aug", "26-Aug",
                                                  "02-Sep", "09-Sep" ,"16-Sep", "23-Sep", "30-Sep",
                                                  "07-Oct", "14-Oct", "21-Oct", "28-Oct"))

DSSAT_PT_S2<-subset(DSSAT_PT_S2, Sow.Date %in%  c("01-Jul","05-Aug", "02-Sep", "07-Oct"))
DSSAT_PT_S2<-subset(DSSAT_PT_S2, cultivar %in%  c("Short","Long"))
unique(DSSAT_PT_S2$Sow.Date)
glimpse(DSSAT_PT_S2)

DSSAT_PT_S2<-DSSAT_PT_S2%>% 
  rename(InCropRainfall =`Total.Seasonal.Rainfall(mm)`)

###############################################################################################
## plotting for general observations
###############################################################################################
unique(DSSAT_PT_S2$Location)
## across locations and years: very similar mean yield across sowing dates but different in variation 

ggplot(DSSAT_PT_S2, aes(x= factor(Sow.Date, levels = S2_SowDate), y = Yield, col=factor(cultivar))) +
  geom_boxplot() +
  xlab("Sowing date") + ylab("Yield") +
  ggtitle("Across year and location by cultivar") +
  theme_bw()+
  theme(axis.text.x = element_text(angle=45, vjust=0.9, hjust=0.8, size=12), 
        axis.text.y = element_text(size=12), axis.title = element_text(size=14),
        plot.title = element_text(hjust=0.5))
# stat_summary(fun =mean, geom="point", shape=23, size=2)


## across years and by location: part of the variation can be explained by differences in location, 
## but still within a location there are huge differences, are these differences purely year-to-year variations?
## is it possible to have the same mean WLY across 9 weeks of planting? WLY = 8750 +- 200 kg.ha 
ggplot(DSSAT_PT_S2, aes(x= factor(Sow.Date, levels = S2_SowDate), y = Yield, col=factor(cultivar)))+
  geom_boxplot() +
  facet_wrap(~Location) +
  xlab("Sowing date") + ylab("Yield") +
  ggtitle("Yield: across Sowing Dates by locations") +
  theme_bw()+
  theme(axis.text.x = element_text(angle=45, vjust=0.9, hjust=0.8, size=12), 
        axis.text.y = element_text(size=12), axis.title = element_text(size=14),
        plot.title = element_text(hjust=0.5), legend.position = "none",
        strip.text = element_text(size = 12))

ggplot(DSSAT_PT_S2, aes(x= factor(SowYear), y = Yield, col=factor(cultivar)))+
  geom_boxplot() +
  facet_wrap(~Location) +
  xlab("Years") + ylab("Yield") +
  ggtitle("Yield: across years by locations") +
  theme_bw()+
  theme(axis.text.x = element_text(angle=45, vjust=0.9, hjust=0.8, size=12), 
        axis.text.y = element_text(size=12), axis.title = element_text(size=14),
        plot.title = element_text(hjust=0.5), legend.position = "none",
        strip.text = element_text(size = 12))

ggplot(DSSAT_PT_S2, aes(x= factor(SowYear), y = InCropRainfall, col=factor(cultivar)))+
  geom_boxplot() +
  facet_wrap(~Location) +
  xlab("Year") + ylab("Rainfall") +
  ggtitle("Rainfall: across years by locations") +
  theme_bw()+
  theme(axis.text.x = element_text(angle=45, vjust=0.9, hjust=0.8, size=12), 
        axis.text.y = element_text(size=12), axis.title = element_text(size=14),
        plot.title = element_text(hjust=0.5), legend.position = "none",
        strip.text = element_text(size = 12))


# ggplot(DSSAT_PT_S2, aes(x= factor(SowYear), y = TotalESW, col=factor(cultivar)))+
#   geom_boxplot() +
#   facet_wrap(~Location) +
#   xlab("Year") + ylab("Soil Water") +
#   ggtitle("Soil Water: across years by locations") +
#   theme_bw()+
#   theme(axis.text.x = element_text(angle=45, vjust=0.9, hjust=0.8, size=12), 
#         axis.text.y = element_text(size=12), axis.title = element_text(size=14),
#         plot.title = element_text(hjust=0.5), legend.position = "none",
#         strip.text = element_text(size = 12))


length(unique(DSSAT_PT_S2[DSSAT_PT_S2$Location == "Iburengerazuba", "lonlat"]))##131
length(unique(DSSAT_PT_S2[DSSAT_PT_S2$Location == "Amajyepfo", "lonlat"]))##197
length(unique(DSSAT_PT_S2[DSSAT_PT_S2$Location == "Amajyaruguru", "lonlat"]))##101
length(unique(DSSAT_PT_S2[DSSAT_PT_S2$Location == "Iburasirazuba", "lonlat"]))##288
length(unique(DSSAT_PT_S2[DSSAT_PT_S2$Location == "Umujyi wa Kigali","lonlat"]))##21


###############################################################################################
## drill down plots, to understand patterns and sources of variations. 
###############################################################################################

## for every location investigate between years variation : here example is given only for one location
locations <- unique(DSSAT_PT_S2$Location)
loc1 <- DSSAT_PT_S2[DSSAT_PT_S2$Location == locations[1], ]

unique(DSSAT_PT_S2$Sow.Date)
unique(S2_SowDate)
#Location 1 A : Between sites yield differences by year and by sowing date
ggplot(loc1,  aes(x= reorder(factor(SowYear), -Yield), y = Yield, col=factor(cultivar))) +
  geom_boxplot() +
  facet_wrap(~ factor(Sow.Date, levels = S2_SowDate), scales="free") +
  xlab("Year") + ylab("Yield") +
  ggtitle(paste("Yield: across field sites by years and sowing date at ", locations[1], sep="")) +
  theme_bw()+
  theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=0.5, size=10), 
        axis.text.y = element_text(size=12), axis.title = element_text(size=14),
        plot.title = element_text(hjust=0.5), legend.position = "none",
        strip.text = element_text(size = 12))

ggplot(loc1,  aes(x= reorder(factor(SowYear), -InCropRainfall), y = InCropRainfall, col=factor(cultivar))) +
  geom_boxplot() +
  facet_wrap(~ factor(Sow.Date, levels = S2_SowDate), scales="free") +
  xlab("Year") + ylab("Rainfall (mm)") +
  ggtitle(paste("Rainfall: across field sites by years and sowing date at ", locations[1], sep="")) +
  theme_bw()+
  theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=0.5, size=10), 
        axis.text.y = element_text(size=12), axis.title = element_text(size=14),
        plot.title = element_text(hjust=0.5), legend.position = "none",
        strip.text = element_text(size = 12))

# ggplot(loc1,  aes(x= reorder(factor(SowYear), -InCropMaxTemperature), y = InCropMaxTemperature, col=factor(cultivar))) +
#   geom_boxplot() +
#   facet_wrap(~ factor(SowDate, levels = S2_SowDate), scales="free") +
#   xlab("Year") + ylab("Max Temperature (C)") +
#   ggtitle(paste("Maximum Temperature: across field sites by years and sowing date at ", locations[1], sep="")) +
#   theme_bw()+
#   theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=0.5, size=12), 
#         axis.text.y = element_text(size=12), axis.title = element_text(size=14),
#         plot.title = element_text(hjust=0.5), legend.position = "none",
#         strip.text = element_text(size = 12))
# 
# 
# ggplot(loc1,  aes(x= reorder(factor(SowYear),-TotalESW), y = TotalESW, col=factor(cultivar))) +
#   geom_boxplot() +
#   facet_wrap(~ factor(SowDate, levels = S2_SowDate), scales="free") +
#   xlab("Year") + ylab("Max Temperature (C)") +
#   ggtitle(paste("Soil Water: across field sites by years and sowing date at ", locations[1], sep="")) +
#   theme_bw()+
#   theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=0.5, size=12), 
#         axis.text.y = element_text(size=12), axis.title = element_text(size=14),
#         plot.title = element_text(hjust=0.5), legend.position = "none",
#         strip.text = element_text(size = 12))
####################################################################################################