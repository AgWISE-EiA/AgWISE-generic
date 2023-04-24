
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
APSIM_MZ_SHT_S2 <- get(load("D:/dev_agwise/AgWISE-UseCaseRAB/02_plantingWLY/APSIM/OutputData/APSIM_MZ_SHT_S2.Rdata") )
APSIM_MZ_MED_S2 <- get(load("D:/dev_agwise/AgWISE-UseCaseRAB/02_plantingWLY/APSIM/OutputData/APSIM_MZ_MED_S2.Rdata") )
APSIM_MZ_LNG_S2 <- get(load("D:/dev_agwise/AgWISE-UseCaseRAB/02_plantingWLY/APSIM/OutputData/APSIM_MZ_LNG_S2.Rdata") )


##############################################################################################
##  preparing data for plotting
##############################################################################################

## add sowing year
APSIM_MZ_SHT_S2$SowYear <- year(ymd_hms(APSIM_MZ_SHT_S2$Maize.SowingDate))
APSIM_MZ_MED_S2$SowYear <- year(ymd_hms(APSIM_MZ_MED_S2$Maize.SowingDate))
APSIM_MZ_LNG_S2$SowYear <- year(ymd_hms(APSIM_MZ_LNG_S2$Maize.SowingDate))


## indicate season incase we want to combine the two seasons data
APSIM_MZ_SHT_S2$season <- "S2"
APSIM_MZ_MED_S2$season <- "S2"
APSIM_MZ_LNG_S2$season <- "S2"


## get sowing dates to order the leevls in ggplot
unique(APSIM_MZ_SHT_S2$SowDate)
unique(APSIM_MZ_MED_S2$SowDate)
unique(APSIM_MZ_LNG_S2$SowDate)

S1_SowDate <- c("01-feb", "08-feb", "15-feb", "22-feb", "28-feb","07-mar", "14-mar", "21-mar", "28-mar")
S2_SowDate <- c("01-aug", "08-aug", "15-aug", "22-aug", "29-aug", "05-sep", "12-sep" ,"19-sep", "26-sep")


## Maize simulation does not have location, get it from 
# RAB_Location <- read.csv("D:/RwandaData/coordinates_Rwanda.csv")
# RAB_Location$lonlat <- paste(RAB_Location$Lon, RAB_Location$Lat, sep="_")
# RAB_Location$Location <- RAB_Location$Municipality
# 
# 
# APSIM_MZ_SHT_S2 <- merge(APSIM_MZ_SHT_S2, RAB_Location[, c("lonlat", "Location")], by="lonlat")
# APSIM_MZ_MED_S2 <- merge(APSIM_MZ_MED_S2, RAB_Location[, c("lonlat", "Location")], by="lonlat")
# APSIM_MZ_LNG_S2 <- merge(APSIM_MZ_LNG_S2, RAB_Location[, c("lonlat", "Location")], by="lonlat")


## map: remove points in water?: lon = 29.2 and lat = c(>-2.3, <-1.8)
RWA <- suppressWarnings(suppressMessages(readOGR(dsn = "D:/dev_agwise/AgWISE-UseCaseRAB/02_plantingWLY/APSIM/shapefiles/", layer="gadm36_RWA_2")))
AEZ <- suppressWarnings(suppressMessages(readOGR(dsn="D:/dev_agwise/AgWISE-UseCaseRAB/02_plantingWLY/APSIM/shapefiles/",  layer="AEZ_DEM_Dissolve")))
RW_aez <- suppressWarnings(suppressMessages(spTransform(AEZ, CRS( "+proj=longlat +ellps=WGS84 +datum=WGS84"))))
RWA_sf <- suppressMessages(st_as_sf(RWA))
RW_aez_sf <- suppressMessages(st_as_sf(RW_aez))


mapData <- APSIM_MZ_MED_S2[APSIM_MZ_MED_S2$SowDate == "15-aug" & APSIM_MZ_MED_S2$SowYear == 2005, ]
ggplot() +
  # geom_sf(data = RW_aez_sf, aes(fill=as.factor(AEZs_no))) +
  geom_sf(data = RWA_sf, aes(fill=as.factor(NAME_1))) +
  geom_point(data = mapData, aes(Longitude, Latitude))+
  theme_bw()+
  theme(legend.position = "none")

APSIM_MZ_SHT_S2$index <- c(1:nrow(APSIM_MZ_SHT_S2))
APSIM_MZ_MED_S2$index <- c(1:nrow(APSIM_MZ_MED_S2))
APSIM_MZ_LNG_S2$index <- c(1:nrow(APSIM_MZ_LNG_S2))

W_data_S <- APSIM_MZ_SHT_S2[APSIM_MZ_SHT_S2$Longitude < 29.2 & 
                              APSIM_MZ_SHT_S2$Latitude < -1.8 & APSIM_MZ_SHT_S2$Latitude > -2.4, ]
W_data_M <- APSIM_MZ_MED_S2[APSIM_MZ_MED_S2$Longitude < 29.2 & 
                            APSIM_MZ_MED_S2$Latitude < -1.8 & APSIM_MZ_MED_S2$Latitude > -2.4, ]
W_data_L <- APSIM_MZ_LNG_S2[APSIM_MZ_LNG_S2$Longitude < 29.2 & 
                              APSIM_MZ_LNG_S2$Latitude < -1.8 & APSIM_MZ_LNG_S2$Latitude > -2.4, ]

APSIM_MZ_SHT_S2 <- droplevels(APSIM_MZ_SHT_S2[!APSIM_MZ_SHT_S2$index %in% W_data_S$index, ])
APSIM_MZ_MED_S2 <- droplevels(APSIM_MZ_MED_S2[!APSIM_MZ_MED_S2$index %in% W_data_M$index, ])
APSIM_MZ_LNG_S2 <- droplevels(APSIM_MZ_LNG_S2[!APSIM_MZ_LNG_S2$index %in% W_data_L$index, ])

### creating the season 2 data across the three cultivars

APSIM_MZ_SHT_S2$cultivar <- "Short"
APSIM_MZ_MED_S2$cultivar <- "Medium"
APSIM_MZ_LNG_S2$cultivar <- "Long"

APSIM_MZ_S2 <- rbind(APSIM_MZ_SHT_S2, APSIM_MZ_MED_S2, APSIM_MZ_LNG_S2)

###############################################################################################
## plotting for general observations
###############################################################################################

## across locations and years: very similar mean yield across sowing dates but different in variation 

ggplot(APSIM_MZ_S2, aes(x= factor(SowDate, levels = S2_SowDate), y = Yield, col=factor(cultivar))) +
  geom_boxplot() +
  xlab("Sowing date") + ylab("Yield") +
  ggtitle("Across year and location by cultivar") +
  theme_bw()+
  theme(axis.text.x = element_text(angle=45, vjust=0.9, hjust=0.8, size=12), 
        axis.text.y = element_text(size=12), axis.title = element_text(size=14),
        plot.title = element_text(hjust=0.5))



## across years and by location: part of the variation can be explained by differences in location, 
## but still within a location there are huge differences, are these differences purely year-to-year variations?
## is it possible to have the same mean WLY across 9 weeks of planting? WLY = 8750 +- 200 kg.ha 
ggplot(APSIM_MZ_S2, aes(x= factor(SowDate, levels = S2_SowDate), y = Yield, col=factor(cultivar)))+
  geom_boxplot() +
  facet_wrap(~Location) +
  xlab("Sowing date") + ylab("Yield") +
  ggtitle("Yield: across years by locations") +
  theme_bw()+
  theme(axis.text.x = element_text(angle=45, vjust=0.9, hjust=0.8, size=12), 
        axis.text.y = element_text(size=12), axis.title = element_text(size=14),
        plot.title = element_text(hjust=0.5), legend.position = "none",
        strip.text = element_text(size = 12))

length(unique(APSIM_MZ_MED_S2[APSIM_MZ_MED_S2$Location == "Iburengerazuba", "lonlat"]))##131
length(unique(APSIM_MZ_MED_S2[APSIM_MZ_MED_S2$Location == "Amajyepfo", "lonlat"]))##197
length(unique(APSIM_MZ_MED_S2[APSIM_MZ_MED_S2$Location == "Amajyaruguru", "lonlat"]))##101
length(unique(APSIM_MZ_MED_S2[APSIM_MZ_MED_S2$Location == "Iburasirazuba", "lonlat"]))##293
length(unique(APSIM_MZ_MED_S2[APSIM_MZ_MED_S2$Location == "Umujyi wa Kigali","lonlat"]))##21


###############################################################################################
## drill down plots, to understand patterns and sources of variations. 
###############################################################################################

## for every location investigate between years variation : here example is given only for one location
locations <- unique(APSIM_MZ_S2$Location)
loc1 <- APSIM_MZ_S2[APSIM_MZ_S2$Location == locations[1], ]


#Location 1 A : Between sites yield differences by year and by sowing date
ggplot(loc1,  aes(x= factor(SowYear), y = Yield, col=factor(cultivar))) +
  geom_boxplot() +
  facet_wrap(~ factor(SowDate, levels = S2_SowDate), scales="free") +
  xlab("Year") + ylab("Yield") +
  ggtitle(paste("Yield: across field sites by years and sowing date at ", locations[1], sep="")) +
  theme_bw()+
  theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=0.5, size=12), 
        axis.text.y = element_text(size=12), axis.title = element_text(size=14),
        plot.title = element_text(hjust=0.5), legend.position = "none",
        strip.text = element_text(size = 12))

# Between locations yield variation increasing dramatically with a week difference in sowing date.
# In some instances, the yield drops from >8 t/ha to <6 t/ha because the sowing date is shifted by 1week 


## Location 1 B: for every site, get the between years variation. 
## TODO check this out by cultivar
meanY <- ddply(loc1[loc1$SowDate == "01-aug", ], .(lonlat), summarize, meanyield  = mean(Yield))
meanY <- meanY[order(meanY$meanyield, decreasing = TRUE), "lonlat"]
loc1$lonlat <- factor(loc1$lonlat, levels = meanY)


ggplot(loc1,  aes(x= factor(lonlat), y = Yield, col=factor(cultivar))) +
  geom_boxplot() +
  facet_wrap(~ factor(SowDate, levels = S2_SowDate), scales="free") +
  xlab("Field locations") + ylab("Yield") +
  ggtitle(paste("Field yield across the years by sowing Date at ", locations[1], sep="")) +
  theme_bw()+
  theme(axis.text.x = element_blank(), 
        axis.text.y = element_text(size=12), axis.title = element_text(size=14),
        plot.title = element_text(hjust=0.5), legend.position = "none",
        strip.text = element_text(size = 12))



loc1_YV <- loc1[loc1$SowDate == "19-sep" & loc1$Yield < 4000, ]
loc1_YV[order(loc1_YV$lonlat, loc1_YV$SowYear), ]

rf <-loc1[loc1$lonlat == "28.975_-2.525" & loc1$SowDate == "19-sep", c("SowYear","Yield", "InCropRainfall", "InCropET")]
Y1982 <- loc1[loc1$SowYear == 1982 & loc1$SowDate == "19-sep", c("Yield")]
Y1982[order(Y1982)]

rf[rf$Yield<4000, ]
hist(rf$InCropET)

# some locations show a very large between years variation but only for some sowing date, which needs to be investigated 
# e.g., for planting on 22 Aug, there are 6 locations with yield < 8000. 
# the outlying yield values are not related to a particular year but it changes from one GPS to another 
# This does not seem systematic (not related to major factors), 
# TODO check the input data for these points and corresponding years 

###############################################################################################
## observing spatial variation of summary values 
###############################################################################################
## getting summary data for every sites by sowing date = summarizing the 40 years simulation
## we could ask by sowing date what was dose the map of max, min, ... yield looks like? 
Summaries <- ddply(APSIM_MZ_S2, .(Longitude, Latitude, lonlat, SowDate, cultivar), summarize,
                   minYield = as.integer(min(Yield)), maxYield = as.integer(max(Yield)), sdYield = as.integer(sd(Yield)),
                   Q25Yield = as.integer(quantile(Yield, probs = 0.25)),
                   Q50Yield = as.integer(quantile(Yield, probs = 0.50)),
                   Q75Yield = as.integer(quantile(Yield, probs = 0.75)),
                   Q90Yield = as.integer(quantile(Yield, probs = 0.90)))

# set thresholds to discretize the data
Summaries_long <- gather(Summaries, Type, Yield, minYield:Q90Yield)
thresholds <- c(0, seq(4000, 17500, by=500))
Summaries_long$discretized <- cut(Summaries_long$Yield, breaks = thresholds, labels = seq(4000, 17500, by=500))

Summaries_wide <- spread(subset(Summaries_long, select = -c(Yield)), Type, discretized)
head(Summaries_wide)
Summaries_wide_S1 <- Summaries_wide[Summaries_wide$SowDate == S2_SowDate[1], ]
Summaries_wide_S2 <- Summaries_wide[Summaries_wide$SowDate == S2_SowDate[2], ]
Summaries_wide_S3 <- Summaries_wide[Summaries_wide$SowDate == S2_SowDate[3], ]
Summaries_wide_S4 <- Summaries_wide[Summaries_wide$SowDate == S2_SowDate[4], ]
Summaries_wide_S5 <- Summaries_wide[Summaries_wide$SowDate == S2_SowDate[5], ]
Summaries_wide_S6 <- Summaries_wide[Summaries_wide$SowDate == S2_SowDate[6], ]
Summaries_wide_S7 <- Summaries_wide[Summaries_wide$SowDate == S2_SowDate[7], ]
Summaries_wide_S8 <- Summaries_wide[Summaries_wide$SowDate == S2_SowDate[8], ]
Summaries_wide_S9 <- Summaries_wide[Summaries_wide$SowDate == S2_SowDate[9], ]


### color palllets 
SHT_col <- droplevels(Summaries_wide[Summaries_wide$cultivar == "Short",])
levels(SHT_col$maxYield)

Med_col <- droplevels(Summaries_wide[Summaries_wide$cultivar == "Medium",])
levels(Med_col$maxYield)

Long_col <- droplevels(Summaries_wide[Summaries_wide$cultivar == "Long",])
levels(Long_col$maxYield)



colorRampPalette(c("magenta1", "darkorchid3", "slateblue","#3427FF", "steelblue3", "seagreen4","#00441B", "green3", "greenyellow", "yellow2","orange" ,"tomato3", "red")
)(5)

cols28 <- colorRampPalette(c("magenta1", "darkorchid3", "slateblue","#3427FF", "steelblue3", "seagreen4","#00441B", "green3", "greenyellow", "yellow2","orange" ,"tomato3", "red"))(28)


cols <- c("4000"="#3427FF", "4500"="#6950FF", "5000"="#AD87FC","5500"="#BCA7EF","6000"="#CCC7E2",
          "6500"="#EDF8B1", "7000"="#A0D79D", "7500"="#7AC67B", "8000"="#67BB6E", "8500"="#54AE63",
          "9000"="#42A057", "9500"="#187538", "10000"="#10642E", "10500"="#00441B")



ggplot()+
  geom_sf(data = RWA_sf) +
  geom_point(data=Summaries_wide_S1, aes(Longitude, Latitude, col=maxYield), size=2.5, shape=15) +
  # scale_color_manual(values=cols28) +
  scale_fill_gradient(low="white", high="blue") +
  facet_wrap(~cultivar) +
  theme_ipsum()+
  theme(legend.position = "bottom")

ggplot()+
  geom_sf(data = RWA_sf) +
  geom_point(data=Summaries_wide_S2, aes(Longitude, Latitude, col=minYield), size=2.5, shape=15) +
  # scale_color_manual(values=cols28) +
  scale_fill_gradient(low="white", high="blue") +
  facet_wrap(~cultivar) +
  theme_ipsum()+
  theme(legend.position = "bottom")

#####################################################################################################
ggplot() +
  geom_point(data = APSIM_MZ_LNG_S2, aes(InCropRainfall, Yield, color = SowDate, group = SowYear))+
  facet_wrap(~Location) +
  scale_shape_manual()+
  theme_bw()+
  theme(legend.position = "left")

