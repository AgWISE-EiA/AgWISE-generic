
# package names
packages <- c("readxl", "tidyverse", "reshape2", "mapview", "lubridate", "sf", "geodata")

# install packages
installed_packages <- packages %in% rownames(installed.packages())
if(any(installed_packages == FALSE)){
  install.packages(packages[!installed_packages])}

# load packages
invisible(lapply(packages, library, character.only = TRUE))

# ------------------------------------------------------------------------------
# potato fertiliser data 22a
data_22a <- read_excel('./data/raw/rwasis-potato/RwaSIS data template_Potato_2022A_RBJ_17Jan2023.xlsx', sheet='RS-PFR-1')

# calculate potato yield
colnames(data_22a)[12] <- 'plot_area_m2'
colnames(data_22a)[13] <- 'harvest_kgplot'
data_22a$yield_kgha <- 10000 * data_22a$harvest_kgplot / data_22a$plot_area_m2
data_22a$yield_tha <- data_22a$yield_kgha/1000

# get columns of interest
data_22a <- data_22a[c(3, 4, 5, 6, 7, 8, 9, 10, 11, 21)]
colnames(data_22a)[1] <- 'gps_lon'
colnames(data_22a)[2] <- 'gps_lat'
colnames(data_22a)[3] <- 'treatment'
colnames(data_22a)[4] <- 'nfert_kgha'
colnames(data_22a)[5] <- 'pfert_kgha'
colnames(data_22a)[6] <- 'kfert_kgha'
colnames(data_22a)[7] <- 'lime_kgha'
colnames(data_22a)[8] <- 'planting_date'
colnames(data_22a)[9] <- 'harvest_date'

# format dates  
data_22a$planting_date <- as.Date(data_22a$planting_date, format='%d/%m/%Y')
data_22a$planting_doy <- yday(data_22a$planting_date)
data_22a$harvest_date <- as.Date(data_22a$harvest_date, format='%d/%m/%Y')
data_22a$harvest_doy <- yday(data_22a$harvest_date)

# add season  
data_22a$season <- '2022A'

# ------------------------------------------------------------------------------
# potato fertiliser data 22a
data_22b <- read_csv('./data/raw/rwasis-potato/SAnDMan_2022-11-03_Measure_Potato_PO.csv')
data_22b <- subset(data_22b, projectCode == 'RS')
data_22b <- subset(data_22b, !is.na(tubersFW))
data_22b <- data_22b[,colSums(is.na(data_22b))==0] # remove columns with NA only

# calculate potato yield
data_22b$plotWidth <- ifelse(data_22b$plotWidth > 10, data_22b$plotWidth/10, data_22b$plotWidth)
data_22b$plotWidth <- ifelse(data_22b$plotWidth == 0, mean(data_22b$plotWidth), data_22b$plotWidth)
data_22b$plotLength <- ifelse(data_22b$plotLength == 0, mean(data_22b$plotLength), data_22b$plotLength)
data_22b$plotArea_m2 <- data_22b$plotWidth * data_22b$plotLength
data_22b$yield_kgha <- 10000 * data_22b$tubersFW / data_22b$plotArea_m2
data_22b$yield_tha <- data_22b$yield_kgha/1000

# get columns of interest
data_22b <- data_22b[c(13, 14, 32, 44)]
colnames(data_22b)[1] <- 'gps_lat'  
colnames(data_22b)[2] <- 'gps_lon'  
colnames(data_22b)[3] <- 'treatment'  

# clean treatment variable
# (1) remove string
data_22b$treatment <- gsub("_[^_]+$", "", data_22b$treatment)
# (2) nutrient response
npk_treatments <- c("Control", "PK", "NP", "NK", "NPK_30N", "NPK_60N", "NPK_90N", "NPK_30P", "NPK_40P", "NPK_50P", "NPK_30K", "NPK_60K", "NPK_80K", "NPK17x3_repA", "NPK17x3_repB", "NPK_increased")
data_22b_npk <- subset(data_22b, treatment %in% npk_treatments)
# (3) rename treatments
data_22b_npk$treatment <- ifelse(data_22b_npk$treatment == 'NPK_increased', 'Increased NPK', data_22b_npk$treatment)
data_22b_npk$treatment <- ifelse(data_22b_npk$treatment == 'NPK_30N', 'NPK (30N)', data_22b_npk$treatment)
data_22b_npk$treatment <- ifelse(data_22b_npk$treatment == 'NPK_60N', 'NPK (60N)', data_22b_npk$treatment)
data_22b_npk$treatment <- ifelse(data_22b_npk$treatment == 'NPK_90N', 'NPK (90N)', data_22b_npk$treatment)
data_22b_npk$treatment <- ifelse(data_22b_npk$treatment == 'NPK_30P', 'NPK (30P)', data_22b_npk$treatment)
data_22b_npk$treatment <- ifelse(data_22b_npk$treatment == 'NPK_40P', 'NPK (40P)', data_22b_npk$treatment)
data_22b_npk$treatment <- ifelse(data_22b_npk$treatment == 'NPK_50P', 'NPK (50P)', data_22b_npk$treatment)
data_22b_npk$treatment <- ifelse(data_22b_npk$treatment == 'NPK_30K', 'NPK (30K)', data_22b_npk$treatment)
data_22b_npk$treatment <- ifelse(data_22b_npk$treatment == 'NPK_60K', 'NPK (60K)', data_22b_npk$treatment)
data_22b_npk$treatment <- ifelse(data_22b_npk$treatment == 'NPK_80K', 'NPK (80K)', data_22b_npk$treatment)
data_22b_npk$treatment <- ifelse(data_22b_npk$treatment == 'NPK17x3_repA', 'NPK 17*3_repA', data_22b_npk$treatment)
data_22b_npk$treatment <- ifelse(data_22b_npk$treatment == 'NPK17x3_repB', 'NPK 17*3_repB', data_22b_npk$treatment)

# add columns missing in 22b dataset, assuming the same rates in both seasons
rates_22a <- unique(data_22a[c('treatment', 'nfert_kgha', 'pfert_kgha', 'kfert_kgha', 'lime_kgha')])
# ADD ROWS WITH MISSING TREATMENTS MANUALLY AS MISSING IN 22A data
rates_22a <- rbind(rates_22a, c('NPK 17*3_repA', 51.00, 22, 42, 'N/A'))
rates_22a <- rbind(rates_22a, c('NPK 17*3_repB', 51.00, 22, 42, 'N/A'))
data_22b_npk <- merge(data_22b_npk, rates_22a, by='treatment')
data_22b_npk$planting_date <- NA  # not available in raw data
data_22b_npk$planting_doy <- NA  # not available in raw data
data_22b_npk$harvest_date <- NA  # not available in raw data
data_22b_npk$harvest_doy <- NA  # not available in raw data

# add season  
data_22b_npk$season <- '2022B'

# ignore lime data for now
# lime_treatments <- c("T1_Control", "T2A_NP_L1_repA", "T2B_NP_L1_repB", "T3_NP_L2", "T4_NP_L3", "T5_NP_L45", "T6_NP_L55", "T7_NP_L7")
# data_22b_lime <- subset(data_22b, treatment %in% lime_treatments)

# ------------------------------------------------------------------------------
# bind two season data
data_potato <- rbind(data_22a, data_22b_npk)

# get farmID based on unique coordinates
gps <- unique(data_potato[c(1, 2, 13)])
gps$farm_id <- row.names(gps)
data_potato <- merge(data_potato, gps, by=c("gps_lon", "gps_lat", 'season'))

# add admin zones from GADM
rwa <- geodata::gadm(country = "RWA", level = 3, path='./data/raw/')
rwa_gadm <- raster::extract(rwa, data_potato[, c('gps_lon', 'gps_lat')]) 
data_potato$country_gadm <- rwa_gadm$COUNTRY 
data_potato$province_gadm <- rwa_gadm$NAME_1 
data_potato$district_gadm <- rwa_gadm$NAME_2 

# save two season data
write.csv(data_potato, './data/processed/rwasis-potato-fertiliser-all-data.csv', row.names=F)

# ------------------------------------------------------------------------------
# some visualizations

# interactive map
map <- data_potato[!is.na(data_potato$gps_lon),]
map <- map %>%
  st_as_sf(coords = c("gps_lon", "gps_lat")) %>%
  st_set_crs(4326)
(p_locations <- mapview::mapview(map["season"], legend=T, col.regions=c('orange', 'blue'), layer.name="season", alpha=0.5))

# mean per treatment x season
mean_yield <- aggregate(data_potato$yield_tha, by=list('treatment'=data_potato$treatment, 'season'=data_potato$season), FUN=mean, na.rm=T)
mean_yield <- mean_yield[order(mean_yield$x),]
colnames(mean_yield)[3] <- 'yield_tha'
write.csv(mean_yield, './data/processed/rwasis-potato-fertiliser-av-yield.csv', row.names=F)

# yield variability
data_22a$treatment_fact <- factor(data_22a$treatment, levels=c('Control', 'PK', 'NP', 'NK', 'NPK (30N)', 'NPK (60N)', 'NPK (90N)', 'NPK (30P)', 'NPK (40P)', 'NPK (50P)', 'NPK (30K)', 'NPK (60K)', 'NPK (80K)', 'Increased NPK', 'NPK 17*3'))
data_22b_npk$treatment_fact <- factor(data_22b_npk$treatment, levels=c('Control', 'PK', 'NP', 'NK', 'NPK (30N)', 'NPK (60N)', 'NPK (90N)', 'NPK (30P)', 'NPK (40P)', 'NPK (50P)', 'NPK (30K)', 'NPK (60K)', 'NPK (80K)', 'Increased NPK', 'NPK 17*3_repA', 'NPK 17*3_repB'))
# plot 1
par(mfrow=c(1,2), mar=c(10,5,3,1), xaxs='i', yaxs='i', las=2, cex.axis=1.3, cex.lab=1.4)
boxplot(yield_tha~treatment_fact, data=data_22a, main='Treatment effect 2022A', xlab='', ylab='Potato yield (t/ha)', ylim=c(0,50))
boxplot(yield_tha~treatment_fact, data=data_22b_npk, main='Treatment effect 2022B', xlab='', ylab='Potato yield (t/ha)', ylim=c(0,50))
# plot 2
par(mfrow=c(1,1), mar=c(5,5,3,1), xaxs='i', yaxs='i', las=2, cex.axis=1.3, cex.lab=1.4)
plot(data_22a$planting_doy, data_22a$yield_tha, main='Sowing and harvest dates 2022A', xlab='Planting date (DOY)', ylab='Potato yield (t/ha)', ylim=c(0,50), xlim=c(0, 365))
points(data_22a$planting_doy, data_22a$yield_tha, pch=21, bg='royalblue')
points(data_22a$harvest_doy, data_22a$yield_tha, pch=21, bg='orange')
legend('bottomright', legend=c('sowing', 'harvesting'), fill=c('royalblue', 'orange'))
# plot 3
par(mfrow=c(1,1), mar=c(13,5,3,1), xaxs='i', yaxs='i', las=2, cex.axis=1.3, cex.lab=1.4)
boxplot(yield_tha~season*province_gadm, data=data_potato, main='Province effect', xlab='', ylab='Potato yield (t/ha)', ylim=c(0,50))
# plot 4
par(mfrow=c(1,2), mar=c(10,5,3,1), xaxs='i', yaxs='i', las=2, cex.axis=1.3, cex.lab=1.4)
season_22a <- subset(data_potato, season == '2022A')
season_22b <- subset(data_potato, season == '2022B')
boxplot(yield_tha~district_gadm, data=season_22a, main='District effect 2022A', xlab='', ylab='Potato yield (t/ha)', ylim=c(0,50))
boxplot(yield_tha~district_gadm, data=season_22b, main='District effect 2022B', xlab='', ylab='Potato yield (t/ha)', ylim=c(0,50))
