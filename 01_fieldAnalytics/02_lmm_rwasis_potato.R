
# package names
packages <- c("readxl", "tidyverse", "reshape2", "mapview", "lubridate", "sf", 
              "geodata", "lmerTest", "lme4", "predictmeans", "topmodel")

# install packages
installed_packages <- packages %in% rownames(installed.packages())
if(any(installed_packages == FALSE)){
  install.packages(packages[!installed_packages])}

# load packages
invisible(lapply(packages, library, character.only = TRUE))

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# potato fertiliser data 22a
data_22a <- read_excel('./RwaSIS data template_Potato_2022A_RBJ_24Oct2022_FR.xlsx', sheet='RS-PFR-1')

# ------------------------------------------------------------------------------
# to get farmID based on unique coordinates
gps <- unique(data_22a[c(3,4)])
gps$farm_id <- row.names(gps)
data_22a <- merge(data_22a, gps, by=c("GPS-lon", "GPS-lat"))

# ------------------------------------------------------------------------------
# calculate potato yield
colnames(data_22a)[12] <- 'plot_area_m2'
colnames(data_22a)[13] <- 'harvest_kgplot'
data_22a$yield_kgha <- 10000 * data_22a$harvest_kgplot / data_22a$plot_area_m2
data_22a$yield_tha <- data_22a$yield_kgha/1000

# ------------------------------------------------------------------------------
# get columns of interest
data_22a <- data_22a[c(20, 1, 2, 5, 6, 7, 8, 9, 10, 11, 22)]
colnames(data_22a)[2] <- 'gps_lon'
colnames(data_22a)[3] <- 'gps_lat'
colnames(data_22a)[4] <- 'treatment'
colnames(data_22a)[5] <- 'nfert_kgha'
colnames(data_22a)[6] <- 'pfert_kgha'
colnames(data_22a)[7] <- 'kfert_kgha'
colnames(data_22a)[8] <- 'lime_kgha'
colnames(data_22a)[9] <- 'planting_date'
colnames(data_22a)[10] <- 'harvest_date'

# ------------------------------------------------------------------------------
# interactive map
map <- data_22a[!is.na(data_22a$gps_lon),]
map <- map %>%
  st_as_sf(coords = c("gps_lon", "gps_lat")) %>%
  st_set_crs(4326)
(p_locations <- mapview::mapview(map, legend = FALSE, alpha=1))

# ------------------------------------------------------------------------------
# get admin zones from GADM
rwa <- geodata::gadm(country = "RWA", level = 3, path='.')
rwa_gadm <- raster::extract(rwa, data_22a[, c('gps_lon', 'gps_lat')]) 
data_22a$country_gadm <- rwa_gadm$COUNTRY 
data_22a$province_gadm <- rwa_gadm$NAME_1 
data_22a$district_gadm <- rwa_gadm$NAME_2 

# ------------------------------------------------------------------------------
# date format 
data_22a$planting_date <- as.Date(data_22a$planting_date, format='%d-%m-%Y')
data_22a$planting_doy <- yday(data_22a$planting_date)
data_22a$harvest_date <- as.Date(data_22a$harvest_date, format='%d-%m-%Y')
data_22a$harvest_doy <- yday(data_22a$harvest_date)

# ------------------------------------------------------------------------------
# get mean yield per treatment
mean_yield_22a <- aggregate(data_22a[c(11)], by=list('treatment'=data_22a$treatment), FUN=mean)
mean_yield_22a <- mean_yield_22a[order(mean_yield_22a$yield_tha),]
data_22a$treatment <- factor(data_22a$treatment, levels=unique(mean_yield_22a$treatment))

# ------------------------------------------------------------------------------
# descriptive analysis of yield variability

par(mfrow=c(1,1), mar=c(10,5,3,1), xaxs='i', yaxs='i', las=2, cex.axis=1.3, cex.lab=1.4)
boxplot(yield_tha~treatment, data=data_22a, main='Treatment effect', xlab='', ylab='Potato yield (t/ha)', ylim=c(0,50))

par(mfrow=c(1,1), mar=c(10,5,3,1), xaxs='i', yaxs='i', las=2, cex.axis=1.3, cex.lab=1.4)
data_22a_subset <- subset(data_22a, treatment == 'Increased NPK')
plot(data_22a_subset$planting_doy, data_22a_subset$yield_tha, main='Sowing and harvest dates (problems!)', xlab='Planting date (DOY)', ylab='Potato yield (t/ha)', ylim=c(0,50), xlim=c(0, 365))
points(data_22a_subset$planting_doy, data_22a_subset$yield_tha, pch=21, bg='royalblue')
points(data_22a_subset$harvest_doy, data_22a_subset$yield_tha, pch=21, bg='orange')
legend('bottomright', legend=c('sowing', 'harvesting'), fill=c('royalblue', 'orange'))

par(mfrow=c(1,1), mar=c(10,5,3,1), xaxs='i', yaxs='i', las=2, cex.axis=1.3, cex.lab=1.4)
boxplot(yield_tha~province_gadm, data=data_22a, main='Province effect', xlab='', ylab='Potato yield (t/ha)', ylim=c(0,50))

par(mfrow=c(1,1), mar=c(10,5,3,1), xaxs='i', yaxs='i', las=2, cex.axis=1.3, cex.lab=1.4)
boxplot(yield_tha~district_gadm, data=data_22a, main='District effect', xlab='', ylab='Potato yield (t/ha)', ylim=c(0,50))

# ------------------------------------------------------------------------------
# linear mixed model 22A

# (1) set factors
data_22a$province_gadm <- as.factor(data_22a$province_gadm)
data_22a$district_gadm <- as.factor(data_22a$district_gadm)
data_22a$farm_id <- as.factor(data_22a$farm_id)

# (2) fit mixed model
model <- lmer(yield_tha ~ treatment + 
                (1|province_gadm/district_gadm/farm_id), 
              data=data_22a, 
              REML=T, na.action=na.exclude)
summary(model)

# (3) get anova table
anova_table <- as.data.frame(anova(model)) 
anova_table$variable <- row.names(anova_table)
row.names(anova_table) <- NULL
print(anova_table)

# (4) get residuals
data_22a$residuals <- resid(model)
hist(data_22a$residuals)

# (5) assess model performance
data_22a$pred_y_all = predict(model, na.action=na.exclude) 
data_22a$pred_y_fe  = predict(model, re.form = NA, na.action=na.exclude) 
perf <- data.frame(R2 = round(cor(data_22a$pred_y_all, data_22a$yield_tha, use="complete.obs"), 3),
                   RMSE = round(100*sqrt(mean((data_22a$yield_tha-data_22a$pred_y_all)^2, na.rm=T)) / mean(data_22a$yield_tha, na.rm=T), 1),
                   ME = round(NSeff(data_22a$yield_tha, data_22a$pred_y_all), 2))
plot(data_22a$yield_tha, data_22a$pred_y_all)
abline(a=0, b=1, col=2)
print(perf)

# (6) get variance components
var_comp <- as.data.frame(lme4::VarCorr(model))
var_comp <- var_comp[c(1,4,5)]
colnames(var_comp)[1] <- 'treatment'
colnames(var_comp)[2] <- 'Estimate'
colnames(var_comp)[3] <- 'Std. Error'
print(var_comp)

# (7) get fixed effects
fixed_eff <- as.data.frame(lme4::fixef(model))
fixed_eff$treatment <- row.names(fixed_eff)
row.names(fixed_eff) <- NULL
print(fixed_eff)

# (8) get random effects (blups)
blups <- data.frame(coef(model)$`farm_id:(district_gadm:province_gadm)`)
blups$farm <- row.names(blups)
row.names(blups) <- NULL
blups$control <- blups$X.Intercept.
blups$PK <- blups$control + blups$treatmentPK
blups$NK <- blups$control + blups$treatmentNK
blups$NP <- blups$control + blups$treatmentNP
blups$NPK_30N <- blups$control + blups$treatmentNPK..30N.
blups$NPK_30K <- blups$control + blups$treatmentNPK..30K.
blups$NPK_17_3 <- blups$control + blups$treatmentNPK.17.3
blups$NPK_80K <- blups$control + blups$treatmentNPK..80K.
blups$NPK_60K <- blups$control + blups$treatmentNPK..60K.
blups$NPK_30P <- blups$control + blups$treatmentNPK..30P.
blups$NPK_60N <- blups$control + blups$treatmentNPK..60N.
blups$NPK_50P <- blups$control + blups$treatmentNPK..50P.
blups$NPK_40P <- blups$control + blups$treatmentNPK..40P.
blups$NPK_90N <- blups$control + blups$treatmentNPK..90N.
blups$increased_NPK <- blups$control + blups$treatmentIncreased.NPK
blups <- blups[c(16:31)]
blups <- melt(blups, id.vars = 'farm')
par(mfrow=c(1,2), mar=c(10,5,3,1), xaxs='i', yaxs='i', las=2, cex.axis=1.3, cex.lab=1.4)
boxplot(yield_tha~treatment, data=data_22a, main='Raw data', xlab='', ylab='Potato yield (t/ha)', ylim=c(0,50))
boxplot(value~variable, data=blups, main='BLUPS for each Farm ID', xlab='', ylab='Potato yield (t/ha)', ylim=c(0,50))
dev.off()

# (9) predict means: treatment
predmeans <- predictmeans::predictmeans(model, "treatment", pairwise=T, adj="tukey", mplot=F, pplot=F)
pool_means <- data.frame(predmeans["Predicted Means"])
pool_sterror <- data.frame(predmeans["Standard Error of Means"])
pool_pvalue  <- predmeans["Pairwise p-value"]
pool_pairwise <- as.data.frame(pool_pvalue$`Pairwise p-value`)
pool_pairwise <- tibble::rownames_to_column(pool_pairwise, "treatment")
pool_group <- attr(pool_pvalue[[1]], "Letter-based representation of pairwise comparisons at significant level '0.05'")
row.names(pool_group) <- NULL
colnames(pool_group)[1] <- 'treatment'
colnames(pool_group)[2] <- 'predmean'
colnames(pool_group)[3] <- 'group'
print(pool_group)

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# potato fertiliser data 22a
data_22b <- read_csv('./SAnDMan_2022-11-03_Measure_Potato_PO.csv')
data_22b <- subset(data_22b, projectCode == 'RS')
data_22b <- subset(data_22b, !is.na(tubersFW))
data_22b <- data_22b[,colSums(is.na(data_22b))==0] # remove columns with NA only

# ------------------------------------------------------------------------------
# to get farmID based on unique coordinates
gps <- unique(data_22b[c(17,18)])
gps$farm_id <- row.names(gps)
data_22b <- merge(data_22b, gps, by=c("lon", "lat"))

# ------------------------------------------------------------------------------
# calculate potato yield
data_22b$plotWidth <- ifelse(data_22b$plotWidth > 10, data_22b$plotWidth/10, data_22b$plotWidth)
data_22b$plotWidth <- ifelse(data_22b$plotWidth == 0, mean(data_22b$plotWidth), data_22b$plotWidth)
data_22b$plotLength <- ifelse(data_22b$plotLength == 0, mean(data_22b$plotLength), data_22b$plotLength)
data_22b$plotArea_m2 <- data_22b$plotWidth * data_22b$plotLength
data_22b$yield_kgha <- 10000 * data_22b$tubersFW / data_22b$plotArea_m2
data_22b$yield_tha <- data_22b$yield_kgha/1000

# ------------------------------------------------------------------------------
# get columns of interest
data_22b <- data_22b[c(1, 2, 32, 42, 45)]
colnames(data_22b)[1] <- 'gps_lon'  
colnames(data_22b)[2] <- 'gps_lat'  
colnames(data_22b)[3] <- 'treatment'  
# missing in 22b dataset
# colnames(data_22a)[5] <- 'nfert_kgha'
# colnames(data_22a)[6] <- 'pfert_kgha'
# colnames(data_22a)[7] <- 'kfert_kgha'
# colnames(data_22a)[8] <- 'lime_kgha'
# colnames(data_22a)[9] <- 'planting_date'
# colnames(data_22a)[10] <- 'harvest_date'

# ------------------------------------------------------------------------------
# clean treatment variable

#  remove string
data_22b$treatment <- gsub("_[^_]+$", "", data_22b$treatment)
unique(data_22b$treatment)

# nutrient response
npk_treatments <- c("Control", "PK", "NP", "NK", 
                    "NPK_30N", "NPK_60N", "NPK_90N", 
                    "NPK_30P", "NPK_40P", "NPK_50P", 
                    "NPK_30K", "NPK_60K", "NPK_80K",   
                    "NPK17x3_repA", "NPK17x3_repB", "NPK_increased")
data_22b_npk <- subset(data_22b, treatment %in% npk_treatments)
unique(data_22b_npk$treatment)

# rename treatments
data_22b_npk$treatment <- ifelse(data_22b_npk$treatment == 'NPK_increased', 'increased NPK', data_22b_npk$treatment)
data_22b_npk$treatment <- ifelse(data_22b_npk$treatment == 'NPK_30N', 'NPK (30N)', data_22b_npk$treatment)
data_22b_npk$treatment <- ifelse(data_22b_npk$treatment == 'NPK_60N', 'NPK (60N)', data_22b_npk$treatment)
data_22b_npk$treatment <- ifelse(data_22b_npk$treatment == 'NPK_90N', 'NPK (90N)', data_22b_npk$treatment)
data_22b_npk$treatment <- ifelse(data_22b_npk$treatment == 'NPK_30P', 'NPK (30P)', data_22b_npk$treatment)
data_22b_npk$treatment <- ifelse(data_22b_npk$treatment == 'NPK_40P', 'NPK (40P)', data_22b_npk$treatment)
data_22b_npk$treatment <- ifelse(data_22b_npk$treatment == 'NPK_50P', 'NPK (50P)', data_22b_npk$treatment)
data_22b_npk$treatment <- ifelse(data_22b_npk$treatment == 'NPK_30K', 'NPK (30K)', data_22b_npk$treatment)
data_22b_npk$treatment <- ifelse(data_22b_npk$treatment == 'NPK_60K', 'NPK (60K)', data_22b_npk$treatment)
data_22b_npk$treatment <- ifelse(data_22b_npk$treatment == 'NPK_80K', 'NPK (80K)', data_22b_npk$treatment)


# lime response (ignore from analysis for now)
lime_treatments <- c("T1_Control", "T2A_NP_L1_repA", "T2B_NP_L1_repB", 
                     "T3_NP_L2", "T4_NP_L3", "T5_NP_L45", "T6_NP_L55", "T7_NP_L7")
data_22b_lime <- subset(data_22b, treatment %in% lime_treatments)
unique(data_22b_lime$treatment)

# ------------------------------------------------------------------------------
# interactive map
map <- data_22b_npk[!is.na(data_22b_npk$gps_lon),]
map <- map %>%
  st_as_sf(coords = c("gps_lon", "gps_lat")) %>%
  st_set_crs(4326)
(p_locations <- mapview::mapview(map, legend = FALSE, alpha=1))

# ------------------------------------------------------------------------------
# get admin zones from GADM
rwa <- geodata::gadm(country = "RWA", level = 3, path='.')
rwa_gadm <- raster::extract(rwa, data_22b_npk[, c('gps_lon', 'gps_lat')]) 
data_22b_npk$country_gadm <- rwa_gadm$COUNTRY 
data_22b_npk$province_gadm <- rwa_gadm$NAME_1 
data_22b_npk$district_gadm <- rwa_gadm$NAME_2 

# ------------------------------------------------------------------------------
# final dataframe
write.csv(data_22b_npk, './rwanda_potato_data_22b.csv')

# ------------------------------------------------------------------------------
# date format 
# missing in 22b dataset
# data_22a$planting_date <- as.Date(data_22a$planting_date, format='%d-%m-%Y')
# data_22a$planting_doy <- yday(data_22a$planting_date)
# data_22a$harvest_date <- as.Date(data_22a$harvest_date, format='%d-%m-%Y')
# data_22a$harvest_doy <- yday(data_22a$harvest_date)

# ------------------------------------------------------------------------------
# get mean yield per treatment
mean_yield_22b <- aggregate(data_22b_npk[c(5)], by=list('treatment'=data_22b_npk$treatment), FUN=mean)
mean_yield_22b <- mean_yield_22b[order(mean_yield_22b$yield_tha),]
data_22b_npk$treatment <- factor(data_22b_npk$treatment, levels=unique(mean_yield_22b$treatment))

# ------------------------------------------------------------------------------
# descriptive analysis of yield variability

par(mfrow=c(1,1), mar=c(10,5,3,1), xaxs='i', yaxs='i', las=2, cex.axis=1.3, cex.lab=1.4)
boxplot(yield_tha~treatment, data=data_22b_npk, main='Treatment effect', xlab='', ylab='Potato yield (t/ha)', ylim=c(0,50))

# par(mfrow=c(1,1), mar=c(10,5,3,1), xaxs='i', yaxs='i', las=2, cex.axis=1.3, cex.lab=1.4)
# data_22b_subset <- subset(data_22b_npk, treatment == 'Increased NPK')
# plot(data_22b_subset$planting_doy, data_22b_subset$yield_tha, main='Sowing and harvest dates (problems!)', xlab='Planting date (DOY)', ylab='Potato yield (t/ha)', ylim=c(0,50), xlim=c(0, 365))
# points(data_22b_subset$planting_doy, data_22b_subset$yield_tha, pch=21, bg='royalblue')
# points(data_22b_subset$harvest_doy, data_22b_subset$yield_tha, pch=21, bg='orange')
# legend('bottomright', legend=c('sowing', 'harvesting'), fill=c('royalblue', 'orange'))

par(mfrow=c(1,1), mar=c(10,5,3,1), xaxs='i', yaxs='i', las=2, cex.axis=1.3, cex.lab=1.4)
boxplot(yield_tha~province_gadm, data=data_22b_npk, main='Province effect', xlab='', ylab='Potato yield (t/ha)', ylim=c(0,50))

par(mfrow=c(1,1), mar=c(10,5,3,1), xaxs='i', yaxs='i', las=2, cex.axis=1.3, cex.lab=1.4)
boxplot(yield_tha~district_gadm, data=data_22b_npk, main='District effect', xlab='', ylab='Potato yield (t/ha)', ylim=c(0,50))
