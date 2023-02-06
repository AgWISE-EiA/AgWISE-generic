
# package names
packages <- c("reshape2", "lmerTest", "lme4", "predictmeans", "topmodel")

# install packages
installed_packages <- packages %in% rownames(installed.packages())
if(any(installed_packages == FALSE)){
  install.packages(packages[!installed_packages])}

# load packages
invisible(lapply(packages, library, character.only = TRUE))

# ------------------------------------------------------------------------------
# rwasis potato data
data <- read.csv('./data/processed/rwasis-potato-fertiliser-all-data.csv')

# set factors
data$province_gadm <- as.factor(data$province_gadm)
data$district_gadm <- as.factor(data$district_gadm)
data$farm_id <- as.factor(data$farm_id)
data$season <- as.factor(data$season)
data$treatment <- as.factor(data$treatment)

# ------------------------------------------------------------------------------
# (1) random-intercept model 
fit0 <- lmer(yield_tha ~ treatment + (1|farm_id), data=data, REML=T, na.action=na.exclude)
# summary
summary(fit0)
# anova table
anova(fit0)  
# residuals
hist(resid(fit0))
# performance
data$pred_y_all = predict(fit0, na.action=na.exclude) 
data$pred_y_fe  = predict(fit0, re.form = NA, na.action=na.exclude) 
perf <- data.frame(R2 = round(cor(data$pred_y_all, data$yield_tha, use="complete.obs"), 3),
                   RMSE = round(100*sqrt(mean((data$yield_tha-data$pred_y_all)^2, na.rm=T)) / mean(data$yield_tha, na.rm=T), 1),
                   ME = round(NSeff(data$yield_tha, data$pred_y_all), 2))
plot(data$yield_tha, data$pred_y_all)
abline(a=0, b=1, col=2)
# random effects (BLUPS)
blups <- data.frame(coef(fit0)$`farm_id`)
blups$farm_id <- row.names(blups)
row.names(blups) <- NULL
blups$Control <- blups$X.Intercept.
blups$PK <- blups$Control + blups$treatmentPK
blups$NK <- blups$Control + blups$treatmentNK
blups$NP <- blups$Control + blups$treatmentNP
blups['NPK (30N)'] <- blups$Control + blups$treatmentNPK..30N.
blups['NPK (30K)'] <- blups$Control + blups$treatmentNPK..30K.
blups['NPK (80K)'] <- blups$Control + blups$treatmentNPK..80K.
blups['NPK (60K)'] <- blups$Control + blups$treatmentNPK..60K.
blups['NPK (30P)'] <- blups$Control + blups$treatmentNPK..30P.
blups['NPK (60N)'] <- blups$Control + blups$treatmentNPK..60N.
blups['NPK (50P)'] <- blups$Control + blups$treatmentNPK..50P.
blups['NPK (40P)'] <- blups$Control + blups$treatmentNPK..40P.
blups['NPK (90N)'] <- blups$Control + blups$treatmentNPK..90N.
blups['Increased NPK'] <- blups$Control + blups$treatmentIncreased.NPK
blups['NPK 17*3'] <- blups$Control + blups$treatmentNPK.17.3
blups['NPK 17*3_repA'] <- blups$Control + blups$treatmentNPK.17.3_repA
blups['NPK 17*3_repB'] <- blups$Control + blups$treatmentNPK.17.3_repB
blups <- blups[c(18:35)]
blups <- melt(blups, id.vars = 'farm_id')
colnames(blups)[2] <- 'treatment'
colnames(blups)[3] <- 'blups_tha'
ids <- data[c("country_gadm", "province_gadm", "district_gadm", "farm_id", "treatment", "gps_lon", "gps_lat", "nfert_kgha", "pfert_kgha", "kfert_kgha")]
blups <- merge(ids, blups, by=c('farm_id', "treatment"))
write.csv(blups, './data/processed/rwasis-potato-fertiliser-blups-fit0.csv', row.names=F)
# plot blups
par(mfrow=c(1,2), mar=c(10,5,3,1), xaxs='i', yaxs='i', las=2, cex.axis=1.3, cex.lab=1.4)
data$treatment_fact <- factor(data$treatment, levels=c('Control', 'PK', 'NP', 'NK', 'NPK (30N)', 'NPK (60N)', 'NPK (90N)', 'NPK (30P)', 'NPK (40P)', 'NPK (50P)', 'NPK (30K)', 'NPK (60K)', 'NPK (80K)', 'Increased NPK', 'NPK 17*3', 'NPK 17*3_repA', 'NPK 17*3_repB'))
blups$treatment_fact <- factor(blups$treatment, levels=c('Control', 'PK', 'NP', 'NK', 'NPK (30N)', 'NPK (60N)', 'NPK (90N)', 'NPK (30P)', 'NPK (40P)', 'NPK (50P)', 'NPK (30K)', 'NPK (60K)', 'NPK (80K)', 'Increased NPK', 'NPK 17*3', 'NPK 17*3_repA', 'NPK 17*3_repB'))
boxplot(yield_tha~treatment_fact, data=data, main='Raw data', xlab='', ylab='Potato yield (t/ha)', ylim=c(0,50))
boxplot(blups_tha~treatment_fact, data=blups, main='BLUPS for each Farm ID', xlab='', ylab='Potato yield (t/ha)', ylim=c(0,50))

# ------------------------------------------------------------------------------
# (2) random-slope model 
fit1 <- lmer(yield_tha ~ treatment * season + (1|farm_id), data=data, REML=T, na.action=na.exclude)
# summary
summary(fit1)
# anova table
anova(fit1)  
# residuals
hist(resid(fit1))
# performance
data$pred_y_all = predict(fit1, na.action=na.exclude) 
data$pred_y_fe  = predict(fit1, re.form = NA, na.action=na.exclude) 
perf <- data.frame(R2 = round(cor(data$pred_y_all, data$yield_tha, use="complete.obs"), 3),
                   RMSE = round(100*sqrt(mean((data$yield_tha-data$pred_y_all)^2, na.rm=T)) / mean(data$yield_tha, na.rm=T), 1),
                   ME = round(NSeff(data$yield_tha, data$pred_y_all), 2))
plot(data$yield_tha, data$pred_y_all)
abline(a=0, b=1, col=2)
# random effects (BLUPS)
blups <- data.frame(coef(fit1)$`farm_id`)
blups$farm_id <- row.names(blups)
row.names(blups) <- NULL
write.csv(blups, './data/processed/rwasis-potato-fertiliser-blups-fit1.csv', row.names=F)
