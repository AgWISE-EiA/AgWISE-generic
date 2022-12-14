#' Title 
#'
#' @param wkdir 
#' @param cell 
#' @param b 
#' @param date 
#' @param crop 
#' @param clck 
#' @param sd 
#' @param ed 
#' @param variety 
#' @param fert 
#' @param rep1 
#' @param rep2 
#'
#' @return
#' @export
#'
#' @examples
apsim.spatial <- function(my_list_clm, wkdir, crop, clck, sd, ed, variety, fert, rep1, rep2) {
  my_packages <- c("spdep", "rgdal", "maptools", "raster", "plyr", "ggplot2", "rgdal",
                   "dplyr", "cowplot","readxl", "apsimx", "gtools", "foreach","doParallel",
                   "ranger")
  list.of.packages <- my_packages
  new.packages <- list.of.packages[!(my_packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  lapply(my_packages, require, character.only = TRUE)
  
  cores<- detectCores()
  myCluster <- makeCluster(cores -2, # number of cores to use
                           type = "PSOCK") # type of cluster
  registerDoParallel(myCluster)
  
  #Change depending on where you crop.apsimx file is stored, this script used the examples#
  setwd(wkdir)
  ex.dir <- auto_detect_apsimx_examples()
  #ex.dir <- system.file("extdata", package = "apsimx")
  extd.dir <-wkdir
  file.copy(paste0(ex.dir, "/", crop),  extd.dir, overwrite = TRUE)
  
  list.files(ex.dir)
  list.files(extd.dir)
  
   ##Get soil data from iscric
  my_list_sol <- foreach (i = 1:(nrow(stn)-1)) %dopar% {
    tryCatch(apsimx::get_isric_soil_profile(lonlat = c(stn$Longitude[[i]], stn$Latitude[[i]]))
             , error=function(err) NA)
  }
  
  ##APSIM PART##
  #Write the weather files to a working directory and Edit the weather as per location
  foreach (i =1:length(my_list_clm)) %dopar% {
    apsimx::write_apsim_met(my_list_clm[[i]], wrt.dir = extd.dir, filename = paste0('wth_loc_',i,'.met'))}
  
  foreach (i =1:length(my_list_clm)) %dopar% {
    dir.create(paste0(extd.dir, '/', i))
    apsimx::edit_apsimx(paste0(crop), 
                        src.dir = extd.dir,
                        wrt.dir = paste0(extd.dir, '/', i),
                        node = "Weather", 
                        value = paste0(extd.dir, "/", 'wth_loc_',i,'.met'), overwrite = TRUE)
  }
  # Edit the soil depending on location there is an issue with soil where one may be required to edit the BD, SAT
  foreach (i =1:length(my_list_sol)) %do% {  
    setwd(paste0(extd.dir, '/', i))
    #my_list_sol[[i]]$soil$BD <-  my_list_sol[[i]]$soil$BD * 0.86
    #my_list_sol[[i]]$soil$crop.LL <-  my_list_sol[[i]]$soil$LL15 + 0.01
    #my_list_sol[[i]]$soil$SAT <-  c(0.521, 0.521, 0.497, 0.488, 0.478, 0.440)
    #edit_apsimx_replace_soil_profile(crop, soil.profile = my_list_sol[[i]], overwrite = TRUE)
    tryCatch(edit_apsimx_replace_soil_profile(crop, soil.profile = my_list_sol[[i]], overwrite = TRUE), error=function(err) NA)
  }
  
  #Edit clock#
  foreach (i =1:length(my_list_sol)) %dopar% {  
    setwd(paste0(extd.dir, '/', i))
    apsimx::edit_apsimx(crop, 
                        node = "Clock",
                        parm = c("Start", "End"),
                        value = clck,
                        overwrite = TRUE)
  }
  # Change the sowing rule for when rain is available
  foreach (i =1:length(my_list_sol)) %dopar% {  
    setwd(paste0(extd.dir, '/', i))
    apsimx::edit_apsimx(crop, 
                        node = "Manager",
                        manager.child = "SowingRule",
                        parm = "StartDate", ## This is for start date
                        value = sd,
                        overwrite = TRUE)
    
    apsimx::edit_apsimx(crop, 
                        node = "Manager",
                        manager.child = "SowingRule",
                        parm = "EndDate", ## This is for end date
                        value = ed,
                        overwrite = TRUE)
    apsimx::edit_apsimx(crop, 
                        node = "Manager",
                        manager.child = "SowingRule",
                        parm = "CultivarName", ## This is for end date
                        value = variety,
                        overwrite = TRUE)
    apsimx::edit_apsimx(crop,
                        node = "Manager",
                        manager.child = "SowingFertiliser",
                        parm = "Amount", 
                        value = fert, 
                        overwrite = TRUE)
    apsimx::edit_apsimx(crop,
                        node = "Report",
                        parm = "VariableNames", 
                        value = rep1, 
                        verbose = TRUE, overwrite = TRUE)
    apsimx::edit_apsimx(crop,
                        node = "Report",
                        parm = "VariableNames", 
                        value = rep2, 
                        verbose = TRUE, overwrite = TRUE)
  }
  
  # Run the simulation for the entire study area  
  my_list_sim<- foreach (i =1:length(my_list_sol)) %dopar% {  
    setwd(paste0(extd.dir, '/', i))  
    tryCatch(apsimx::apsimx(crop, value = "HarvestReport"), error=function(err) NA)
    #apsim.spatial("D:/project", 3, "KE", c("2020-01-01","2022-01-01"), "soybean.apsimx", c("2010-11-01T00:00:00", "2020-12-31T00:00:00"),"1-nov", "30-nov", "Davis")
  }
  return(my_list_sim)
}
