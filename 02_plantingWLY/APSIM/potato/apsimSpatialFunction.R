#' Title
#'
#' @param my_list_clm 
#' @param wkdir 
#' @param crop 
#' @param clck 
#' @param sd 
#' @param ed 
#' @param variety 
#' @param rep1 
#'
#' @return
#' @export
#'
#' @examples
apsim.spatial <- function(my_list_clm, wkdir, crop, clck, sd, ed, variety, rep1) {
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
  ex.dir <- "D:/potato"
  #ex.dir <- system.file("extdata", package = "apsimx")
  extd.dir <-wkdir
  file.copy(paste0(ex.dir, "/", crop),  extd.dir, overwrite = TRUE)
  
  #list.files(ex.dir)
  list.files(ex.dir)
  
  #Get soil data from iscric
  my_list_sol <- foreach (i = 1:nrow(stn)) %dopar% {
    tryCatch(apsimx::get_isric_soil_profile(lonlat = c(stn$Longitude[i], stn$Latitude[[i]]))
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
  
  my_list_sol <- foreach (i = 1:length(my_list_clm)) %dopar% {
    tryCatch(apsimx::get_isric_soil_profile(lonlat = c(stn$Longitude[[i]], stn$Latitude[[i]]))
             , error=function(err) NA)
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
                        node = "Report",
                        parm = "VariableNames", 
                        value = rep1, 
                        verbose = TRUE, overwrite = TRUE)
  }
  
  # Run the simulation for the entire study area    
  my_list_sim<- foreach (i =1:length(my_list_sol)) %dopar% {  
    setwd(paste0(extd.dir, '/', i))  
    tryCatch(apsimx::apsimx(crop, value = "HarvestReport"), error=function(err) NA)
  }
  return(my_list_sim)
}