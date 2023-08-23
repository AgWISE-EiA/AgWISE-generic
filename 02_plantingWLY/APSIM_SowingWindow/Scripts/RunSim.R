# Run the simulation for the entire study area  
#' Title length(my_list_clm)
#'
#' @param my_list_clm 
#' @param extd.dir 
#' @param stn 
#'
#' @return
#' @export
#'
#' @examples
my_list_sim<- function(crop, my_list_clm, extd.dir, stn, my_list_soil){
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
  
  my_list_sims<- foreach (i =1:length(my_list_clm)) %dopar% {  
    setwd(paste0(extd.dir, '/', i))  
    tryCatch(apsimx::apsimx(crop, value = "HarvestReport"), error=function(err) NA)
  }
  
  newlist<- foreach (i =1:length(my_list_clm)) %dopar% { 
    if(is.na(my_list_sims[i]) == TRUE) {
      setwd(paste0(extd.dir, '/', i)) 
      tryCatch({my_list_soil$soil$SAT <-c(0.521, 0.521, 0.497, 0.488, 0.478, 0.440)}, error=function(e) {NA})
      apsimx::edit_apsimx_replace_soil_profile(crop, soil.profile = my_list_soil, overwrite = TRUE) 
      my_list_sims[[i]]<-tryCatch(apsimx::apsimx(crop, value = "HarvestReport"), error=function(err) NA)
    }
    else  my_list_sims[[i]]
  }
  
   return(newlist)
}

