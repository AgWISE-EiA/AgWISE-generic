fix_apsimx_soil_profile <- function(x, soil.var = c("SAT", "BD"), particle.density = 2.65, verbose = TRUE){
  
  if(!inherits(x, "soil_profile"))
    stop("object should be of class 'soil_profile'", call. = FALSE)
  ## Heuristics for fixing soil profiles
  
  soil.var <- match.arg(soil.var)
  
  ## Bulk density and saturation issue
  max_bd <- (1 - x$soil$SAT) * 2.65
  bd_diff <- max_bd - x$soil$BD
  
  for(j in seq_along(max_bd)){
    if(bd_diff[j] <= 0){
      x$soil$SAT[j] <- 1 - x$soil$BD[j] / 2.65 - 0.001
      if(verbose && soil.var == "SAT"){
        cat("Saturation of:", x$soil$SAT[j], "in layer:", j, "was above acceptable value of:", 1 - x$soil$BD[j] / 2.65, ".",
            "It was adjusted to:", 1 - x$soil$BD[j] / 2.65 - 0.001, "\n")              
      }
      ## Fixing LL and air dry issue
      if(x$soil$LL15 < x$soil$AirDry){
        if(verbose){
          cat("LL15 cannot be lower than AirDry in layer:", j,".\n",
              "It was adjusted to the value of AirDry.\n")
        }
        x$soil$LL15[j] <- x$soil$AirDry[j]
      }
    }
  }
  return(x)
}