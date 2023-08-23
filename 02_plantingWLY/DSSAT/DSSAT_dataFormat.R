#################################################################################################################
## sourcing required packages 
#################################################################################################################
packages_required <- c("doParallel", "foreach", "DSSAT", "chirps", "apsimx", "tidyverse", "dplyr", "timechange", "lubridate")

# check and install packages that are not yet installed
installed_packages <- packages_required %in% rownames(installed.packages())
if(any(installed_packages == FALSE)){
  install.packages(packages_required[!installed_packages])}

# load required packages
invisible(lapply(packages_required, library, character.only = TRUE))


#################################################################################################################
## the function to format soil and weather data for DSSAT
#################################################################################################################

#' Title
#' 
#' @param jobs number of cores to use for parallel computation 
#' @param sdate is starting date in yyyy-mm-dd format
#' @param edate is end date for simulation in yyyy-mm-dd format
#' @param ex.name folder to write out the soil and weather data in DSSAT format
#' @param path.to.extdata the path where the external data (soil.sol) used as template for formatting is saved
#' @param path.weather.data is the path where the GPS coordinates, temp, solar radiation and rainfall data are saved
#' @param Tmax is a data frame in csv format with n+1 columns with the Date (mm/dd/yyyy) as the first column and Tmax in column for every point  
#' @param Tmin is a data frame in csv format with n+1 columns with the Date (mm/dd/yyyy) as the first column and Tmin in column for every point 
#' @param SRad is a data frame in csv format with n+1 columns with the Date (mm/dd/yyyy) as the first column and solar radiation in column for every point 
#' @param rain.data is a data frame in csv format with n+1 columns with the Date (mm/dd/yyyy) as the first column and rainfall in column for every point 
#' @param lonlat.data is a data frame in csv format with longitude and latitude 
#'
#' @return it writes out the output in ex.name folder
#'
#' @examples dssat.extdata(sdate = '1981-01-01', edate = '2021-12-31', ex.name = 'MC_DSSAT_test', jobs = 8,
#' path.to.extdata = '/home/jovyan/agwise/EiA_Analytics/AgWISE-UseCaseRAB/02_plantingWLY/DSSAT/extdata')
#' 
dssat.extdata(jobs = 8, sdate = '1981-01-01', edate = '2021-12-31', ex.name = 'MC_DSSAT_test', 
              path.to.extdata = '/home/jovyan/agwise/EiA_Analytics/AgWISE-UseCaseRAB/02_plantingWLY/DSSAT/extdata',
              path.weather.data = '/home/jovyan/agwise/EiA_Analytics/AgWISE-UseCaseRAB/02_plantingWLY/DSSAT/inputData',
              Tmax = 'Tmax.data.coordinates_Rwanda.csv',
              Tmin = 'Tmin.data.coordinates_Rwanda.csv',
              SRad = 'S.Rad.data.coordinates_Rwanda.csv',
              rain.data = 'Rainfall.data.coordinates_Rwanda.csv',
              lonlat.data = 'coordinates_Rwanda.csv'
)

dssat.extdata <- function(jobs,sdate,edate,ex.name,path.to.extdata, path.weather.data, Tmax, Tmin, SRad, rain.data, lonlat.data){
  
  
  # Set number of parallel workers
  cls <- parallel::makePSOCKcluster(jobs)
  doParallel::registerDoParallel(cls)
  #Set working directory (where the file is)
  setwd(path.to.extdata)
  
  
  #Bring data together
  Tmaxdata <- read.csv(paste(path.weather.data, Tmax, sep="/"))
  Tmindata <- read.csv(paste(path.weather.data, Tmin, sep="/"))
  Sraddata <- read.csv(paste(path.weather.data, SRad, sep="/"))
  Rainfalldata <- read.csv(paste(path.weather.data, rain.data, sep="/"))
  coords <- read.csv(paste(path.weather.data, lonlat.data, sep="/"))
  grid <- as.matrix(coords)
  
  
  # Create experiment directory
  dir.create(file.path(paste(path.to.extdata, ex.name, sep = "/")))
  
  foreach::foreach(i=seq_along(grid[,1]), .export = '.GlobalEnv', .inorder = TRUE, .packages = c("tidyverse", "lubridate", "apsimx","DSSAT")) %dopar% {
    dir.create(file.path(paste(path.to.extdata,ex.name,paste0('EXTE', formatC(width = 4, (as.integer(i)-1), flag = "0")), sep = "/")))
    setwd(paste(path.to.extdata,ex.name,paste0('EXTE', formatC(width = 4, (as.integer(i)-1), flag = "0")), sep = "/"))
    
    DATE <-Tmaxdata[,1]
    TMAX <-Tmaxdata[,i+1]
    TMIN <-Tmindata[,i+1]
    SRAD <-Sraddata[,i+1]
    RAIN <-Rainfalldata[,i+1]
    
    tst <- data.frame(DATE,TMAX,TMIN,SRAD,RAIN)
    
    x <- grid[i,1]
    y <- grid[i,2]
    ##########################################
    # Get soil ISRIC data
    s <- tryCatch(
      expr = {
        apsimx::get_isric_soil_profile(lonlat = c(x,y))
      },
      error = function(e){
        return(list(soil=data.frame(LL15=as.integer(-99),
                                    DUL=as.integer(-99),
                                    SAT=as.integer(-99),
                                    SKS=as.integer(-99),
                                    SSS=as.integer(-99),
                                    BDM=as.integer(-99),
                                    LOC=as.integer(-99),
                                    LCL=as.integer(-99),
                                    LSI=as.integer(-99),
                                    LNI=as.integer(-99),
                                    LHW=as.integer(-99),
                                    CEC=as.integer(-99))))
      })
    Depth<-c(5,15,30,60,100,200)
    LL15<-s$soil$LL15
    DUL<-s$soil$DUL
    SAT<-s$soil$SAT
    SKS<-s$soil$KS
    SSS<-round(as.numeric(SKS), digits = 1)
    BDM<-s$soil$BD
    LOC<-s$soil$Carbon
    LCL<-s$soil$ParticleSizeClay
    LSI<-s$soil$ParticleSizeSilt
    LNI<-as.numeric(s$soil$Nitrogen)*0.0001
    LHW<-s$soil$PH
    CEC<-s$soil$CEC
    
    ## in the following lines, I assume the read-write-read is needed to keep the orignal soil.sol intact?
    
    sol <- suppressWarnings(DSSAT::read_sol(paste(path.to.extdata, "soil.sol", sep="/"), id_soil = "IBPN910025"))
    DSSAT::write_sol(sol, paste(path.to.extdata, "NEW.SOL", sep="/"), append = FALSE)
    ex_profile <- DSSAT::read_sol(paste(path.weather.data, "NEW.SOL", sep="/"), id_soil = "IBPN910025")
    
    
    soilid <- ex_profile %>%
      mutate(PEDON=paste0('TRAN', formatC(width = 6, (as.integer(i)-1), flag = "0")),
             SLB=Depth,
             SLLL=LL15,
             SSAT=SAT,
             SDUL=DUL,
             SSKS=SSS,
             SBDM=BDM,
             SLOC=LOC,
             SLCL=LCL,
             SLSI=LSI,
             SLNI=LNI,
             SLHW=LHW,
             SCEC=CEC)
    
    DSSAT::write_sol(soilid, 'SOIL.SOL', append = FALSE)
    DSSAT::write_sol(sol, 'NEW.SOL', append = FALSE)
    
    ##########################################
    #Read data from local machine
    tst$DATE <- as.POSIXct(tst$DATE, format = "%m/%d/%Y", tz = "UTC")
    
    # Calculate long-term average temperature (TAV)
    tav <- tst %>%
      summarize(TAV=mean((TMAX+TMIN)/2))
    
    # Calculate monthly temperature amplitude (AMP)
    amp <- tst %>%
      # Extract month from DATE column
      mutate(month = month(as.Date(tst$DATE,format = "%y%j"))) %>%
      # Group data by month
      group_by(month) %>%
      # Calculate monthly means
      summarize(monthly_avg = mean((TMAX+TMIN)/2)) %>%
      # Calculate AMP as half the difference between minimum and
      #     maximum monthly temperature
      summarize(AMP = (max(monthly_avg)-min(monthly_avg))/2)
    
    # Generate new general information table
    general_new <- tibble(
      INSI = "RWAN",
      LAT = coords[i, 2],
      LONG = coords[i, 1],
      ELEV = 1490,
      TAV = tav,
      AMP = amp,
      REFHT = 2,
      WNDHT = 2
    )
    
    # Add station information 
    attr(tst, "GENERAL") <- general_new
    
    DSSAT::write_wth(tst, paste0("WHTE", formatC(width = 4, (as.integer(i)-1), flag = "0"), ".WTH"))
    setwd(path.to.extdata)
    gc()
  }  
}




