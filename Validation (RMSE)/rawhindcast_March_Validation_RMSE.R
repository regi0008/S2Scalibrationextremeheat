library(loadeR)
library(downscaleR)
library(transformeR)
library(visualizeR)
library(calibratoR)
library(ncdf4)
library(abind)
library(s2dverification)
library(SpecsVerification)
library(easyVerification)
library(verification)
library(RColorBrewer)
library(Metrics)

loadNcdf <- function(filePath, varname, tz = 'GMT', ...) {
  nc <- nc_open(filePath)
  var <- nc$var
  # Use name to locate the variable
  call_1 <- as.call(c(
    list(as.name('$'), var, varname)
  ))
  var <- eval(call_1)
  if(is.null(var)) stop('No such variable name, check source file.')
  
  dimNames <- unlist(lapply(1:length(var$dim), function(x) var$dim[[x]]$name))
  print(dimNames)
  
  # Only deals with the most common dimensions, futher dimensions will be added in future.
  dimIndex <- grepAndMatch(c('lon', 'lat', 'time', 'member'), dimNames)
  print(dimIndex)
  if (length(dimIndex) < 3) stop('Your file has less than 3 dimensions.')
  
  # First needs to identify the variable name, load the right data
  message('Loading data...')
  nc_data <- ncvar_get(nc, var)
  message('Processing...')
  
  gridData <- list()
  gridData$Variable$varName <- varname
  gridData$xyCoords$x <- var$dim[[dimIndex[1]]]$vals
  attributes(gridData$xyCoords$x)$name <- dimNames[dimIndex[1]]
  
  gridData$xyCoords$y <- var$dim[[dimIndex[2]]]$vals
  attributes(gridData$xyCoords$y)$name <- dimNames[dimIndex[2]]
  
  # Time part needs to be taken seperately
  timeUnit <- strsplit(var$dim[[dimIndex[3]]]$units, split = ' since')[[1]][1]
  timeDiff <- var$dim[[dimIndex[3]]]$vals
  # To get real time, time since when has to be grabbed from the dataset.
  timeSince <- as.POSIXlt(strsplit(var$dim[[dimIndex[3]]]$units, split = 'since')[[1]][2], tz = tz)
  
  #  Date <- rep(timeSince, length(timeDiff))
  unitDic <- data.frame(weeks = 'weeks', days = 'days', hours = 'hours',
                        minutes = 'mins', seconds = 'secs')
  
  timeDiff <- as.difftime(timeDiff, units = as.character(unitDic[1, timeUnit]))
  
  #   if (grepl('day', timeUnit)) {
  #     Date$mday <- Date$mday + timeDiff
  #   } else if (grepl('second', timeUnit)) {
  #     Date$sec <- Date$sec + timeDiff
  #   }
  Date <- timeSince + timeDiff
  
  # data directly loaded from ncdf4 will drop the dimension with only one value.
  # the varsize shows the real dimension, without any dropping.
  dim(nc_data) <- var$varsize
  
  # Right now there is no need to add end Date, in furture, may be added as needed.
  gridData$Dates$start <- as.character(Date)
  
  # Adding data to grid data
  # At leaset should be 3 dimensions, lon, lat, time.
  gridData$Data <- nc_data
  
  attributes(gridData$Data)$dimensions <- dimNames
  
  if (!is.na(dimIndex[4])) 
    gridData$Members <- var$dim[[dimIndex[4]]]$vals
  nc_close(nc)
  
  output <- gridData
  
  return(output)
  
}

writeNcdf_verf <- function(gridData, filePath, missingValue = 1e20, tz = 'GMT', units = NULL, version = 3) {
  
  name <- gridData$Variable$varName
  # First defines dimensions.
  dimLon <- ncdim_def('lon', 'degrees_east', gridData$xyCoords$x)
  dimLat <- ncdim_def('lat', 'degrees_north', gridData$xyCoords$y)
  dimMem <- NULL
  dimTime <- NULL
  
  #---------------------------------------  
  # default list
  dimList <- list(dimLat, dimLon)
  
  # In order to keep the dim list exactly the same with the original one, it needs to be changed.
  dimIndex <- grepAndMatch(c('lat', 'lon', 'time'), attributes(gridData$Data)$dimensions)
  #---------------------------------------  
  # Then defines data
  var <- ncvar_def(name, "units", dimList, missingValue)
  
  # Here for ncdf4, there is an option to create version 4 ncdf, in future, it
  # may added here.
  if (version == 3) {
    nc <- nc_create(filePath, var) 
  } else if (version == 4) {
    nc <- nc_create(filePath, var, force_v4 = TRUE)
  } else {
    stop("Which ncdf version you want? Only 3 and 4 can be selected!")
  }
  
  ncatt_put(nc, 'lat', "standard_name","latitude")
  ncatt_put(nc, 'lat', "_CoordinateAxisType","Lat")
  ncatt_put(nc, 'lon', "standard_name","longitude")
  ncatt_put(nc, 'lon', "_CoordinateAxisType","Lon")
  
  # This part has to be put
  ncatt_put(nc, 0, "Conventions","CF-1.4")
  ncatt_put(nc, 0, 'WrittenBy', 'CCRS SSP Team, adapted from hyfo package')
  
  data <- aperm(gridData$Data, dimIndex)
  ncvar_put(nc, name, data)
  nc_close(nc)
  
}

# in order to first grep than match.
# match only provides for exactly match, 
# dimIndex <- grepAndMatch(c('lon', 'lat', 'time', 'member'), dimNames)
grepAndMatch <- function(x, table) {
  index <- unlist(lapply(x, function(x) {
    a <- grep(x, table)
  }))
  return(index)
}

#------------------------------------------
#LOADING OF FILES THROUGH HYFO PACKAGE:
dir <- "C:/Users/regin/Desktop/R/S2Scalibrationextremeheat/loadeR"
#predictor (raw hincast):
fcst <- loadNcdf(file.path(dir, "2t_201902_Mar_format_asc.nc"), "tas")
#predictand:
obs <- loadNcdf(file.path(dir, "2t_era5_Mar_1993_2016_format_asc.nc"), "tas")
#------------------------------------------
#VALIDATION: ROOT MEAN SQUARE ERROR CALCULATION

#need library(Metrics) to use rmse()
  #1) square the errors between fcst$Data (of each member) and obs$Data
  #2) find the mean of the squared errors
  #3) take the squareroot of that resulting mean

#use sapply function method:
#indiv_mse <- sapply(1:25, function(i) {
#  mse(fcst$Data[,,,i], obs$Data)
#})
#print(indiv_mse)
#sum up all members
#total_mse <- sum(indiv_mse)
#print(total_mse)
#based on formula, rmse = sqrt(mse/n), where n is no. of samples.
#take n = 1.
#rmse_raw <- sqrt(total_mse/1)
#print(rmse_raw)
#------------------------------------------
#try easyVerification package method: EnsRmse(ens,obs)

#EnsRmse(as.matrix(fcst$Data), as.vector(obs$Data)) would give the error below:
#Error in EnsError(ens = ens, obs = obs, type = "rmse") : 
#  length(obs) == nrow(ens) is not TRUE

#RMSE_raw <- sapply(1:25, function(i) {
#  EnsRmse(as.matrix(fcst$Data[,,,i]), as.vector(obs$Data))
#})
#print(RMSE_raw)
#------------------------------------------
#COMPUTE RMSE

calculate_rmse_fcst_raw <- veriApply(verifun = "EnsRmse", fcst = fcst$Data, obs = obs$Data)

#Output array calculate_crps_fcst_cal_CCR to netcdf file
metadata <- list(calculate_rmse_fcst_raw = list(units = 'unit'))
attr(calculate_rmse_fcst_raw, 'variables') <- metadata
names(dim(calculate_rmse_fcst_raw)) <- c('lon', 'lat')

lon <- seq(90, 140)
dim(lon) <- length(lon)
metadata <- list(lon = list(units = 'degrees_east'))
attr(lon, 'variables') <- metadata
names(dim(lon)) <- 'lon'

lat <- seq(-10, 20)
dim(lat) <- length(lat)
metadata <- list(lat = list(units = 'degrees_north'))
attr(lat, 'variables') <- metadata
names(dim(lat)) <- 'lat'

rmse_fcst_raw_fileName <- "rmse_fcst_raw_new.nc"
ArrayToNetCDF(list(lon, lat, calculate_rmse_fcst_raw), rmse_fcst_raw_fileName)
