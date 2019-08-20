#Verification example from 2018_ECOMS-UDG-ClimateServices.pdf
#for mean winter temperature forecasts in north-atlantic domain
#NCEP

library(loadeR)
library(loadeR.ECOMS)
library(transformeR)
library(visualizeR)
library(s2dverification)
library(easyVerification)
library(SpecsVerification)
library(tidync)
library(tidyr)
library(dplyr)

#for data access authentication
loginUDG(username = "uSeRnAmE_regi0008", password = "pAssWord")

#NCEP Reanalysis SLP
#url <- "http://meteo.unican.es/work/UDG/NCEP_psl_DJF_annual_1949_2010_LD.Rdata"
#temp_file <- tempfile()
#download.file(url, destfile = temp_file)
#load(temp_file, .GlobalEnv, verbose = TRUE)

#CFSv2 SLP
#url <- "http://meteo.unican.es/work/UDG/CFS_24_lm1_psl_DJF_annual_1983_2010_LD.Rdata"
#temp_file <- tempfile()
#download.file(url, destfile = temp_file)
#load(temp_file, .GlobalEnv, verbose = TRUE)

#for NCEP Reanalysis data:
#lonLim <- c(-90,40)
#latLim <- c(20,80)
#var <- "psl"           #"psl" refers to sea-level pressure

#NCEP Reanalysis1 data
#dataset= NCEP_reanalysis1
#original data are 6-hourly, thus data are aggregated by loadeR.ECOMS
#by introducing argument time = "DD" to convert the data from 6-hour to daily.
#aggr.d = "mean" is to indicate aggregation function
#aggr.m - "mean" is to indicate that daily data will be monthly averaged
#ncep.psl <- loadECOMS(dataset = "NCEP_reanalysis1",
#                      var=var,
#                      lonLim = lonLim,
#                      latLim = latLim,
#                      season = c(12,1,2),
#                      years = NULL,
#                      time = "DD",
#                      aggr.d = "mean",
#                      aggr.m = "mean")

#reanalysis period = 1950:2010
#plot the mean sea-level pressure winter climatology:
#spatialPlot(climatology(ncep.psl),
#            backdrop.theme = "coastline",
#            main = "NCEP mean DJF SLP (1950-2010)")


#CFSv2 seasonal hindcast data
#dataset = CFSv2_seasonal
#necessary to specify for hindcast data:
#initialization time (e.g. for 1 month ahead predictions: leadMonth = 1)
#the first 24 members: "members = 1:24"
#cfs.psl <- loadECOMS(dataset = "CFSv2_seasonal",
#                     var=var,
#                     lonLim = lonLim,
#                     latLim = latLim,
#                     season = c(12,1,2),
#                     years = NULL,
#                     time = "DD",
#                     aggr.d = "mean",
#                     aggr.m = "mean",
#                     members = 1:24,
#                     leadMonth = 1)

#spatialPlot(climatology(cfs.psl),
#            backdrop.theme = "coastline",
#            main = "Mean DJF SLP (1983-2010)")

#temporal aggregation
#monthly data are annually aggregated
#ncep.psl <- aggregateGrid(grid = ncep.psl, aggr.y = list(FUN = "mean"))
#cfs.psl <- aggregateGrid(grid = cfs.psl, aggr.y = list(FUN = "mean"))

#CFSv2 data are interpolated to match NCEP reanalysis grid:
#tas.cfs <- interpGrid(tas.cfs, new.coordinates = getGrid(tas.ncep), method = "bilinear")
#plot resulting hindcast data:
#spatialPlot(climatology(tas.cfs),
#            backdrop.theme = "coastline",
#            main = "Mean DJF Temperature (1983-2010)")

#-----------------------------------------------------------

#Correlation Analysis
#object cor contains correlation coefficients for each point.
#need to convert list into array
summary(NCEP_Iberia_tas)
summary(CFS_Iberia_tas)

obs <- NCEP_Iberia_tas[["Data"]]                 #type = double
obs_array <- array(as.numeric(unlist(obs)))
#obs_vector <- as.vector(obs)
#dim(obs_array) = 86640

fcst <- CFS_Iberia_tas[["Data"]]                 #type = double
fcst_array <- array(as.numeric(unlist(fcst)))
#fcst_vector <- as.vector(fcst)
#dim(fst_array) = 2599200

cor <- veriApply(verifun = "EnsCorr",
                 fcst = fcst_array,
                 obs = obs_array,
                 tdim = length(dim(fcst))-1,
                 ensdim = length(dim(fcst)))

#above error:
#Error in veriApply(verifun = "EnsCorr", fcst = fcst_array, obs = obs_array,  : 
#c(ensdim, tdim) <= nfdims are not all TRUE

#easyVeri2grid is a bridging function that performs transformation
#from the raw verification matrices produced by library(easyVerification)
#to the grid structure with all required metadata for data collection and representation by climate4R bundle
cor.grid <- easyVeri2grid(easyVeri.mat = cor,
                          obs.grid = NCEP_Iberia_tas[["Data"]],
                          verifun = "EnsCorr")
#above error:
#$ operator is invalid for atomic vectors

#plot correlation map
spatialPlot(climatology(cor.grid),
            backdrop.theme = "countries",
            at = seq(-1,1,0.05),
            colorkey = list(space = "bottom"),
            main = "Ensemble correlation",
            sub = "CFSv2 24 member - DJF Mean Temperature (1982-2010")

#above error:
#Error in getDim(grid) : object 'cor.grid' not found
#-----------------------------------------

#since there is error with dimension/length between fcst and obs,
#try with package "tidync" using hyper_filter(), hyper_array() etc.
#read file NCEP_Iberia_tas_obs.nc
#read file CFS_Iberia_tas_forecast.nc

#hyper_array()/hyper_slice() function extracts raw data as a list of one or more arrays.
#This can be the entire variable/s or after dimension-slicing using hyper_filter() expressions.

filename_obs <- "NCEP_Iberia_tas_obs.nc"
obs <- system.file("C:/Users/regin/Desktop/R", filename_obs, package = "tidync")
extract_obs <- tidync(obs) %>%
  hyper_slice(obs, select_var = "time")

print(extract_obs)


#-----------------------------------------