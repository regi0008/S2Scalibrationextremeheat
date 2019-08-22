#Verification example from 2018_ECOMS-UDG-ClimateServices.pdf
#for mean winter temperature forecasts in North-Atlantic domain
#NCEP

library(loadeR)
library(loadeR.ECOMS)
library(transformeR)
library(visualizeR)
library(s2dverification)
library(easyVerification)
library(SpecsVerification)
library(RColorBrewer)

#for data access authentication
loginUDG(username = "uSeRnAmE_regi0008", password = "pAssWord")

#NCEP Reanalysis SLP
url <- "http://meteo.unican.es/work/UDG/NCEP_tas_DJF_annual_1983_2010_LD.Rdata"
temp_file <- tempfile()
download.file(url, destfile = temp_file)
load(temp_file, .GlobalEnv, verbose = TRUE)

#CFSv2 SLP
url <- "http://meteo.unican.es/work/UDG/CFS_24_lm1_tas_DJF_annual_1983_2010_LD.Rdata"
temp_file <- tempfile()
download.file(url, destfile = temp_file)
load(temp_file, .GlobalEnv, verbose = TRUE)

#geographical window:
lonLim <- c(-90,40)
latLim <- c(20,80)

#NCEP Reanalysis1 data
#dataset = NCEP_reanalysis1
#original data are 6-hourly, thus data are aggregated by loadeR.ECOMS
#by introducing argument time = "DD" to convert the data from 6-hour to daily.
#aggr.d = "mean" is to indicate aggregation function
#aggr.m - "mean" is to indicate that daily data will be monthly averaged
tas.ncep <- loadECOMS(dataset = "NCEP_reanalysis1",
                      var="tas",
                      lonLim = lonLim,
                      latLim = latLim,
                      season = c(12,1,2),
                      years = NULL,
                      time = "DD",
                      aggr.d = "mean",
                      aggr.m = "mean")

#plot the mean sea-level pressure winter climatology:
spatialPlot(climatology(tas.ncep),
            backdrop.theme = "coastline",
            main = "NCEP Mean DJF temperature (1983-2010)")


#CFSv2 seasonal hindcast predictions data
#dataset = CFSv2_seasonal
#necessary to specify for hindcast data:
#initialization time (e.g. for 1 month ahead predictions: leadMonth = 1)
#the first 24 members: "members = 1:24"
tas.cfs <- loadECOMS(dataset = "CFSv2_seasonal",
                     var="tas",
                     lonLim = lonLim,
                     latLim = latLim,
                     season = c(12,1,2),
                     years = NULL,
                     time = "DD",
                     aggr.d = "mean",
                     aggr.m = "mean",
                     members = 1:24,
                     leadMonth = 1)

#temporal aggregation and interpolation
#monthly data are annually aggregated
tas.ncep <- aggregateGrid(grid = tas.ncep, aggr.y = list(FUN = "mean"))
tas.cfs <- aggregateGrid(grid = tas.cfs, aggr.y = list(FUN = "mean"))

#CFSv2 data are interpolated to match NCEP reanalysis grid:
tas.cfs <- interpGrid(tas.cfs, new.coordinates = getGrid(tas.ncep), method = "bilinear")

#plot resulting hindcast data:
spatialPlot(climatology(tas.cfs),
            backdrop.theme = "coastline",
            main = "Mean DJF Temperature (1983-2010)")

#-----------------------------------------------------------

#Correlation Analysis
#object cor contains correlation coefficients for each point.
#need to convert list into array

obs <- tas.ncep[["Data"]]                 #type = double
#obs_array <- array(as.numeric(unlist(obs)))
#obs_vector <- as.vector(obs)
#dim(obs_array) = 86640

fcst <- tas.cfs[["Data"]]                 #type = double
#fcst_array <- array(as.numeric(unlist(fcst)))
#fcst_vector <- as.vector(fcst)
#dim(fst_array) = 2599200

cor <- veriApply(verifun = "EnsCorr",
                 fcst = fcst,
                 obs = obs,
                 tdim = 2,
                 ensdim = 1)

#easyVeri2grid is a bridging function that performs transformation using transformeR
#from the raw verification matrices produced by library(easyVerification)
#to the grid structure with all required metadata for data collection and representation by climate4R bundle
cor.grid <- easyVeri2grid(easyVeri.mat = cor,
                          obs.grid = tas.ncep,
                          verifun = "EnsCorr")

library(RColorBrewer)

#plot correlation map (time dimension is a singleton)
spatialPlot(climatology(cor.grid),
            backdrop.theme = "countries",
            color.theme = "RdYlBu",
            at = seq(-1,1,0.05),
            main = "Ensemble correlation",
            sub = "CFSv2 24 member - DJF Mean Temperature (1982-2010")

#-----------------------------------------