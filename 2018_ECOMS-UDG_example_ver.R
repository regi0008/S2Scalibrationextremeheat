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

#Ensemble Correlation
#object cor contains correlation coefficients for each point.

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
#transforms the raw verification matrices produced by library(easyVerification)
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

#ROC Area
#recall - observation: tas.ncep, forecast: tas.cfs

#EnsRoca computes the area under the ROC curve given the observations.

roca <- veriApply(verifun = "EnsRoca",
                  fcst = fcst,
                  obs = obs,
                  prob = c(1/3,2/3),    #Tercile Probabilities
                  tdim = 2,
                  ensdim = 1)

l <- lapply(roca[1:3], "easyVeri2grid", tas.ncep)
mg <- makeMultiGrid(l)
str(mg)

#-----------------------------------------

#plot climatology

spatialPlot(mg,
            backdrop.theme = "countries",
            color.theme = "RdYlBu",
            names.attr = c("Lower tercile", "Middle tericle", "Upper tericle"),
            layout =c(3,1),
            main = "Area under the ROC curve",
            sub = "CFSv2 24 member - DJF Mean Temperature (1983-2010)")

#-----------------------------------------

#Compute Forecast/Observed Anomalies

library(s2dverification)

#still have problems from here onwards
tas.cfs.array <- array(as.numeric(as.character(unlist(tas.cfs))))
tas.ncep.array <- array(as.numeric(as.character(unlist(tas.ncep))))

ano_tas.cfs <- Ano(tas.cfs.array, climatology(tas.cfs))
ano_tas.ncep <- Ano(tas.ncep.array, climatology(tas.ncep))
#runmean_nb_months <- 12
#dim_to_smooth <- 4 #smooth along lead-times
#smooth_ano_tascfs <- smoothing(ano_tascfs, runmean_nb_months, dim_to_smooth)
#smooth_ano_tasncep <- smoothing(ano_tasncep, runmean_nb_months, dim_to_smooth)

#Plot time series of smoothed anomalies
#error!!
PlotAno(ano_tas.cfs, ano_tas.ncep,
        toptitle = paste('smoothed anomalies'),
        ytitle = c('K', 'K', 'K'),
        fileout = 'anomalies')

#-----------------------------------------

