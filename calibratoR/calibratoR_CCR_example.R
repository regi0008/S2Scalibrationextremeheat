#climate conserving recalibration example

library(loadeR)
library(downscaleR)
library(transformeR)
library(visualizeR)
library(calibratoR)
library(loadeR.ECOMS)
library(SpecsVerification)

#issue: need to constrain domain
#loading datasets of boreal winter temp over Iberia
#seasonal forecasts
data(CFS_Iberia_tas)
fst <- subsetGrid(CFS_Iberia_tas, lonLim = c(-10,10), latLim = c(30,45), years = 1983:2001)
#fcst = CFS_Iberia_tas
#observational data
data(NCEP_Iberia_tas)
obs = NCEP_Iberia_tas

fcst <- aggregrateGrid(fcst, aggr.y = list(FUN = "mean", na.rm=TRUE))
obs <- aggregrateGrid(obs, aggr.y = list(FUN = "mean", na.rm = TRUE))


#interpolating forecasts to the observations' resolution
fcst <- interpGrid(fcst, new.coordinates = getGrid(obs))
#apply calibraton
fcst_cal <- calCCR(fcst, obs, crossval = TRUE, apply.to = "all")

#error probably is here!!!!!
#plot climo
spatialPlot(makeMultiGrid(climatology(obs), climatology(fcst, by.member = FALSE), 
                          climatology(fcst_cal, by.member = FALSE)),
                          backdrop.theme = "coastline",
                          layout = c(3, 1),
                          names.attr = c("NCEP", "CFS (raw)", "CFS (calibrated)"))
#find out how to download as gridded data