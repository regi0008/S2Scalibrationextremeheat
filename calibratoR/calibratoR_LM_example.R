#linear regression example

library(loadeR)
library(downscaleR)
library(transformeR)
library(visualizeR)
library(calibratoR)
library(loadeR.ECOMS)
library(SpecsVerification)

#loading datasets of boreal winter temp over Iberia
#seasonal forecasts CFS
data(CFS_Iberia_tas)
#fcst = CFS_Iberia_tas
#issue: need to constrain domain
fcst <- subsetGrid(CFS_Iberia_tas, lonLim = c(-10,10), latLim = c(30,45), years = 1983:2001)
#observational data NCEP
data(NCEP_Iberia_tas)
obs = NCEP_Iberia_tas

fcst <- aggregrateGrid(fcst, aggr.y = list(FUN = "mean", na.rm=TRUE))
obs <- aggregrateGrid(obs, aggr.y = list(FUN = "mean", na.rm = TRUE))


#interpolating forecasts to the observations' resolution
fcst <- interpGrid(fcst, new.coordinates = getGrid(obs))
#apply calibraton
fcst_cal <- calLM(fcst, obs, crossval = TRUE, apply.to = "all")

#plot climo
spatialPlot(makeMultiGrid(climatology(obs),
                          climatology(fcst, by.member = FALSE), 
                          climatology(fcst_cal, by.member = FALSE)),
            backdrop.theme = "coastline",
            layout = c(3, 1),
            names.attr = c("NCEP", "CFS (raw)", "CFS (calibrated)"))
#find out how to download as gridded data