#linear regression calibration example

library(loadeR)
library(downscaleR)
library(transformeR)
library(visualizeR)
library(calibratoR)
library(loadeR.2nc)
library(ncdf4)
library(SpecsVerification)

#loading datasets of boreal winter temp over Iberia
#seasonal forecasts CFS
data(CFS_Iberia_tas)
fcst = CFS_Iberia_tas
#issue solved: need to constrain domain
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

#download / load as gridded data into local folder
#name of output file
fileName <- "cal_LM_gridded.nc"

#include a global attribute:
globalAttributeList <- list("institution" = "SantanderMetGroup, http://www.meteo.unican.es/")

#include two variable attribute:
varAttributeList <- list(var_attr1 = "one_attribute", var_attr2 = "another_attribute")

#create file
grid2nc(fcst_cal,
        NetCDFOutFile = fileName,
        missval = 1e20,
        prec = "float",
        globalAttributes = globalAttributeList,
        varAttributes = varAttributeList,
        shuffle = TRUE,
        verbose = TRUE)
