#Re-gridding/spatial interpolation
#using gridded data from NCEP/NCAR Reanalysis 1 dataset

library(transformeR)   #required for function interpGrid()
library(downscaleR)
library(visualizeR)    #for spatialPlot()
library(loadeR)
library(SpecsVerification)

data(NCEP_Iberia_tas)
spatialPlot(climatology(NCEP_Iberia_tas, list(FUN = mean, na.rm = TRUE)),
            backdrop.theme = "countries",
            scales = list(draw=TRUE),
            main = attr(NCEP_Iberia_tas$Variable, "longname"))

#method1 = "bilinear", and interpolate to smaller domain using 0.5 deg res
gridtogrid <- interpGrid(NCEP_Iberia_tas,
                         new.coordinates = list(x=c(-10,5,0.5), y=c(36,44.5)),
                         method="bilinear")
spatialPlot(climatology(gridtogrid, list(FUN = mean, na.rm = TRUE)),
            backdrop.theme = "countries",
            scales = list(draw=TRUE),
            main = attr(NCEP_Iberia_tas$Variable, "longname"))

attributes(gridtogrid$xyCoords)

#method2 = "nearest", and perform nearest neighbour interpolation
#may have error
gridtogrid <- interpGrid(NCEP_Iberia_tas,
                         new.coordinates = list(x=c(-10,5,0.5), y=c(36,44.5)),
                         method="nearest")
spatialPlot(climatology(gridtogrid, list(FUN = mean, na.rm = TRUE)),
            backdrop.theme = "countries",
            scales = list(draw=TRUE),
            main = attr(NCEP_Iberia_tas$Variable, "longname"))