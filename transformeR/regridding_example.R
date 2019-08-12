#Re-gridding/spatial interpolation

library(transformeR)
library(downscaleR)
library(visualizeR)
library(loadeR)
library(SpecsVerification)

data(NCEP_Iberia_tas)
spatialPlot(climatology(NCEP_Iberia_tas, list(FUN = mean, na.rm = TRUE)),
            backdrop.theme = "countries",
            scales = list(draw=TRUE),
            main = attr(NCEP_Iberia_tas$Variable, "longname"))

#method = "bilinear"/"nearest", and interpolate to smaller domain using 0.5 deg res
gridtogrid <- interpGrid(NCEP_Iberia_tas,
                         new.coordinates = list(x=c(-10,5,0.5), y=c(36,44.5)),
                         method="nearest")
spatialPlot(climatology(gridtogrid, list(FUN = mean, na.rm = TRUE)),
            backdrop.theme = "countries",
            scales = list(draw=TRUE),
            main = attr(NCEP_Iberia_tas$Variable, "longname"))

attributes(gridtogrid$xyCoords)

#problems with using function plotMeanGrid()!!!!!

