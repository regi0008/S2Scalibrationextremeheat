library(transformeR)
library(downscaleR)
library(visualizeR)
library(loadeR)
library(SpecsVerification)

#time period domain
#output will show "1982-12-01 GMT" "2002-03-01 GMT"
data(NCEP_Iberia_tas)
range(NCEP_Iberia_tas$Dates)

#now we make a subset of the time period using subsetGrid
#output will show "1994-12-01 GMT" "1998-03-01 GMT"
sub.time <- subsetGrid(NCEP_Iberia_tas, years=1995:1998)
range(sub.time$Dates)

#now get geographic domain
data(NCEP_Iberia_tas)
spatialPlot(climatology(NCEP_Iberia_tas, clim.fun = list(FUN = "mean", na.rm = TRUE)),
              backdrop.theme = "countries",
              scales = list(draw = TRUE))

#now we make a subset of that geographic domain, also using subsetGrid
#geographic domain based on example, can be changed
sub.geo <- subsetGrid(NCEP_Iberia_tas, lonLim = c(-1,3), latLim = c(39,43))
spatialPlot(climatology(sub.geo, clim.fun = list(FUN = "mean", na.rm = TRUE)), 
            backdrop.theme = "countries",
            scales = list(draw = TRUE))