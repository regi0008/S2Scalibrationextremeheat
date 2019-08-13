#Time series visualization

library(transformeR)
library(downscaleR)
library(visualizeR)
library(loadeR)
library(SpecsVerification)
library(CSTools)

#compare to time series
#first is the multimember grid (seasonal forecast CFS_Iberia_tas, period=1983:2002)
#second is a single grid without members (obs data, EOBS_Iberia_tas, period=1983:2002).
#Need to apply function subsetGrid() to choose desired time interval
data(CFS_Iberia_tas)
data(EOBS_Iberia_tas)

#combind plot with members (CFS) and without members (EOBS)
a <- subsetGrid(CFS_Iberia_tas, years = 1985:1992)
b <- subsetGrid(EOBS_Iberia_tas, years = 1985:1992)
temporalPlot("EOBS" = b, "CFS" = a,
             show.na=TRUE,
             xyplot.custom = list(ylim = c(-5,15)))

data(VALUE_Iberia_tas)
value <- subsetGrid(VALUE_Iberia_tas, years = 1988:1990)
temporalPlot("EOBS" = b, "CFS" = a,
             "VALUE" = value, lwd = 0.9,
             aggr.spatial = list(FUN = min, na.rm = TRUE),
             show.na=TRUE,
             xyplot.custom = list(main="winter temperature",
                                  ylab = "Celcius", ylim = c(-20,12)))
                                  

#plot a single location only
a1 <- subsetGrid(a, lonLim = 2, latLim = 42)
b1 <- subsetGrid(b, lonLim = 2, latLim = 42)

temporalPlot("EOBS" = b1, "CFS" = a1,
             cols = c("purple", "yellow"),
             show.na=TRUE,
             xyplot.custom = list(main = "winter temperature", ylab = "Celsius", ylim = c(-10,15)))
