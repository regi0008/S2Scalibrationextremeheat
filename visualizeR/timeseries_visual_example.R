#Time series visualization

library(transformeR)     #for subsetGrid()
library(downscaleR)
library(visualizeR)      #for temporalPlot()
library(loadeR)

#compare to time series
#first is the multimember grid (seasonal forecast CFS_Iberia_tas, period=1983:2002)
#second is a single grid without members (obs data, EOBS_Iberia_tas, period=1983:2002).
#Need to apply function subsetGrid() to choose desired time interval

#seasonal forecast
data(CFS_Iberia_tas)
#observational data
data(EOBS_Iberia_tas)

#combind plot
a <- subsetGrid(CFS_Iberia_tas, years = 1995:2000)
b <- subsetGrid(EOBS_Iberia_tas, years = 1995:2000)
temporalPlot("EOBS" = b, "CFS" = a)

#consider a narrower time interval
data(VALUE_Iberia_tas)
value <- subsetGrid(VALUE_Iberia_tas, years = 1988:1990)
temporalPlot("EOBS" = b, "CFS" = a, "VALUE" = value, lwd = 2,
             xyplot.custom = list(main="winter temp",
                                  ylab = "Celcius", ylim = c(-2,20)))

#plot for a single location only
a1 <- subsetGrid(a, lonLim = 2, latLim = 42)
b1 <- subsetGrid(b, lonLim = 2, latLim = 42)

temporalPlot("EOBS" = b1, "CFS" = a1,
             cols = c("green", "blue"), show.na=TRUE,
             xyplot.custom = list(main = "winter temp", ylab = "Celsius"))
