#downscaleR
#bias correcting seasonal forecast data from 9 models
#period: 1983-2001 (observed period)
#test period: 2002 (non-observed period)
#based on temperature

#observation via EOBS_Iberia_tas
#a grid containing E_OBS daily data of mean temp.
data(EOBS_Iberia_tas)
y <- subsetGrid(EOBS_Iberia_tas, years = 1983:2001)

#predictor via CFS_Iberia_tas
#a multi-member grid containing CFSv2
#seasonal forecast data of daily mean temp
data(CFS_Iberia_tas)
x <- subsetGrid(CFS_Iberia_tas, years = 1983:2001)

#predictor in test period
newdata <- subsetGrid(CFS_Iberia_tas, years = 2002)

library(visualizeR)

#function spatialPlot draws all members of CFS data in the same figure
spatialPlot(climatology(y, clim.fun = list(FUN = mean, na.rm = T)), 
            backdrop.theme = "countries", 
            scales = list(draw = T))

#bias correction applied to AN OBSERVED PERIOD
cal <- biasCorrection(y = y,
                      x = x,
                      newdata = x,
                      method = "eqm")
#after calibrating, next is to validate results against the obs reference.
#qucikDiagnostics() plots daily/annual series and the annual
#correlation map of diff. grid objects
#plot on the left is a time-series of the original simulation (in red)
#calibrated simulation (in blue), observation (in black)
#plot on the right is the quantile-quantile plot,
#showing the diff. in original and calibrated
loc <- c(-5, 42)
quickDiagnostics(y, x, cal, location = loc)

#to plot the different members of CFS data
spatialPlot(climatology(cal))


#bias correction applied to a NON-OBSERVED PERIOD
#tercilePlot() is for visualization of forecast skill of seasonal climate predic.
#it performs the mean of all stations and shows the skill (ROCSS)
#of the seasonal forecasting models
tercilePlot(CFS_Iberia_tas, obs = EOBS_Iberia_tas, year.target = 2002, color.pal = "ypb")

#try between "scaling" and "eqm" method of bias correction to bias correct out-of-sample data (newdata, period: 2002)
#first method: scaling
cal1 <- biasCorrection(y = y,
                       x = x,
                       newdata = newdata, 
                       method = "scaling",
                       scaling.type = "multiplicative")
#second method: eqm
cal2 <- biasCorrection(y = y,
                       x = x,
                       newdata = newdata,
                       method = "eqm")

#show the spread of the spatial mean of the ensemble
temporalPlot(y, x, newdata, cal1, cols = c("black", "red", "red", "blue"))
#and show the effect of the correction
temporalPlot(newdata, cal1, cols = c("red", "blue"))
#lastly a comparison between the two BC methods
temporalPlot(cal1, cal2, cols = c("blue", "green"))