#downscaleR
#bias correcting seasonal forecast data from 9 models
#period: 1983-2001 (observed period)
#test period: 2002 (non-observed period)
#based on temperature

library(visualizeR)      #for spatialPlot(), temporalPlot()
library(downscaleR)
library(loadeR.2nc)      #for downloading gridded dataset (scroll below to see)

#observation via EOBS_Iberia_tas
#a grid containing E_OBS daily data of mean temp.
data(EOBS_Iberia_tas)
y <- subsetGrid(EOBS_Iberia_tas, years = 1983:2001)

#predictor via CFS_Iberia_tas
#a multi-member grid containing CFSv2
#seasonal forecast data of daily mean temp
data(CFS_Iberia_tas)
x <- subsetGrid(CFS_Iberia_tas, years = 1983:2001)

#predictor in test period (non-observed period)
newdata <- subsetGrid(CFS_Iberia_tas, years = 2002)

#spatialPlot() draws all members of CFS data in the same figure
spatialPlot(climatology(y, clim.fun = list(FUN = mean, na.rm = T)), 
            backdrop.theme = "countries", 
            scales = list(draw = T))

#bias correction applied to AN OBSERVED PERIOD
cal <- biasCorrection(y = y,
                      x = x,
                      newdata = x,
                      method = "eqm")
#--------------------------------
#download / load as gridded data into local folder for method = variance "eqm"
#name of output file
fileName <- "cal_eqm0_gridded.nc"

#include a global attribute:
globalAttributeList <- list("institution" = "SantanderMetGroup, http://www.meteo.unican.es/")

#include two variable attribute:
varAttributeList <- list(var_attr1 = "one_attribute", var_attr2 = "another_attribute")

#create file
grid2nc(cal,
        NetCDFOutFile = fileName,
        missval = 1e20,
        prec = "float",
        globalAttributes = globalAttributeList,
        varAttributes = varAttributeList,
        shuffle = TRUE,
        verbose = TRUE)


#--------------------------------
#after calibrating, next is to validate results against the obs reference.

#qucikDiagnostics() plots daily/annual series and the annual
#correlation map of diff. grid objects

#plot on the left is a time-series of the original simulation (in red)
#calibrated simulation (in blue), observation (in black)
#plot on the right is the quantile-quantile plot,
#showing the diff. in original and calibrated and effect of the applied method
loc <- c(-5, 42)
quickDiagnostics(y, x, cal, location = loc)

#to plot the different members of CFS data
spatialPlot(climatology(cal))


#bias correction applied to a NON-OBSERVED PERIOD
#tercilePlot() is for visualization of forecast skill of seasonal climate predic.
#it performs the mean of all stations and shows the skill (ROCSS)
#of the seasonal forecasting models
tercilePlot(CFS_Iberia_tas, obs = EOBS_Iberia_tas, year.target = 2002, color.pal = "ypb")

#try between "variance" and "eqm" method of bias correction to bias correct out-of-sample data (newdata, period: 2002)
#first method: Variance
cal1 <- biasCorrection(y = y,
                       x = x,
                       newdata = newdata, 
                       method = "variance")

#second method: eqm
cal2 <- biasCorrection(y = y,
                       x = x,
                       newdata = newdata,
                       method = "eqm")

#--------------------------------
#download / load as gridded data into local folder for method = variance "MVA"
#name of output file
fileName <- "cal_variance_gridded.nc"

#include a global attribute:
globalAttributeList <- list("institution" = "SantanderMetGroup, http://www.meteo.unican.es/")

#include two variable attribute:
varAttributeList <- list(var_attr1 = "one_attribute", var_attr2 = "another_attribute")

#create file
grid2nc(cal1,
        NetCDFOutFile = fileName,
        missval = 1e20,
        prec = "float",
        globalAttributes = globalAttributeList,
        varAttributes = varAttributeList,
        shuffle = TRUE,
        verbose = TRUE)
#--------------------------------
#download / load as gridded data into local folder for method = emphirical quantile mapping "eqm"
#name of output file
fileName <- "cal_eqm_gridded.nc"

#include a global attribute:
globalAttributeList <- list("institution" = "SantanderMetGroup, http://www.meteo.unican.es/")

#include two variable attribute:
varAttributeList <- list(var_attr1 = "one_attribute", var_attr2 = "another_attribute")

#create file
grid2nc(cal2,
        NetCDFOutFile = fileName,
        missval = 1e20,
        prec = "float",
        globalAttributes = globalAttributeList,
        varAttributes = varAttributeList,
        shuffle = TRUE,
        verbose = TRUE)
#--------------------------------
#TEMPORAL PLOTS
#show the spread of the spatial mean of the ensemble
#plot the data used for calibration and out-of-sample series (corrected with 'eqm' method and non-corrected)
temporalPlot(y, x, newdata, cal2, cols = c("black", "yellow", "red", "purple"),
             xyplot.custom = list(ylim = c(-4,16)))

#and show the effect of the correction (by only plotting year 2002)
temporalPlot(newdata, cal2, cols = c("red", "purple"),
             xyplot.custom = list(ylim = c(-2,16)))

#lastly a comparison between the two BC methods - cal1-scaling and cal2-eqm in this example
temporalPlot(cal1, cal2, cols = c("blue", "purple"),
             xyplot.custom = list(ylim = c(-2,16)))