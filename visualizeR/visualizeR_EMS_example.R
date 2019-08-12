#Example: 2018_visualizeR_EMS.pdf
#visualizeR: communication and visualization of
#uncertainty in seasonal climate prediction
#hello

library(devtools)
library(loadeR)
library(visualizeR)
library(transformeR)
library(downscaleR)

#Seasonal hindcast and operational predictions are taken from
#CFSv2 seasonal forecasting system by NCEP
#Data from NCEP-NCAR reanalysis 1 is used for reference for verif.
#All dataset is for near-surface air temp for Dec-Jan-Feb (DJF)
#Hindcast dataset spans temporal perid 1983:2010. Total 28 years.

data(tas.cfs)
data(tas.cfs.operative.2016)
data(tas.ncep)

#adjust spatial res. of all data to 5 deg. lat-lon res.
#to improve visualization at a global scale

newgrid <- getGrid(tas.cfs)
attr(newgrid, "resX") <- 5
attr(newgrid, "resY") <- 5
lower.res <- function(x,newgrid) {
    interpGrid(x, new.coordinates = newgrid,
               method = "bilinear",
               bilin.method = "fields")
}

obs <- lower.res(tas.ncep, newgrid)

hindcast <- lower.res(tas.cfs, newgrid)
forecast <- lower.res(tas.cfs.operative.2016, newgrid)

#the %d is a placeholder for an integer variable inside a string printed out
subtitle <- sprintf("Reference date: NCEP; Hindcast: CFS (%d members); %d-%d",
                    length(hindcast$Members),
                    getYearsAsINDEX(hindcast)[1],
                    tail(getYearsAsINDEX(hindcast),1))

#for this tercile plot, red = upper tercile, blue = lower, yellow = mid.
bubblePlot(hindcast, obs, forecast = forecast,
           bubble.size = 1,
           subtitle = subtitle,
           size.as.probability = FALSE,
           score = FALSE)

#for the same spatial plot but focusing on quality of forecast,
#we can set argument size.as.probability = TRUE to
#get probability of the most likely tercile!!!
bubblePlot(hindcast, obs, forecast = forecast,
           bubble.size = 1,
           subtitle = subtitle,
           size.as.probability = TRUE,
           score = FALSE)

#ROCSS aims to show diff. levels of transparency proportional
#to the ROCSS value in each grid point.
#We can set the argument score = TRUE for this.
#In the legend, red = above (size: 100% likelihood), blue = below (50%), yellow = normal(75%)
bubblePlot(hindcast, obs, forecast = forecast,
           bubble.size = 1,
           subtitle = subtitle,
           size.as.probability = TRUE,
           score = TRUE)

#to mask out areas where the forecast cannot be trusted
#set ROCSS score.range inside bubblePlot().
#allow user to focus on areas that have more confidence
bubblePlot(hindcast, obs, forecast = forecast,
           bubble.size = 1,
           subtitle = subtitle,
           size.as.probability = TRUE,
           score = TRUE,
           score.range = c(0.5,1))

#Cropping to a specific region, e.g. Southeast Asia region????
crop.sea <- function(x) subsetGrid(x, lonLim = c(90, 135), latLim= c(-25,25))
hindcast.sea <- crop.sea(hindcast)
forecast.sea <- crop.sea(forecast)
obs.sea <- crop.sea(obs)

#then plot the following bubble plot for that specific region:
bubblePlot(hindcast.sea, obs.sea, forecast = forecast.sea,
           bubble.size = 1,
           subtitle = subtitle,
           size.as.probability = TRUE,
           score = TRUE)

#TERCILE PLOTS - use tercilePlot()
tercilePlot(hindcast.sea, obs.sea, forecast = forecast.sea, subtitle = subtitle)

#USING TERCILE PLOTS CONSIDERING FORECAST OF A SPECIFIC YEAR OF THE FORECASTS
#E.G. YEAR = 1998
#set forecast argument = NULL, and then
#select your target hindcast year as the hindcast of the forecast 
#"year.target = 1998"
tercilePlot(hindcast.sea, obs.sea, forecast = NULL, year.target = 1998, subtitle = subtitle)

#TERCILE BAR PLOTS - use tercileBarplot()
tercileBarplot(hindcast.sea, obs.sea, forecast = forecast.sea,
               score.threshold = 0.6,
               subtitle = subtitle)

#RELIABILITY CATEGORIES - use reliabilityCategories()
#it computes reliability categories for probabilistic forecasts
#n.bins = no. of probability bins considered
#labels are respectively for the no. of events
#cex0 = min. no of points shown in reliability diagram
#cex.scale = scaling factor for points sizes in reliability diagram (see help)
realiable.sea <- reliabilityCategories(hindcast = hindcast.sea,
                                       obs = obs.sea,
                                       n.events = 3,
                                       labels = c("Below", "Average", "Above"),
                                       n.bins = 5,
                                       cex0 = 0.5,
                                       cex.scale = 20)

#can also plot reliability categories via map plots
#not very useful when we locate a more specific region in the map???
realiable.sea.map <- reliabilityCategories(hindcast.sea,
                                           obs.sea,
                                           n.events = 3,
                                           labels = c("Below", "Average", "Above"),
                                           n.bins = 5,
                                           n.boot = 1000,
                                           conf.level = 0.9,
                                           region = AR5regions)

#spread plots for daily data
#using ECOMS-UDG (but this will take up to a few hours
#depending on network traffic, temporal and spatial res.)
