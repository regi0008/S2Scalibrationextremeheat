#Spatial Visualization
library(transformeR)
library(downscaleR)
library(visualizeR)
library(loadeR)
library(SpecsVerification)

#compute the climatic mean via climatology() to each member
#of the multi-member grid and apply spatialPlot
data(CFS_Iberia_tas)
clim <- climatology(CFS_Iberia_tas, by.member = TRUE)
spatialPlot(clim, backdrop.theme = "coastline", set.min = 5,set.max=15)

#to get a subset of members (e.g. only members 1 to 4):
spatialPlot(clim, backdrop.theme = "coastline", set.min = 5,set.max=15, zcol = 1:4)

#or regional focus
spatialPlot(clim, backdrop.theme = "coastline",
            set.min = 5,set.max=15,
            zcol = 1:4,
            scales = list(draw=TRUE))

#if it is ensemble mean, by.member should set to FALSE
clim <- climatology(CFS_Iberia_tas, by.member = FALSE)
spatialPlot(clim, scales = list(draw=TRUE), contour = TRUE, set.min = 5,set.max=15, main = "July Ensemble Mean (temperature)")

#to plot multigrids
data(NCEP_Iberia_psl)
#split data into monthly climo
monthly.clim <- lapply(getSeason(NCEP_Iberia_psl), function(x) {
  climatology(subsetGrid(NCEP_Iberia_psl, season = x))
})

mg <- do.call("makeMultiGrid",
              c(monthly.clim, skip.temporal.check = TRUE))

spatialPlot(mg, backdrop.theme = "coastline",
            names.attr = c("DEC","JAN","FEB"),
            main = "mean climo 1991-2010",
            scales=list(draw=TRUE))
            
