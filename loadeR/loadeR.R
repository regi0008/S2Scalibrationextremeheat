library(loadeR)
library(visualizeR)
# change the dir link only
dir <- "C:/users/Work/Desktop/Regine_project/ecmwf_hindcast_data_c3s_regine/temp/201902"
list.files(dir, pattern = ".grib$")

di <- dataInventory(file.path(dir, "2t_19930201.grib"))
str(di)

pr <- loadGridData(dataset = file.path(dir, "2t_19930201.grib"), var = "2_metre_temperature_surface")                  
str(pr)

#Plot the mean value of Feb, Mar, Apr and May 2019
spatialPlot(climatology(pr, clim.fun = list(FUN = mean, na.rm = T)), 
            backdrop.theme = "countries", 
            scales = list(draw = T), rev.colors = TRUE)
