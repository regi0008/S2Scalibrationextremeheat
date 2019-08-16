#aggregate hindcast dataset into one
#makeAggregatedDataset(source.dir, ncml.file, file.ext = "nc",
#                      aggr.dim = "time", pattern = NULL, recursive = FALSE,
#                      verbose = TRUE, timeUnitsChange = TRUE)


#load necessary libraries
library(loadeR)
library(downscaleR)
library(transformeR)
library(ncdf4)

dir <- file.path("C:/Users/regin/Desktop/R/Datasets_for_project/hindcast_dataset")
list.files(dir, pattern = "\\.nc$")

#makeAggregatedDataset() returns a NcML file at a specified location
#from multiple files aggregated together along a selected dimension.
#!!current problem below returned!!:
#error in file(ncml.file, "w"): cannot open the connection, cannot open ncml file.
makeAggregatedDataset(source.dir = dir,
                      ncml.file = "C:/Users/regin/Desktop/R/hindcast_dataset/hindcast.ncml",
                      aggr.dim = "2 metre temperature",
                      verbose = TRUE)

di <- dataInventory(dataset = "C:/Users/regin/Desktop/R/hindcast_dataset/ncml_test.ncml")

#names(di)