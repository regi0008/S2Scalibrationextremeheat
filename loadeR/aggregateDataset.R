#aggregate hindcast dataset into one

#load necessary libraries
library(loadeR)
library(downscaleR)
library(transformeR)
library(ncdf4)

dir <- file.path("C:/Users/regin/Desktop/R/Datasets_for_project/ecmwf_hindcast_data_c3s_regine/temp/201902")
list.files(dir, pattern = "\\.nc$")

#current problem below
makeAggregatedDataset(source.dir = dir,
                      ncml.file = "C:/Users/regin/Desktop/R/temp/ncml_test.ncml",
                      verbose = TRUE)

di <- dataInventory(dataset = "C:/Users/regin/Desktop/R/temp/ncml_test.ncml")

#names(di)
#str(di$Q)