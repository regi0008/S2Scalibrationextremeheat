library(loadeR)
library(visualizeR)
library(loadeR.2nc)
library(ncdf4)

data(NCEP_Iberia_tas)
obs = NCEP_Iberia_tas

#name of output file
fileName <- "NCEP_Iberia_tas.nc"

#include a global attribute:
globalAttributeList <- list("institution" = "SantanderMetGroup, http://www.meteo.unican.es/")

#include two variable attribute:
varAttributeList <- list(var_attr1 = "one_attribute", var_attr2 = "another_attribute")

#create file
grid2nc(obs,
        NetCDFOutFile = fileName,
        missval = -999,
        prec = "float",
        globalAttributes = globalAttributeList,
        varAttributes = varAttributeList,
        shuffle = TRUE,
        verbose = TRUE)