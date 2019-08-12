#example from SantanderMetGroup on loadeR
#"Dataset definition and loading local grid data"
#1. Loading from a single file
#dataset below comes from NCEP/NCAR Reanalysis 1,
#period: 1961-2010
#domain: Iberian Peninsula

library(loadeR)

download.file("http://meteo.unican.es/work/loadeR/data/Iberia_NCEP.tar.gz", 
              destfile = "mydirectory/Iberia_NCEP.tar.gz")

# Extract files from the tar.gz file
untar("mydirectory/Iberia_NCEP.tar.gz", exdir = "mydirectory")
dir <- "mydirectory/Iberia_NCEP"

#list the files in the directory
pwd(dir)


