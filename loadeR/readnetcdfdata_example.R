#load ncdf4 package
library(ncdf4)
library(raster)
library(ggplot2)

#set path and filename
ncpath <- "C:/Users/regin/Desktop/R/Datasets_for_project/ecmwf_hindcast_data_c3s_regine/temp/201902/"
#example: read hindcast member - 1st February 2015 (2m temperature)
ncname <- "2t_20150201"
ncfname <- paste(ncpath, ncname, ".nc", sep="")
dname <- "2T_GDS0_SFC"

#open a netCDF file using nc_open()
ncin <- nc_open(ncfname)
print(ncin)

#trying to read something here - temperature
tmp_array <- ncvar_get(ncin,dname)
dlname <- ncatt_get(ncin,dname,"long_name")
dunits <- ncatt_get(ncin,dname, "units")
fillvalue <- ncatt_get(ncin,dname, "_FillValue")
dim(tmp_array)

#get longitude and latitude
lon <- ncvar_get(ncin, "g0_lon_3")
nlon <- dim(lon)
head(lon)
lat <- ncvar_get(ncin, "g0_lat_2")
nlat <- dim(lat)
head(lat)
print(c(nlon,nlat))
#get time
time <- ncvar_get(ncin, "forecast_time1")
time
tunits <- ncatt_get(ncin, "time", "units")
nt <- dim(time)
nt

#check what's in the current workspace
ls()

#reshaping from raster to rectangular
library(chron)
library(lattice)
library(RColorBrewer)

#get a slice of data
m <- 1
2T_GDS0_SFC_slice <- 2T_GDS0_SFC_array[,,m]

#quick map
image(lon,lat,2T_GDS0_SFC_slice,col=rev(brewer.pal(10,"RdBu")))

#levelplot of the slice
grid <- expand.grid(lon=lon, lat=lat)
cutpts <- c(-50,-40,-30,-20,-10,0,10,20,30,40,50)
levelplot(dname ~ lon * lat, data=grid, at=cutpts, cuts=11,pretty=T)
col.regions=(rev(brewer.pal(10,"RdBu")))

#create dataframe -- reshape data
#matrix (nlon*nlat rows by 2 cols) of lons and lats
lonlat <- as.matrix(expand.grid(lon,lat))
dim(lonlat)

#vector of "2T_GDS0_SFC" values
2T_GDS0_SFC_vec <- as.vector(2T_GDS0_SFC_slice)
length(2T_GDS0_SFC_vec)

#create dataframe and add names
dname_df01 <- data.frame(cbind(lonlat,2T_GDS0_SFC_vec))
names(2T_GDS0_SFC_df01) <- c("lon", "lat", paste(dname, as.character(m), sep="_"))
head(na.omit(2T_GDS0_SFC_df01), 10)