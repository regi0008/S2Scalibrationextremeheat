#Probalisitic visualization:
#Tercile based probability map using tercileMap()
#stratifies probabilities according to terciles and
#return map showing areas that have increased probability
#of being either below the lower tercile or above upper tercile


library(transformeR)
library(downscaleR)
library(visualizeR)
library(loadeR)

#using 9 members of CFSv2 seasonal pred. for winter temp.
#period = 1983:2001 as hindcast
#period = 2002 as forecast

data(CFS_Iberia_tas)
str(CFS_Iberia_tas)
hindcast <- subsetGrid(CFS_Iberia_tas, years = 1983:2001)
forecast <- subsetGrid(CFS_Iberia_tas, years = 2002)
tercileMap(hindcast,forecast)

tercileMap(hindcast, forecast, backdrop.theme = "countries", 
           main = list("CFS temperature 2002", cex = 0.5))