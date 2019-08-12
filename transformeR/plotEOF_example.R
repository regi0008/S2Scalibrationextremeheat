#plotEOF example
#under transformeR

library(loadeR)
library(downscaleR)
library(transformeR)
library(visualizeR)

#load winter temp. of period 1981-2010
data(NCEP_Iberia_hus850)
pca <- prinComp(NCEP_Iberia_hus850, v.exp = 0.9)
#plot of all EOFs
plotEOF(pca)
#plot first 4 EOFs
plotEOF(pca, n.eofs = 4, backdrop.theme = "coastline")
#plot only the second EOF
plotEOF(pca, zcol = 2, backdrop.theme = "coastline",
        main = "2nd EOF ('zcol = 2')")

#PCA Analysis of a multigrid (multiple variables)
#problems with var below
mg <- makeMultiGrid(NCEP_Iberia_hus850, NCEP_Iberia_ta850)
pca2 <- prinComp(mg, n.eofs = 9)
names(pca2)

plotEOF(pca2, "ta@@850", backdrop.theme = "coastline")