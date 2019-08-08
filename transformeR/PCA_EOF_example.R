library(transformeR)
library(downscaleR)
library(visualizeR)
library(loadeR)

#principal component analysis (PCA)
#aka emphirical orthogonal functions (EOF) analysis
#for downscaling

#in transformeR, the PCA/EOF analysis is undertaken by prinComp()
#and related methods such as gridFromPCs, plotEOF etc.

data(NCEP_Iberia_tas)
pred <- makeMultiGrid(NCEP_Iberia_tas)

#prinComp() performs PCA (principal component analysis) of grids, multigrids, multimember multigrids
#range of v.exp = (0,1]. Determines no. of EOFs to be retained
pca.pred <- prinComp(pred, v.exp = .90)

plotEOF(pca.pred, "tp")

#to plot the first two EOFs
spatialPlot(pca.pred, "tp", n.eofs = 2)

