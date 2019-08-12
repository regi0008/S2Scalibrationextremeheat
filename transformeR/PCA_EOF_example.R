library(transformeR)
library(downscaleR)
library(visualizeR)
library(loadeR)
library(SpecsVerification)

#principal component analysis (PCA)
#aka emphirical orthogonal functions (EOF) analysis

#in transformeR, the PCA/EOF analysis is undertaken by prinComp()
#and related methods such as gridFromPCs, plotEOF etc.

data(NCEP_Iberia_tas)
pred <- makeMultiGrid(NCEP_Iberia_tas)

pca.pred <- prinComp(pred, v.exp = .90)

plotEOF(pca.pred, "tp")

#to plot the first two EOFs
spatialPlotEOF(pca.pred, "tp", n.eofs = 2)

