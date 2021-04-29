library(datasets)
library(dplyr)
library(MASS)
library(cluster)
library(DataExplorer)
library(ggplot2)
library(ggbeeswarm)
library(ggfortify)
library(ggbiplot)
library(ggcorrplot)
library(wesanderson)
library(cowplot)
library(EnvStats)
library(arules)
library(e1071)
library(kableExtra)
library(reshape2)
library(rgl)
library(car)


data(state)
state <- as.data.frame(state.x77)
state$region <- state.region
state$division <- state.division
state.subset <- subset(state, select=-c(region, division))


after.pca <- prcomp(state.subset, retx=T, center=T, scale.=T)


scatter3d(x=after.pca$x[, 1], y=after.pca$x[, 2], z=after.pca$x[,3], surface=FALSE, 
          groups = as.factor(state$region), ellipsoid = TRUE)
s3d <- scatter3d(x=after.pca$x[, 1], y=after.pca$x[, 2], z=after.pca$x[,3], surface=FALSE, 
                groups = as.factor(state$region), ellipsoid = TRUE)
text(s3d$xyz.convert(after.pca$x[, 1:3]), labels = rownames(state))
