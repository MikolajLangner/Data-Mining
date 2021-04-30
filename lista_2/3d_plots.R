library(datasets)
library(dplyr)
library(cluster)
library(wesanderson)
library(RColorBrewer)
library(rgl)
library(car)


# Zadanie 2 - PCA w 3D

data(state)
state <- as.data.frame(state.x77)
state$region <- state.region
state$division <- state.division
state.subset <- subset(state, select=-c(region, division))

after.pca <- prcomp(state.subset, retx=T, center=T, scale.=T)

scatter3d(x = after.pca$x[, 1], y = after.pca$x[, 2], z = after.pca$x[,3],
          groups = state$region, surface = FALSE, ellipsoid = TRUE, 
          xlab = 'PC1', ylab = 'PC2', zlab = 'PC3',
          surface.col = wes_palette("Darjeeling1")[1:4])


# Zadanie 3 - MDS w 3D

stars <- read.csv("Stars.csv")

stars <- sample_n(read.csv("Stars.csv"), 100)
stars$Color <- as.factor(stars$Color)
stars$Spectral_Class <- as.factor(stars$Spectral_Class)
data.for.mds <- subset(stars, select=-Type)

dist.matrix <- as.matrix(daisy(data.for.mds))

mds <- as.matrix(cmdscale(dist.matrix, k = 3))


scatter3d(x=mds[, 1], y=mds[, 2], z=mds[,3], surface=FALSE, 
          groups = as.factor(stars$Type), ellipsoid = TRUE, 
          xlab = "MDS1", ylab = "MDS2", zlab = "MDS3", 
          labels = parse0("", 1:100))
