library(DataExplorer)
library(datasets)
library(dplyr)
library(tidyr)
library(xtable)
library(ggplot2)
library(reshape2)
library(wesanderson)
library(e1071)
library(caret)
library(mlbench)
library(class)
library(rpart)
library(rpart.plot)
library(ipred)


data(wine)
n <- dim(wine)[1]
variable.number <- ncol(wine)
observations.number <- nrow(wine)
NaN.number <- sum(is.na(wine))
class.number <- length(unique(wine$Type))


set.seed(42)
train.index <- sample(n, 2/3 * n)
train.data <- wine %>% slice(train.index)
test.data <- wine %>% slice(-train.index)
train.subset <- data.frame(train.data[, c(1, 2, 7, 8)])
test.subset <- data.frame(test.data[, c(1, 2, 7, 8)])

train.etiquettes <- train.data$Type
test.etiquettes <- test.data$Type
subset.train.etiquettes <- train.subset$Type
subset.test.etiquettes <- test.subset$Type

cv <- trainControl(method="cv", number=5)


model <- train(Type ~ Alcohol + Flavanoids, data = train.subset, method = "knn", trControl = cv)

tuned.knn.test.pred <- predict(model, test.data)
tuned.knn.train.pred <- predict(model, train.data)

tuned.predictions <- predict(model, test.subset)

var1 <- wine$Alcohol
var2 <- wine$Flavanoids

var3 <- test.subset$Alcohol
var4 <- test.subset$Flavanoids
tuned.predictions <- predict(model, test.subset)

test.confusion <- table(tuned.predictions, test.etiquettes)

x.test <- expand.grid(Alcohol = seq(min(var1), max(var1), by = 0.1), 
                      Flavanoids = seq(min(var2), max(var2), by=0.1))

x.test.pred <- predict(model, x.test)
x.test.prob <- predict(model, x.test,type = "prob")

mat <- cbind(seq(1, 1872, by=1), sapply(as.numeric(x.test.pred), max))
x.test.prob <- x.test.prob[mat]

df1 <- data.frame(x.test, class = x.test.pred,
                  prob = x.test.prob)
df2 <- data.frame(x = var3, y = var4, class = tuned.predictions)

ggplot() +
  geom_point(aes(x=Alcohol, y=Flavanoids, col = class), data = df1, size = 1) + 
  geom_point(aes(x = x, y = y, col = class), data = df2, size = 4.5, shape = 1) + 
  theme_bw() +
  geom_contour(aes(x=Alcohol, y=Flavanoids, z = df1$prob, col = class), 
                   data = df1, size = 1, bins = 1)

prob15 <- matrix(x.test.prob, length(df1$Alcohol), length(df1$Flavanoids))
contour(df1$Alcohol, df1$Flavanoids, prob15, labels="", xlab="", ylab="", main=
          "15-nearest neighbour", axes=FALSE)



