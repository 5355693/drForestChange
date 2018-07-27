library(raster)
library(rgdal)

#Import the multi-band
predictors <- stack("lossImageLCMasked.tif")
predictors

#Rename the bands
names(predictors) <- c("forestLoss", "landCover","elevation", "slope", "friction")
hist(predictors[[2]])
#Take a look
plot(predictors) # all bands

#Look at one band within the country boundaries:
library(maptools) # to get map data
data(wrld_simpl)
plot(predictors,3)
plot(wrld_simpl, add = T)

# The LC band includes 0 values (the mask value), so we need to create a mask so that the 
# stack includes only cells with LC values of 4 or 6
lcMask <- predictors[[2]]
lcMask[(lcMask != 4) & (lcMask != 6)] <- NA
lcMask
cols <- rainbow(2)
plot(lcMask, col = cols)

#Mask the predictors by the mask, giving us only the 2 cover types of interest
predictorsMasked <- mask(predictors,lcMask)

#generate training cells by drawing a random sample stratified by forest loss.
#this generates a matrix of 500 cells in each type.
trainingPoints <- sampleStratified(x = predictorsMasked[[1]], size = 500, exp = 100, na.rm = TRUE,
                                   xy = F, ext = NULL, sp = TRUE)
plot(predictorsMasked[[2]])
plot(trainingPoints, add = T)

trainingRaster <- rasterize(trainingPoints, predictorsMasked, field = "forestLoss") #rasterize the training points

covmasked <- mask(predictorsMasked, trainingRaster) #create a raster of only the training points ()
hist(covmasked[[2]])

#create a dataframe of training data:
valuetable <- getValues(covmasked)
valuetable <- na.omit(valuetable)
valuetable <- as.data.frame(valuetable)
summary(valuetable)
valuetable$forestLoss <- factor(valuetable$forestLoss, levels = c(0,1))
valuetable$landCover <- factor(valuetable$landCover, levels = c(4,6))
summary(valuetable)
boxplot(elevation ~ forestLoss, data = valuetable)
boxplot(slope ~ forestLoss, data = valuetable)
boxplot(friction ~ forestLoss, data = valuetable)
xtabs(~landCover+forestLoss, data = valuetable)

## Construct a random forest model
# Covariates (x) are found in columns 1 to 5 of valuetable
# Training classes (y) are found in the 'class' column of valuetable
## Caution: this step takes fairly long!
# but can be shortened by setting importance=FALSE

# Adjusting mtry and ntree (up to 10000) doesn't make much difference in error rates.
library(randomForest)
modelRF <- randomForest(x=valuetable[ ,c(2:5)], y=valuetable$forestLoss,
                        ntree = 3000,
                        proximity = T,
                        importance = TRUE)
## Inspect the structure and element names of the resulting model
modelRF
names(modelRF)
## Inspect the confusion matrix of the OOB error assessment: ~42% OOB Error w/1000 training points
modelRF$confusion
# to make the confusion matrix more readable
colnames(modelRF$confusion) <- c("No forest loss", "Forest loss", "class.error")
rownames(modelRF$confusion) <- c("No forest loss", "Forest loss")
modelRF$confusion
varImpPlot(modelRF)

predForestLoss <- predict(predictorsMasked, model=modelRF, na.rm=TRUE, progress = "text", filename = "predictedForestLoss")
writeRaster(predForestLoss,"predictedForestLoss", format = "GTiff", progress = "text")
cols <- cm.colors(10)
plot(predForestLoss, col = cols)

## See if we can improve performance with more training points:
#generate training cells by drawing a random sample stratified by forest loss.
#this generates a matrix of 500 cells in each type.
trainingPoints1000 <- sampleStratified(x = predictorsMasked[[1]], size = 1000, exp = 100, na.rm = TRUE,
                                   xy = F, ext = NULL, sp = TRUE)

trainingRaster1000 <- rasterize(trainingPoints1000, predictorsMasked, field = "forestLoss") #rasterize the training points

covmasked1000 <- mask(predictorsMasked, trainingRaster1000) #create a raster of only the training points ()

#create a dataframe of training data:
valuetable1000 <- getValues(covmasked1000)
valuetable1000 <- na.omit(valuetable1000)
valuetable1000 <- as.data.frame(valuetable1000)
summary(valuetable1000)
valuetable1000$forestLoss <- factor(valuetable1000$forestLoss, levels = c(0,1))
valuetable1000$landCover <- factor(valuetable1000$landCover, levels = c(4,6))
summary(valuetable1000)

boxplot(elevation ~ forestLoss, data = valuetable1000)
boxplot(slope ~ forestLoss, data = valuetable1000)
boxplot(friction ~ forestLoss, data = valuetable1000)
xtabs(~landCover+forestLoss, data = valuetable1000)

## Construct a random forest model
# Covariates (x) are found in columns 1 to 5 of valuetable
# Training classes (y) are found in the 'class' column of valuetable
## Caution: this step takes fairly long!
# but can be shortened by setting importance=FALSE

# Adjusting mtry and ntree (up to 10000) doesn't make much difference in error rates.
library(randomForest)
modelRF1000 <- randomForest(x=valuetable1000[ ,c(2:5)], y=valuetable1000$forestLoss,
                        ntree = 3000,
                        proximity = T,
                        importance = TRUE)
## Inspect the structure and element names of the resulting model
modelRF1000
modelRF1000$proximity

## Inspect the confusion matrix of the OOB error assessment: ~46% OOB Error w/2000 training points
modelRF$confusion
varImpPlot(modelRF1000)
