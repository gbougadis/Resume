set.seed(69)

# Original df modeling
iphone$iphonesentiment<- as.factor(iphone$iphonesentiment)
trainingSplit <- createDataPartition(iphone$iphonesentiment, p = 0.70, list = F)
iphone_train <- iphone[trainingSplit, ]
iphone_test <- iphone[-trainingSplit, ]

# iPhone Clean Train/Test Split
iphoneClean$iphonesentiment<- as.factor(iphone$iphonesentiment)
trainingCleanSplit <- createDataPartition(iphoneClean$iphonesentiment, p = 0.70, list = F)
iphoneClean_train <- iphoneClean[trainingCleanSplit, ]
iphoneClean_test <- iphoneClean[-trainingCleanSplit, ]

# recoded Train/Test Split
RCSplit <- createDataPartition(iphoneRC$iphonesentiment, p = 0.70, list = F)
iphoneRC_train <- iphoneRC[RCSplit, ]
iphoneRC_test <- iphoneRC[-RCSplit, ]

# RFE Train/Test Split
RFESplit <- createDataPartition(iphoneRFE$iphonesentiment, p = 0.70, list = F)
iphoneRFE_train <- iphoneRFE[RFESplit, ]
iphoneRFE_test <- iphoneRFE[-RFESplit, ]

train_control <- trainControl(method = "repeatedcv", 
                              number = 10, 
                              repeats = 3,
                              allowParallel = T, 
                              verboseIter = T)


# PCA on Training Data ----------------------------------------------------
preprocessParams <- preProcess(iphone_train[,-59],
                      method = c('center', 'scale', 'pca'),
                      thresh = 0.90)

print(preprocessParams)


# using predict function to apply pca paramets, create training, and exclude dependant variables
train.pca <- predict(preprocessParams, iphone_train[,-59])

# adding the dependent variable to the training df
train.pca$iphonesentiment <- iphone_train$iphonesentiment

test.pca <- predict(preprocessParams, iphone_test[,-59])
test.pca$iphonesentiment <- iphone_test$iphonesentiment

# results
str(train.pca)
str(test.pca)



#  C5.0 Training ----------------------------------------------------------
# Full Data training
modelC5v1 <- train(iphonesentiment ~.
                   , data = iphone_train
                   , method = 'C5.0'
                   , trControl = train_control)
predict_C5v1 <- predict(modelC5v1, iphone_test)
confusion_C5v1 <- confusionMatrix(predict_C5v1, as.factor(iphone_test$iphonesentiment))
confusion_C5v1


# Training on clean dataframe (12 variables)
cleanmodelC5v1 <- train(iphonesentiment ~.
                        , data = iphoneClean_train
                        , method = 'C5.0'
                        , trControl = train_control)
predictClean_C5v1 <- predict(cleanmodelC5v1, iphoneClean_test)
confusionClean_C5v1 <- confusionMatrix(predictClean_C5v1, as.factor(iphoneClean_test$iphonesentiment))
confusionClean_C5v1


# Training on RC df
modelRC_C5v1 <- train(iphonesentiment ~.
                         , data = iphoneRC_train
                         , method = 'C5.0'
                         , trControl = train_control)
predictRC_C5v1 <- predict(modelRC_C5v1, iphoneRC_test)
confusionRC_C5v1 <- confusionMatrix(predictRC_C5v1, as.factor(iphoneRC_test$iphonesentiment))
confusionRC_C5v1


# Training on RFE df
modelRFE_C5v1 <- train(iphonesentiment ~.
                      , data = iphoneRFE_train
                      , method = 'C5.0'
                      , trControl = train_control)
predictRFE_C5v1 <- predict(modelRFE_C5v1, iphoneRFE_test)
confusionRFE_C5v1 <- confusionMatrix(predictRFE_C5v1, as.factor(iphoneRFE_test$iphonesentiment))
confusionRFE_C5v1


# Training on PCA df
modelPCA_C5v1 <- train(iphonesentiment ~.
                       , data = train.pca
                       , method = 'C5.0'
                       , trControl = train_control)
predictPCA_C5v1 <- predict(modelPCA_C5v1, test.pca)
confusionPCA_C5v1 <- confusionMatrix(predictPCA_C5v1, as.factor(test.pca$iphonesentiment))
confusionPCA_C5v1

# Random Forest Training --------------------------------------------------
modelRFv1 <- train(iphonesentiment ~.
                   , data = iphone_train
                   , method = 'rf'
                   , trControl = train_control)
predict_RFv1 <- predict(modelRFv1, iphone_test)
confusion_RFv1 <- confusionMatrix(predict_RFv1, as.factor(iphone_test$iphonesentiment))
confusion_RFv1


# Training on clean dataframe (12 variables)
RCModelRFv1 <- train(iphonesentiment ~.
                        , data = iphoneClean_train
                        , method = 'rf'
                        , trControl = train_control)
RCModel_RFv1 <- predict(cleanmodelRFv1, iphoneClean_test)
RCModel_RFv1 <- confusionMatrix(predictClean_RFv1, as.factor(iphoneClean_test$iphonesentiment))
RCModel_RFv1


# Training on RC df
modelRC_RFv1 <- train(iphonesentiment ~.
                      , data = iphoneRC_train
                      , method = 'rf'
                      , trControl = train_control)
predictRC_RFv1 <- predict(modelRC_RFv1, iphoneRC_test)
confusionRC_RFv1 <- confusionMatrix(predictRC_RFv1, as.factor(iphoneRC_test$iphonesentiment))
confusionRC_RFv1


# Training on RFE df
modelRFE_RFv1 <- train(iphonesentiment ~.
                       , data = iphoneRFE_train
                       , method = 'rf'
                       , trControl = train_control)
predictRFE_RFv1 <- predict(modelRFE_RFv1, iphoneRFE_test)
confusionRFE_RFv1 <- confusionMatrix(predictRFE_RFv1, as.factor(iphoneRFE_test$iphonesentiment))
confusionRFE_RFv1


# Training on PCA df
modelPCA_RFv1 <- train(iphonesentiment ~.
                       , data = train.pca
                       , method = 'rf'
                       , trControl = train_control)
predictPCA_RFv1 <- predict(modelPCA_RFv1, test.pca)
confusionPCA_RFv1 <- confusionMatrix(predictPCA_RFv1, as.factor(test.pca$iphonesentiment))
confusionPCA_RFv1

# SVM Model Training ------------------------------------------------------
# e1071 package
library(e1071)
modelSVMv1 <- svm(formula = iphonesentiment ~ ., data = iphone_train, method = "C-classification")
predict_SVMv1 <- predict(modelSVMv1, iphone_test)
confusion_SVMv1 <- confusionMatrix(predict_SVMv1, as.factor(iphone_test$iphonesentiment))
confusion_SVMv1

# Training on clean dataframe (12 variables)
cleanmodelSVMv1 <- svm(formula = iphonesentiment ~ ., data = iphoneClean_train, method = "C-classification")
predictClean_SVMv1 <- predict(cleanmodelSVMv1, iphoneClean_test)
confusionClean_SVMv1 <- confusionMatrix(predictClean_SVMv1, as.factor(iphoneClean_test$iphonesentiment))
confusionClean_SVMv1


# Training on RC df
modelRC_SVMv1 <- svm(formula = iphonesentiment ~ ., data = iphoneRC_train, method = "C-classification")
predictRC_SVMv1 <- predict(modelRC_SVMv1, iphoneRC_test)
confusionRC_SVMv1 <- confusionMatrix(predictRC_SVMv1, as.factor(iphoneRC_test$iphonesentiment))
confusionRC_SVMv1

# Training on RFE df
modelRFE_SVMv1 <- svm(formula = iphonesentiment ~ ., data = iphoneRFE_train, method = "C-classification")
predictRFE_SVMv1 <- predict(modelRFE_SVMv1, iphoneRFE_test)
confusionRFE_SVMv1 <- confusionMatrix(predictRFE_SVMv1, as.factor(iphoneRFE_test$iphonesentiment))
confusionRFE_SVMv1


# Train on PCA df
modelPCA_SVMv1 <- svm(formula = iphonesentiment ~ ., data = train.pca, method = "C-classification")
predictPCA_SVMv1 <- predict(modelPCA_SVMv1, test.pca)
confusionPCA_SVMv1 <- confusionMatrix(predictPCA_SVMv1, as.factor(test.pca$iphonesentiment))
confusionPCA_SVMv1


# kknn Model Training -----------------------------------------------------
library(kknn)
modelKNNv1 <- train(iphonesentiment ~.
      , data = iphone_train
      , method = "kknn"
      , trControl=train_control
      , preProcess = c("center", "scale")
      , tuneLength = 10)
predict_KNNv1 <- predict(modelKNNv1, iphone_test)
confusion_KNNv1 <- confusionMatrix(predict_KNNv1, as.factor(iphone_test$iphonesentiment))
confusion_KNNv1


# Training on clean dataframe (12 variables)
modelCleanKNNv1 <- train(iphonesentiment ~.
                    , data = iphoneClean_train
                    , method = "kknn"
                    , trControl=train_control
                    , preProcess = c("center", "scale")
                    , tuneLength = 10)
predictClean_KNNv1 <- predict(modelCleanKNNv1, iphoneClean_test)
confusionClean_KNNv1 <- confusionMatrix(predictClean_KNNv1, as.factor(iphoneClean_test$iphonesentiment))
confusionClean_KNNv1


# Training on RC df
modelRC_KNNv1 <- train(iphonesentiment ~.
                      , data = iphoneRC_train
                      , method = 'kknn'
                      , trControl = train_control)
predictRC_KNNv1 <- predict(modelRC_KNNv1, iphoneRC_test)
confusionRC_KNNv1 <- confusionMatrix(predictRC_KNNv1, as.factor(iphoneRC_test$iphonesentiment))
confusionRC_KNNv1


# Training on RFE df
modelRFE_KNNv1 <- train(iphonesentiment ~.
                       , data = iphoneRFE_train
                       , method = 'kknn'
                       , trControl = train_control)
predictRFE_KNNv1 <- predict(modelRFE_KNNv1, iphoneRFE_test)
confusionRFE_KNNv1 <- confusionMatrix(predictRFE_KNNv1, as.factor(iphoneRFE_test$iphonesentiment))
confusionRFE_KNNv1


# Train on PCA df
modelPCA_KNNv1 <- train(iphonesentiment ~.
                        , data = train.pca
                        , method = 'kknn'
                        , trControl = train_control)
predictPCA_KNNv1 <- predict(modelPCA_KNNv1, test.pca)
confusionPCA_KNNv1 <- confusionMatrix(predictPCA_KNNv1, as.factor(test.pca$iphonesentiment))
confusionPCA_KNNv1


# postResampling ----------------------------------------------------------
# Full Data
postResample(predict_C5v1, iphone_test$iphonesentiment)
postResample(predict_RFv1, iphone_test$iphonesentiment)
postResample(predict_SVMv1, iphone_test$iphonesentiment)
postResample(predict_KNNv1, iphone_test$iphonesentiment)

# Cleaned model
postResample(predictClean_C5v1, iphoneClean_test$iphonesentiment)
postResample(predictClean_RFv1, iphoneClean_test$iphonesentiment)
postResample(predictClean_SVMv1, iphoneClean_test$iphonesentiment)
postResample(predictClean_KNNv1, iphoneClean_test$iphonesentiment)


# rc Model Results
postResample(predictRC_C5v1, iphoneRC_test$iphonesentiment)
postResample(predictRC_RFv1, iphoneRC_test$iphonesentiment)
postResample(predictRC_SVMv1, iphoneRC_test$iphonesentiment)
postResample(predictRC_KNNv1, iphoneRC_test$iphonesentiment)

# RFE model
postResample(predictRFE_C5v1, iphoneRFE_test$iphonesentiment)
postResample(predictRFE_RFv1, iphoneRFE_test$iphonesentiment)
postResample(predictRFE_SVMv1, iphoneRFE_test$iphonesentiment)
postResample(predictRFE_KNNv1, iphoneRFE_test$iphonesentiment)

# PCA model results
postResample(predictPCA_C5v1, test.pca$iphonesentiment)
postResample(predictPCA_RFv1, test.pca$iphonesentiment)
postResample(predictPCA_SVMv1, test.pca$iphonesentiment)
postResample(predictPCA_KNNv1, test.pca$iphonesentiment)


# **** Galaxy Training Below **** -----------------------------------------



# Galaxy Predictions ------------------------------------------------------
# Original df modeling
galaxy$galaxysentiment<- as.factor(galaxy$galaxysentiment)
trainingGalSplit <- createDataPartition(galaxy$galaxysentiment, p = 0.70, list = F)
galaxy_train <- galaxy[trainingGalSplit, ]
galaxy_test <- galaxy[-trainingGalSplit, ]


# recoded Train/Test Split
galaxyRC$galaxysentiment<- as.factor(galaxyRC$galaxysentiment)
RCSplitGal <- createDataPartition(galaxyRC$galaxysentiment, p = 0.70, list = F)
GalRC_train <- galaxyRC[RCSplitGal, ]
GalRC_test <- galaxyRC[-RCSplitGal, ]



#  C5.0 Galaxy Training ----------------------------------------------------------
# Galaxy Full Data training
modelGalC5v1 <- train(galaxysentiment ~.
                   , data = galaxy_train
                   , method = 'C5.0'
                   , trControl = train_control)
predict_GalaxyC5v1 <- predict(modelGalC5v1, galaxy_test)
confusion_GalC5v1 <- confusionMatrix(predict_GalaxyC5v1, as.factor(galaxy_test$galaxysentiment))
confusion_GalC5v1


# recoded Galaxy Full Data Training
modelGalRC_C5v1 <- train(galaxysentiment ~.
                         , data = GalRC_train
                         , method = 'C5.0'
                         , trControl = train_control)
predict_GalaxyRC_C5v1 <- predict(modelGalRC_C5v1, GalRC_test)
confusion_GalRC_C5v1 <- confusionMatrix(predict_GalaxyRC_C5v1, as.factor(GalRC_test$galaxysentiment))
confusion_GalRC_C5v1



# Random Forest Galaxy Training --------------------------------------------------
modelGalRFv1 <- train(galaxysentiment ~.
                   , data = galaxy_train
                   , method = 'rf'
                   , trControl = train_control)
predict_GalRFv1 <- predict(modelGalRFv1, galaxy_test)
confusion_GalRFv1 <- confusionMatrix(predict_GalRFv1, as.factor(galaxy_test$galaxysentiment))
confusion_RFv1


# recoded Galaxy Full Data Training
modelGalRC_RFv1 <- train(galaxysentiment ~.
                         , data = GalRC_train
                         , method = 'rf'
                         , trControl = train_control)
predict_GalaxyRC_RFv1 <- predict(modelGalRC_RFv1, GalRC_test)
confusion_GalRC_RFv1 <- confusionMatrix(predict_GalaxyRC_RFv1, as.factor(GalRC_test$galaxysentiment))
confusion_GalRC_RFv1


# SVM Galaxy Model Training ------------------------------------------------------
modelGalSVMv1 <- svm(formula = galaxysentiment ~ ., data = galaxy_train, method = "C-classification")
predict_GalSVMv1 <- predict(modelGalSVMv1, galaxy_test)
confusion_GalSVMv1 <- confusionMatrix(predict_GalSVMv1, as.factor(galaxy_test$galaxysentiment))
confusion_GalSVMv1


# recoded Galaxy Full Data Training
modelGalSVM_RFv1 <- svm(formula = galaxysentiment ~ ., data = GalRC_train, method = "C-classification")
predict_GalaxyRC_SVMv1 <- predict(modelGalSVM_RFv1, GalRC_test)
confusion_GalRC_SVMv1 <- confusionMatrix(predict_GalaxyRC_SVMv1, as.factor(GalRC_test$galaxysentiment))
confusion_GalRC_SVMv1

