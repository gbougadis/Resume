# SVM Model ---------------------------------------------------------------

# Loading environment
library(caret)
library(doParallel)

# Assigning cores to cluster
clust <- makeCluster(4)
registerDoParallel(clust)
getDoParWorkers()

# Training data for SVM
train_control <- trainControl(method = "repeatedcv", 
                              number = 10, 
                              allowParallel = T, 
                              verboseIter = T)

#Creating data partition
training_part0 <- createDataPartition(type0$unique_id, p = .70, list = FALSE)
train_type0 <- type0[training_part0,]
test_type0 <- type0[-training_part0,]


training_part1 <- createDataPartition(type1$unique_id, p = .70, list = FALSE)
train_type1 <- type1[training_part1,]
test_type1 <- type1[-training_part1,]


training_part2 <- createDataPartition(type2$unique_id, p = .70, list = FALSE)
train_type2 <- type2[training_part2,]
test_type2 <- type2[-training_part2,]



# Random Forest Training and Prediction ---------------------------------------------
# BUILDINGID 0 using Random Forest in caret
rf0 <- caret::train(unique_id ~., 
              data = train_type0, 
              method = "rf", 
              trControl = train_control,  
              preProcess = c("zv", "center","scale"))
print(rf0)
predict_rf0 <- predict(rf0, test_type0)
# Optimal number of predictors was 145
confusion0_rf <- confusionMatrix(predict_rf0, as.factor(test_type0$unique_id))
postResample(predict_rf0, test_type0$unique_id)



# BUILDINGID 1 using Random Forest in caret
rf1 <- train(unique_id ~., 
                data = train_type1, 
                method = "rf", 
                trControl = train_control,  
                preProcess = c("zv", "center","scale"))
print(rf1)
predict_rf1 <- predict(rf1, test_type1)
confusion_rf1 <- confusionMatrix(predict_rf1, as.factor(test_type1$unique_id))
postResample(predict_rf1, test_type1$unique_id)



# BUILDINGID 2 using Random Forest in caret
rf2 <- train(unique_id ~., 
              data = train_type2,
              method = "rf", 
              trControl = train_control,  
              preProcess = c("zv", "center","scale"))
print(rf2)
predict_rf2 <- predict(rf2, test_type2)
confusion_rf2 <- confusionMatrix(predict_rf2, as.factor(test_type2$unique_id))
postResample(predict_rf2, test_type2$unique_id)







# KNN Model Assessment ---------------------------------------------------------------------
# BUILDING ID 0 KNN
knn_0 <- caret::train(unique_id ~., 
               data = train_type0, 
               method = "knn", 
               trControl = train_control)
print(knn_0)
predict_knn0 <- predict(knn_0, test_type0)
confusion0_knn <- confusionMatrix(predict_knn0, as.factor(test_type0$unique_id))
confusion0_knn
postResample(predict_knn0, test_type0$unique_id)


# BUILDINGID 1 KNN
knn_1 <- caret::train(unique_id ~., 
               data = train_type1, 
               method = "knn", 
               trControl = train_control)
print(knn_1)
predict_knn1 <- predict(knn_1, test_type1)
confusion1_knn <- confusionMatrix(predict_knn1, as.factor(test_type1$unique_id))
confusion1_knn
postResample(predict_knn1, test_type1$unique_id)


# BUILDINGID 2 KNN
knn_2 <- caret::train(unique_id ~., 
               data = train_type2, 
               method = "knn", 
               trControl = train_control)
print(knn_2)
predict_knn2 <- predict(knn_2, test_type2)
confusion2_knn <- confusionMatrix(predict_knn2, as.factor(test_type2$unique_id))
confusion2_knn
postResample(predict_knn2, test_type2$unique_id)



# KNN Summary





# C5.0 --------------------------------------------------------------------

# BUILDINGID 0
c5_type0 <- caret::train(unique_id ~., 
                  data = train_type0, 
                  method = "C5.0", 
                  trControl = train_control)
print(c5_type0)
predict_c5_type0 <- predict(c5_type0, test_type0)
confusion0_c5 <- confusionMatrix(predict_c5_type0, as.factor(test_type0$unique_id))
confusion0_c5
postResample(predict_c5_type0, test_type0$unique_id)


# BUILDINGID 1 C5.0
c5_type1 <- caret::train(unique_id ~., 
                  data = train_type1, 
                  method = "C5.0", 
                  trControl = train_control)
print(c5_type1)
predict_c5_type1 <- predict(c5_type1, test_type1)
confusion1_c5 <- confusionMatrix(predict_c5_type1, as.factor(test_type1$unique_id))
confusion0_c5
postResample(predict_c5_type1, test_type1$unique_id)


# BUILDINGID 2 C5.0
c5_type2 <- caret::train(unique_id ~., 
                  data = train_type2, 
                  method = "C5.0", 
                  trControl = train_control,  
                  preProcess = c("zv", "center","scale"))
print(c5_type2)
predict_c5_type2 <- predict(c5_type2, test_type2)
confusion2_c5 <- confusionMatrix(predict_c5_type2, as.factor(test_type2$unique_id))
confusion2_c5
postResample(predict_c5_type2, test_type2$unique_id)



