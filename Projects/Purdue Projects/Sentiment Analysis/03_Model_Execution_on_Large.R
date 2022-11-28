# Random forest has yielded the best results

large <- read.csv('LargeMatrix.csv')
large$id <- NULL

# inspecting the results
summary(iphoneRC)
str(iphoneRC)
iphoneRC$iphonesentiment <- as.factor(iphoneRC$iphonesentiment)

largePredictions <- predict(modelRC_RFv1, large)                                    
summary(largePredictions)

large$iphonesentiment <- largePredictions

str(large$iphonesentiment)

par(mfrow = c(1,1))
plot(large$iphonesentiment)
