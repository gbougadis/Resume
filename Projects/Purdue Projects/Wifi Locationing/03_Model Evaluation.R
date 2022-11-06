library(ggplot2)



# Model evaluation --------------------------------------------------------
# Model Eval for BUILDINGID 0
ModelData0 <- resamples(list(RF = rf0, C50 = c5_type0, KNN = knn_0))
summary(ModelData0)


# Model Eval for BUILDINGID 1
ModelData1 <- resamples(list(RF = rf1, C50 = c5_type1, KNN = knn_1))
summary(ModelData1)

# Model Eval for BUILDINGID 2
ModelData2 <- resamples(list(RF = rf2, C50 = c5_type2, KNN = knn_2))
summary(ModelData2)

# Create df for visualization
df_accuracy_mean <- data.frame(model = c("RF_0", "C5.0_0", "KNN_0"
                           , "RF_1", "C5.0_1", "KNN_1"
                           , "RF_2", "C5.0_2", "KNN_2")
                           , BUILDINGID = c("BUILDINGID 0", "BUILDINGID 0", "BUILDINGID 0",
                                            "BUILDINGID 1", "BUILDINGID 1", "BUILDINGID 1",
                                            "BUILDINGID 2", "BUILDINGID 2", "BUILDINGID 2")
                 , avg_accuracy = c(0.7345, 0.6943, 0.5272
                                    , 0.7374, 0.815, 0.6346
                                    , 0.7837, 0.731, 0.5902))


df_accuracy_kappa <- data.frame(model = c("RF_0", "C5.0_0", "KNN_0"
                                         , "RF_1", "C5.0_1", "KNN_1"
                                         , "RF_2", "C5.0_2", "KNN_2")
                               , BUILDINGID = c("BUILDINGID 0", "BUILDINGID 0", "BUILDINGID 0",
                                                "BUILDINGID 1", "BUILDINGID 1", "BUILDINGID 1",
                                                "BUILDINGID 2", "BUILDINGID 2", "BUILDINGID 2")
                               , avg_kappa = c(0.733, 0.693, 0.525
                                                  , 0.736, 0.814, 0.632
                                                  , 0.783, 0.73, 0.589))


# Note to self: Creating tables as done above is not ideal, but will work in this instance. Change later (not optimal).




# Visualizing accuracy
ggplot(df_accuracy_mean, 
       aes(x = model, y = avg_accuracy)) +
        geom_col(aes(fill = BUILDINGID), width = 0.7) + 
        theme_minimal() + 
        geom_text(aes(label = avg_accuracy, vjust = -0.5)) +
        labs(title = 'Model Accuracy Assessment')

# Visualizing Kappa
ggplot(df_accuracy_kappa, 
       aes(x = model, y = avg_kappa)) +
  geom_col(aes(fill = BUILDINGID), width = 0.7) + 
  theme_minimal() + 
  geom_text(aes(label = avg_kappa, vjust = -0.5)) +
  labs(title = 'Model Kappa Assessment')


# Plotting by iteration ---------------------------------------------------
plot(rf1)
plot(c5_type1)
plot(knn_1)

# Random forest variable importance BUILDINGID1
rf_varimportance <- varImp(rf1)
plot(rf_varimportance, top = 10)

# C5.0 variable importance BUILDINGID 1
c5_varimportance <- varImp(c5_type1)
plot(c5_varimportance, top = 10)

# KNN variable importance BUILDINGID 1
knn_varimportance <- varImp(knn_1)
plot(knn_varimportance, top = 10)
