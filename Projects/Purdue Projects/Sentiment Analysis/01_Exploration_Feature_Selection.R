# Gavin Bougadis
# Nov 11, 2022


# Environment -------------------------------------------------------------
library(doParallel)
library(caret)
library(dplyr)
library(tidyr)
library(readr)
library(MASS)
library(ggplot2)
library(GGally)
library(plotly)
library(corrplot)
library(caret)
library(tibble)

detectCores()
cluster <- makeCluster(5)
registerDoParallel(cluster)
getDoParWorkers()

galaxy <- read.csv('galaxy_smallmatrix_labeled_9d.csv')
iphone <- read.csv('iphone_smallmatrix_labeled_8d.csv')
large <- read.csv('LargeMatrix.csv')



# Data Cleansing ----------------------------------------------------------
galaxy$galaxysentiment <- as.factor(galaxy$galaxysentiment)



# Exploring Galaxy DF -----------------------------------------------------
str(galaxy)
colnames(galaxy)

mean(galaxy$galaxysentiment)
mean(iphone$iphonesentiment)
# Negative mention of iphone display
mean(galaxy$iphonedisneg)
mean(galaxy$iphonecamneg)

#ggplot(galaxy, aes(x = galaxysentiment)) +
#  geom_bar(width=0.5, fill = "coral") +
#  geom_text(stat='count', aes(label=stat(count)), vjust=-0.5) +
#  theme_classic()

plot_ly(galaxy, x = ~galaxy$galaxysentiment, type = 'histogram')

summary(galaxy)


# Exploring iPhone DF -----------------------------------------------------
str(iphone)
colnames(iphone)

mean(iphone$iphonesentiment)

# Negative mention of iphone display
mean(galaxy$samsungdisneg)
mean(galaxy$samsungcamneg)

#ggplot(galaxy, aes(x = galaxysentiment)) +
#  geom_bar(width=0.5, fill = "coral") +
#  geom_text(stat='count', aes(label=stat(count)), vjust=-0.5) +
#  theme_classic()

plot_ly(x = ~iphone$iphonesentiment, type = 'histogram')
summary(iphone)



# Correlation -------------------------------------------------------------
options(max.print=1000000)
zero_var <- nearZeroVar(iphone, saveMetrics = FALSE)
zero_var
View(cor(large))
iphoneClean <- iphone[, -zero_var]
cor(iphoneClean)

View(cor(iphoneClean))

# Recursive Feature Selection
set.seed(101)
iphoneSample <- iphoneClean[sample(1:nrow(iphoneClean), 1000, replace = FALSE),]

rfeCtrl <- rfeControl(functions = rfFuncs,
                      method = 'repeatedcv',
                      repeats = 5,
                      verbose = F)

# omitting the response variable
# attribute 12 is iPhone sentiment
rfeResults <- rfe(iphoneSample[,1:11],
                  iphoneSample$iphonesentiment,
                  sizes = c(1:11),
                  rfeControl = rfeCtrl)

rfeResults
# Plot results
plot(rfeResults, type = c('g', 'o'))

iphoneRFE <- iphoneSample[, predictors(rfeResults)]
iphoneRFE$iphonesentiment <- iphoneSample$iphonesentiment
str(iphoneRFE)


# Ranking variable importance
imp <- varImp(rfeResults, scale = F)
imp <- as.data.frame(imp)

ggplot(data= imp, aes(x=reorder(rownames(imp), Overall),y=Overall)) +
  geom_bar(position="dodge",stat="identity",width = 0, color = "black") + 
  coord_flip() + geom_point(color='skyblue') + xlab(" Importance Score")+
  ggtitle("Variable Importance") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))

summary(iphoneRFE)     
# Visualizing distributions
par(mfrow = c(3,3))
for (cols in colnames(iphoneRFE)) {
  iphoneRFE %>% pull(cols) %>% hist()
}
# Normalize variables before trying polr regression model


# Changing data types
iphoneRFE$iphonesentiment <- as.factor(iphoneRFE$iphonesentiment)



# Engineering the dependant variable --------------------------------------
iphoneRC <- iphone
# recoding the sentiment to combine the factor levels 0,1 and 4,5 . We can combine to understand postivie and negative sentiment
iphoneRC$iphonesentiment <- recode(iphoneRC$iphonesentiment, '0' = 1
                                   , '1' = 1
                                   , '2' = 2
                                   , '3' = 3
                                   , '4' = 4
                                   , '5' = 4)
# inspecting the results
summary(iphoneRC)
str(iphoneRC)
iphoneRC$iphonesentiment <- as.factor(iphoneRC$iphonesentiment)




# Galaxy data prep --------------------------------------------------------
galaxyRC <- galaxy
# Recoding galaxysentiment
galaxyRC$galaxysentiment <-  recode(galaxyRC$galaxysentiment, '0' = 1
                                               , '1' = 1
                                               , '2' = 2
                                               , '3' = 3
                                               , '4' = 4
                                               , '5' = 4)




