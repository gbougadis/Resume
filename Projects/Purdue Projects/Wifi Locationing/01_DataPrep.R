# Goal --------------------------------------------------------------------
# Client would like us to investigate the feasibility of using "wifi fingerprinting" to determine 
# a person's location in indoor spaces. Wifi fingerprinting uses the signals from multiple wifi 
# hotspots within the building to determine location, analogously to how GPS uses satellite signals.


# Loading Environment -----------------------------------------------------
library(caret)
library(devtools)
library(dplyr)
library(doParallel)
library(tidyr)

training <- read.csv("trainingData.csv")
validation <- read.csv("validationData.csv")


# Data Cleansing ----------------------------------------------------------
building_vars <- c('BUILDINGID', 'FLOOR', 'SPACEID', 'RELATIVEPOSITION')

# PCA
no_buildingvars <- training %>% select(-one_of(building_vars))
buildingvars <- training %>% select(building_vars)
nzv <- nearZeroVar(no_buildingvars, saveMetrics = TRUE)
head(nzv)
tail(nzv)

print(paste('Range: ', range(nzv$percentUnique)))
dim(nzv[nzv$percentUnique > 0.1,])

# Filtering 0 Variance
training <- training[-which(apply(training, 2, var) == 0 )] 

# Filtering uniqueness
clean_train <- training[c(rownames(nzv[nzv$percentUnique > 0.1,]))]
clean_train <- cbind(clean_train, buildingvars)

# Filtering variance

# Create a an identifier that combines building, floor, and specific location attributes
clean_train$unique_id <- paste(training$BUILDINGID, training$FLOOR, training$SPACEID, training$RELATIVEPOSITION, sep = "")

# Same location identifier created for training set
validation$unique_id <- paste(validation$BUILDINGID, validation$FLOOR, validation$SPACEID, validation$RELATIVEPOSITION, sep = "")
validation$unique_id <- as.factor(validation$unique_id)

str(clean_train$unique_id)
clean_train$unique_id <- as.factor(clean_train$unique_id)

# PCA
train.pca <- prcomp(training, center = TRUE, scale. = FALSE)
View(summary(train.pca))

# Retrieving the unique types of buildings in R to divide them.
unique(clean_train$BUILDINGID)


# Sampling the data by building type --------------------------------------
# Location and Time
loctime <- c('LONGITUDE', 'LATITUDE', 'TIMESTAMP', 'FLOOR', 'SPACEID', "BUILDINGID", "RELATIVEPOSITION")
# Building type '0'
type0 <- clean_train %>% filter(BUILDINGID == 0) %>% select(-loctime)
# Building type '1'
type1 <- clean_train %>% filter(BUILDINGID == 1) %>% select(-loctime)
# Building type '2'
type2 <- clean_train %>% filter(BUILDINGID == 2) %>% select(-loctime)



test_type0 <- validation %>% filter(BUILDINGID == 0)
# Building type '1'
test_type1 <- validation %>% filter(BUILDINGID == 1) 
# Building type '2'
test_type2 <- validation %>% filter(BUILDINGID == 2)


type0$unique_id <- factor(type0$unique_id)
type1$unique_id <- factor(type1$unique_id)
type2$unique_id <- factor(type2$unique_id)

test_type0$unique_id <- factor(test_type0$unique_id)
test_type1$unique_id <- factor(test_type1$unique_id)
test_type2$unique_id <- factor(test_type2$unique_id)




# Data Exploration --------------------------------------------------------
summary(training)
str(training)
summary(validation)

plot(clean_train$LONGITUDE, clean_train$LATITUDE)




