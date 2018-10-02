'''
Author: Karthik
Date created: 12/01/2016
Date Modified: 12/10/2016
Update: Converting Ordinal values as numerical from factors
'''
#=====================================================================================
							# Loading the data into R #
#=====================================================================================

#Reading from train.csv:
set.seed(1)
library(data.table)
HP_train_raw = read.csv(file.choose(), header = T, na.strings = "NA")
HP_test = read.csv(file.choose(), header = T)
HP_train = HP_train_raw[,1:80]
HP_Complete_data = rbind(HP_train,HP_test)

# Install Packages
install.packages("Metrics")
install.packages("randomForest")
install.packages("lars")
install.packages("xgboost")
install.packages("caret")
install.packages("car")

# Load Packages
library(MASS) 
library(Metrics)
library(corrplot)
library(randomForest)
library(caret)
library(lars)
library(ggplot2)
library(xgboost)
library(Matrix)
library(methods)
library(data.table)
library(FeatureHashing)
require(dplyr)
library(pROC)
library(stringr)
library(dummies)
library(kernlab)
library(mlbench)

#=====================================================================================
								# Data Cleaning #
#=====================================================================================

#---------------------------------------------------------------------------
	# TRAINING DATA #
#---------------------------------------------------------------------------

#------------------------- Primary checks -------------------------#

dim(HP_train_raw)
#[1] 1460   81

#Understanding the datatype of each variable
sapply(HP_train_raw, class)

HP_train = HP_train_raw

#------------------------- Subsetting and changing data types -------------------------#
#splitting the data on type
Nom_data_nm = names(HP_train[c(1,2,3,6,7,9,11,13,14,15,16,17,22,23,24,25,26,30,40,42,59,75,79,80)])
Ord_data_nm <- names(HP_train[c(8,10,12,18,19,28,29,31,32,33,34,36,41,43,54,56,58,61,64,65,66,73,74)])
Num_data_nm <- names(HP_train[c(4,5,27,35,37,38,39,44,45,46,47,63,67,68,69,70,71,72,76,81)])
Disc_data_nm <- names(HP_train[c(20,48,49,50,51,52,53,55,57,60,62,77,78)])

Nom_data <- HP_train[,Nom_data_nm]
Ord_data <- HP_train[,Ord_data_nm]
Num_data <- HP_train[,Num_data_nm]
Disc_data <- HP_train[,Disc_data_nm]


#---------------------- Nominal Data ----------------------#
# Determine data types in the data set
data_types <- sapply(Nom_data, class)
unique_data_types <- unique(data_types)

# Separate attributes by data type
data_labels <- lapply(unique_data_types,function(x){ names(data_types[data_types == x])})
names(data_labels) <- unique_data_types

data_labels

#correcting the datatypes to factors 
HP_train$Id = as.factor(HP_train$Id)
HP_train$MSSubClass = as.factor(HP_train$MSSubClass)

Nom_data <- HP_train[,Nom_data_nm]
#summary(Nom_data)

#---------------------- Ordinal Data ----------------------#
# Determine data types in the data set
data_types <- sapply(Ord_data, class)
unique_data_types <- unique(data_types)

# Separate attributes by data type
data_labels <- lapply(unique_data_types,function(x){ names(data_types[data_types == x])})
names(data_labels) <- unique_data_types

data_labels

#correcting the datatypes of few variable
HP_train$OverallQual = as.factor(HP_train$OverallQual)
HP_train$OverallCond = as.factor(HP_train$OverallCond)

Ord_data <- HP_train[,Ord_data_nm]
#summary(Ord_test_data)


#---------- Re-naming certain records as "No" to avoid confusion with NAs ----------#

#replacing the "NA" attribute with a categorical value "NO"
#---------------------- Nominal Data ----------------------#
HP_train$Alley = as.character(HP_train$Alley)
HP_train$GarageType = as.character(HP_train$GarageType)
HP_train$MiscFeature = as.character(HP_train$MiscFeature)

HP_train$Alley[is.na(HP_train$Alley)] <- "No"
HP_train$GarageType [is.na(HP_train$GarageType)] <- "No"
HP_train$MiscFeature [is.na(HP_train$MiscFeature)] <- "No"

HP_train$Alley = as.factor(HP_train$Alley)
HP_train$GarageType = as.factor(HP_train$GarageType)
HP_train$MiscFeature = as.factor(HP_train$MiscFeature)

Nom_data <- HP_train[,Nom_data_nm]

#---------------------- Ordinal Data ----------------------#
HP_train$BsmtQual = as.character(HP_train$BsmtQual)
HP_train$BsmtCond = as.character(HP_train$BsmtCond)
HP_train$BsmtExposure = as.character(HP_train$BsmtExposure)
HP_train$BsmtFinType1 = as.character(HP_train$BsmtFinType1)
HP_train$BsmtFinType2 = as.character(HP_train$BsmtFinType2)
HP_train$FireplaceQu = as.character(HP_train$FireplaceQu)
HP_train$GarageFinish = as.character(HP_train$GarageFinish)
HP_train$GarageQual = as.character(HP_train$GarageQual)
HP_train$GarageCond = as.character(HP_train$GarageCond)
HP_train$PoolQC = as.character(HP_train$PoolQC)
HP_train$Fence = as.character(HP_train$Fence)

HP_train$BsmtQual[is.na(HP_train$BsmtQual)] <- "No"
HP_train$BsmtExposure[is.na(HP_train$BsmtExposure)] <- "No"
HP_train$BsmtFinType1[is.na(HP_train$BsmtFinType1)] <- "No"
HP_train$BsmtFinType2[is.na(HP_train$BsmtFinType2)] <- "No"
HP_train$FireplaceQu[is.na(HP_train$FireplaceQu)] <- "No"
HP_train$GarageFinish[is.na(HP_train$GarageFinish)] <- "No"
HP_train$GarageQual[is.na(HP_train$GarageQual)] <- "No"
HP_train$GarageCond[is.na(HP_train$GarageCond)] <- "No"
HP_train$PoolQC[is.na(HP_train$PoolQC)] <- "No"
HP_train$Fence[is.na(HP_train$Fence)] <- "No"
HP_train$BsmtCond[is.na(HP_train$BsmtCond)] <- "No"

HP_train$BsmtQual = as.factor(HP_train$BsmtQual)
HP_train$BsmtExposure = as.factor(HP_train$BsmtExposure)
HP_train$BsmtFinType1 = as.factor(HP_train$BsmtFinType1)
HP_train$BsmtFinType2 = as.factor(HP_train$BsmtFinType2)
HP_train$FireplaceQu = as.factor(HP_train$FireplaceQu)
HP_train$GarageFinish = as.factor(HP_train$GarageFinish)
HP_train$GarageQual = as.factor(HP_train$GarageQual)
HP_train$GarageCond = as.factor(HP_train$GarageCond)
HP_train$PoolQC = as.factor(HP_train$PoolQC)
HP_train$Fence = as.factor(HP_train$Fence)
HP_train$BsmtCond = as.factor(HP_train$BsmtCond)

Ord_data <- HP_train[,Ord_data_nm]


#---------------------------------------------------------------------------
	# TEST DATA #
#---------------------------------------------------------------------------

HP_test2 = HP_test
#------------------------- Primary checks -------------------------#
dim(HP_test2)
#[1] 1459   80

#Understanding the datatype of each variable
#sapply(Nom_test_data, class)

#------------------------- Subsetting and changing data types -------------------------#
#splitting the data on type
Nom_test_data_nm <- names(HP_test2[c(1,2,3,6,7,9,11,13,14,15,16,17,22,23,24,25,26,30,40,42,59,75,79,80)])
Ord_test_data_nm <- names(HP_test2[c(8,10,12,18,19,28,29,31,32,33,34,36,41,43,54,56,58,61,64,65,66,73,74)])
Num_test_data_nm <- names(HP_test2[c(4,5,27,35,37,38,39,44,45,46,47,63,67,68,69,70,71,72,76)])
Disc_test_data_nm <- names(HP_test2[c(20,48,49,50,51,52,53,55,57,60,62,77,78)])

Nom_test_data <- HP_test2[,Nom_test_data_nm]
Ord_test_data <- HP_test2[,Ord_test_data_nm]
Num_test_data <- HP_test2[,Num_test_data_nm]
Disc_test_data <- HP_test2[,Disc_test_data_nm]

#---------------------- Nominal Data ----------------------#
# Determine data types in the data set
data_types <- sapply(Nom_test_data, class)
unique_data_types <- unique(data_types)

# Separate attributes by data type
data_labels <- lapply(unique_data_types,function(x){ names(data_types[data_types == x])})
names(data_labels) <- unique_data_types

data_labels

#correcting the datatypes of few variable
HP_test2$Id = as.factor(HP_test2$Id)
HP_test2$MSSubClass = as.factor(HP_test2$MSSubClass)

Nom_test_data <- HP_test2[,Nom_test_data_nm]
#summary(Nom_test_data)

#---------------------- Ordinal Data ----------------------#
# Determine data types in the data set
data_types <- sapply(Ord_test_data, class)
unique_data_types <- unique(data_types)

# Separate attributes by data type
data_labels <- lapply(unique_data_types,function(x){ names(data_types[data_types == x])})
names(data_labels) <- unique_data_types

data_labels

#correcting the datatypes of few variable
HP_test2$OverallQual = as.factor(HP_test2$OverallQual)
HP_test2$OverallCond = as.factor(HP_test2$OverallCond)

Ord_test_data <- HP_test2[,Ord_test_data_nm]
#summary(Ord_test_data)

#---------- Re-naming certain records to avoid confusion with NAs ----------#

#replacing the "NA" attribute with a categorical value "NO"

#---------------------- Nominal Data ----------------------#
HP_test2$Alley = as.character(HP_test2$Alley)
HP_test2$GarageType = as.character(HP_test2$GarageType)
HP_test2$MiscFeature = as.character(HP_test2$MiscFeature)

HP_test2$Alley[is.na(HP_test2$Alley)] <- "No"
HP_test2$GarageType[is.na(HP_test2$GarageType)] <- "No"
HP_test2$MiscFeature[is.na(HP_test2$MiscFeature)] <- "No"

HP_test2$Alley = as.factor(HP_test2$Alley)
HP_test2$GarageType = as.factor(HP_test2$GarageType)
HP_test2$MiscFeature = as.factor(HP_test2$MiscFeature)

Nom_test_data <- HP_test2[,Nom_test_data_nm]

#---------------------- Ordinal Data ----------------------#
HP_test2$BsmtQual = as.character(HP_test2$BsmtQual)
HP_test2$BsmtCond = as.character(HP_test2$BsmtCond)
HP_test2$BsmtExposure = as.character(HP_test2$BsmtExposure)
HP_test2$BsmtFinType1 = as.character(HP_test2$BsmtFinType1)
HP_test2$BsmtFinType2 = as.character(HP_test2$BsmtFinType2)
HP_test2$FireplaceQu = as.character(HP_test2$FireplaceQu)
HP_test2$GarageFinish = as.character(HP_test2$GarageFinish)
HP_test2$GarageQual = as.character(HP_test2$GarageQual)
HP_test2$GarageCond = as.character(HP_test2$GarageCond)
HP_test2$PoolQC = as.character(HP_test2$PoolQC)
HP_test2$Fence = as.character(HP_test2$Fence)

HP_test2$BsmtQual[is.na(HP_test2$BsmtQual)] <- "No"
HP_test2$BsmtCond[is.na(HP_test2$BsmtCond)] <- "No"
HP_test2$BsmtExposure[is.na(HP_test2$BsmtExposure)] <- "No"
HP_test2$BsmtFinType1[is.na(HP_test2$BsmtFinType1)] <- "No"
HP_test2$BsmtFinType2[is.na(HP_test2$BsmtFinType2)] <- "No"
HP_test2$FireplaceQu[is.na(HP_test2$FireplaceQu)] <- "No"
HP_test2$GarageFinish[is.na(HP_test2$GarageFinish)] <- "No"
HP_test2$GarageQual[is.na(HP_test2$GarageQual)] <- "No"
HP_test2$GarageCond[is.na(HP_test2$GarageCond)] <- "No"
HP_test2$PoolQC[is.na(HP_test2$PoolQC)] <- "No"
HP_test2$Fence[is.na(HP_test2$Fence)] <- "No"

HP_test2$BsmtQual = as.factor(HP_test2$BsmtQual)
HP_test2$BsmtCond = as.factor(HP_test2$BsmtCond)
HP_test2$BsmtExposure = as.factor(HP_test2$BsmtExposure)
HP_test2$BsmtFinType1 = as.factor(HP_test2$BsmtFinType1)
HP_test2$BsmtFinType2 = as.factor(HP_test2$BsmtFinType2)
HP_test2$FireplaceQu = as.factor(HP_test2$FireplaceQu)
HP_test2$GarageFinish = as.factor(HP_test2$GarageFinish)
HP_test2$GarageQual = as.factor(HP_test2$GarageQual)
HP_test2$GarageCond = as.factor(HP_test2$GarageCond)
HP_test2$PoolQC = as.factor(HP_test2$PoolQC)
HP_test2$Fence = as.factor(HP_test2$Fence)

Ord_test_data <- HP_test2[,Ord_test_data_nm]



#=====================================================================================
							# Imputing Missing Values #
#=====================================================================================

#---------------------------------------------------------------------------
	# TRAINING DATA #
#---------------------------------------------------------------------------

#Replacing missing records for all categorical values with the mode
#---------------------- Nominal Data ----------------------#
HP_train$MasVnrType[is.na(HP_train$MasVnrType)] <- names(which.max(table(HP_train$MasVnrType)))

Nom_data <- HP_train[,Nom_data_nm]
#summary(Nom_data$MasVnrType)

#---------------------- Ordinal Data ----------------------#
HP_train$Electrical[is.na(HP_train$Electrical)] <- names(which.max(table(HP_train$Electrical)))

Ord_data <- HP_train[,Ord_data_nm]
#summary(Ord_data$Electrical)

#---------------------- Numerical Data ----------------------#
#Imputing missing data for variable "LotFrontage"
## replacing missing values with mean across Neighbourhood and OverallQual

#creating a comp_data dataset that has complete records
comp_data = HP_train[complete.cases(HP_train$LotFrontage),]

#making a copy of HP_train
HP_train2 = HP_train

## List of OverallQual, Neighborhood, MissingValue
List_LF = data.frame(HP_train2[,c(13,18)])
List_LF=List_LF[!duplicated(List_LF), ]
#str(List_LF)

## Mean across OverallQual, Neighborhood
list_LF1 = aggregate(comp_data$LotFrontage, by = list(comp_data$OverallQual, comp_data$Neighborhood), mean, na.rm = TRUE)
names(list_LF1)<-paste(c("OverallQual","Neighborhood","x"))
#str(list_LF1)

List_LF = merge(List_LF, list_LF1, all = TRUE)
#str(List_LF)

## Mean across OverallQual
list_LF2 = aggregate(comp_data$LotFrontage, by = list(comp_data$OverallQual), mean, na.rm = TRUE)
names(list_LF2)<-paste(c("OverallQual","y"))
#str(list_LF2)

List_LF = merge(List_LF, list_LF2, all = TRUE)
#str(List_LF)

## Replacing means that weren't replace across two variables with mean across one variable
for(i in which(is.na(List_LF$x))) {
    List_LF$x[i] = List_LF$y[i]
}   

for(i in which(is.na(HP_train2$LotFrontage))){
    HP_train2$LotFrontage[i] = List_LF$x[which((List_LF$OverallQual == HP_train2$OverallQual[i]) & (List_LF$Neighborhood == HP_train2$Neighborhood[i]))]
}  

HP_train2$LotFrontage= as.integer(HP_train2$LotFrontage)

Num_data <- HP_train2[,Num_data_nm]
#summary(Num_data$LotFrontage)
    
#--------------------------------------------------------------------------------#
    
#Imputing missing data for variable "MasVnrArea"
## replacing missing values with mean across "MasVnrType"

HP_train2$MasVnrArea[HP_train2$MasVnrType == 'None'] = 0
HP_train2$MasVnrArea= as.integer(HP_train2$MasVnrArea)

Num_data <- HP_train2[,Num_data_nm]
#summary(Num_data$MasVnrArea)

#--------------------------------------------------------------------------------#

#Imputing missing data for "Bsmt" varaibles
HP_train2$BsmtFinSF1[HP_train2$BsmtQual == 'No'] = 0
HP_train2$BsmtFinSF2[HP_train2$BsmtQual == 'No'] = 0
HP_train2$BsmtUnfSF[HP_train2$BsmtQual == 'No'] = 0
HP_train2$TotalBsmtSF[HP_train2$BsmtQual == 'No'] = 0
HP_train2$BsmtFullBath[HP_train2$BsmtQual == 'No'] = 0
HP_train2$BsmtHalfBath[HP_train2$BsmtQual == 'No'] = 0

#str(HP_train2)
data_types <- sapply(HP_train2, class)
unique_data_types <- unique(data_types)

# Separate attributes by data type
data_labels <- lapply(unique_data_types,function(x){ names(data_types[data_types == x])})
names(data_labels) <- unique_data_types

data_labels

HP_train2$BsmtFinSF1= as.integer(HP_train2$BsmtFinSF1)
HP_train2$BsmtFinSF2= as.integer(HP_train2$BsmtFinSF2)
HP_train2$BsmtUnfSF= as.integer(HP_train2$BsmtUnfSF)
HP_train2$TotalBsmtSF= as.integer(HP_train2$TotalBsmtSF)
HP_train2$BsmtFullBath= as.integer(HP_train2$BsmtFullBath)
HP_train2$BsmtHalfBath= as.integer(HP_train2$BsmtHalfBath)

#--------------------------------------------------------------------------------

#Imputing missing data for "Garage" variables

HP_train2$GarageArea[HP_train2$GarageType == 'No'] = 0
HP_train2$GarageArea[HP_train2$GarageFinish == 'No'] = 0

HP_train2$GarageArea= as.integer(HP_train2$GarageArea)

HP_train2$PoolArea[HP_train2$PoolQC == 'No'] = 0
HP_train2$PoolArea= as.integer(HP_train2$PoolArea)

Num_data <- HP_train2[,Num_data_nm]
#summary(HP_train2$GarageArea)


#---------------------- Discrete Data ----------------------#    
#Replacing the missing with a dummy 2016 value
#Checks: HP_train2$GarageYrBlt[c(40,49,79,89,90)]
for(i in which(is.na(HP_train2$GarageYrBlt) & HP_train2$GarageType == "No")){
    HP_train2$GarageYrBlt[i] = 2016
}

for(i in which(is.na(HP_train2$GarageYrBlt) & HP_train2$GarageFinish == "No")){
    HP_train2$GarageYrBlt[i] = 2016
}

HP_train2$GarageCars[HP_train2$GarageYrBlt == '2016'] = 0

HP_train2$GarageCars= as.integer(HP_train2$GarageCars)
HP_train2$GarageYrBlt= as.integer(HP_train2$GarageYrBlt)

#Checks: HP_train2$GarageYrBlt[c(40,49,79,89,90)]

Disc_data <- HP_train2[,Disc_data_nm]
#summary(Disc_data)


#------------------------- Vaiable Creation -------------------------#
## Creating 'GarageAge' as a new variable
HP_train2$GarageAge = 2016 - HP_train2$GarageYrBlt

## Creating 'HouseAge' as a new variable
HP_train2$HouseAge = 2016 - HP_train2$YearBuilt

## Creating 'SoldMonthsAgo' as a new variable
HP_train2$SoldMonthsAgo = ((2016-1800)*12 + 12) - ((HP_train2$YrSold-1800)*12 + HP_train2$MoSold)

HP_train2$GarageAge= as.integer(HP_train2$GarageAge)
HP_train2$HouseAge= as.integer(HP_train2$HouseAge)
HP_train2$SoldMonthsAgo= as.integer(HP_train2$SoldMonthsAgo)

Num_data <- HP_train2[,c(Num_data_nm,"GarageAge","HouseAge","SoldMonthsAgo")]
#summary(HP_test2)

#splitting the data on type
Nom_data <- HP_train2[,Nom_data_nm]
Ord_data <- HP_train2[,Ord_data_nm]
Num_data <- HP_train2[,c(Num_data_nm,"GarageAge","HouseAge","SoldMonthsAgo")]
Disc_data_nm <- names(HP_train2[c(48,49,50,51,52,53,55,57,62)]) #Removing 20,60,77,78
Disc_data <- HP_train2[,Disc_data_nm]

HP_train3 = HP_train2[,-c(20,60,77,78)]
#---------------------------------------------------------------------------
	# TEST DATA #
#---------------------------------------------------------------------------

#Replacing missing records for all categorical values with the mode
#---------------------- Nominal Data ----------------------#
HP_test2$MSZoning[is.na(HP_test2$MSZoning)] <- names(which.max(table(HP_test2$MSZoning)))
HP_test2$Exterior1st[is.na(HP_test2$Exterior1st)] <- names(which.max(table(HP_test2$Exterior1st)))
HP_test2$Exterior2nd[is.na(HP_test2$Exterior2nd)] <- names(which.max(table(HP_test2$Exterior2nd)))
HP_test2$MasVnrType[is.na(HP_test2$MasVnrType)] <- names(which.max(table(HP_test2$MasVnrType)))
HP_test2$SaleType[is.na(HP_test2$SaleType)] <- names(which.max(table(HP_test2$SaleType)))

Nom_test_data <- HP_test2[,Nom_test_data_nm]
#summary(Nom_test_data$MSZoning)

#---------------------- Ordinal Data ----------------------#
HP_test2$Utilities[is.na(HP_test2$Utilities)] <- names(which.max(table(HP_test2$Utilities)))
HP_test2$KitchenQual[is.na(HP_test2$KitchenQual)] <- names(which.max(table(HP_test2$KitchenQual)))
HP_test2$Functional[is.na(HP_test2$Functional)] <- names(which.max(table(HP_test2$Functional)))

Ord_test_data <- HP_test2[,Ord_test_data_nm]
#summary(Ord_test_data$Utilities)

#---------------------- Numerical Data ----------------------#
#Imputing missing data for variable "LotFrontage"
## replacing missing values with mean across Neighbourhood and OverallQual

#creating a comp_tst_data dataset that has complete records
comp_tst_data = HP_test2[complete.cases(HP_test2$LotFrontage),]

## List of OverallQual, Neighborhood, MissingValue
List_tst_LF = data.frame(HP_test2[,c(13,18)])
List_tst_LF=List_tst_LF[!duplicated(List_tst_LF), ]
#str(List_tst_LF)

## Mean across OverallQual, Neighborhood
List_tst_LF1 = aggregate(comp_tst_data$LotFrontage, by = list(comp_tst_data$OverallQual, comp_tst_data$Neighborhood), mean, na.rm = TRUE)
names(List_tst_LF1)<-paste(c("OverallQual","Neighborhood","x"))
#str(List_tst_LF1)

List_tst_LF = merge(List_tst_LF, List_tst_LF1, all = TRUE)
#str(List_tst_LF)

## Mean across OverallQual
List_tst_LF2 = aggregate(comp_tst_data$LotFrontage, by = list(comp_tst_data$OverallQual), mean, na.rm = TRUE)
names(List_tst_LF2)<-paste(c("OverallQual","y"))
#str(List_tst_LF2)

List_tst_LF = merge(List_tst_LF, List_tst_LF2, all = TRUE)
#str(List_tst_LF)

## Replacing means that weren't replace across two variables with mean across one variable
for(i in which(is.na(List_tst_LF$x))) {
    List_tst_LF$x[i] = List_tst_LF$y[i]
}   

for(i in which(is.na(HP_test2$LotFrontage))){
    HP_test2$LotFrontage[i] = List_tst_LF$x[which((List_tst_LF$OverallQual == HP_test2$OverallQual[i]) & (List_tst_LF$Neighborhood == HP_test2$Neighborhood[i]))]
}  

HP_test2$LotFrontage= as.integer(HP_test2$LotFrontage)

Num_test_data <- HP_test2[,Num_test_data_nm]
#summary(Num_data$LotFrontage)

#--------------------------------------------------------------------------------

#Imputing missing data for variable "MasVnrArea"
## Imputing missing values with mean across "MasVnrType"

HP_test2$MasVnrArea[HP_test2$MasVnrType == 'None'] = 0
HP_test2$MasVnrArea= as.integer(HP_test2$MasVnrArea)

Num_test_data <- HP_test2[,Num_test_data_nm]
#summary(Num_test_data$MasVnrArea)

#--------------------------------------------------------------------------------

#Imputing missing data for "Bsmt" varaibles

HP_test2$BsmtFinSF1[HP_test2$BsmtQual == 'No'] = 0
HP_test2$BsmtFinSF2[HP_test2$BsmtQual == 'No'] = 0
HP_test2$BsmtUnfSF[HP_test2$BsmtQual == 'No'] = 0
HP_test2$TotalBsmtSF[HP_test2$BsmtQual == 'No'] = 0
HP_test2$BsmtFullBath[HP_test2$BsmtQual == 'No'] = 0
HP_test2$BsmtHalfBath[HP_test2$BsmtQual == 'No'] = 0

#str(HP_test2)

data_types <- sapply(HP_test2, class)
unique_data_types <- unique(data_types)

# Separate attributes by data type
data_labels <- lapply(unique_data_types,function(x){ names(data_types[data_types == x])})
names(data_labels) <- unique_data_types

data_labels

HP_test2$BsmtFinSF1= as.integer(HP_test2$BsmtFinSF1)
HP_test2$BsmtFinSF2= as.integer(HP_test2$BsmtFinSF2)
HP_test2$BsmtUnfSF= as.integer(HP_test2$BsmtUnfSF)
HP_test2$TotalBsmtSF= as.integer(HP_test2$TotalBsmtSF)
HP_test2$BsmtFullBath= as.integer(HP_test2$BsmtFullBath)
HP_test2$BsmtHalfBath= as.integer(HP_test2$BsmtHalfBath)

#--------------------------------------------------------------------------------

#Imputing missing data for "Garage" variables

HP_test2$GarageArea[HP_test2$GarageType == 'No'] = 0
HP_test2$GarageArea[HP_test2$GarageFinish == 'No'] = 0

HP_test2$GarageArea= as.integer(HP_test2$GarageArea)

HP_test2$PoolArea[HP_test2$PoolQC == 'No'] = 0
HP_test2$PoolArea= as.integer(HP_test2$PoolArea)

Num_test_data <- HP_test2[,Num_test_data_nm]
#summary(HP_test2$GarageArea)


#--------------------- Discrete Data ---------------------#
Disc_test_data <- HP_test2[,Disc_test_data_nm]
#summary(Disc_test_data)

#Replacing the missing with a dummy 2016 value
#Checks: HP_test2$GarageYrBlt[c(54,72,80,1455,1458)]
for(i in which(is.na(HP_test2$GarageYrBlt) & HP_test2$GarageType == "No")){
    HP_test2$GarageYrBlt[i] = 2016
}

for(i in which(is.na(HP_test2$GarageYrBlt) & HP_test2$GarageFinish == "No")){
    HP_test2$GarageYrBlt[i] = 2016
}

HP_test2$GarageCars[HP_test2$GarageYrBlt == '2016'] = 0

HP_test2$GarageCars= as.integer(HP_test2$GarageCars)
HP_test2$GarageYrBlt= as.integer(HP_test2$GarageYrBlt)

#Checks: HP_test2$GarageYrBlt[c(54,72,80,1455,1458)]

Disc_test_data <- HP_test2[,Disc_test_data_nm]
#summary(Disc_test_data)


#------------------------- Vaiable Creation -------------------------#
## Creating 'GarageAge' as a new variable
HP_test2$GarageAge = 2016 - HP_test2$GarageYrBlt

## Creating 'HouseAge' as a new variable
HP_test2$HouseAge = 2016 - HP_test2$YearBuilt

## Creating 'SoldMonthsAgo' as a new variable
HP_test2$SoldMonthsAgo = ((2016-1800)*12 + 12) - ((HP_test2$YrSold-1800)*12 + HP_test2$MoSold)

HP_test2$GarageAge= as.integer(HP_test2$GarageAge)
HP_test2$HouseAge= as.integer(HP_test2$HouseAge)
HP_test2$SoldMonthsAgo= as.integer(HP_test2$SoldMonthsAgo)

Num_test_data <- HP_test2[,c(Num_test_data_nm,"GarageAge","HouseAge","SoldMonthsAgo")]

HP_test2 = HP_test2[,-c(20,60,77,78)]
#summary(HP_test2)

#splitting the data on type
Nom_test_data <- HP_test2[,Nom_test_data_nm]
Ord_test_data <- HP_test2[,Ord_test_data_nm]
Num_test_data <- HP_test2[,c(Num_test_data_nm,"GarageAge","HouseAge","SoldMonthsAgo")]
Disc_test_data_nm <- names(HP_test2[c(48,49,50,51,52,53,55,57,62)]) #Removing 20,60,77,78
Disc_test_data <- HP_test2[,Disc_test_data_nm]

HP_test3 = HP_test2


#=====================================================================================
			#Variable conversions: Converting Ordinal to Numerical
#=====================================================================================

#---------------------------------------------------------------------------
	# TRAINING DATA #
#---------------------------------------------------------------------------
#Converting Ordinal categorical to integers by assigning a dummy number high to low as n to 1
HP_train3$LotShape<- as.character(HP_train3$LotShape)
HP_train3$LotShape[HP_train3$LotShape == "Reg"] <- '4'
HP_train3$LotShape[HP_train3$LotShape == "IR1"] <- '3'
HP_train3$LotShape[HP_train3$LotShape == "IR2"] <- '2'
HP_train3$LotShape[HP_train3$LotShape == "IR3"] <- '1'
HP_train3$LotShape<- as.integer(HP_train3$LotShape)

HP_train3$Utilities<- as.character(HP_train3$Utilities)
HP_train3$Utilities[HP_train3$Utilities == "AllPub"] <- '4'
HP_train3$Utilities[HP_train3$Utilities == "NoSewr"] <- '3'
HP_train3$Utilities[HP_train3$Utilities == "NoSeWa"] <- '2'
HP_train3$Utilities[HP_train3$Utilities == "ELO"] <- '1'
HP_train3$Utilities<- as.integer(HP_train3$Utilities)

HP_train3$LandSlope<- as.character(HP_train3$LandSlope)
HP_train3$LandSlope[HP_train3$LandSlope == "Gtl"] <- '3'
HP_train3$LandSlope[HP_train3$LandSlope == "Mod"] <- '2'
HP_train3$LandSlope[HP_train3$LandSlope == "Sev"] <- '1'
HP_train3$LandSlope<- as.integer(HP_train3$LandSlope)

HP_train3$GarageFinish<- as.character(HP_train3$GarageFinish)
HP_train3$GarageFinish[HP_train3$GarageFinish == "Fin"] <- '4'
HP_train3$GarageFinish[HP_train3$GarageFinish == "RFn"] <- '3'
HP_train3$GarageFinish[HP_train3$GarageFinish == "Unf"] <- '2'
HP_train3$GarageFinish[HP_train3$GarageFinish == "No"] <- '1'
HP_train3$GarageFinish<- as.integer(HP_train3$GarageFinish)

HP_train3$OverallQual<- as.integer(HP_train3$OverallQual)

HP_train3$OverallCond<- as.integer(HP_train3$OverallCond)

HP_train3$ExterQual<- as.character(HP_train3$ExterQual)
HP_train3$ExterQual[HP_train3$ExterQual== "Ex"] <- '5'
HP_train3$ExterQual[HP_train3$ExterQual== "Gd"] <- '4'
HP_train3$ExterQual[HP_train3$ExterQual== "TA"] <- '3'
HP_train3$ExterQual[HP_train3$ExterQual== "Fa"] <- '2'
HP_train3$ExterQual[HP_train3$ExterQual== "Po"] <- '1'
HP_train3$ExterQual<-as.integer(HP_train3$ExterQual)

HP_train3$GarageQual<- as.character(HP_train3$GarageQual)
HP_train3$GarageQual[HP_train3$GarageQual== "Ex"] <- '5'
HP_train3$GarageQual[HP_train3$GarageQual== "Gd"] <- '4'
HP_train3$GarageQual[HP_train3$GarageQual== "TA"] <- '3'
HP_train3$GarageQual[HP_train3$GarageQual== "Fa"] <- '2'
HP_train3$GarageQual[HP_train3$GarageQual== "Po"] <- '1'
HP_train3$GarageQual[HP_train3$GarageQual== "No"] <- '0'
HP_train3$GarageQual<-as.integer(HP_train3$GarageQual)

HP_train3$ExterCond<- as.character(HP_train3$ExterCond)
HP_train3$ExterCond[HP_train3$ExterCond== "Ex"] <- '5'
HP_train3$ExterCond[HP_train3$ExterCond== "Gd"] <- '4'
HP_train3$ExterCond[HP_train3$ExterCond== "TA"] <- '3'
HP_train3$ExterCond[HP_train3$ExterCond== "Fa"] <- '2'
HP_train3$ExterCond[HP_train3$ExterCond== "Po"] <- '1'
HP_train3$ExterCond<-as.integer(HP_train3$ExterCond)

HP_train3$BsmtQual<- as.character(HP_train3$BsmtQual)
HP_train3$BsmtQual[HP_train3$BsmtQual== "Ex"] <- '5'
HP_train3$BsmtQual[HP_train3$BsmtQual== "Gd"] <- '4'
HP_train3$BsmtQual[HP_train3$BsmtQual== "TA"] <- '3'
HP_train3$BsmtQual[HP_train3$BsmtQual== "Fa"] <- '2'
HP_train3$BsmtQual[HP_train3$BsmtQual== "No"] <- '0'
HP_train3$BsmtQual<-as.integer(HP_train3$BsmtQual)

HP_train3$BsmtCond<- as.character(HP_train3$BsmtCond)
HP_train3$BsmtCond[HP_train3$BsmtCond== "Ex"] <- '5'
HP_train3$BsmtCond[HP_train3$BsmtCond== "Gd"] <- '4'
HP_train3$BsmtCond[HP_train3$BsmtCond== "TA"] <- '3'
HP_train3$BsmtCond[HP_train3$BsmtCond== "Fa"] <- '2'
HP_train3$BsmtCond[HP_train3$BsmtCond== "Po"] <- '1'
HP_train3$BsmtCond[HP_train3$BsmtCond== "No"] <- '0'
HP_train3$BsmtCond<-as.integer(HP_train3$BsmtCond)

HP_train3$BsmtExposure<- as.character(HP_train3$BsmtExposure)
HP_train3$BsmtExposure[HP_train3$BsmtExposure== "Gd"]<- '3'
HP_train3$BsmtExposure[HP_train3$BsmtExposure== "Av"]<- '2'
HP_train3$BsmtExposure[HP_train3$BsmtExposure== "Mn"]<- '1'
HP_train3$BsmtExposure[HP_train3$BsmtExposure== "No"]<- '0'
HP_train3$BsmtExposure<-as.integer(HP_train3$BsmtExposure)

HP_train3$BsmtFinType1<- as.character(HP_train3$BsmtFinType1)
HP_train3$BsmtFinType1[HP_train3$BsmtFinType1== "GLQ"]<- '6'
HP_train3$BsmtFinType1[HP_train3$BsmtFinType1== "ALQ"]<- '5'
HP_train3$BsmtFinType1[HP_train3$BsmtFinType1== "BLQ"]<- '4'
HP_train3$BsmtFinType1[HP_train3$BsmtFinType1== "Rec"]<- '3'
HP_train3$BsmtFinType1[HP_train3$BsmtFinType1== "LwQ"]<- '2'
HP_train3$BsmtFinType1[HP_train3$BsmtFinType1== "Unf"]<- '1'
HP_train3$BsmtFinType1[HP_train3$BsmtFinType1== "No"]<- '0'
HP_train3$BsmtFinType1<-as.integer(HP_train3$BsmtFinType1)

HP_train3$BsmtFinType2<- as.character(HP_train3$BsmtFinType2)
HP_train3$BsmtFinType2[HP_train3$BsmtFinType2== "GLQ"]<- '6'
HP_train3$BsmtFinType2[HP_train3$BsmtFinType2== "ALQ"]<- '5'
HP_train3$BsmtFinType2[HP_train3$BsmtFinType2== "BLQ"]<- '4'
HP_train3$BsmtFinType2[HP_train3$BsmtFinType2== "Rec"]<- '3'
HP_train3$BsmtFinType2[HP_train3$BsmtFinType2== "LwQ"]<- '2'
HP_train3$BsmtFinType2[HP_train3$BsmtFinType2== "Unf"]<- '1'
HP_train3$BsmtFinType2[HP_train3$BsmtFinType2== "No"]<- '0'
HP_train3$BsmtFinType2<-as.integer(HP_train3$BsmtFinType2)

HP_train3$HeatingQC<- as.character(HP_train3$HeatingQC)
HP_train3$HeatingQC[HP_train3$HeatingQC== "Ex"]<- '5'
HP_train3$HeatingQC[HP_train3$HeatingQC== "Gd"]<- '4'
HP_train3$HeatingQC[HP_train3$HeatingQC== "TA"]<- '3'
HP_train3$HeatingQC[HP_train3$HeatingQC== "Fa"]<- '2'
HP_train3$HeatingQC[HP_train3$HeatingQC== "Po"]<- '1'
HP_train3$HeatingQC<-as.integer(HP_train3$HeatingQC)

HP_train3$Electrical<- as.character(HP_train3$Electrical)
HP_train3$Electrical[HP_train3$Electrical== "SBrkr"]<- '5'
HP_train3$Electrical[HP_train3$Electrical== "FuseA"]<- '4'
HP_train3$Electrical[HP_train3$Electrical== "FuseF"]<- '3'
HP_train3$Electrical[HP_train3$Electrical== "FuseP"]<- '2'
HP_train3$Electrical[HP_train3$Electrical== "Mix"]<- '1'
HP_train3$Electrical<-as.integer(HP_train3$Electrical)

HP_train3$KitchenQual<- as.character(HP_train3$KitchenQual)
HP_train3$KitchenQual[HP_train3$KitchenQual== "Ex"]<- '5'
HP_train3$KitchenQual[HP_train3$KitchenQual== "Gd"]<- '4'
HP_train3$KitchenQual[HP_train3$KitchenQual== "TA"]<- '3'
HP_train3$KitchenQual[HP_train3$KitchenQual== "Fa"]<- '2'
HP_train3$KitchenQual[HP_train3$KitchenQual== "Po"]<- '1'
HP_train3$KitchenQual<-as.integer(HP_train3$KitchenQual)

HP_train3$Functional<- as.character(HP_train3$Functional)
HP_train3$Functional[HP_train3$Functional== "Typ"]<- '8'
HP_train3$Functional[HP_train3$Functional== "Min1"]<- '7'
HP_train3$Functional[HP_train3$Functional== "Min2"]<- '6'
HP_train3$Functional[HP_train3$Functional== "Mod"]<- '5'
HP_train3$Functional[HP_train3$Functional== "Maj1"]<- '4'
HP_train3$Functional[HP_train3$Functional== "Maj2"]<- '3'
HP_train3$Functional[HP_train3$Functional== "Sev"]<- '2'
HP_train3$Functional[HP_train3$Functional== "Sal"]<- '1'
HP_train3$Functional<-as.integer(HP_train3$Functional)

HP_train3$FireplaceQu<- as.character(HP_train3$FireplaceQu)
HP_train3$FireplaceQu[HP_train3$FireplaceQu== "Ex"]<- '5'
HP_train3$FireplaceQu[HP_train3$FireplaceQu== "Gd"]<- '4'
HP_train3$FireplaceQu[HP_train3$FireplaceQu== "TA"]<- '3'
HP_train3$FireplaceQu[HP_train3$FireplaceQu== "Fa"]<- '2'
HP_train3$FireplaceQu[HP_train3$FireplaceQu== "Po"]<- '1'
HP_train3$FireplaceQu[HP_train3$FireplaceQu== "No"]<- '0'
HP_train3$FireplaceQu<-as.integer(HP_train3$FireplaceQu)

HP_train3$GarageCond<- as.character(HP_train3$GarageCond)
HP_train3$GarageCond[HP_train3$GarageCond== "Ex"]<- '5'
HP_train3$GarageCond[HP_train3$GarageCond== "Gd"]<- '4'
HP_train3$GarageCond[HP_train3$GarageCond== "TA"]<- '3'
HP_train3$GarageCond[HP_train3$GarageCond== "Fa"]<- '2'
HP_train3$GarageCond[HP_train3$GarageCond== "Po"]<- '1'
HP_train3$GarageCond[HP_train3$GarageCond== "No"]<- '0'
HP_train3$GarageCond<-as.integer(HP_train3$GarageCond)

HP_train3$PavedDrive<- as.character(HP_train3$PavedDrive)
HP_train3$PavedDrive[HP_train3$PavedDrive== "Y"]<- '2'
HP_train3$PavedDrive[HP_train3$PavedDrive== "P"]<- '1'
HP_train3$PavedDrive[HP_train3$PavedDrive== "N"]<- '0'
HP_train3$PavedDrive<-as.integer(HP_train3$PavedDrive)

HP_train3$PoolQC<- as.character(HP_train3$PoolQC)
HP_train3$PoolQC[HP_train3$PoolQC== "Ex"]<- '5'
HP_train3$PoolQC[HP_train3$PoolQC== "Gd"]<- '4'
HP_train3$PoolQC[HP_train3$PoolQC== "TA"]<- '3'
HP_train3$PoolQC[HP_train3$PoolQC== "Fa"]<- '2'
HP_train3$PoolQC[HP_train3$PoolQC== "Po"]<- '1'
HP_train3$PoolQC[HP_train3$PoolQC== "No"]<- '0'
HP_train3$PoolQC<-as.integer(HP_train3$PoolQC)

HP_train3$Fence<- as.character(HP_train3$Fence)
HP_train3$Fence[HP_train3$Fence== "GdPrv"]<- '4'
HP_train3$Fence[HP_train3$Fence== "MnPrv"]<- '3'
HP_train3$Fence[HP_train3$Fence== "GdWo"]<- '2'
HP_train3$Fence[HP_train3$Fence== "MnWw"]<- '1'
HP_train3$Fence[HP_train3$Fence== "No"]<- '0'
HP_train3$Fence<-as.integer(HP_train3$Fence)

data_types <- sapply(HP_train3, class)
unique_data_types <- unique(data_types)

# Separate attributes by data type
data_labels <- lapply(unique_data_types,function(x){ names(data_types[data_types == x])})
names(data_labels) <- unique_data_types

data_labels

train_int = HP_train3[,c(data_labels$integer)]
train_cat = HP_train3[,c(data_labels$factor)]


#---------------------------------------------------------------------------
	# TEST DATA #
#---------------------------------------------------------------------------

#Converting Ordinal categorical to integers by assigning a dummy number high to low as n to 1
HP_test3$LotShape<- as.character(HP_test3$LotShape)
HP_test3$LotShape[HP_test3$LotShape == "Reg"] <- '4'
HP_test3$LotShape[HP_test3$LotShape == "IR1"] <- '3'
HP_test3$LotShape[HP_test3$LotShape == "IR2"] <- '2'
HP_test3$LotShape[HP_test3$LotShape == "IR3"] <- '1'
HP_test3$LotShape<- as.integer(HP_test3$LotShape)

HP_test3$Utilities<- as.character(HP_test3$Utilities)
HP_test3$Utilities[HP_test3$Utilities == "AllPub"] <- '4'
HP_test3$Utilities[HP_test3$Utilities == "NoSewr"] <- '3'
HP_test3$Utilities[HP_test3$Utilities == "NoSeWa"] <- '2'
HP_test3$Utilities[HP_test3$Utilities == "ELO"] <- '1'
HP_test3$Utilities<- as.integer(HP_test3$Utilities)

HP_test3$LandSlope<- as.character(HP_test3$LandSlope)
HP_test3$LandSlope[HP_test3$LandSlope == "Gtl"] <- '3'
HP_test3$LandSlope[HP_test3$LandSlope == "Mod"] <- '2'
HP_test3$LandSlope[HP_test3$LandSlope == "Sev"] <- '1'
HP_test3$LandSlope<- as.integer(HP_test3$LandSlope)

HP_test3$GarageFinish<- as.character(HP_test3$GarageFinish)
HP_test3$GarageFinish[HP_test3$GarageFinish == "Fin"] <- '4'
HP_test3$GarageFinish[HP_test3$GarageFinish == "RFn"] <- '3'
HP_test3$GarageFinish[HP_test3$GarageFinish == "Unf"] <- '2'
HP_test3$GarageFinish[HP_test3$GarageFinish == "No"] <- '1'
HP_test3$GarageFinish<- as.integer(HP_test3$GarageFinish)

HP_test3$OverallQual<- as.integer(HP_test3$OverallQual)

HP_test3$OverallCond<- as.integer(HP_test3$OverallCond)

HP_test3$ExterQual<- as.character(HP_test3$ExterQual)
HP_test3$ExterQual[HP_test3$ExterQual== "Ex"] <- '5'
HP_test3$ExterQual[HP_test3$ExterQual== "Gd"] <- '4'
HP_test3$ExterQual[HP_test3$ExterQual== "TA"] <- '3'
HP_test3$ExterQual[HP_test3$ExterQual== "Fa"] <- '2'
HP_test3$ExterQual[HP_test3$ExterQual== "Po"] <- '1'
HP_test3$ExterQual<-as.integer(HP_test3$ExterQual)

HP_test3$GarageQual<- as.character(HP_test3$GarageQual)
HP_test3$GarageQual[HP_test3$GarageQual== "Ex"] <- '5'
HP_test3$GarageQual[HP_test3$GarageQual== "Gd"] <- '4'
HP_test3$GarageQual[HP_test3$GarageQual== "TA"] <- '3'
HP_test3$GarageQual[HP_test3$GarageQual== "Fa"] <- '2'
HP_test3$GarageQual[HP_test3$GarageQual== "Po"] <- '1'
HP_test3$GarageQual[HP_test3$GarageQual== "No"] <- '0'
HP_test3$GarageQual<-as.integer(HP_test3$GarageQual)

HP_test3$ExterCond<- as.character(HP_test3$ExterCond)
HP_test3$ExterCond[HP_test3$ExterCond== "Ex"] <- '5'
HP_test3$ExterCond[HP_test3$ExterCond== "Gd"] <- '4'
HP_test3$ExterCond[HP_test3$ExterCond== "TA"] <- '3'
HP_test3$ExterCond[HP_test3$ExterCond== "Fa"] <- '2'
HP_test3$ExterCond[HP_test3$ExterCond== "Po"] <- '1'
HP_test3$ExterCond<-as.integer(HP_test3$ExterCond)

HP_test3$BsmtQual<- as.character(HP_test3$BsmtQual)
HP_test3$BsmtQual[HP_test3$BsmtQual== "Ex"] <- '5'
HP_test3$BsmtQual[HP_test3$BsmtQual== "Gd"] <- '4'
HP_test3$BsmtQual[HP_test3$BsmtQual== "TA"] <- '3'
HP_test3$BsmtQual[HP_test3$BsmtQual== "Fa"] <- '2'
HP_test3$BsmtQual[HP_test3$BsmtQual== "No"] <- '0'
HP_test3$BsmtQual<-as.integer(HP_test3$BsmtQual)

HP_test3$BsmtCond<- as.character(HP_test3$BsmtCond)
HP_test3$BsmtCond[HP_test3$BsmtCond== "Ex"] <- '5'
HP_test3$BsmtCond[HP_test3$BsmtCond== "Gd"] <- '4'
HP_test3$BsmtCond[HP_test3$BsmtCond== "TA"] <- '3'
HP_test3$BsmtCond[HP_test3$BsmtCond== "Fa"] <- '2'
HP_test3$BsmtCond[HP_test3$BsmtCond== "Po"] <- '1'
HP_test3$BsmtCond[HP_test3$BsmtCond== "No"] <- '0'
HP_test3$BsmtCond<-as.integer(HP_test3$BsmtCond)

HP_test3$BsmtExposure<- as.character(HP_test3$BsmtExposure)
HP_test3$BsmtExposure[HP_test3$BsmtExposure== "Gd"]<- '3'
HP_test3$BsmtExposure[HP_test3$BsmtExposure== "Av"]<- '2'
HP_test3$BsmtExposure[HP_test3$BsmtExposure== "Mn"]<- '1'
HP_test3$BsmtExposure[HP_test3$BsmtExposure== "No"]<- '0'
HP_test3$BsmtExposure<-as.integer(HP_test3$BsmtExposure)

HP_test3$BsmtFinType1<- as.character(HP_test3$BsmtFinType1)
HP_test3$BsmtFinType1[HP_test3$BsmtFinType1== "GLQ"]<- '6'
HP_test3$BsmtFinType1[HP_test3$BsmtFinType1== "ALQ"]<- '5'
HP_test3$BsmtFinType1[HP_test3$BsmtFinType1== "BLQ"]<- '4'
HP_test3$BsmtFinType1[HP_test3$BsmtFinType1== "Rec"]<- '3'
HP_test3$BsmtFinType1[HP_test3$BsmtFinType1== "LwQ"]<- '2'
HP_test3$BsmtFinType1[HP_test3$BsmtFinType1== "Unf"]<- '1'
HP_test3$BsmtFinType1[HP_test3$BsmtFinType1== "No"]<- '0'
HP_test3$BsmtFinType1<-as.integer(HP_test3$BsmtFinType1)

HP_test3$BsmtFinType2<- as.character(HP_test3$BsmtFinType2)
HP_test3$BsmtFinType2[HP_test3$BsmtFinType2== "GLQ"]<- '6'
HP_test3$BsmtFinType2[HP_test3$BsmtFinType2== "ALQ"]<- '5'
HP_test3$BsmtFinType2[HP_test3$BsmtFinType2== "BLQ"]<- '4'
HP_test3$BsmtFinType2[HP_test3$BsmtFinType2== "Rec"]<- '3'
HP_test3$BsmtFinType2[HP_test3$BsmtFinType2== "LwQ"]<- '2'
HP_test3$BsmtFinType2[HP_test3$BsmtFinType2== "Unf"]<- '1'
HP_test3$BsmtFinType2[HP_test3$BsmtFinType2== "No"]<- '0'
HP_test3$BsmtFinType2<-as.integer(HP_test3$BsmtFinType2)

HP_test3$HeatingQC<- as.character(HP_test3$HeatingQC)
HP_test3$HeatingQC[HP_test3$HeatingQC== "Ex"]<- '5'
HP_test3$HeatingQC[HP_test3$HeatingQC== "Gd"]<- '4'
HP_test3$HeatingQC[HP_test3$HeatingQC== "TA"]<- '3'
HP_test3$HeatingQC[HP_test3$HeatingQC== "Fa"]<- '2'
HP_test3$HeatingQC[HP_test3$HeatingQC== "Po"]<- '1'
HP_test3$HeatingQC<-as.integer(HP_test3$HeatingQC)

HP_test3$Electrical<- as.character(HP_test3$Electrical)
HP_test3$Electrical[HP_test3$Electrical== "SBrkr"]<- '5'
HP_test3$Electrical[HP_test3$Electrical== "FuseA"]<- '4'
HP_test3$Electrical[HP_test3$Electrical== "FuseF"]<- '3'
HP_test3$Electrical[HP_test3$Electrical== "FuseP"]<- '2'
HP_test3$Electrical[HP_test3$Electrical== "Mix"]<- '1'
HP_test3$Electrical<-as.integer(HP_test3$Electrical)

HP_test3$KitchenQual<- as.character(HP_test3$KitchenQual)
HP_test3$KitchenQual[HP_test3$KitchenQual== "Ex"]<- '5'
HP_test3$KitchenQual[HP_test3$KitchenQual== "Gd"]<- '4'
HP_test3$KitchenQual[HP_test3$KitchenQual== "TA"]<- '3'
HP_test3$KitchenQual[HP_test3$KitchenQual== "Fa"]<- '2'
HP_test3$KitchenQual[HP_test3$KitchenQual== "Po"]<- '1'
HP_test3$KitchenQual<-as.integer(HP_test3$KitchenQual)

HP_test3$Functional<- as.character(HP_test3$Functional)
HP_test3$Functional[HP_test3$Functional== "Typ"]<- '8'
HP_test3$Functional[HP_test3$Functional== "Min1"]<- '7'
HP_test3$Functional[HP_test3$Functional== "Min2"]<- '6'
HP_test3$Functional[HP_test3$Functional== "Mod"]<- '5'
HP_test3$Functional[HP_test3$Functional== "Maj1"]<- '4'
HP_test3$Functional[HP_test3$Functional== "Maj2"]<- '3'
HP_test3$Functional[HP_test3$Functional== "Sev"]<- '2'
HP_test3$Functional[HP_test3$Functional== "Sal"]<- '1'
HP_test3$Functional<-as.integer(HP_test3$Functional)

HP_test3$FireplaceQu<- as.character(HP_test3$FireplaceQu)
HP_test3$FireplaceQu[HP_test3$FireplaceQu== "Ex"]<- '5'
HP_test3$FireplaceQu[HP_test3$FireplaceQu== "Gd"]<- '4'
HP_test3$FireplaceQu[HP_test3$FireplaceQu== "TA"]<- '3'
HP_test3$FireplaceQu[HP_test3$FireplaceQu== "Fa"]<- '2'
HP_test3$FireplaceQu[HP_test3$FireplaceQu== "Po"]<- '1'
HP_test3$FireplaceQu[HP_test3$FireplaceQu== "No"]<- '0'
HP_test3$FireplaceQu<-as.integer(HP_test3$FireplaceQu)

HP_test3$GarageCond<- as.character(HP_test3$GarageCond)
HP_test3$GarageCond[HP_test3$GarageCond== "Ex"]<- '5'
HP_test3$GarageCond[HP_test3$GarageCond== "Gd"]<- '4'
HP_test3$GarageCond[HP_test3$GarageCond== "TA"]<- '3'
HP_test3$GarageCond[HP_test3$GarageCond== "Fa"]<- '2'
HP_test3$GarageCond[HP_test3$GarageCond== "Po"]<- '1'
HP_test3$GarageCond[HP_test3$GarageCond== "No"]<- '0'
HP_test3$GarageCond<-as.integer(HP_test3$GarageCond)

HP_test3$PavedDrive<- as.character(HP_test3$PavedDrive)
HP_test3$PavedDrive[HP_test3$PavedDrive== "Y"]<- '2'
HP_test3$PavedDrive[HP_test3$PavedDrive== "P"]<- '1'
HP_test3$PavedDrive[HP_test3$PavedDrive== "N"]<- '0'
HP_test3$PavedDrive<-as.integer(HP_test3$PavedDrive)

HP_test3$PoolQC<- as.character(HP_test3$PoolQC)
HP_test3$PoolQC[HP_test3$PoolQC== "Ex"]<- '5'
HP_test3$PoolQC[HP_test3$PoolQC== "Gd"]<- '4'
HP_test3$PoolQC[HP_test3$PoolQC== "TA"]<- '3'
HP_test3$PoolQC[HP_test3$PoolQC== "Fa"]<- '2'
HP_test3$PoolQC[HP_test3$PoolQC== "Po"]<- '1'
HP_test3$PoolQC[HP_test3$PoolQC== "No"]<- '0'
HP_test3$PoolQC<-as.integer(HP_test3$PoolQC)

HP_test3$Fence<- as.character(HP_test3$Fence)
HP_test3$Fence[HP_test3$Fence== "GdPrv"]<- '4'
HP_test3$Fence[HP_test3$Fence== "MnPrv"]<- '3'
HP_test3$Fence[HP_test3$Fence== "GdWo"]<- '2'
HP_test3$Fence[HP_test3$Fence== "MnWw"]<- '1'
HP_test3$Fence[HP_test3$Fence== "No"]<- '0'
HP_test3$Fence<-as.integer(HP_test3$Fence)

data_types <- sapply(HP_test3, class)
unique_data_types <- unique(data_types)

# Separate attributes by data type
data_labels <- lapply(unique_data_types,function(x){ names(data_types[data_types == x])})
names(data_labels) <- unique_data_types

data_labels

test_int = HP_test3[,c(data_labels$integer)]
test_cat = HP_test3[,c(data_labels$factor)]



#=====================================================================================
	#Variable conversions: Converting Nominal to Numerical using sparse matrix
#=====================================================================================
HP_test4 = HP_test3
HP_train4 = HP_train3

levels(HP_train4$MSSubClass)<- c(levels(HP_train4$MSSubClass), '150')

levels(HP_test4$Condition2)<- c(levels(HP_test4$Condition2), 'RRAe', 'RRAn', 'RRNn')
levels(HP_test4$HouseStyle)<- c(levels(HP_test4$HouseStyle), '2.5Fin')
levels(HP_test4$RoofMatl)<- c(levels(HP_test4$RoofMatl), 'ClyTile', 'Membran', 'Metal', 'Roll')
levels(HP_test4$Exterior1st)<- c(levels(HP_test4$Exterior1st), 'ImStucc', 'Stone')
levels(HP_test4$Exterior2nd)<- c(levels(HP_test4$Exterior2nd), 'Other')
levels(HP_test4$Heating)<- c(levels(HP_test4$Heating), 'Floor', 'OthW')
levels(HP_test4$MiscFeature)<- c(levels(HP_test4$MiscFeature), 'Tenc')

#Checks:
'''
data_types <- sapply(HP_train4, class)
unique_data_types <- unique(data_types)
# Separate attributes by data type
data_labels <- lapply(unique_data_types,function(x){ names(data_types[data_types == x])})
names(data_labels) <- unique_data_types
data_labels

train_int = HP_train4[,c(data_labels$integer)]
train_cat = HP_train4[,c(data_labels$factor)]

data_types <- sapply(HP_test4, class)
unique_data_types <- unique(data_types)
# Separate attributes by data type
data_labels <- lapply(unique_data_types,function(x){ names(data_types[data_types == x])})
names(data_labels) <- unique_data_types
data_labels

test_int = HP_test4[,c(data_labels$integer)]
test_cat = HP_test4[,c(data_labels$factor)]
'''

#-------------------------------------------------------------------------------------
	#Generating a dummy Variables
#-------------------------------------------------------------------------------------
library(caret)
dummy_test = HP_test4[,-1]
dummy_train = HP_train4[-grep('SalePrice', colnames(HP_train4))]
dummy_train = dummy_train[,-1]
##Combine train and test data
train_test = rbind(dummy_train,dummy_test)

dummy = dummyVars(~ ., data = dummy_train)
dummyVar_train = predict(dummy, newdata = dummy_train)
dummyVar_train = data.frame(dummyVar_train)

dummy = dummyVars(~ ., data = dummy_test)
dummyVar_test = predict(dummy, newdata = dummy_test)
dummyVar_test = data.frame(dummyVar_test)

dummyVar_train = dummyVar_train[,order(names(dummyVar_train),decreasing = F)]
dummyVar_test = dummyVar_test[,order(names(dummyVar_test),decreasing = F)]

dummyVar_train_test = rbind(dummyVar_train,dummyVar_test)

#Checks:
'''
data_types <- sapply(dummyVar_test, class)
unique_data_types <- unique(data_types)
# Separate attributes by data type
data_labels <- lapply(unique_data_types,function(x){ names(data_types[data_types == x])})
names(data_labels) <- unique_data_types
data_labels
'''

#-------------------------------------------------------------------------------------
	#Generating a SparseMatrix
#-------------------------------------------------------------------------------------

dummyVar_train = predict(dummy, newdata = dummy_train)
Sparse_train <- Matrix(dummyVar_train, sparse = TRUE)

dummyVar_test = predict(dummy, newdata = dummy_test)
Sparse_test <- Matrix(dummyVar_test, sparse = TRUE)


bst <- xgb.train(data=Sparse_train, booster = "gblinear", max.depth=2, nthread = 2, nround=2, 
                                watchlist=watchlist, eval.metric = "error", eval.metric = "logloss", objective = "binary:logistic")
#=====================================================================================
				#Variable Reduction: Reducing the features
#=====================================================================================

#---------------------------------------------------------------------------
	# TRAINING DATA #
#---------------------------------------------------------------------------

#------------------------- Univariate Analysis -------------------------#
'''
library(Publish)

#Univariate analysis for Nominal data
Nom<- names(Nom_data)
Nom<- Nom[!Nom %in% c("Id")]
nom1<- paste(Nom, collapse = " + ")
form.Nom<- as.formula(paste("", nom1, sep = "~"))
univariate = univariateTable(form.Nom ,data=Nom_data)

#Removing variables from above Univariate
#   Id, Street, Alley, LandContour, Condition1, Condition2, BldgType, RoofMatl, 
#   Heating, CentralAir, MiscFeature, SaleType, SaleCondition

Nom_data = Nom_data[ , !(names(Nom_data) %in% c("Id", "Street", "Alley", "LandContour", "Condition1", "Condition2", "BldgType", "RoofMatl", "Heating", "CentralAir", "MiscFeature", "SaleType", "SaleCondition", "GarageYrBlt"))]
HP_train4 = HP_train4[ , !(names(HP_train4) %in% c("Id", "Street", "Alley", "LandContour", "Condition1", "Condition2", "BldgType", "RoofMatl", "Heating", "CentralAir", "MiscFeature", "SaleType", "SaleCondition", "GarageYrBlt"))]


#Univariate analysis for Ordinal data
Ord<- names(Ord_data)
Ord1<- paste(Ord, collapse = " + ")
form.Ord<- as.formula(paste("", Ord1, sep = "~"))
univariate = univariateTable(form.Ord ,data=Ord_data)

#Removing variables from above Univariate
#   Utilities, LandSlope, ExterCond, BsmtCond, BsmtFinType2, Electrical, 
#   Functional, GarageQual, GarageCond, PavedDrive, PoolQC, Fence 

Ord_data = Ord_data[ , !(names(Ord_data) %in% c("Utilities", "LandSlope", "ExterCond", "BsmtCond", "BsmtFinType2", "Electrical", "Functional", "GarageQual", "GarageCond", "PavedDrive", "PoolQC", "Fence"))]
HP_train4 = HP_train4[ , !(names(HP_train4) %in% c("Utilities", "LandSlope", "ExterCond", "BsmtCond", "BsmtFinType2", "Electrical", "Functional", "GarageQual", "GarageCond", "PavedDrive", "PoolQC", "Fence"))]
'''

#------------------------------ Correlation ------------------------------#
setwd('C:/Users/Ak/Desktop/DM Project/House Pricing')
x = cor(train_int)
write.csv(x,file="Correlation.csv",row.names=FALSE)


'''
#Setting a soft cut-off of 0.5 and a hard cut-off of 0.75 we are able to reduce 2 variables
#X2ndFlrSF	X1stFlrSF
Removing continuous variables high correlation values
#   X2ndFlrSF
Num_data = Num_data[ , !(names(Num_data) %in% c("X2ndFlrSF"))]
HP_train4 = HP_train4[ , !(names(HP_train4) %in% c("X2ndFlrSF"))]
'''

#------------------------------ Outlier Treatment ------------------------------#
'''
dummy_num = HP_train2[c(4,5,27,35,37,38,39,44,45,46,47,63,67,68,69,70,71,72,76,81,82)]

#Boxplot method:
#---------------

outlier_values <- boxplot.stats(dummy_num$LotFrontage)$out  # outlier values.
boxplot(dummy_num$LotFrontage, main="Pressure Height", boxwex=.5)
mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=.6)

Not too useful - unable to identify the exact outliers

#Cooks Distance:
#---------------
mod <- lm(SalePrice ~ ., data=dummy_num)
cooksd <- cooks.distance(mod)

plot(cooksd, pch="*", cex=1.5, main="Influential Obs by Cooks distance")  # plot cooks distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels

influential <- as.numeric(names(cooksd)[(cooksd > 4*mean(cooksd))])
influential
#  186  314  336  347  441  524  692  804  811  899 1170 1171 1183 1231 1299 1424

remove(x)
x <- array("list",length(influential))
for (j in 1:length(influential)) {
    x[j]=row.names(dummy_num[which(row.names(dummy_num) == influential[j]),])
}

dummy_num[x, ]


#outlierTest from car package gives the most extreme observation:
#----------------------------------------------------------------
car::outlierTest(mod)
dummy_num[x ,]
    rstudent unadjusted p-value Bonferonni p
1299 -18.593024         2.7506e-69   3.9912e-66
524   -9.539507         5.9308e-21   8.6055e-18
1183   8.037065         1.9087e-15   2.7695e-12
899    6.016325         2.2624e-09   3.2827e-06
692    5.846411         6.2127e-09   9.0146e-06


#scatter plot
#------------
plot(dummy_num[,3],dummy_num$SalePrice) 

install.packages("moments")
library(moments)
kurtosis(dummy_num[,3])
skewness(dummy_num[,3])
dim(dummy_num)[2]
for (i in 1:dim(dummy_num)[2]) {
    print(i)
    print(skewness(log(dummy_num[,i])))
}
dummy_num$TotalBsmtSF[which(dummy_num$TotalBsmtSF<1)]
log(dummy_num$TotalBsmtSF)
skewness(log(dummy_num$TotalBsmtSF))

library(e1071)                    
skewness(dummy_num[,3])  
kurtosis(dummy_num[,3])
'''


#------------------------- Feature Selection: Random forest using Boruta -------------------------#
install.packages("Boruta")
install.packages("ranger")
library(Boruta)

#Run Boruta on this data
set.seed(1)
Boruta.HP_train <- Boruta(HP_train3$SalePrice~., data=HP_train3, pValue = 0.05,mcAdj = TRUE,maxRuns = 200,doTrace = 2) 

Confirmed.HP_train = names(Boruta.HP_train$finalDecision[which(Boruta.HP_train$finalDecision == "Confirmed")])
Rejected.HP_train = names(Boruta.HP_train$finalDecision[which(Boruta.HP_train$finalDecision == "Rejected")])
Tentative.HP_train = names(Boruta.HP_train$finalDecision[which(Boruta.HP_train$finalDecision == "Tentative")])

HP_train5 = HP_train3[ , !(names(HP_train3) %in% 
                               c("Id","Street","Utilities","LotConfig","Condition1","Condition2","RoofMatl",
                               "ExterCond","BsmtFinSF2","Heating","LowQualFinSF","BsmtHalfBath",
                               "EnclosedPorch","X3SsnPorch","ScreenPorch","PoolArea","PoolQC","MiscFeature",
                               "MiscVal","SaleType","SoldMonthsAgo"))]
'''
Boruta performed 199 iterations in 26.32917 mins.
 49 attributes confirmed important: BedroomAbvGr, BldgType, BsmtCond, BsmtExposure, BsmtFinSF1 and
44 more.
21 attributes confirmed unimportant: BsmtFinSF2, BsmtHalfBath, Condition1, Condition2,
EnclosedPorch and 16 more.
9 tentative attributes left: Alley, BsmtFinType2, Electrical, Fence, Functional and 4 more.
'''


#Random forest to identify the highly influencial variables
install.packages("randomForest")
library(randomForest)

Y_noID<- HP_train5
cat('Random forest run on all attributes:\n');
set.seed(1)
x = randomForest(Y_noID$SalePrice~.,data=Y_noID)
'''
Call: Random forest run on all attributes
 randomForest(formula = Y_noID$SalePrice ~ ., data = Y_noID) 
               Type of random forest: regression
                     Number of trees: 500
No. of variables tried at each split: 19

          Mean of squared residuals: 741722028
                    % Var explained: 88.24
'''
varImpPlot(x)



cat('Random forest run only on 10 confirmed attributes:\n');
set.seed(1)
x = randomForest(Y_noID$SalePrice~ GrLivArea + OverallQual + Neighborhood + GarageCars + ExterQual + TotalBsmtSF, data=Y_noID'''
Call:
 randomForest(formula = Y_noID$SalePrice ~ GrLivArea + OverallQual + Neighborhood + GarageCars + ExterQual + TotalBsmtSF, data = Y_noID) 
               Type of random forest: regression
                     Number of trees: 500
No. of variables tried at each split: 2

          Mean of squared residuals: 933111241
                    % Var explained: 85.2
'''
varImpPlot(x)



cat('Random forest run only on 6 confirmed attributes:\n');
set.seed(1)
x = randomForest(Y_noID$SalePrice~ GrLivArea + OverallQual + Neighborhood + GarageCars + TotalBsmtSF, data=Y_noID)
'''
Call:
 randomForest(formula = Y_noID$SalePrice ~ GrLivArea + OverallQual + Neighborhood + GarageCars + TotalBsmtSF, data = Y_noID) 
               Type of random forest: regression
                     Number of trees: 500
No. of variables tried at each split: 1

          Mean of squared residuals: 954206210
                    % Var explained: 84.87
'''

#------------------------- Recursive feature selection -------------------------#
HP_train7 = HP_train3[,-1]
# ensure the results are repeatable
# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
results <- rfe(SalePrice~., data=HP_train7, rfeControl=control)
# summarize the results
print(results)
# list the chosen features
predictors(results)
'''
Recursive feature selection

Outer resampling method: Cross-Validated (10 fold) 
Resampling performance over subset size:
 Variables  RMSE Rsquared RMSESD RsquaredSD Selected
         4 34893   0.8107   4515    0.04749         
         8 31637   0.8426   5342    0.05453         
        16 28333   0.8737   5808    0.05664         
       211 27990   0.8813   5213    0.04676        *

The top 5 variables (out of 211):
   GrLivArea, OverallQual, TotalBsmtSF, X1stFlrSF, GarageCars

> # list the chosen features
> predictors(results)
  [1] "GrLivArea"            "OverallQual"          "TotalBsmtSF"          "X1stFlrSF"           
  [5] "GarageCars"           "X2ndFlrSF"            "GarageArea"           "BsmtFinSF1"          
  [9] "ExterQual"            "LotArea"              "FireplaceQu"          "BsmtFinType1" 
'''


# Repeating with the data from Boruta
# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
set,seed(1)
results <- rfe(SalePrice~., data=HP_train5, rfeControl=control)
# summarize the results
print(results)
# list the chosen features
predictors(results)

'''
Recursive feature selection
Outer resampling method: Cross-Validated (10 fold) 
Resampling performance over subset size:

Variables  RMSE Rsquared RMSESD RsquaredSD Selected
4 34925   0.8081   5621    0.05741         
8 31309   0.8449   5312    0.05072         
16 27897   0.8777   5606    0.04943         
155 27596   0.8820   5714    0.05184        *

The top 5 variables (out of 155):
GrLivArea, OverallQual, X1stFlrSF, TotalBsmtSF, X2ndFlrSF

> predictors(results)
[1] "GrLivArea"            "OverallQual"          "X1stFlrSF"            "TotalBsmtSF"         
[5] "X2ndFlrSF"            "GarageCars"           "ExterQual"            "GarageArea"          
[9] "BsmtFinSF1"           "LotArea"              "HouseAge"             "FireplaceQu"         
'''


#---------------------------------------------------------------------------
	# TEST DATA #
#---------------------------------------------------------------------------

HP_test5 = HP_test3[ , !(names(HP_test3) %in% 
                               c("Id","Street","Utilities","LotConfig","Condition1","Condition2","RoofMatl",
								"ExterCond","BsmtFinSF2","Heating","Electrical","LowQualFinSF","BsmtHalfBath",
								"EnclosedPorch","X3SsnPorch","ScreenPorch","PoolArea","PoolQC","MiscFeature",
								"MiscVal","SaleType","SoldMonthsAgo"))]





#=====================================================================================
									#MODELING
#=====================================================================================

HP_train6 = HP_train3[,-1]
HP_test6 = HP_test3

#------------------------- Linear Model -------------------------#
HP_test6$MSSubClass[which(HP_test6$MSSubClass == 150)] <- 160
set.seed(1)
model1 = lm(HP_train6$SalePrice ~., data = HP_train6)
summary(model1)
'''
Residual standard error: 24430 on 1252 degrees of freedom
Multiple R-squared:  0.9189,	Adjusted R-squared:  0.9054 
F-statistic: 68.48 on 207 and 1252 DF,  p-value: < 2.2e-16
'''
SSE = sum(model1$residuals^2)
predict.Test = predict(model1, newdata = HP_test6)

setwd("C:/Users/Ak/Desktop/DM Project/House Pricing")
lm_submission <- cbind(Id=HP_test$Id,SalePrice=predict.Test)
write.csv(lm_submission,file="lm_sumbission1.csv",row.names=FALSE)

head(lm_submission)    

#Your submission scored 0.18986


#------------------------- Linear Model2 -------------------------#
HP_test6$MSSubClass[which(HP_test6$MSSubClass == 150)] <- 160
set.seed(1)
attach(HP_train6)
model2 = lm(SalePrice ~ GrLivArea + OverallQual + TotalBsmtSF + GarageArea + Neighborhood 
                + BsmtFinSF1 + ExterQual + LotArea + HouseAge, data = HP_train6)
summary(model2)
detach("HP_train6")
'''
Residual standard error: 33870 on 1427 degrees of freedom
Multiple R-squared:  0.8222,	Adjusted R-squared:  0.8182 
F-statistic: 206.2 on 32 and 1427 DF,  p-value: < 2.2e-16
'''

SSE = sum(model2$residuals^2)
predict.Test = predict(model2, newdata = HP_test6)
lm_submission2 <- cbind(Id=HP_test$Id,SalePrice=predict.Test)
setwd("C:/Users/Ak/Desktop/DM Project/House Pricing")
write.csv(lm_submission2,file="lm_sumbission2.csv",row.names=FALSE)

head(lm_submission2) 
summary(lm_submission2) 

#Your submission scored 0.16677    


#------------------------- Linear Model3 -------------------------#
HP_test6$MSSubClass[which(HP_test6$MSSubClass == 150)] <- 160
attach(HP_train6)
set.seed(1)
model3 = lm(SalePrice ~ GrLivArea + OverallQual + Neighborhood + HouseAge + TotalBsmtSF, data = HP_train6)
summary(model3)
detach("HP_train6")
'''
Residual standard error: 35650 on 1431 degrees of freedom
Multiple R-squared:  0.8025,	Adjusted R-squared:  0.7987 
F-statistic: 207.7 on 28 and 1431 DF,  p-value: < 2.2e-16
'''

SSE = sum(model3$residuals^2)
predict.Test = predict(model3, newdata = HP_test6)
lm_submission3 <- cbind(Id=HP_test$Id,SalePrice=predict.Test)
setwd("C:/Users/Ak/Desktop/DM Project/House Pricing")
write.csv(lm_submission3,file="lm_submission3.csv",row.names=FALSE)

head(lm_submission3) 
summary(lm_submission3) 

#Your submission scored 0.19720
------------------------------------------------------------------------------------------------


#------------------------- XGBoost -------------------------#

XGBoost only works with numeric vectors. 
#Preparation of Data for using XGBoost
library(xgboost)
install.packages("readr")
library(readr)
library(stringr)
library(caret)
library(car)
library(Matrix)

sparse_matrix <- sparse.model.matrix(SalePrice ~ .-1, data = HP_train6)
head(sparse_matrix)
write.csv(sparse_matrix,file="sparse_matrix.csv",row.names=TRUE)
sparse_matrixxxx = data.frame(sparse_matrix)

bst <- xgboost(data = sparse_matrix, label = HP_train6$SalePrice, max.depth = 4,
               eta = 1, nthread = 2, nround = 100,objective = "reg:linear")

importance <- xgb.importance(sparse_matrix@Dimnames[[2]], model = bst)
importance[1:20]





dummies = dummyVars(~ ., data = HP_train6)
train_data = predict(dummies, newdata = HP_train6)
train_data = data.frame(train_data)
trainingD = train_data 

library(xgboost)

tuneFitD7 = list(objective = "reg:linear",
                 num_class = c(20, 30, 60),
                 max_depth = 100,
                 eta = c(0.05, 0.10),
                 gamma = 0.01, 
                 subsample = c(0.9),
                 colsample_bytree = 0.8, 
                 min_child_weight = 4,
                 max_delta_step = 1,
                 nthread=6
)

modelFitD7cv = xgb.cv(data = data.matrix(Sparse_train), 
                      label = HP_train4$SalePrice, 
                      params = tuneFitD7, 
                      nfold = 5, 
                      nrounds = 100,
                      verbose = T)
					  
					  
dummy = HP_train4$SalePrice
dummy_test = HP_test6
dummy_train = dummy[-grep('SalePrice', colnames(dummy))]
##Combine train and test data
train_test = rbind(dummy_train,dummy_test)

dummy = dummyVars(~ ., data = dummy_train)
Sparse_train = predict(dummy, newdata = dummy_train)
Sparse_train = data.frame(Sparse_train)

dummy = dummyVars(~ ., data = HP_test6)
Sparse_test = predict(dummy, newdata = HP_test6)
Sparse_test = data.frame(Sparse_test)

Sparse_train_test = rbind(Sparse_train,Sparse_test)