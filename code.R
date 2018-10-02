set.seed(415)
library(data.table)
library(randomForest)
library(elasticnet)
library(xgboost)
library(readr)
library(stringr)
library(caret)
library(car)
HP_data_raw = read.csv(file.choose(), header = T, na.strings = "NA")
HP_test = read.csv(file.choose(), header = T)
HP_train = HP_data_raw[,1:80]
HP_Complete_data = rbind(HP_train,HP_test)
  dim(HP_data_raw)
  str(HP_data_raw)
  for(i in 1:81) {
     print(class(HP_data_raw[,i]))
  }
  names(HP_data_raw[])
  Nom_data <- HP_data_raw[c(1,2,3,6,7,9,11,13,14,15,16,17,22,23,24,25,26,30,40,42,59,75,79,80)]
  Ord_data <- HP_data_raw[c(8,10,12,18,19,28,29,31,32,33,34,36,41,43,54,56,58,61,64,65,66,73,74)]
  Num_data <- HP_data_raw[c(4,5,27,35,37,38,39,44,45,46,47,63,67,68,69,70,71,72,76,81)]
  Disc_data <- HP_data_raw[c(20,48,49,50,51,52,53,55,57,60,62,77,78)]
  str(Nom_data)
  str(Ord_data)
  str(Num_data)
  
  str(Disc_data)
  Nom_data$Id = as.factor(Nom_data$Id)
  Nom_data$MSSubClass = as.factor(Nom_data$MSSubClass)
  Ord_data$OverallQual = as.factor(Ord_data$OverallQual)
   Ord_data$OverallCond = as.factor(Ord_data$OverallCond)
   summary(Nom_data)
   Nom_data$Alley = as.character(Nom_data$Alley)
   Nom_data$Alley[is.na(Nom_data$Alley)] <- "No"
   Nom_data$Alley = as.factor(Nom_data$Alley)
   Nom_data$GarageType = as.character(Nom_data$GarageType)
   Nom_data$MiscFeature = as.character(Nom_data$MiscFeature)
   Nom_data$GarageType [is.na(Nom_data$GarageType)] <- "No"
   Nom_data$MiscFeature [is.na(Nom_data$MiscFeature)] <- "No"
   Nom_data$GarageType = as.factor(Nom_data$GarageType)
   Nom_data$MiscFeature = as.factor(Nom_data$MiscFeature)
    Ord_data$BsmtQual = as.character(Ord_data$BsmtQual)
   Ord_data$BsmtExposure = as.character(Ord_data$BsmtExposure)
   Ord_data$BsmtFinType1 = as.character(Ord_data$BsmtFinType1)
   Ord_data$BsmtFinType2 = as.character(Ord_data$BsmtFinType2)
   Ord_data$FireplaceQu = as.character(Ord_data$FireplaceQu)
   Ord_data$GarageFinish = as.character(Ord_data$GarageFinish)
   Ord_data$GarageQual = as.character(Ord_data$GarageQual)
   Ord_data$GarageCond = as.character(Ord_data$GarageCond)
   Ord_data$PoolQC = as.character(Ord_data$PoolQC)
   Ord_data$Fence = as.character(Ord_data$Fence)
   Ord_data$BsmtCond = as.character(Ord_data$BsmtCond)
  Ord_data$BsmtQual[is.na(Ord_data$BsmtQual)] <- "No"
   Ord_data$BsmtExposure[is.na(Ord_data$BsmtExposure)] <- "No"
   Ord_data$BsmtFinType1[is.na(Ord_data$BsmtFinType1)] <- "No"
   Ord_data$BsmtFinType2[is.na(Ord_data$BsmtFinType2)] <- "No"
   Ord_data$FireplaceQu[is.na(Ord_data$FireplaceQu)] <- "No"
   Ord_data$GarageFinish[is.na(Ord_data$GarageFinish)] <- "No"
   Ord_data$GarageQual[is.na(Ord_data$GarageQual)] <- "No"
   Ord_data$GarageCond[is.na(Ord_data$GarageCond)] <- "No"
   Ord_data$PoolQC[is.na(Ord_data$PoolQC)] <- "No"
   Ord_data$Fence[is.na(Ord_data$Fence)] <- "No"
   Ord_data$BsmtCond[is.na(Ord_data$BsmtCond)] <- "No"
  Ord_data$BsmtQual = as.factor(Ord_data$BsmtQual)
  Ord_data$BsmtExposure = as.factor(Ord_data$BsmtExposure)
  Ord_data$BsmtFinType1 = as.factor(Ord_data$BsmtFinType1)
  Ord_data$BsmtFinType2 = as.factor(Ord_data$BsmtFinType2)
  Ord_data$FireplaceQu = as.factor(Ord_data$FireplaceQu)
  Ord_data$GarageFinish = as.factor(Ord_data$GarageFinish)
  Ord_data$GarageQual = as.factor(Ord_data$GarageQual)
  Ord_data$GarageCond = as.factor(Ord_data$GarageCond)
  Ord_data$PoolQC = as.factor(Ord_data$PoolQC)
  Ord_data$Fence = as.factor(Ord_data$Fence)
  Ord_data$BsmtCond = as.factor(Ord_data$BsmtCond)
  summary(Num_data)
  summary(Disc_data
          )
  
  Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  Mode(Nom_data$MasVnrType)
  
  Nom_data$MasVnrType = as.character(Nom_data$MasVnrType)
  Nom_data$MasVnrType[is.na(Nom_data$MasVnrType)] <- "None"
  Nom_data$MasVnrType = as.factor(Nom_data$MasVnrType)
  
  Mode(Ord_data$Electrical
       )
  
  Ord_data$Electrical = as.character(Ord_data$Electrical)
  Ord_data$Electrical[is.na(Ord_data$Electrical)] <- "SBrkr"
  Ord_data$Electrical= as.factor(Ord_data$Electrical)
  
  Mode(Num_data$LotFrontage)
  
  lotf<-Num_data[-which(is.na(Num_data$LotFrontage)), ]
  Mode(lotf$LotFrontage)
  
  Num_data$LotFrontage[is.na(Num_data$LotFrontage)] <- 60
  
  masv<-Num_data[-which(is.na(Num_data$MasVnrArea)), ]
  Mode(masv$MasVnrArea)
  
  Num_data$MasVnrArea[is.na(Num_data$MasVnrArea)] <- 0
  
  Disc_data$GarageYrBlt[is.na(Disc_data$GarageYrBlt)] <- 9999
  clean_data<-cbind(Ord_data,Num_data,Disc_data,Nom_data)
  clean_disc_data<-write.csv(Disc_data, file.choose())
  write.csv(Nom_data, file.choose())
  write.csv(Ord_data, file.choose())
  write.csv(Num_data, file.choose())
  
  #performing random forests
  varnames<- names(Nom_data)
  varnames<-varnames[!varnames %in% c("Id")]
  varnames1<- paste(varnames, collapse = "+")
rf.form<- as.formula(paste("Id", varnames1, sep = "~"))
  nom_fit<- randomForest(rf.form, Nom_data, importance = TRUE, ntree = 200)
  plot(nom_fit)
  varImpPlot(nom_fit)
 
  
  numnames<- names(Num_data)
  numnames<-numnames[!numnames %in% c("SalePrice")]
  numnames1<- paste(numnames, collapse = "+")
  nf.form<- as.formula(paste("SalePrice", numnames1, sep = "~"))
  num_fit<- randomForest(nf.form, Num_data, importance = TRUE, ntree = 100)
  plot(nom_fit)
  varImpPlot(num_fit)
  
  
  discnames<- names(Disc_data)
  discnames<-varnames[!discnames %in% c("SalePrice")]
  discnames1<- paste(discnames, collapse = "+")
  df.form<- as.formula(paste("SalePrice", discnames1, sep = "~"))
  Disc_fit<- randomForest(df.form, Disc_data, importance = TRUE, ntree = 200)
  plot(nom_fit)
  varImpPlot(nom_fit)
  

    trainnames<- names(clean_data)
  trainnames<-trainnames[!trainnames %in% c("SalePrice")]
  trainnames<-trainnames[!trainnames %in% c("Id")]
  trainnames1<- paste(trainnames, collapse = "+")
  train.form<- as.formula(paste("SalePrice", trainnames1, sep = "~"))
  train_fit<- randomForest(train.form, clean_data, importance = TRUE, ntree = 200)
  plot(nom_fit)
  varImpPlot(train_fit)
  
  
  trainimp<-data.frame(importance(train_fit, type = 2))
  trainimp$varaibles<-row.names(trainimp)
  trainimp[order(trainimp$MeanDecreaseGini, decreasing = T)]
  trainimp$IncNodePurity<- sort(trainimp$IncNodePurity)
  write.csv( clean_data, file.choose())
  
  #converting cateogorical to numerical.
  
  clean_data= clean_data=pd.get_dummies(clean_data)
  dmy<- dummyVars("~.", data= clean_data)
  converted_data<- data.frame(predict(dmy, newdata= clean_data))
  print(converted_data)
  
  dmyneigh<- dummyVars("~ Neighborhood", data = clean_data)
  dmyneigh1<- data.frame(predict(dmyneigh, newdata= clean_data))
  
  
  dmybsmtqual<- dummyVars("~ BsmtQual", data = clean_data)
  dmybsmtqual1<- data.frame(predict(dmybsmtqual, newdata= clean_data))
  
  model2<- cbind(dmybsmtqual1,dmyneigh1, clean_data$Neighborhood,clean_data$SalePrice, clean_data$OverallQual,clean_data$OverallCond, clean_data$MSSubClass)
  names(model2)
  model1 <- lm(clean_data$SalePrice ~ clean_data$OverallQual+clean_data$OverallCond+clean_data$MSSubClass , data = model2)
  predicted <- predict.lm(model1, newdata = HP_test)
  
  
  names(model2) <- as.character(1:35)
  model1 <- lm(32 ~ 1+2+3+4+5+6+7+8+9+10+11+12+13+14+15+16+17+18+19+20+21+22+23+24+25+26+27+28+29+30+31+33+34+35, data = model2)
   
   
  #converting ordinal categorical to numerical by assigning from high to low as n to 1
  
  which(Ord_data$LotShape == "IR1")
  Ord_data$LotShape<- as.numeric(Ord_data$LotShape)
  Ord_data$LotShape[Ord_data$LotShape == "IR1"] <- 0
  Ord_data$LotShape[Ord_data$LotShape == "IR2"] <- 0
  Ord_data$LotShape[Ord_data$LotShape == "IR3"] <- 0
  Ord_data$LotShape[Ord_data$LotShape == "Reg"] <- 1
  Ord_data$LotShape<-as.factor(Ord_data$LotShape)
  
  Ord_data$ExterQual<-as.numeric(Ord_data$ExterQual)
  Ord_data$ExterQual[Ord_data$ExterQual== "Ex"] <- 4
  Ord_data$ExterQual[Ord_data$ExterQual== "Gd"] <- 3
  Ord_data$ExterQual[Ord_data$ExterQual== "TA"] <- 2
  Ord_data$ExterQual[Ord_data$ExterQual== "Fa"] <- 1
  Ord_data$ExterQual<-as.factor(Ord_data$ExterQual)
  
  Ord_data$BsmtQual<-as.numeric(Ord_data$BsmtQual)
  Ord_data$BsmtQual[Ord_data$BsmtQual== "Ex"]<- 4
  Ord_data$BsmtQual[Ord_data$BsmtQual== "Gd"]<- 3
  Ord_data$BsmtQual[Ord_data$BsmtQual== "TA"]<- 2
  Ord_data$BsmtQual[Ord_data$BsmtQual== "Fa"]<- 1
  Ord_data$BsmtQual<-as.factor(Ord_data$BsmtQual)
  
  Ord_data$BsmtExposure<-as.numeric((Ord_data$BsmtExposure))
  Ord_data$BsmtExposure[Ord_data$BsmtExposure== "Gd"]<- 4
  Ord_data$BsmtExposure[Ord_data$BsmtExposure== "Av"]<- 3
  Ord_data$BsmtExposure[Ord_data$BsmtExposure== "Mn"]<- 2
  Ord_data$BsmtExposure[Ord_data$BsmtExposure== "No"]<- 1
  Ord_data$BsmtExposure<-as.factor(Ord_data$BsmtExposure)
  
  Ord_data$BsmtFinType1<-as.numeric(Ord_data$BsmtFinType1)
  Ord_data$BsmtFinType1[Ord_data$BsmtFinType1== "GLQ"]<- 6
  Ord_data$BsmtFinType1[Ord_data$BsmtFinType1== "ALQ"]<- 5
  Ord_data$BsmtFinType1[Ord_data$BsmtFinType1== "BLQ"]<- 4
  Ord_data$BsmtFinType1[Ord_data$BsmtFinType1== "Rec"]<- 3
  Ord_data$BsmtFinType1[Ord_data$BsmtFinType1== "LwQ"]<- 2
  Ord_data$BsmtFinType1[Ord_data$BsmtFinType1== "Unf"]<- 1
  Ord_data$BsmtFinType1[Ord_data$BsmtFinType1== "No"]<- 0
  Ord_data$BsmtFinType1<-as.factor(Ord_data$BsmtFinType1)
  
  Ord_data$HeatingQC<-as.numeric(Ord_data$HeatingQC)
  Ord_data$HeatingQC[Ord_data$HeatingQC== "Ex"]<- 3
  Ord_data$HeatingQC[Ord_data$HeatingQC== "Gd"]<- 2
  Ord_data$HeatingQC[Ord_data$HeatingQC== "TA"]<- 1
  Ord_data$HeatingQC[Ord_data$HeatingQC== "Fa"]<- 1
  Ord_data$HeatingQC[Ord_data$HeatingQC== "Po"]<- 1
  Ord_data$HeatingQC<-as.factor(Ord_data$HeatingQC)
  
  Ord_data$KitchenQual<-as.numeric(Ord_data$KitchenQual)
  Ord_data$KitchenQual[Ord_data$KitchenQual== "Ex"]<-3
  Ord_data$KitchenQual[Ord_data$KitchenQual== "Gd"]<-2
  Ord_data$KitchenQual[Ord_data$KitchenQual== "TA"]<-1
  Ord_data$KitchenQual[Ord_data$KitchenQual== "Fa"]<-1
  Ord_data$KitchenQual<-as.factor(Ord_data$KitchenQual)
  
  Ord_data$FireplaceQu<-as.numeric(Ord_data$FireplaceQu)
  Ord_data$FireplaceQu[Ord_data$FireplaceQu== "Ex"]<- 3
  Ord_data$FireplaceQu[Ord_data$FireplaceQu== "Gd"]<- 3
  Ord_data$FireplaceQu[Ord_data$FireplaceQu== "TA"]<- 2
  Ord_data$FireplaceQu[Ord_data$FireplaceQu== "Fa"]<- 2
  Ord_data$FireplaceQu[Ord_data$FireplaceQu== "Po"]<- 2
  Ord_data$FireplaceQu[Ord_data$FireplaceQu== "No"]<- 1
  Ord_data$FireplaceQu<-as.factor(Ord_data$FireplaceQu)
  
  Ord_data$GarageFinish<-as.numeric(Ord_data$GarageFinish)
  Ord_data$GarageFinish[Ord_data$GarageFinish== "Fin"]<- 4
  Ord_data$GarageFinish[Ord_data$GarageFinish== "RFn"]<- 3
  Ord_data$GarageFinish[Ord_data$GarageFinish== "Unf"]<- 2
  Ord_data$GarageFinish[Ord_data$GarageFinish== "No"]<- 1
  Ord_data$GarageFinish<-as.factor(Ord_data$GarageFinish)
  
  
  # remooving the unimportant variables from ordinal data
  
  Ord_data = Ord_data[ , !(names(Ord_data) %in% c("Utilities", "LandSlope", "ExterCond", "BsmtCond", "BsmtFinType2", "Electrical", "Functional", "GarageQual", "GarageCond", "PavedDrive", "PoolQC", "Fence"))]
  
  Nom_data = Nom_data[, !(names(Nom_data) %in% c("Street", "CentralAir", "Alley", "LandContour", "Condition1", "Condition2", "BldgType", "RoofMatl", "Heating", "MiscFeature", "SaleType", "SaleCondition") )]
  
  
  #random forests of clean data
  
  cleannames<- names(clean_data)
  cleannames<-cleannames[!cleannames %in% c("SalePrice")]
  cleannames1<- paste(cleannames, collapse = "+")
  clean.form<- as.formula(paste("SalePrice", cleannames1, sep = "~"))
  clean_fit<- randomForest(clean.form, clean_data, importance = TRUE, ntree = 200)
  plot(nom_fit)
  varImpPlot(train_fit)
  
  model1 <- lm(clean_data.SalePrice ~ ., data = run)
  
    pred<- cbind(run)
  
  