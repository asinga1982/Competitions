#Kaggle house price prediction
library(doSNOW)
library(doBy)
library(data.table)
library(rpart)
library(elasticnet)
#library(rqPen)
library(relaxo)

set.seed(37596)
cv.5.folds <- createMultiFolds(train$SalePrice[-c(524,969,1299)], k = 5, times = 10)

ctrl.5 <- trainControl(method = "repeatedcv", number = 5, repeats = 10,
                       index = cv.5.folds)

fitControl <- trainControl(method = "cv", number = 5)

#pro.cent.scale <- preProcess(method=c("center", "scale"))

lm.labels <- log(train$SalePrice[1:1460])
lm.labels1 <- train$SalePrice[1:1460]

lm.cv <- function(seed, training, labels, ctrl, mthd) {
#  cl <- makeCluster(6, type = "SOCK")
#  registerDoSNOW(cl)
  
  set.seed(seed)
  # Leverage formula interface for training
  lm1.cv <- train(x = training, y = labels, method = mthd, tuneLength=30,
            #      preProcess =  c("center", "scale"),
                    trControl = ctrl)
  
  #Shutdown cluster
#  stopCluster(cl)
#  RMSE(log(labels), log(lm1.cv$pred))
  
  return (lm1.cv)
}

lm1.cv <- function(ctrl) {

  set.seed(94622)
  # Leverage formula interface for training
  lm1.cv <- train(log(SalePrice)~OverallQual.Cate:OverallCond.cate+bq+kq+OverallQual+lotarea+lotareasq+age.atSale+loggrlive+
                    newneighbour1+ total.BsmtSF.limited+fq+be+gq+CentralAir+MSZoning+GarageCars+
                    subclass+room+recently.built+bathyes+garg.type+PavedDrive+screenporch+
                    KitchenAbvGr+WoodDeckSF, method = "lm", tuneLength=10, data=comb[-c(524,969,1299, 1461:2919),],
              preProcess =  c("pca"),
              trControl = ctrl)
  
  return (lm1.cv)
}


lasso.cv <- function(seed, training, labels, ctrl) {
  set.seed(seed)
  # Leverage formula interface for training
  lasso1.cv <- train(x = training, y = labels, method = "glmnet", tuneLength=50,
                        preProcess =  c("center", "scale"),
                  trControl = ctrl)
  
  return (lasso1.cv)
}

rf.cv <- function(seed, training, labels, ctrl) {

  set.seed(seed)
  # Leverage formula interface for training
  rf1.cv <- train(x = training, y = labels, method = "rf", tuneLength = 5,  
                  #      preProcess =  c("center", "scale"),
                  trControl = ctrl, ntree=3000)
  
  return (rf1.cv)
}


rf1.cv <- function(seed, ctrl) {
  
  set.seed(seed)
  # Leverage formula interface for training
  rf1.cv <- train(log(SalePrice)~OverallQual.Cate:OverallCond.cate+bq+kq+OverallQual+lotarea+lotareasq+age.atSale+loggrlive+
                    newneighbour1+ total.BsmtSF.limited+fq+be+gq+CentralAir+MSZoning+GarageCars+
                    subclass+room+recently.built+bathyes+garg.type+PavedDrive+screenporch+
                    KitchenAbvGr+WoodDeckSF, data=comb[-c(524,969,1299, 1461:2919),],
                method = "rf", 
                tuneLength = 5,  
                  #      preProcess =  c("center", "scale"),
                trControl = ctrl, ntree=2000)
  
  return (rf1.cv)
}


#Rpart cv
rpt.cv <- function(seed, training, labels, ctrl) {
  
  set.seed(seed)
  # Leverage formula interface for training
  rpt1.cv <- train(x = training, y = labels, method = "rpart", tuneLength = 20,   
                  #      preProcess =  c("center", "scale"), 
                  trControl = ctrl)
  
  return (rpt1.cv)
}

# Grab features
#features <- c("OverallQual", "lotarea", "loggrlive","GarageArea","YearRemodAdd",
#              "newneighbour1", "roomsize", "TotalBsmtSF", "gf", "TotRmsAbvGrd", "bq", "fq", "kq", 
#              "be", "eq", "sc", "bc", "gq", "pool", "CentralAir", "MSZoning", "GarageCars", 
#              "subclass")

features.1 <- c("OverallQual", "lotarea", "lotareasq" ,"loggrlive", 
              "newneighbour1", "total.BsmtSF.limited", "fq", "kq", 
              "be", "sc", "bc", "gq", "CentralAir", "MSZoning", "GarageCars", 
              "subclass", "room" , "recently.built", "found", "bathyes", "garg.type", 
              "PavedDrive", "screenporch", "age.atSale", "sale.type", "KitchenAbvGr", 
              "WoodDeckSF")


# extracted selected features and centre the data
train.1 <- comb[, features.1]

preProc <- preProcess(train.1, method = c("center", "scale"))

train.centred <- predict(preProc, train.1)

#Single lm run:
train.df1 <- data.frame(cbind(train.centred[1:1460,], lm.labels))

summary(lm(lm.labels~., data = train.df1))


lm.5.cv.1 <- lm1.cv( ctrl.5)
# Run CV and check out results on linear Reg
lm.3.cv.1 <- lm.cv(94622, train.centred[1:1460,], lm.labels, ctrl.5, "BstLm")
lm.3.cv.1

# Run CV and check out results on lasso
x <- dummyVars(~.,train.centred)
x2 <- predict(x, train.centred)
lasso.3.cv.1 <- lasso.cv(94622, x2[1:1460,], lm.labels, ctrl.5)
lasso.3.cv.1

# Run CV and check out results on RF
rf.1.cv.5 <- rf.cv(94622, train.1[1:1460,], lm.labels1, ctrl.5)
rf.1.cv.5

rf.2.cv.5 <- rf1.cv(94622, fitControl)


# Run CV and check out results on RF
rpt.1.cv.5 <- rpt.cv(94622, train.1[1:1460,], lm.labels1, ctrl.5)
rpt.1.cv.5

summary(lm.3.cv.1$finalModel)

#Prepare test set and predict
test.submit.df <- comb[1461:2919, features.1]
lm.1.preds <- exp(predict(lm.3.cv.1$finalModel, train.centred[1461:2919,]))

lasso.1.preds <- exp(predict(lasso.3.cv.1$finalModel, x2[1461:2919,], 
                             s=lasso.3.cv.1$finalModel$lambdaOpt))

newlm <- lm(log(SalePrice)~OverallQual.Cate:OverallCond.cate+bq+kq+OverallQual+lotarea+lotareasq+age.atSale+loggrlive+
              newneighbour1+ total.BsmtSF.limited+fq+be+gq+CentralAir+MSZoning+GarageCars+
              subclass+room+recently.built+bathyes+garg.type+PavedDrive+screenporch+
              KitchenAbvGr+WoodDeckSF,data=comb[-c(524,1299, 1461:2919),]) 

#XGBoost
xx <- model.matrix(~OverallQual.Cate:OverallCond.cate+bq+kq+OverallQual+lotarea+lotareasq+age.atSale+loggrlive+
                     newneighbour1+ total.BsmtSF.limited+fq+be+gq+CentralAir+MSZoning+GarageCars+
                     subclass+room+recently.built+bathyes+garg.type+PavedDrive+screenporch+
                     KitchenAbvGr+WoodDeckSF-1, data=comb[-c(524,969,1299, 1461:2919),])

yy <- lm.labels[-c(524,969,1299)]

dtrain <- xgb.DMatrix(data = xx, label= yy)

set.seed(1234)
bstDMatrix <- xgb.cv(data = dtrain, max.depth = 25, eta = .1, nthread = 4, nround = 200, 
                     objective = "reg:linear", nfold=5)
set.seed(1234)
bstModel <- xgb.train(data = dtrain, max.depth = 25, eta = .1, nthread = 4, nround = 200, 
                     objective = "reg:linear")


txx <- model.matrix(~OverallQual.Cate:OverallCond.cate+bq+kq+OverallQual+lotarea+lotareasq+age.atSale+loggrlive+
                     newneighbour1+ total.BsmtSF.limited+fq+be+gq+CentralAir+MSZoning+GarageCars+
                     subclass+room+recently.built+bathyes+garg.type+PavedDrive+screenporch+
                     KitchenAbvGr+WoodDeckSF-1, data=comb[1461:2919,])

pred1 <- predict(bstModel, txx)
tpred1 <- exp(pred1)

#Combinin Models
DF <- data.frame(exp(lm.5.cv.1$finalModel$fitted.values),  exp(rf.2.cv.5$finalModel$predicted), actual=comb$SalePrice[-c(524,969,1299, 1461:2919)])

#Write to output for submission
submit.df1 <- data.frame(Id = rep(1461:2919), SalePrice = lm.1.preds)
write.csv(submit.df1, "Pred_16Jan_03.csv",row.names = FALSE)



#Plots
with(comb[1:1460,] ,{qplot((TotRmsAbvGrd), log(SalePrice), geom=c("point", "smooth"))} )

qplot(comb$SalePrice[1:1460], col=comb$subclass[1:1460] ,geom="density")

#Residue Analysis
qplot(lm.1.cv.1$finalModel$fitted.values,lm.1.cv.1$finalModel$residuals, geom=c("point"),
      col=as.factor(comb$roomsize[1:1460]))


qplot(comb$Id[1:1460], lm.3.cv.1$finalModel$residuals, geom=c("point"))

View(comb[which(lm.3.cv.1$finalModel$residuals < -0.5),])


#Featrue Engg:
#1. Combining Neighours by response 
Neigh.Summary <- summaryBy(SalePrice ~ newneighbour, data = comb[1:1460,], 
                           FUN = list(mean, max, min, median, sd))

Neigh.Summary <- setorder(Neigh.Summary,SalePrice.median)

comb$newneighbour1 <- as.character(comb$newneighbour)

comb$newneighbour1[comb$newneighbour == "NoRidge" |
                   comb$newneighbour == "NridgHt" ]    <- "NoRidge,NridgHt"

comb$newneighbour1[comb$newneighbour == "Somerst" |
                     comb$newneighbour == "Timber" ]    <- "Somerst,Timber"

comb$newneighbour1[comb$newneighbour == "SawyerW" |
                   comb$newneighbour == "Gilbert" |
                   comb$newneighbour == "NWAmes" |  
                   comb$newneighbour == "CollgCr" |   
                   comb$newneighbour == "Crawfor" ] <- "SawyerW,Gilbert,NWAmes,CollgCr,Crawfor"

comb$newneighbour1[comb$newneighbour == "IDOTRR" |
                   comb$newneighbour == "OldTown" |
                   comb$newneighbour == "Edwards" |  
                     comb$newneighbour == "BrkSide" ]  <- "IDOTRR,OldTown,Edwards,BrkSide"

comb$newneighbour1[comb$newneighbour == "Sawyer" |
                   comb$newneighbour == "NAmes" |
                     comb$newneighbour == "Mitchel"  ]    <- "Sawyer,NAmes,Mitchel"

comb$newneighbour1[comb$newneighbour == "Other" ]    <- "Other"
comb$newneighbour1 <- as.facto(comb$newneighbour1)

comb$loggrlive <- log(comb$grlive)

comb$bedroom <- comb$BedroomAbvGr
comb$bedroom[comb$BedroomAbvGr == 0] <- 1
comb$roomsize <- comb$GrLivArea/comb$bedroom

comb$total.BsmtSF.limited <- comb$TotalBsmtSF
comb$total.BsmtSF.limited[comb$TotalBsmtSF > 2200] <- 2200

comb$lotshape <- comb$LotShape
comb$lotshape[comb$LotShape != "Reg"] <- "Irr"
comb$lotshape <- as.factor(comb$lotshape)
