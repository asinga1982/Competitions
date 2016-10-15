## MNIST data set, Hand written Digit Recognition using PCA and XGBoost

#Problem Statement:
Here, we need to identify the digit in given images. 
We have total 70,000 images (MNIST data set), out of which 49,000 are part of train images with the label of digit and rest 21,000 images are unlabeled (known as test images).   
Now, We need to identify the digit for test images.   

## Pre processing  
require(EBImage)  
images <- list.files()  
tdf <- data.frame()  

## Read images and convert them to grey scale.   
for(i in 1:20000)  
{
    # Read image  
    img <- readImage(images[i])  
    # make image as Grey  
    greying <- channel(img,"gray")  

    # Get the image as a matrix  
    img_matrix <- greying@.Data  
    # Coerce to a vector  
    img_vector <- as.vector(t(img_matrix))  
    # Add label  
    #vec <- c(label, img_vector)  

    # Bind rows  
    tdf <- rbind(tdf,img_vector)  
    # Print status info  
    #print(paste("Done ", i, sep = ""))
}  

train <- read.csv("train.csv", header = T)  
df <- cbind(train[c(1:20000),2], df)  

## Remove zero variance features  
df1 <- removeConstantFeatures(df)  

##Removed Pixels  
df <- df[,-c(1,	2,	3,	4,	5,	6,	7,	17,	18,	19,	20,	21,	22,	23,	24,	25,	26,	27,	28,	29,	30,	31,	32,	33,	34,	43,	44,	45,	46,	52,	53,	54,	55,	56,	57,	58,	59,	60,	83,	84,	85,	86,	87,	112,	113,	114,	140,	141,	142,	169,	197,	225,	253,	281,	309,	337,	365,	393,	421,	449,	477,	505,	533,	561,	589,	617,	618,	645,	646,	672,	673,	674,	700,	701,	702,	727,	728,	729,	730,	731,	732,	755,	756,	757,	758,	759,	760,	761,	769,	770,	771,	777,	778,	779,	780,	781,	782,	783,	784)]

label <- train[1:20000,2]  

##Reduce dimensions using PCA  
df1 <- prcomp(df)  
pcatrain <- df1$x[,1:120]  

library(xgboost)  
label <- as.factor(label)  
dtrain <- xgb.DMatrix(data = pcatrain, label= label)  

##Cross Validation using XgBoost  
bstDMatrix <- xgb.cv(data = dtrain, max.depth = 10, eta = .78, nthread = 2, nround =30, objective = "multi:softmax", num_class = 11, nfold=5)  

##XgBoost Model  
bstDMatrix <- xgboost(data = dtrain, max.depth = 10, eta = .78, nthread = 2, nround=25, objective = "multi:softmax", num_class = 11, nfold=5)  

##Accuracy: 93%  
