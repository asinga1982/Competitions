
# MNIST data set, Hand written Digit Recognition using Convolutional Neural Network

#Problem Statement:
Here, we need to identify the digit in given images. 
We have total 70,000 images (MNIST data set), out of which 49,000 are part of train images with the label of digit and rest 21,000 images are unlabeled (known as test images).   
Now, We need to identify the digit for test images.   

library(mxnet)  

# Data Preparation  
train <- data.matrix(train)  
test <- data.matrix(test)  
train.x <- train[,-1]  
train.y <- train[,1]  
train.x <- t(train.x)  
test <- t(test)  

