
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

# Convolutional NN
data <- mx.symbol.Variable('data')
# first convolution layer with 20 5x5 filters, pooling with max 2x2 
conv1 <- mx.symbol.Convolution(data=data, kernel=c(5,5), num_filter=20)
relu1 <- mx.symbol.Activation(data=conv1, act_type="relu")
pool1 <- mx.symbol.Pooling(data=relu1, pool_type="max",
                            kernel=c(2,2), stride=c(2,2))

# second convolution layer with 50 5x5 filters, pooling with max 2x2
conv2 <- mx.symbol.Convolution(data=pool1, kernel=c(5,5), num_filter=50)
relu2 <- mx.symbol.Activation(data=conv2, act_type="relu")
pool2 <- mx.symbol.Pooling(data=relu2, pool_type="max",
                            kernel=c(2,2), stride=c(2,2))

# first fullconnected layer with 500 neurons
flatten <- mx.symbol.Flatten(data=pool2)
fc1 <- mx.symbol.FullyConnected(data=flatten, num_hidden=500)
relu3 <- mx.symbol.Activation(data=fc1, act_type="relu")

# second fullconnected layer with 10 neurons (output)
fc2 <- mx.symbol.FullyConnected(data=relu3, num_hidden=10)
# loss
lenet <- mx.symbol.SoftmaxOutput(data=fc2)

# data for convolution set up
train.array <- train.x
dim(train.array) <- c(28, 28, 1, ncol(train.x))
test.array <- test
dim(test.array) <- c(28, 28, 1, ncol(test))
mx.set.seed(0)

# Model Training
model <- mx.model.FeedForward.create(lenet, X=train.array, y=train.y,
                                      ctx=device.cpu, num.round=20, array.batch.size=100,
                                      learning.rate=0.05, momentum=0.9, wd=0.00001,
                                      eval.metric=mx.metric.accuracy,
                                      epoch.end.callback=mx.callback.log.train.metric(100))

# Predicting Labels
preds <- predict(model, test.array)
pred.label <- max.col(t(preds)) - 1

#Accuracy 98.5% 
