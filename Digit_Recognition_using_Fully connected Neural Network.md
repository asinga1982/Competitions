# MNIST data set, Hand written Digit Recognition using Fully connected Neural Network

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

# Fully connected 4 layer Neural network  
data <- mx.symbol.Variable("data")  
fc1 <- mx.symbol.FullyConnected(data, name="fc1", num_hidden=128)  
act1 <- mx.symbol.Activation(fc1, name="relu1", act_type="relu")  
fc2 <- mx.symbol.FullyConnected(act1, name="fc2", num_hidden=64)  
act2 <- mx.symbol.Activation(fc2, name="relu2", act_type="relu")  
fc3 <- mx.symbol.FullyConnected(act2, name="fc3", num_hidden=10)  
softmax <- mx.symbol.SoftmaxOutput(fc3, name="sm")  
devices <- mx.cpu()  
mx.set.seed(0)  

# Model Training    
model <- mx.model.FeedForward.create(softmax, X=train.x, y=train.y,  
                                     ctx=devices, num.round=10, array.batch.size=100,  
                                     learning.rate=0.07, momentum=0.9,  eval.metric=mx.metric.accuracy,  
                                     initializer=mx.init.uniform(0.07),  
                                     epoch.end.callback=mx.callback.log.train.metric(100))  
                                     
                                     
# Predict output
preds <- predict(model, test)  
pred.label <- max.col(t(preds)) - 1  
