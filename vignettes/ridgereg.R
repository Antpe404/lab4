## ------------------------------------------------------------------------
library(lab4)
data("longley")
names(longley)[1] <- "y"
mylmrid <- ridgereg(formula=y~. , data = longley, lambda = 2) #fitting the model
pred.data <- longley[11:15,-1] + 2  #generate some new data to predict 

pred(l = mylmrid, newdata = pred.data)


## ------------------------------------------------------------------------

library(caret)
library(mlbench)

data(BostonHousing)

set.seed(3456)
trainIndex <- createDataPartition(BostonHousing$chas, p = .75, 
                                  list = FALSE, 
                                  times = 1)

bostonTrain <- BostonHousing[trainIndex,]
bostonTest <- BostonHousing[-trainIndex,]




## ------------------------------------------------------------------------
#linear model
ridFit <- train(crim ~ . , data = bostonTrain, method = "lm") 

#forward selection
ridFitForward <- train(crim ~ . , data = bostonTrain, method = "leapForward") 




## ------------------------------------------------------------------------
predictions<-as.data.frame(x= 
  predict(ridFitForward,newdata = bostonTest) 
  ) 
colnames(predictions) <- "pred"

ggplot()+geom_point(data = bostonTest,aes(y=crim,x=c(1:length(bostonTest$crim))))+
  geom_point(data=predictions,aes(y = pred,x=c(1:length(bostonTest$crim))),col="red") + xlab("")


## ------------------------------------------------------------------------

fitControl <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 10)

set.seed(3245)
wigdeweg <- train(crim~ .,data=bostonTrain,method="ridge", trControl = fitControl,
                  tuneGrid= expand.grid(lambda=seq(0,20,1)) )

print(wigdeweg)

