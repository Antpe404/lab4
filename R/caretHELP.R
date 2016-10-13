
#create a new list 

ridgeregXY <- list(type=c("Regression"), 
                   library="lab4",
                   loop=NULL)

#The parameter elements


ridgeregXY$parameters <- data.frame(parameter="lambda",
                                    class="numeric",
                                    label="lambda")


#lmGrid <- function(x, y, len = NULL, search = "grid") {
# lambda <- data.frame( = seq(0,200,by=10))
#}

#ridgeregXY$grid <- lmGrid


Fit <- function(y, x, lambda, param, lev, last, classProbs, ...){
  ridgeregXY(y=y, x=x, lambda=param$lambda)$y.hat 
}
ridgeregXY$fit <- Fit

ridgeregXY$prob<-list(NULL)



ridgeregXY$predict<-function(modelFit, newdata, preProc=NULL, submodels=NULL){
  pred(modelFit, newdata)
}

ridgeregXY$sort<-function (x) x[order(-x$lambda), ]

ridgeregXY$label<-"ridgeregXY"



ridgeregXY$grid <- function(y,x, len=NULL, search="grid"){
  data.frame(lambda=c(0.001,0.5,5,30,70,400))
}


fitControl <- trainControl(
  method = "cv",
  number = 10,
  repeats = 10)

set.seed(3245)
lm.ridge <- train(y=bostonTrain$crim, x=is.numeric(bostonTrain[,-1]), method=ridgeregXY, trControl = fitControl)
#lm.ridge         

