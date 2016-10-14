## ------------------------------------------------------------------------
library(lab4)
data("longley")
names(longley)[1] <- "y"
mylmrid <- ridgereg(formula=y~. , data = longley, lambda = 2) #fitting the model
pred.data <- longley[11:15,-1] + 2  #generate some new data to predict 

pred(l = mylmrid, newdata = pred.data)


