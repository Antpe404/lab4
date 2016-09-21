## ---- echo=TRUE----------------------------------------------------------
library(lab4)
mylm<-linreg( Petal.Length ~ Petal.Width, iris)


## ---- echo=TRUE----------------------------------------------------------
print(mylm)

## ---- echo=TRUE----------------------------------------------------------
#Show the first 5 fitted values of the mylm-object.
head( pred(mylm), 5)


## ---- echo=TRUE----------------------------------------------------------
resid(mylm)[1:5] #Prints the first five elements

## ---- echo = TRUE--------------------------------------------------------
summary(mylm)


## ------------------------------------------------------------------------
coef(mylm)

## ---- fig.width = 7.4, fig.height = 5, message= FALSE--------------------
plot(mylm)

