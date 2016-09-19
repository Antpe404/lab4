data.class() #Behover nog den har far att testa om det är S3 eller RC.

if(!is.expression(formula)) stop("The given formula is not a correct expression") #oklar om den är nödvändig

test<-function(formula,data){
  
  
  desmat <- model.matrix(formula , data) #Extracts the model matrix
  depvar <- all.vars(formula)[1]         #Extracts the name of the y-variable 
  depvar <- as.matrix(data[depvar])     #Extracts the data of the y-variable 
  # and overwrites it with the data-colum 
  
  betahat <- solve( t(desmat) %*% desmat )  %*% t(desmat) %*% depvar # (X' %*% X)^-1 %*% X' %*% y
  
  yhat <- desmat %*% betahat # Calculating the y-hat  , y_hat = X %*% beta_hat 
  
  residErr <- depvar - yhat  #Calculating the residuals e= y- y_hat 
 
  degreesFreedom <- nrow(desmat)-ncol(desmat)  #Degrees of freedom, n - p 
  #Vet inte vfr KS hade -1 på slutet. TÄnker mig att df bör vara nrow-antal coef eg. Typ nrow(data)-length(betahat)
  
  residVar <-( t(residErr) %*% residErr) / degreesFreedom #Calculating the residual variance (e' %*% e) / df  
  
 VarRegCoef<- as.vector(residVar)*solve(t(desmat) %*% desmat)
VarRegCoef<-diag(VarRegCoef)
 #Vet inte om det är denna som ska has?
  
 t_values <- betahat / sqrt(VarRegCoef)
  #Denna tar första elementet i betahat, dvs Beta0-skattningen, genom roten av dess varians.
  #Andra elementet i t_test är således beta1-skattningen genom roten av dess varians.
 
 
 p_values<-(1-pt(abs(t_values), df=148))*2
 #p_values<-p_values*2
 
 listan<-list(betahat, yhat, residErr, degreesFreedom, residVar, VarRegCoef, t_values, p_values)
 class(listan)<-"linreg"
 return(listan)
}

test(Sepal.Length~ Sepal.Width, iris)
fm2 <- lm(Sepal.Length ~ Petal.Length, iris)
summary(fm2)

############ALLT NEDANFÖR ÄR EGNA MANUELLA TESTER

fm1 <- lm(Sepal.Length ~ Sepal.Width, iris) #Blir samma som i Emils kod     
desmat<-model.matrix(fm1)#Vilket visas här.

formula<-(Sepal.Length~Sepal.Width)
depvar<-all.vars(formula)[1]
depvar<-as.matrix(iris[depvar])


#Manuell uträkning av betahat
del1<-solve((t(desmat)%*%desmat))#2*2
del2<-del1%*%t(desmat)#(2*2)*(2*150)->2*150
betahat<-del2%*%depvar

#Manuell uträkning av yhat
yhat<-desmat%*%betahat

#Manuell uträkning av res
res<-depvar-yhat
sum(res)#Ser ut att vara 0, fint.

#Manuellt uträkning av df
df<-nrow(desmat)-ncol(desmat)

#Manuellt uträkning av residual variance
resvar<-(t(res)%*%res)/df

#Manuellt test av regression coefficients
VarRegCoef<- as.vector(resvar)*solve(t(desmat) %*% desmat)
VarRegCoef<-diag(VarRegCoef)
#Om det vi söker är variance of the slope and beta0 bör detta vara rätt. 
#VarRegCoef är i nuläget covariance matrixen, alltså densamma som vcov(fm1) i nedan testande
#lm-funktionen. vcov(fm1)=VarRegCoef.

#Manuellt t-test
t_values <- betahat / sqrt(VarRegCoef)
p_values<-1-pt(abs(t_values), df=148)
p_values<-p_values*2

summary(fm1)



var.hat.bhat <- res.var2 %*%solve(t(des.mat) %*% des.mat ) #Calculating 


model.matrix(~Sepal.Length + Sepal.Width, iris)
model.matrix(Sepal.Length ~ Sepal.Width, iris)

dd <- data.frame(a = gl(3,4), b = gl(4,1,12)) # balanced 2-way
dd

options("contrasts")
model.matrix(~ a + b, dd)

Sepal.Length~Sepal.Width[1]
all.vars(Sepal.Length~Sepal.Width)[1]


dim(t(model.matrix(fm1)))
dim(iris[depvar])

(solve(t(model.matrix(fm1))%*%model.matrix(fm1))%*%t(model.matrix(fm1)))%*%iris[depvar]
solve(t(model.matrix(fm1))%*%iris[depvar])

dim(iris[depvar])
iris[depvar]

solve((t(desmat)%*%desmat))
solve((t(desmat)%*%desmat))%*%t(desmat)#2*150
solve((t(desmat)%*%desmat))%*%t(desmat)%*%as.matrix(depvar)

lm(Sepal.Length ~ Sepal.Width, iris)
fm1$df.residual


#Angående the var of the reg coef.
summary(fm1)$coefficients
summary(fm1)$coefficients[2,2]
(summary(fm1)$coefficients[2,2])**2
#Ovanstående rad equals nedanstående
vcov(fm1)[2,2]
