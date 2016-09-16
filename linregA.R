data.class() #Behover nog den har far att testa om det är S3 eller RC.

if(!is.expression(formula)) stop("The given formula is not a correct expression") #oklar om den är nödvändig

test<-function(formula,data){
  
  
  des.mat <- model.matrix(formula , data) #Extracts the model matrix
  dep.var <- all.vars(formula)[1]         #Extracts the name of the y-variable 
  dep.var <- as.matrix(data[dep.var])     #Extracts the data of the y-variable 
  # and overwrites it with the data-colum 
  
  betahat <- solve( t(des.mat) %*% des.mat )  %*% t(des.mat) %*% dep.var #Calculating the beta coeffs. (X' %*% X)^-1 %*% X' %*% y
  
  yhat <- des.mat %*% betahat # Calculating the y-hat  , y_hat = X %*% beta_hat 
  
  resid <- dep.var - yhat  #Calculating the residuals e= y- y_hat 
  #################
  df <- nrow(desmat)-ncol(desmat)  #Degrees of freedom, n - p 
  #Vet inte vfr KS hade -1 på slutet. TÄnker mig att df bör vara nrow-antal coef eg. Typ nrow(data)-length(betahat)
  
  residVar <-( t(res.err) %*% res.err ) / degree.free #Calculating the residual variance (e' %*% e) / df  
  
  var.hat.bhat <- res.var2 %*%  solve( t(des.mat) %*% des.mat ) #Calculating 
  
  t.beta <- beta.hat / sqrt( var.hat.bhat )
  my.pvalues<- pt(t.beta)
  
}

test(Sepal.Length~ Sepal.Width, iris)

############ALLT NEDANFÖR ÄR EGNA MANUELLA TESTER

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

model.matrix(~Sepal.Length + Sepal.Width, iris)
model.matrix(Sepal.Length ~ Sepal.Width, iris)

dd <- data.frame(a = gl(3,4), b = gl(4,1,12)) # balanced 2-way
dd

options("contrasts")
model.matrix(~ a + b, dd)

fm1 <- lm(Sepal.Length ~ Sepal.Width, iris) #Blir samma som i Emils kod     
desmat<-model.matrix(fm1)#Vilket visas här.

Sepal.Length~Sepal.Width[1]
all.vars(Sepal.Length~Sepal.Width)[1]

formula<-(Sepal.Length~Sepal.Width)
depvar<-all.vars(formula)[1]
depvar<-as.matrix(iris[depvar])

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
