data.class() #Behover nog den har far att testa om det är S3 eller RC.

if(!is.expression(formula)) stop("The given formula is not a correct expression") #oklar om den är nödvändig

test<-function(formula,data){
  
  
  des.mat <- model.matrix(formula , data) #Extracts the model matrix
  dep.var <- all.vars(formula)[1]         #Extracts the name of the y-variable 
  dep.var <- as.matrix(data[dep.var])     #Extracts the data of the y-variable 
                                          # and overwrites it with the data-colum 

  beta.hat <- solve( t(des.mat) %*% des.mat )  %*% t(des.mat) %*% dep.var #Calculating the beta coeffs. (X' %*% X)^-1 %*% X' %*% y
  
  y.hat <- des.mat %*% beta.hat # Calculating the y-hat  , y_hat = X %*% beta_hat 
  
  res.err <- dep.var - y.hat  #Calculating the residuals e= y- y_hat 
  #################
  degree.free <- nrow(des.mat) - ncol(des.mat) - 1  #Degrees of freedom, n - p 
  #OKLART OM p = b0 till b4 = 5  eller om det är p = b1 -b4 = 4 
  
  res.var2 <-( t(res.err) %*% res.err ) / degree.free #Calculating the residual variance (e' %*% e) / df  
  
  var.hat.bhat <- res.var2 %*%  solve( t(des.mat) %*% des.mat ) #Calculating 
  
  t.beta <- beta.hat / sqrt( var.hat.bhat )
  my.pvalues<- pt(t.beta)
  
  }

test(Sepal.Length~ Sepal.Width, iris)
