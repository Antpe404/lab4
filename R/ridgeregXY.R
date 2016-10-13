ridgeregXY<-function(x, y , lambda = 0){
  
  des.mat <- cbind(rep(1, nrow(x)), x)
  colnames(des.mat)[1]<-"(Intercept)"
  dep.var<-as.matrix(y)
  des.mat[,2:ncol(des.mat)] <- scale(des.mat[,2:ncol(des.mat)])
  des.mat<-as.matrix(des.mat)
  lambda<-diag(lambda, nrow=dim(t(des.mat)%*%des.mat)[1], ncol=dim(t(des.mat)%*%des.mat)[2])
  
  BETAridge <- solve((t(des.mat)%*%des.mat) + lambda) %*% t(des.mat) %*% (dep.var)
  
  y.hat <- (des.mat %*% BETAridge)  
  
  l<-list(coefficients =BETAridge, y.hat = y.hat,formula = formula, dataset=deparse(substitute(data)))
  class(l) <- "ridgereg"
  return(l)
}
#colnames(longley)[1]<-"y"
#y<-longley$y
#x<-longley[,2:ncol(longley)]

#ridgeregXY(y=longley$y, x=longley[,2:ncol(longley)], lambda=2 )
