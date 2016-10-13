#' Ridge regression
#'
#' Computes a ridge model using the Ridge -square method
#' The function also fitted values. 
#'
#' @param x    independent variable (x)
#'    
#' @param y               The dependent (y) 
#'                  
#' @param data       a data set from which the used variables are taken
#' @return an list of class 'ridgereg' containing beta coefficients and fitted values.
#' 
#' @details 
#'  These measures are stored in a list of class 'ridgereg'. 
#' 
#'  
#' @references 
#'
#' 
#' @examples 
#' ## Make a simple linear regression of how the Petal.Length responds to the Petal.Width in the dataset iris.
#'    ridgereg(Petal.Length ~ Species, lambda = 5, iris)
#'
#' @export

ridgeregXY<-function(y,x , lambda = 0){
  
  #des.mat <- cbind(rep(1, nrow(x)), x)
  #colnames(des.mat)[1]<-"(Intercept)"
  #dep.var<-as.matrix(y)
  #des.mat[,2:ncol(des.mat)] <- scale(des.mat[,2:ncol(des.mat)])
  #des.mat<-as.matrix(des.mat)
  #lambda<-diag(lambda, nrow=dim(t(des.mat)%*%des.mat)[1], ncol=dim(t(des.mat)%*%des.mat)[2])
  
  des.mat<- as.numeric(as.matrix(x))
  dep.var<- as.matrix(as.numeric(y))
  des.mat<- scale(des.mat)
  des.mat<- as.matrix(des.mat)
  lambda<-diag(lambda, nrow=dim(t(des.mat)%*%des.mat)[1], ncol=dim(t(des.mat)%*%des.mat)[2])
  
  BETAridge <- solve((t(des.mat)%*%des.mat) + lambda) %*% t(des.mat) %*% (dep.var)
  
  y.hat <- (des.mat %*% BETAridge)  
  
  l<-list(coefficients =BETAridge, y.hat = y.hat)
  class(l) <- "ridgereg"

  return(l) 
}
#colnames(longley)[1]<-"y"
#y<-longley$y
#x<-longley[,2:ncol(longley)]

#ridgeregXY(y=longley$y, x=longley[,2:ncol(longley)], lambda=2 )
