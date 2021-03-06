#' The fitted values from the linear regression 'linreg'
#' 
#' The pred()-statement returns a vector including all fitted values from the linear regression.
#' 
#'
#' @export 

pred.linreg<-function(l,newdata = NA) {
  
  if(is.na(newdata)){
    return(l$y.hat)
    
  } else if(is.matrix(newdata) | is.data.frame(newdata)) {
    if(!all(newdata[,1] == 1)){ 
      newdata<-as.matrix(cbind(as.matrix(rep(1,nrow(newdata)),ncol=1),newdata))
    }
    
    yhat <- newdata %*% t(l$coefficients) #Skriv funktionen h�r
    return(yhat)
  }
}
