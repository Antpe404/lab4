#' Available methods in 'linreg'
#' 
#' The methods available for the linreg-package are described here.
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
    
    yhat <- newdata %*% t(l$coefficients) #Skriv funktionen här
    return(yhat)
  }
}
