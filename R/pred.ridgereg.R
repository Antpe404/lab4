#' The fitted values from the Ridge regression 'ridgereg'
#' 
#' The pred()-statement returns a vector including all fitted values from the Ridge regression.
#' 
#'
#' @export 

pred.ridgereg<-function(l,newdata = NA) {
  
  if(is.na(newdata)){
    return(l$y.hat)
    
  } else if(is.matrix(newdata) | is.data.frame(newdata)) {
    if(!all(newdata[,1] == 1)){ 
      newdata<-as.matrix(cbind(as.matrix(rep(1,nrow(newdata)),ncol=1),newdata))
      #Creats an extra column with 1's so the b0-coef gets calculated.
      }
    
    yhat <- newdata %*% t(l$coefficients) 
    return(yhat)
  }
}