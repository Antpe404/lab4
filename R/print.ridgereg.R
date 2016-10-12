#' The coefficients from the Ridge regression 'ridgereg'.
#' 
#' The print() returns the names and the regression coefficients. 
#' 
#' 
#' @export




print.ridgereg<- function(l){
  call<-  l$formula
  coeff<- as.vector(l$coefficients)
  names(coeff)<- c(paste(rownames(l$coefficients)))
  result<- list(Formula =call, Coefficients = coeff, Dataset=l$dataset)
  print(result)
}

#print(mylm)
