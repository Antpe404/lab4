#' The coefficients from the linear regression 'linreg'.
#' 
#' The print() returns the names and the regression coefficients. 
#' 
#' 
#' @export




print.linreg<- function(l){
  call<-  l$formula
  coeff<- as.vector(l$coefficients)
  names(coeff)<- c('(Intercept)', paste(l$formula[3]))
  result<- list(Formula =call, Coefficients = coeff, Dataset=l$dataset)
  print(result)
}
#print(mylm) 
#print.linreg(mylm)