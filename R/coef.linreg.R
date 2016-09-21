#' The coefficients from the linear regression 'linreg'
#' 
#' The coef() returns all estimates for the coefficients in your linear regression.
#' 
#' 
#' @export

coef.linreg <- function(l) { 
  #print(paste("Formula: (", l$formula[2], " ~ ", l$formula[3],") ,   data: ", l$dataset, sep=""))
  Coefficients<-as.vector(l$coefficients)
  names(Coefficients) = c("(Intercept)", paste(l$formula[3]))
  print(Coefficients)
}
