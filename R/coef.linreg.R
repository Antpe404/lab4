#' Available methods in 'linreg'
#' 
#' The methods available for the linreg-package are described here.
#' 
#' 
#' @export

coef.linreg <- function(l) { 
  #print(paste("Formula: (", l$formula[2], " ~ ", l$formula[3],") ,   data: ", l$dataset, sep=""))
  Coefficients<-as.vector(l$coefficients)
  names(Coefficients) = c("(Intercept)", paste(l$formula[3]))
  print(Coefficients)
}
