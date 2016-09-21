#' Available methods in 'linreg'
#' 
#' The methods available for the linreg-package are described here.
#' 
#' 
#' @export

residuals.linreg <- function(l) { 
  print("Vector containing the residuals")
  return(as.vector(l$res.err))
  
}

#resid(mylm)
