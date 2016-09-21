#' The residuals from the linear regression 'linreg'
#' 
#' The resid() returns all the residuals from the model in a vector.
#' 
#' 
#' @export

residuals.linreg <- function(l) { 
  print("Vector containing the residuals")
  return(as.vector(l$res.err))
  
}

#resid(mylm)
