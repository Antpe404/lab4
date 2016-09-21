#' The coefficients from the linear regression 'linreg'.
#' 
#' The print() returns the names and the regression coefficients. 
#' 
#' 
#' @export




print.linreg <- function(l) { 
  print(paste("Formula: (", l$formula[2], " ~ ", l$formula[3],") ,   data: ", l$dataset, sep=""))
  print("                    ")
  print(l$coefficients)
}

#print(mylm) 
#print.linreg(mylm)