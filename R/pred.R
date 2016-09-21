#' The fitted values from the linear regression 'linreg'
#' 
#' The pred()-statement returns a vector including all fitted values from the linear regression.
#' 
#'
#'
#' @export 

pred <- function(l,newdata =NA) UseMethod("pred")
