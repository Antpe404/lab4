#' The coefficients from the Ridge regression  of class 'ridgereg'
#' 
#' The coef() returns all estimates for the coefficients in your Ridge regression.
#' 
#' 
#' @export

coef.ridgereg <- function(l) { 
  #print(paste("Formula: (", l$formula[2], " ~ ", l$formula[3],") ,   data: ", l$dataset, sep=""))
  Coefficients<-as.vector(l$coefficients)
  names(Coefficients) = c(paste(rownames(l$coefficients)))
  print(Coefficients)
}


#coef(mylm)
#l$coefficients
