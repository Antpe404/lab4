#' Summary of the linear regression
#' 
#' summary provides the most important measures and values from the linear regression.
#' 
#' 
#' @export


summary.linreg <- function(l) { 
  print(paste("Formula: (", l$formula[2], " ~ ", l$formula[3],") ,   data: ", l$dataset, sep=""))
  
  dejtarejm = data.frame(matrix(vector(), 2, 4,
                                dimnames=list(c("(Intercept)", paste(l$formula[3])), c("Estimation", "Std.Error", "T-value", "P-value"))),
                         stringsAsFactors=F)
  
  dejtarejm[,1]<-as.vector(l$coefficients)
  dejtarejm[,2]<-as.vector(sqrt(l$var.hat.bhat))
  dejtarejm[,3]<-as.vector(l$t.beta)
  dejtarejm[,4]<-as.vector(l$my.pvalues)
  
  print(dejtarejm)
  cat("The degrees of freedom is ", l$degrees_of_freedom,
      " and the estimate of the residual standard error is ", sqrt(l$res.var2),".", sep="")
  
}