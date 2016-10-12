#' Ridge regression
#'
#' Computes a ridge model using the Ridge -square method
#' The function also fitted values. 
#'
#' @param formula    an object of class "formula" or a character vector on the form y ~ x 
#'                   The dependent (y) and independent variable (x)
#'                  
#' @param data       a data set from which the used variables are taken
#' @return an list of class 'ridgereg' containing beta coefficients and fitted values.
#' 
#' @details 
#'  These measures are stored in a list of class 'ridgereg'. 
#' 
#'  
#' @references 
#'
#' 
#' @examples 
#' ## Make a simple linear regression of how the Petal.Length responds to the Petal.Width in the dataset iris.
#'    ridgereg(Petal.Length ~ Species, lambda = 5, iris)
#'
#' @export


ridgereg<-function(formula, lambda = 0 ,data){
  
  if(is.character(formula) == TRUE){
    formula <- as.formula(formula)
  }
  if(class(formula) != "formula"){
    stop("The formula is not in the correct format")
  }
  
  if( is.matrix(data) == TRUE ){
    data = as.data.frame(data)
  }
  if(nrow(data) < 5 )  {
    stop("To few rows of data")
  }
  
  if( ncol(data) < 1 )  {
    stop("To few columms of data, please supply more than one row of data")
  }
  
  
  
  des.mat <- model.matrix(formula , data) #Extracts the model matrix
  dep.var <- all.vars(formula)[1]         #Extracts the name of the y-variable 
  dep.var <- as.matrix(data[dep.var])     #Extracts the data of the y-variable 
  # and overwrites it with the data-colum 
  
  des.mat <- scale(des.mat)
  lambda<-diag(lambda, nrow=dim(dep.var)[1], nrow=dim(dep.var)[2]) #creats a matrix that represents lambda%*%I   (I identitetsmatrisen)
  
  
  bridge <- solve(t(des.mat)%*%des.mat + lambda) %*% t(des.mat) %*% (dep.var) 
  
  y.hat <- (des.mat %*% bridge)  
  
  
  l<-list(coefficients =bridge, y.hat = y.hat,formula = formula, dataset=deparse(substitute(data)))
  class(l) <- "ridgereg"
  return(l)
}


