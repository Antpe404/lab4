#' The least-square method 
#'
#' Computes a linear model using the least-square estimation. 
#' The function also computes serveral diagnostic measures such as p-value, variance and fitted values. 
#'
#' @param formula    an object of class "formula" or a character vector on the form y ~ x 
#'                   The dependent (y) and independent variable (x)
#'                  
#' @param data       a data set from which the used variables are taken (data rejm is prefered)
#' @return an list of class 'linreg' containing the following data:
#' 
#' @details 
#' The 'linreg' function uses ordinary linear algebra to compute the most 
#' important measures for a linear regression. 
#' These measures are stored in a list of class 'linreg'. 
#' 
#'  
#' @references 
#'
#' http://en.wikipedia.org/wiki/Least_squares
#' http://en.wikipedia.org/wiki/Linear_regression
#' 
#' @examples 
#' ## Make a simple linear regression of how the Petal.Length responds to the Petal.Width in the dataset iris.
#'    linreg(Petal.Length ~ Species, iris)
#'
#' @export
 



linreg<-function(formula,data){
  
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

  beta.hat <- solve( t(des.mat) %*% des.mat )  %*% t(des.mat) %*% dep.var #Calculating the beta coeffs. (X' %*% X)^-1 %*% X' %*% y
  
  y.hat <- des.mat %*% beta.hat # Calculating the y-hat  , y_hat = X %*% beta_hat 
  
  res.err <- dep.var - y.hat  #Calculating the residuals e= y- y_hat 
  
  degree.free <- nrow(des.mat) - ncol(des.mat)  #Degrees of freedom, n - p 
  #OKLART OM p = b0 till b4 = 5  eller om det är p = b1 -b4 = 4 
  
  res.var2 <-( t(res.err) %*% res.err ) / degree.free #Calculating the residual variance (e' %*% e) / df  
  
  var.hat.bhat <-diag( as.vector(res.var2) *  solve( t(des.mat) %*% des.mat )  )#Calculating 
  
  t.beta <- beta.hat / sqrt( var.hat.bhat )
  my.pvalues<- (1 - pt( abs( t.beta ) ,df = degree.free) ) * 2 
  
  l<-list( coefficients = t(beta.hat) , degree.free = degree.free, res.var2 = res.var2, var.hat.bhat = var.hat.bhat,
           t.beta = t.beta, my.pvalues = my.pvalues, formula = formula, dataset=deparse(substitute(data)),
          data=cbind(des.mat,dep.var),y.hat=y.hat,res.err=res.err)
  class(l) <- "linreg"
  rownames(l$coefficients) <- ""
  return(l)
  }

#mylm<-test( Petal.Length ~ Species, iris)
#mylm<-test(Sepal.Length ~ Sepal.Width,data = iris)
#linreg( Petal.Length ~ Petal.Length, iris)
#skit<-test(Sepal.Length~ Petal.Width, iris)


#data.class() #Behover nog den har far att testa om det är S3 eller RC.
#if(!is.expression(formula)) stop("The given formula is not a correct expression") #oklar om den är nödvändig



