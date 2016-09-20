print.linreg <- function(l) { 
print(paste("Formula: (", l$formula[2], " ~ ", l$formula[3],") ,   data: ", mylm$dataset, sep=""))
print("                    ")
print(l$coefficients)
}

print(mylm) 
print.linreg(mylm)

resid.linreg <- function(l) { 
  print(paste("Vector containing the residuals"))
  
  print(as.vector(l$res.err))
}

resid(mylm)
resid.linreg(mylm)



predict.linreg <- function(l) { 
  print(paste("Vector containing the predicted values"))
  
  print(l$y.hat)
}

predict(mylm)
resid.linreg(mylm)

coef.linreg <- function(l) { 
  #print(paste("Formula: (", l$formula[2], " ~ ", l$formula[3],") ,   data: ", l$dataset, sep=""))
  Coefficients<-as.vector(l$coefficients)
  names(Coefficients) = c("(Intercept)", paste(l$formula[3]))
  print(Coefficients)
}
coef(mylm)



summary.linreg <- function(l) { 
  print(paste("Formula: (", l$formula[2], " ~ ", l$formula[3],") ,   data: ", l$dataset, sep=""))
  
  dejtarejm = data.frame(matrix(vector(), 2, 4,
                                dimnames=list(c("(Intercept)", paste(l$formula[3])), c("Estimation", "Std.Error", "T-value", "P-value"))),
                         stringsAsFactors=F)
  
  dejtarejm[,1]<-as.vector(l$coefficients)
  dejtarejm[,2]<-as.vector(sqrt(l$var_of_reg_coef))
  dejtarejm[,3]<-as.vector(l$t_values)
  dejtarejm[,4]<-as.vector(l$p_values)
  
  print(dejtarejm)
  cat("The degrees of freedom is ", l$degrees_of_freedom,
      " and the estimate of the residual standard error is ", sqrt(l$residual_variance),".", sep="")
  
}

summary(ap)


#### Plott funktionen 

library(ggplot2)

mylm$res.err
mylm$y.hat
plotdata <- data.frame(res=mylm$res.err,pred=mylm$y.hat)
colnames(plotdata) <- c("res","pred")
plotdata$stdres <- scale(plotdata$res,center=FALSE)

ggplot(plotdata,aes(y=res,x=pred)) + 
  geom_point() +
  geom_smooth(method="lm",se=FALSE,col="grey",linetype = "dashed") +
  geom_smooth(se=FALSE,col="red") 
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1))+
  xlab(paste("Fitted Values", paste(as.character(mylm$formula)[c(2,1,3)],collapse = " "), sep = " \n ")) +
  ylab("Residuals") +ggtitle("Residuals vs Fitted")



ggplot(plotdata,aes(y=stdres,x=pred)) + 
  geom_point() +
  geom_smooth(method="lm",se=FALSE,col="grey",linetype = "dashed")+
  geom_smooth(se=FALSE,col="red") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1))+
  xlab(paste("Fitted Values", paste(as.character(mylm$formula)[c(2,1,3)],collapse = " "), sep = " \n ")) +
  ylab(expression(sqrt("Standardized Residuals"))) +ggtitle("Scale vs Location")
