print.linreg <- function(l) { 
print(paste("Formula: (", l$formula[2], " ~ ", l$formula[3],") ,   data: ", l$dataset, sep=""))
print("                    ")
print(l$coefficients)
}

#print(mylm) 
#print.linreg(mylm)

resid.linreg <- function(l) { 
  print(paste("Vector containing the residuals"))
  
  print(as.vector(l$res.err))
}

#resid(mylm)
#resid.linreg(mylm)



predict.linreg <- function(l) { 
  print(paste("Vector containing the predicted values"))
  
  print(l$y.hat)
}

#predict(mylm)
#resid.linreg(mylm)

coef.linreg <- function(l) { 
  #print(paste("Formula: (", l$formula[2], " ~ ", l$formula[3],") ,   data: ", l$dataset, sep=""))
  Coefficients<-as.vector(l$coefficients)
  names(Coefficients) = c("(Intercept)", paste(l$formula[3]))
  print(Coefficients)
}

#coef(mylm)



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

#summary(mylm)

### pred

pred <- function(l,newdata =NA) UseMethod("pred")
pred.linreg<-function(l,newdata = NA) {
  
  if(is.na(newdata)){
    return(print(l$y.hat))
    
  } else if(is.matrix(newdata) | is.data.frame(newdata)) {
    if(!all(newdata[,1] == 1)){ 
      newdata<-as.matrix(cbind(as.matrix(rep(1,nrow(newdata)),ncol=1),newdata))
    }
    
    yhat <- newdata %*% t(l$coefficients) #Skriv funktionen här
    return(print(yhat))
  }
}


#pred(mylm)

#pred(skit)




#### Plott funktionen 
plot.linreg<-function(l){
require(ggplot2)
require(gridExtra)

l$res.err
l$y.hat
plotdata <- data.frame(res=l$res.err,pred=l$y.hat)
colnames(plotdata) <- c("res","pred")
plotdata$stdres <- sqrt(abs(scale(plotdata$res,center=FALSE)))

p1<-ggplot(plotdata,aes(y=res,x=pred)) + 
  geom_point() +
  geom_smooth(method="lm",se=FALSE,col="grey",linetype = "dashed") +
  #geom_smooth(se=FALSE,col="red") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  xlab(paste("Fitted Values", paste(as.character(l$formula)[c(2,1,3)],collapse = " "), sep = " \n ")) +
  ylab("Residuals") +ggtitle("Residuals vs Fitted")



p2<-ggplot(plotdata,aes(y=stdres,x=pred)) + 
  geom_point() +
  geom_smooth(method="lm",se=FALSE,col="grey",linetype = "dashed")+
  geom_smooth(se=FALSE,col="red") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1) )+
  xlab(paste("Fitted Values", paste(as.character(l$formula)[c(2,1,3)],collapse = " "), sep = " \n ")) +
  ylab(expression(sqrt("Standardized Residuals"))) +ggtitle("Scale vs Location")

p3<-arrangeGrob(p1,p2,ncol=2)
plot(p3)
}

#plot(mylm)
