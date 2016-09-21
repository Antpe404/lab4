#' Available methods in 'linreg'
#' 
#' The methods available for the linreg-package are described here.
#' 
#' 
#' @export




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

