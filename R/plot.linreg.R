#' Plots regarding the residuals
#' 
#' The plot() makes two plots about the relationship between the fitted values and the residuals.
#' 
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
  
  p1 <- ggplot(plotdata,aes(y=res,x=pred)) 
  p1 <- p1  +  geom_point() 
  p1 <- p1  +  geom_hline(yintercept = 0,col="grey",linetype = "dashed") 
  p1 <- p1  +  stat_smooth(method = "loess", col = "red", se = FALSE) 
  p1 <- p1  +  theme_bw()  
  p1 <- p1  +  theme(panel.grid.major = element_blank() , panel.grid.minor = element_blank(),
                panel.border = element_rect(colour = "black", fill=0, size=1) ) +
   
    xlab(paste("Fitted Values", paste(as.character(l$formula)[c(2,1,3)],collapse = " "), sep = " \n ")) +
    ylab("Residuals") +ggtitle("Residuals vs Fitted")
  
  
  
  p2 <- ggplot(plotdata,aes(y=stdres,x=pred))  
  p2 <- p2  +  geom_point() 
  p2 <- p2  +  stat_smooth(method ="loess",col="red", se = FALSE) 
  p2 <- p2  +  theme_bw() 
      
  p2 <- p2  +  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
               panel.border = element_rect(colour = "black", fill=0, size=1) )+
    
    xlab(paste("Fitted Values", paste(as.character(l$formula)[c(2,1,3)],collapse = " "), sep = " \n ")) +
    ylab(expression(sqrt("Standardized Residuals"))) +ggtitle("Scale vs Location")
  
  p3<-arrangeGrob(p1,p2,ncol=2)
  suppressWarnings(plot(p3))
}

#plot(mylm)

