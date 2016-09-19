print.linreg <- function(l) { 
print(paste("Formula: (", l$formula[2], " ~ ", l$formula[3],") ,   data: ", mylm$dataset, sep=""))
print("                    ")
print(l$coefficients)
}

print(mylm)
print.linreg(mylm)