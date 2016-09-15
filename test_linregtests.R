test_that("linregtest1 ",{expect_equal(linreg(data=iris, formula=Sepal.Length~Sepal.Width),linreg$coefficients=lm(iris$Sepal.Length ~ iris$Sepal.Width)$coefficients, linreg$residuals=lm(iris$Sepal.Length ~ iris$Sepal.Width)$residuals), linreg$pred=lm(iris$Sepal.Length ~ iris$Sepal.Width)$fitted.values})
test_that("linregtest2 ",{expect_error(linreg(data="textfil", formula=Sepal.Length~Sepal.Width)}))
test_that("linregtest3 ",{expect_error(linreg(data=iris, formula=Sepal.Length~Species)}))
test_that("linregtest4 ",{expect_error(linreg(data=iris, formula=Sepal.Length[1:100]~Sepal.Width[1:110])}))


#test_that("concarnetestet2 ",{expect_equal(euclidian(123612,13892347912),4)})
#test_that("concarnetestet3 ",{expect_error(euclidian("TUUUSEN",13892347912))})
#test_that("concarnetestet4 ",{expect_error(euclidian(100,"Swedish Husman"))})
#test_that("concarnetestet5 ",{expect_error(euclidian( c(22,41),c(12,31))) })
#test_that("concarnetestet6 ",{expect_error(euclidian(TRUE,1000))})
#test_that("concarnetestet7 ",{expect_error(euclidian(data.frame(matrix(c(1:100),ncol=5)),1000))})
#test_that("concarnetestet8 ",{expect_error(euclidian(1.5,1000))})
#test_that("concarnetestet9 ",{expect_error(euclidian(12,0))})