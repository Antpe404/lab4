test_that("linregtest1 ",{
  
  mylm<-linreg(Petal.Width~Petal.Length,data=iris)
  test<-lm(Petal.Width~Petal.Length,data=iris)
  
  expect_equal(round(as.vector(mylm$coefficients), 10), round(as.vector(test$coefficients), 10))
  expect_equal(round(as.vector(mylm$res.err), 10),round(as.vector(test$residuals), 10))
  expect_equal(round(as.vector(mylm$y.hat), 10), round(as.vector(test$fitted.values), 10))
  expect_equal(round(as.vector(mylm$t.beta), 10), as.vector(round(unlist(summary(test)[4])[5:6], 10)))
  
})

test_that("linregtest2 ", {
  
  expect_error(linreg(Petal.Width-Petal.Length,data=iris))
  expect_error(linreg(Petal.Width,Petal.Length,data=iris))
  expect_error(linreg(Petallangd~Petal.Length,data=iris))
  expect_warning(linreg(Petal.Width~Petal.Width,data=iris))
  expect_error(linreg(c(1:20)~c(21:40)), data=iris)
})