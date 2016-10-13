test_that("Ridge Test 1 ",{
  
  names(longley)[1] <- "y"
  myridge<-ridgereg(formula=y ~ ., data=longley, lambda=2)
  test<-lm.ridge(y~.,data=longley, lambda=2)
  
  myridge2<-ridgereg(formula=y ~ ., data=longley, lambda=1)
  test2<-lm.ridge(y~.,data=longley, lambda=1)
  
  
  expect_equal(round(as.vector(myridge$coefficients[2:length(myridge$coefficients)])), round(as.vector(test$coef)))
  #expect_equal(round(as.vector(mylm$y.hat), 10), round(as.vector(test$fitted.values), 10))
  expect_equal(class(myridge),"ridgereg")
  
  expect_equal(round(as.vector(myridge2$coefficients[2:length(myridge$coefficients)])), round(as.vector(test2$coef)))
  #expect_equal(round(as.vector(mylm$y.hat), 10), round(as.vector(test$fitted.values), 10))
  expect_equal(class(myridge2),"ridgereg")
})

test_that("Ridge Test 1", {
  
  expect_error(ridgereg(formula=y ~ , data=longley, lambda=2))
  expect_error(ridgereg(Petal.Width~Petal.Length,data=iris[1:4,]))
  expect_error(ridgereg(Petallangd~Petal.Length,data=iris))
  expect_error(ridgereg(c(1:20)~c(21:40),data=longley, lambda=2))
})