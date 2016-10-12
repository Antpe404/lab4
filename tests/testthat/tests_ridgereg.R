test_that("Ridge Test 1 ",{
  
  mylm<-ridgereg(Petal.Width~Petal.Length,data=iris)
  test<-lm.ridge(Petal.Width~Petal.Length,data=iris)
  
  expect_equal(round(as.vector(mylm$coefficients), 10), round(as.vector(test$coefficients), 10))
  expect_equal(round(as.vector(mylm$y.hat), 10), round(as.vector(test$fitted.values), 10))
  expect_equal(class(mylm),"ridgereg")
})

test_that("Ridge Test 1", {
  
  expect_error(ridgereg(Petal.Width-Petal.Length,data=iris))
  expect_error(ridgereg(Petal.Width,Petal.Length,data=iris))
  expect_error(ridgereg(Petallangd~Petal.Length,data=iris))
  expect_warning(ridgereg(Petal.Width~Petal.Width,data=iris))
  expect_error(ridgereg(c(1:20)~c(21:40)), data=iris)
})