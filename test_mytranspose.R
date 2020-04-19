context("transpose")

myvar1 <-  matrix(1:10, nrow=5, ncol=2)
myvar2 <-  matrix(NA, nrow=0, ncol=0)
myvar3 <-  matrix(c(1,2), nrow=1, ncol=2)
myvar4 <-  matrix(c(1,2), nrow=2, ncol=1)
myvar5 <- c(1,2,NA,3)
myvar6 <- c(NA)
myvar7 <- c()

d <- c(1,2,3,4)
e <- c("red", "white", "red", NA)
f <- c(TRUE,TRUE,TRUE,FALSE)
mydata8 <- data.frame(d,e,f)
a <- as.data.frame(t(mydata8))
colnames(a) <- c(1,2,3,4)





test_that("matix case 1 test",{
  expect_equal(t(myvar1), mytranspose(myvar1)) 
})



test_that("matix case 2 test",{
  expect_equal(t(myvar2), mytranspose(myvar2)) 
})



test_that("matix case 3 test",{
  expect_equal(t(myvar3), mytranspose(myvar3)) 
})



test_that("matix case 4 test",{
  expect_equal(t(myvar4), mytranspose(myvar4)) 
})



test_that("vector case 1 test",{
  expect_equal(t(matrix(myvar5, nrow = 1)), mytranspose(myvar5)) 
})



test_that("vector case 2 test",{
  expect_equivalent(t(matrix(myvar6, nrow = 1)), mytranspose(myvar6)) 
})



test_that("vector case 3 test",{
  expect_equal(NULL, mytranspose(myvar7)) 
})



test_that("dataframe test",{
  expect_equivalent(a, mytranspose(mydata8)) 
})



