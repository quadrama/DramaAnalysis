context("keyness()")

toler <- 1e-4



test_that("keyness(...) on dummy data produces correct results", {
  mat <- matrix(c(10,5,0,0,3,10,8,5,1), nrow=3, byrow = TRUE)
  colnames(mat) <- c("a", "b", "c")
  rownames(mat) <- c("m1", "f1", "m2")
  ca <- keyness(mat, factor(c("m","f","m")))
  expect_length(ca, 2)
  expect_equal(names(ca), c("c", "a"))
  expect_equal(ca[[1]], 17.49316, tolerance = toler)
  expect_equal(ca[[2]], 13.33346, tolerance = toler)
})

test_that("keyness(..., method=logratio) on dummy data produces correct results", {
  mat <- matrix(c(10,5,1,1,3,10,8,5,1), nrow=3, byrow = TRUE)
  colnames(mat) <- c("a", "b", "c")
  rownames(mat) <- c("m1", "f1", "m2")

  ca <- keyness(mat, factor(c("m","f","m")), method="logratio", minimalFrequency = 0)
  expect_length(ca, 3)
  expect_equal(names(ca), c("a", "b", "c"))
  expect_equal(ca[[1]], 3.0703893)
  expect_equal(ca[[3]], -3.4214638)
})

data(rksp.0)
rksp.0.ft <- frequencytable(rksp.0, byFigure = TRUE)
g <- factor(c("m","m","m","m","f","m","m","m","f","m","m","f","m"))


test_that("keyness(...) produces correct results", {
  ca <- keyness(rksp.0.ft, g)
  expect_length(ca, 143)
  
  expect_equal(ca[[1]],  26.206012,  tolerance=toler)
  expect_equal(ca[[5]],  16.386089, tolerance=toler)
  expect_equal(ca[[10]], 12.432355, tolerance=toler)
  expect_equal(ca[[40]],  7.399028, tolerance=toler)
  
  expect_equal(names(ca)[[1]],  "vater")
  expect_equal(names(ca)[[5]],  "dir")
  expect_equal(names(ca)[[10]], "nichts")
  expect_equal(names(ca)[[40]], "verachten")
})

test_that("keyness(..., method=logratio) produces correct results", {
  ca <- keyness(rksp.0.ft, g, method="logratio", minimalFrequency = 10)
  expect_length(ca, 89)
  
  expect_equal(ca[[1]],   0.60327134, tolerance=toler)
  expect_equal(ca[[5]],   0.36939942,   tolerance=toler)
  expect_equal(ca[[10]],  0.26540170,  tolerance=toler)
  expect_equal(ca[[20]],  0.08569568, tolerance=toler)
  expect_equal(ca[[75]], -0.94516529, tolerance=toler)
  
  expect_equal(names(ca)[[1]],  "mussen")
  expect_equal(names(ca)[[5]],  "wie")
  expect_equal(names(ca)[[10]], "ja")
  expect_equal(names(ca)[[20]], "wenn")
  expect_equal(names(ca)[[75]], "horen")
})


