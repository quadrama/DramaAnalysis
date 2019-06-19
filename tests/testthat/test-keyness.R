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
rksp.0.ft <- frequencytable(rksp.0, byCharacter = TRUE)
g <- factor(c("m","m","m","m","f","m","m","m","f","m","m","f","m"))


test_that("keyness(...) produces correct results", {
  ca <- keyness(rksp.0.ft, g)
  expect_length(ca, 147)
  
  expect_equal(ca[[1]],  27.685821,  tolerance=toler)
  expect_equal(ca[[5]],  16.336084, tolerance=toler)
  expect_equal(ca[[10]], 12.363960, tolerance=toler)
  expect_equal(ca[[40]],  7.388949, tolerance=toler)
  
  expect_equal(names(ca)[[1]],  "vater")
  expect_equal(names(ca)[[5]],  "dir")
  expect_equal(names(ca)[[10]], "nichts")
  expect_equal(names(ca)[[40]], "religion")
})

test_that("keyness(..., method=logratio) produces correct results", {
  ca <- keyness(rksp.0.ft, g, method="logratio", minimalFrequency = 10)
  expect_length(ca, 92)
  
  expect_equal(ca[[1]],   0.85075056, tolerance=toler)
  expect_equal(ca[[5]],   0.35277988,   tolerance=toler)
  expect_equal(ca[[10]],  0.30808676,  tolerance=toler)
  expect_equal(ca[[20]],  0.08749703, tolerance=toler)
  expect_equal(ca[[75]], -0.83277578, tolerance=toler)
  
  expect_equal(names(ca)[[1]],  "um")
  expect_equal(names(ca)[[5]],  "wollen")
  expect_equal(names(ca)[[10]], "doch")
  expect_equal(names(ca)[[20]], "prinz")
  expect_equal(names(ca)[[75]], "mich")
})


