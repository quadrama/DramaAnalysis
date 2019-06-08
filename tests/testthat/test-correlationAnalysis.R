context("correlationAnalysis()")

toler <- 1e-4

g <- factor(c("m","m","m","m","f","m","m","m","f","m","m","f","m"))


test_that("correlationAnalysis(...) on dummy data produces correct results", {
  mat <- matrix(c(10,5,0,0,3,10,8,5,1), nrow=3, byrow = TRUE)
  colnames(mat) <- c("a", "b", "c")
  rownames(mat) <- c("m1", "f1", "m2")
  ca <- correlationAnalysis(mat, factor(c("m","f","m")), culling=0)
  expect_equal(nrow(ca), 3)
  expect_equal(ncol(ca), 3)
  expect_equal(ca$category, c("f", "m", "m"))
})

data(rksp.0)
rksp.0.ft <- frequencytable(rksp.0, byFigure = TRUE)


test_that("correlationAnalysis(...) produces correct results", {
  ca <- correlationAnalysis(rksp.0.ft, g, culling = 10)
  expect_equal(nrow(ca), 38)
  expect_equal(ncol(ca), 3)
  expect_equal(ca[1,1], -0.4403665)
  expect_equal(ca[1,2], "mein")
  expect_equal(ca[1,3], "f")
  expect_equal(ca[5,1], -0.393069)
  expect_equal(ca[5,2], "ihn")
  expect_equal(ca[5,3], "f")
})


test_that("correlationAnalysis(..., culling = 12) produces correct results", {
  ca <- correlationAnalysis(rksp.0.ft, g, culling = 12)
  expect_equal(nrow(ca), 7)
  expect_equal(ncol(ca), 3)
  expect_equal(ca[1,1], -0.3420352, tolerance = toler)
  expect_equal(ca[1,2], "haben")
  expect_equal(ca[1,3], "f")
  expect_equal(ca[5,1], -0.2935777, tolerance = toler)
  expect_equal(ca[5,2], "und")
  expect_equal(ca[5,3], "f")
})

rksp.0.ft <- frequencytable(rksp.0, byFigure = TRUE, acceptedPOS = postags$de$n)

test_that("correlationAnalysis(..., culling = 8) produces correct results", {
  ca <- correlationAnalysis(rksp.0.ft, g, culling = 8)
  expect_equal(nrow(ca), 3)
  expect_equal(ncol(ca), 3)
  expect_equal(ca[1,1], -0.3721482, tolerance = toler)
  expect_equal(ca[1,2], "tochter")
  expect_equal(ca[1,3], "f")
})
