context("tfidf()")

mat <- matrix(c(0.10,0.20,0,
                0,0.20,0,
                0.1,0.2,0.1,
                0.8,0.4,0.9),nrow=3,ncol=4)
mat2 <- tfidf(mat)
test_that("tfidf-function produces correct output", {
  expect_equal(mat2[2,1],0.13862944,tolerance=0.001)
  expect_equal(mat2[2,2],0.2772589,tolerance=0.001)
  expect_equal(mat2[2,3],0.05753641,tolerance=0.001)
  expect_equal(mat2[2,4],0.1150728,tolerance=0.001)
})