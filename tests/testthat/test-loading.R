context("loadDrama()")


test_that("loading of multiple plays at once works", {
  skip_on_travis()
  skip_on_cran()
  
  text <- loadDrama(c("test:rksp.0", "test:rjmw.0"))
  
  expect_equal(max(text$segments$Number.Act), 5)
  expect_equal(max(text$segments$Number.Scene), 11)
})
  
