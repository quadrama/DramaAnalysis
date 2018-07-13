context("Data & base_dictionary")

data(rksp.0)
data(rjmw.0)

test_that("rksp.0$mtext and rjmw.0$mtext have correct dimensions", {
  expect_length(rksp.0$mtext, 18)
  expect_length(rjmw.0$mtext, 18)
  expect_length(rksp.0$mtext$begin, 25365)
  expect_length(rjmw.0$mtext$begin, 31201)
})

test_that("base_dictionary has correct dimensions", {
  expect_length(base_dictionary, 5)
  expect_length(base_dictionary$Familie, 73)
  expect_length(base_dictionary$Liebe, 94)
  expect_length(base_dictionary$Krieg, 112)
  expect_length(base_dictionary$Ratio, 108)
  expect_length(base_dictionary$Religion, 57)
})
