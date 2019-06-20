context("Utility")

test_that("ensureSuffix() produces correct output", {
  expect_equal(ensureSuffix("bla","a"),"bla")
  expect_equal(ensureSuffix("bl","a"),"bla")
  expect_equal(ensureSuffix("","a"),"a")
  expect_equal(ensureSuffix("bla",""),"bla")
})