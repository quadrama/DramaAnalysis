context("loadDramaTEI")

tei_file <- system.file("extdata", "example_tei.xml", package="DramaAnalysis", mustWork=TRUE)
d <- loadDramaTEI(tei_file, dataDirectory="")

test_that("loadDramaTEI parses example_tei.xml correctly", {
  expect_true(inherits(d, "QDDrama"))
  expect_true(inherits(d, "list"))
  expect_equal(length(d), 6)
})

test_that("loadDramaTEI produces correct text table", {
  expect_true(inherits(d$text, "QDHasUtteranceBE"))
  expect_true(inherits(d$text, "data.table"))
  expect_equal(length(d$text), 10)
  expect_equal(nrow(d$text), 71)
})

# TODO: further tests