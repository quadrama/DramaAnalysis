context("loadDramaTEI()")

tei_file <- "example_tei.xml"
d <- loadDramaTEI(tei_file)

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
  expect_equal(d$text$Token.surface[1], "This")
  expect_equal(d$text$Token.surface[70], "mandatory")
})

test_that("loadDramaTEI produces correct segments table", {
  expect_true(inherits(d$segments, "QDHasSegments"))
  expect_true(inherits(d$segments, "data.table"))
  expect_equal(length(d$segments), 8)
  expect_equal(nrow(d$segments), 2)
})

test_that("loadDramaTEI produces correct characters table", {
  expect_true(inherits(d$characters, "data.table"))
  expect_equal(length(d$characters), 6)
  expect_equal(nrow(d$characters), 2)
  expect_equal(d$characters$figure_id[1], "foo")
  expect_equal(d$characters$figure_id[2], "bar")
})

test_that("loadDramaTEI produces correct meta table", {
  expect_true(inherits(d$meta, "data.table"))
  expect_equal(length(d$meta), 12)
  expect_equal(nrow(d$meta), 1)
  expect_equal(d$meta$documentTitle, "TEI Example File")
})

test_that("loadDramaTEI produces correct stageDirections table", {
  expect_true(inherits(d$stageDirections, "QDHasUtteranceBE"))
  expect_true(inherits(d$stageDirections, "data.table"))
  expect_equal(length(d$stageDirections), 10)
  expect_equal(nrow(d$stageDirections), 38)
  expect_equal(d$stageDirections$Token.surface[1], "This")
  expect_equal(d$stageDirections$Token.surface[37], "elements")
})