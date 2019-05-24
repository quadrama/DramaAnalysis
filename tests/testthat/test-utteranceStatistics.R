context("utteranceStatistics()")

data(rksp.0)

ustat <- utteranceStatistics(rksp.0)
test_that("uterranceStatistics() has correct type", {
  expect_true(inherits(ustat, "QDUtteranceStatistics"))
  expect_true(inherits(ustat, "QDHasCharacter"))
  expect_true(inherits(ustat, "data.frame"))
})

ustat <- utteranceStatistics(rksp.0, normalizeByDramaLength = FALSE)
test_that("utteranceStatistics(rksp.0, normalizeByDramaLength = FALSE) 
          has correct dimensions and produces correct output.", {
  expect_equal(length(ustat), 5)
  expect_equal(length(ustat$drama), 835)
  expect_equal(median(ustat$utteranceLength), 16)
  expect_equal(median(ustat[ustat$character=="odoardo",]$utteranceLength), 14)
})

ustat <- utteranceStatistics(rksp.0, normalizeByDramaLength = TRUE)
test_that("utteranceStatistics(rksp.0, normalizeByDramaLength = TRUE) 
          has correct dimensions and produces correct output", {
  expect_equal(length(ustat), 5)
  expect_equal(length(ustat$drama), 835)
  expect_equal(median(ustat$utteranceLength), 0.0006525285, tolerance=0.00001)
  expect_equal(median(ustat[ustat$character=="odoardo",]$utteranceLength), 0.0005709625, tolerance=0.00001)
})