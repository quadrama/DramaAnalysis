context("filter")

data("rksp.0")


# utteranceStatistics()

ustat <- utteranceStatistics(rksp.0)

filtered <- filter(ustat, rksp.0, by="rank", threshold=1)
test_that("filter(by=rank,threshold=1) produces correct results", {
  expect_equal(nrow(filtered), 221)
  expect_equal(ncol(ustat), ncol(filtered))
})

filtered <- filter(ustat, rksp.0, by="rank", threshold=2)
test_that("filter(by=rank,threshold=2) produces correct results", {
  expect_equal(nrow(filtered), 378)
  expect_equal(ncol(ustat), ncol(filtered))
})

# dictionaryStatistics()

dstat <- dictionaryStatistics(rksp.0, segment="Drama")

filtered <- filter(dstat, rksp.0, by="rank", threshold=1)
test_that("filter(by=rank, threshold=1) produces correct results", {
  expect_equal(nrow(filtered), 1)
  expect_equal(ncol(dstat), ncol(filtered))
})

filtered <- filter(dstat, rksp.0, by="rank", threshold=2)
test_that("filter(by=rank, threshold=2) produces correct results", {
  expect_equal(nrow(filtered), 2)
  expect_equal(ncol(dstat), ncol(filtered))
})