context("dramaTail()")

data(rksp.0)

segmentedText <- segment(rksp.0$text, rksp.0$segments)

t <- dramaTail(segmentedText, n=1)
test_that("dramaTail(rksp.0$mtext, n=1) subsets one act", {
  expect_length(unique(t$begin.Act), 1)
})

t <- dramaTail(segmentedText, n=3)
test_that("dramaTail(rksp.0$mtext, n=3) subsets three acts", {
  expect_length(unique(t$begin.Act), 1)
})

suppressWarnings(t <- dramaTail(segmentedText, n=6))
test_that("dramaTail(rksp.0$mtext, n=6) throws a warning and returns an empty table because the play only has 5 acts", {
  expect_warning(dramaTail(segmentedText, n=6), "Play has only 5 acts.")
  expect_length(unique(t$begin.Act), 0)
  expect_that(nrow(t), equals(0))
})

t <- dramaTail(segmentedText, n=1, op=">=")
test_that("dramaTail(rksp.0$mtext, n=1, op='>=') subsets one act", {
  expect_length(unique(t$begin.Act), 1)
})

t <- dramaTail(segmentedText, n=3, op=">=")
test_that("dramaTail(rksp.0$mtext, n=3, op='>=') subsets three acts", {
  expect_length(unique(t$begin.Act), 3)
})

suppressWarnings(t <- dramaTail(segmentedText, n=6, op=">="))
test_that("dramaTail(rksp.0$mtext, n=6, op='>=') throws a warning and returns an empty table because the play only has 5 acts", {
  expect_warning(dramaTail(segmentedText, n=6, op='>='), "Play has only 5 acts.")
  expect_length(unique(t$begin.Act), 0)
})

t <- dramaTail(segmentedText, by="Scene", n=1)
test_that("dramaTail(rksp.0$mtext, by='Scene', n=1) subsets one scene", {
  expect_length(unique(t$begin.Scene), 1)
})

t <- dramaTail(segmentedText, by="Scene", n=12)
test_that("dramaTail(rksp.0$mtext, by='Scene', n=12) subsets one scene", {
  expect_length(unique(t$begin.Scene), 1)
})

t <- dramaTail(segmentedText, by="Scene", n=30)
test_that("dramaTail(rksp.0$mtext, by='Scene', n=30) subsets one scene", {
  expect_length(unique(t$begin.Scene), 1)
})