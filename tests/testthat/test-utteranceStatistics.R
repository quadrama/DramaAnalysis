context("utteranceStatistics()")

data(rksp.0)

suppressWarnings(ustat <- utteranceStatistics(rksp.0$mtext, numberOfFigures = 100, 
                             normalizeByDramaLength = FALSE))
test_that("utteranceStatistics(rksp.0$mtext, numberOfFigures = 100, normalizeByDramaLength = FALSE) 
          has correct dimensions, produces correct output and throws a warning because the play 
          consists of less than 100 figures.", {
  expect_warning(utteranceStatistics(rksp.0$mtext, numberOfFigures = 100, 
                                    normalizeByDramaLength = FALSE))
  expect_that(length(ustat), equals(5))
  expect_that(length(ustat$drama), equals(835))
  expect_that(median(ustat$utteranceLength), equals(17))
  expect_that(median(ustat[ustat$figure=="ODOARDO GALOTTI",5]), equals(14))
})


ustat <- utteranceStatistics(rksp.0$mtext, numberOfFigures = 10, 
                             normalizeByDramaLength = TRUE)
test_that("utteranceStatistics(rksp.0$mtext, numberOfFigures = 10, normalizeByDramaLength = TRUE) 
          has correct dimensions and produces correct output", {
  expect_that(length(ustat), equals(6))
  expect_that(length(ustat$drama), equals(812))
  expect_equal(median(ustat$utteranceLength), 0.0006996082, tolerance=0.00001)
  expect_equal(median(ustat[ustat$figure=="ODOARDO GALOTTI",5]), 0.0005596866, tolerance=0.00001)
})