context("limitFigures()")

data(rksp.0)

limited <- limitFigures(rksp.0$mtext, by="rank", threshold=1)
test_that("limitFigures(rksp.0$mtext, by='rank', threshold=1) 
          limits to only one character (MARINELLI)", {
  expect_length(unique(limited$Speaker.figure_id), 1)
  expect_equal(as.character(unique(limited$Speaker.figure_surface)), "MARINELLI")
})

limited <- limitFigures(rksp.0$mtext, by="tokens", threshold=1)
test_that("limitFigures(rksp.0$mtext, by='tokens', threshold=1) 
          is equal to rksp.0$mtext (contains all 13 characters)", {
  expect_length(unique(limited$Speaker.figure_id), 13)
})

limited <- limitFigures(rksp.0$mtext, by="rank", other=TRUE)
test_that("limitFigures(rksp.0$mtext, by='rank', other=TRUE) 
          limits to 11 (top 10 by rank + 'OTHER')", {
  expect_length(unique(limited$Speaker.figure_id), 11)
  expect_equal("OTHER" %in% unique(limited$Speaker.figure_id), TRUE)
})