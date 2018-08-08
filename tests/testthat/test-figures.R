context("Figures")

data(rksp.0)
toler <- 1e-4

fstat <- figureStatistics(rksp.0$mtext, names = FALSE)
test_that("figureStatistics(rksp.0$mtext, names = FALSE) 
          has correct dimensions and produces correct output", {
  expect_length(colnames(fstat), 11)
  expect_length(rownames(fstat), 13)
  expect_equal(sum(fstat$tokens), 25365)
  expect_equal(as.numeric(fstat[1,8]), 35.36943, tolerance=toler)
  expect_equal(as.numeric(fstat[3,5]), 764, tolerance=toler)
})

fstat <- figureStatistics(rksp.0$mtext, names = TRUE, normalize = TRUE)
test_that("figureStatistics(rksp.0$mtext, names = TRUE, normalize = TRUE) 
          has correct dimensions and produces correct output", {
  expect_length(colnames(fstat), 11)
  expect_length(rownames(fstat), 13)
  expect_equal(as.numeric(fstat[1,8]), 35.36943, tolerance=toler)
  expect_equal(as.numeric(fstat[3,5]), 0.030120244, tolerance=toler)
  expect_equal(sum(fstat$tokens), 1, tolerance=toler)
  expect_equal(sum(fstat$utterances), 1, tolerance=toler)
  expect_equal(as.character(fstat[1,4]$figure), "DER PRINZ")
})

# TODO: fix #114
fstat <- figureStatistics(rksp.0$mtext, segment="Act", filter_punctuation = TRUE, normalize=TRUE)
test_that("figureStatistics(rksp.0$mtext, segment='Act') 
          has correct dimensions and produces correct output", {
  expect_length(colnames(fstat), 12)
  expect_length(rownames(fstat), 28)
  expect_equal(sum(fstat$tokens), 1, tolerance=toler)
})

fstat <- figureStatistics(rksp.0$mtext, segment="Scene", normalize=TRUE)
test_that("figureStatistics(rksp.0$mtext, segment='Scene', normalize=TRUE) 
          has correct dimensions and produces correct output", {
  expect_length(colnames(fstat), 13)
  expect_length(rownames(fstat), 94)
  expect_equal(sum(fstat$tokens), 1, tolerance=toler)
})

# TODO: figurematrix()
fm <- figurematrix(figureStatistics(rksp.0$mtext))
test_that("figurematrix(figureStatistics(rksp.0$mtext)) 
          has correct dimensions and produces correct output", {
  expect_length(fm, 3)
  expect_length(fm$labels, 13)
  expect_length(fm$values, 14)
  expect_length(fm$cs, 14) # 
  expect_equal(fm$values[1], 5660)
  expect_equal(fm$values[1] > fm$values[2], TRUE)
})

fm <- figurematrix(figureStatistics(rksp.0$mtext), column="utterances", order=1)
test_that("figurematrix(figureStatistics(rksp.0$mtext), column='utterances', order=1) 
          has correct dimensions and produces correct output", {
  expect_length(fm, 3)
  expect_length(fm$labels, 13)
  expect_length(fm$values, 14)
  expect_length(fm$cs, 14) #
  expect_equal(fm$values[1], 6)
  expect_equal(fm$values[2] < fm$values[3], TRUE)
  expect_equal(fm$values[1], fm$values[2])
})

# TODO: add tests for rankFiguresByDramatisPersonae() ?

# TODO: add tests for rankFiguresByAppearance() ?
