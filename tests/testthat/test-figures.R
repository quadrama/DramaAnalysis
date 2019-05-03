context("Figures")

data(rksp.0)
toler <- 1e-4


# characterStatistics()

fstat <- characterStatistics(rksp.0)
test_that("characterStatistics(rksp.0) 
          has correct dimensions and produces correct output", {
  expect_length(colnames(fstat), 11)
  expect_length(rownames(fstat), 13)
  expect_equal(sum(fstat$tokens), 24520)
  expect_equal(as.numeric(fstat[1,8]), 35.36943, tolerance=toler)
  expect_equal(as.numeric(fstat[3,5]), 764, tolerance=toler)
})

fstat <- characterStatistics(rksp.0, normalize = TRUE)
test_that("characterStatistics(rksp.0, normalize = TRUE) 
          has correct dimensions and produces correct output", {
  expect_length(colnames(fstat), 11)
  expect_length(rownames(fstat), 13)
  expect_equal(as.numeric(fstat[1,8]), 35.36943, tolerance=toler)
  expect_equal(as.numeric(fstat[3,5]), 0.030120244, tolerance=toler)
  expect_equal(sum(fstat$tokens), 1, tolerance=toler)
  expect_equal(sum(fstat$utterances), 1, tolerance=toler)
  expect_equal(as.character(fstat[1,4]$character), "DER PRINZ")
})

fstat <- characterStatistics(rksp.0, segment="Act", filter_punctuation = TRUE, normalize=TRUE)
test_that("characterStatistics(rksp.0, segment='Act') 
          has correct dimensions and produces correct output", {
  expect_length(colnames(fstat), 12)
  expect_length(rownames(fstat), 28)
  expect_equal(sum(fstat$tokens), 1, tolerance=toler)
})

fstat <- characterStatistics(rksp.0, segment="Scene", normalize=TRUE)
test_that("characterStatistics(rksp.0, segment='Scene', normalize=TRUE) 
          has correct dimensions and produces correct output", {
  expect_length(colnames(fstat), 13)
  expect_length(rownames(fstat), 94)
  expect_equal(sum(fstat$tokens), 1, tolerance=toler)
})



# rankFiguresByDramatisPersonae()

fstat_ranked <- rankFiguresByDramatisPersonae(figureStatistics(rksp.0$mtext))
test_that("rankFiguresByDramatisPersonae(figureStatistics(rksp.0$mtext)) 
          has correct dimensions and produces correct output", {
  expect_equal(ncol(fstat_ranked), 12)
  expect_equal(colnames(fstat_ranked)[12], "Rank (dramatis personae)")
  expect_equal(as.integer(fstat_ranked[1,12]), 1)
})


# TODO: add tests for rankFiguresByAppearance() ?