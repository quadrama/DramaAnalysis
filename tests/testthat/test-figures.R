context("characterStatistics()")

data(rksp.0)
data(rjmw.0)
toler <- 1e-4


# characterStatistics()

test_that("characterStatistics(rksp.0) 
          has correct dimensions and produces correct output", {
  fstat <- characterStatistics(rksp.0)
  expect_length(colnames(fstat), 10)
  expect_length(rownames(fstat), 13)
  expect_equal(sum(fstat$tokens), 25365)
  expect_equal(as.numeric(fstat[1,7]), 35.36943, tolerance=toler)
  expect_equal(as.numeric(fstat[3,4]), 764, tolerance=toler)
})

test_that("characterStatistics(rksp.0, normalize = TRUE) 
          has correct dimensions and produces correct output", {
  fstat <- characterStatistics(rksp.0, normalize = TRUE)
  expect_length(colnames(fstat), 10)
  expect_length(rownames(fstat), 13)
  expect_equal(as.numeric(fstat[1,7]), 35.36943, tolerance=toler)
  expect_equal(as.numeric(fstat[3,4]), 0.03012024, tolerance=toler)
  expect_equal(sum(fstat$tokens), 1, tolerance=toler)
  expect_equal(sum(fstat$utterances), 1, tolerance=toler)
  expect_equal(as.character(fstat$character[1]), "der_prinz")
})

fstat <- characterStatistics(rksp.0, segment="Act", filterPunctuation = TRUE, normalize=TRUE)
test_that("characterStatistics(rksp.0, segment='Act') 
          has correct dimensions and produces correct output", {
  expect_length(colnames(fstat), 11)
  expect_length(rownames(fstat), 28)
  expect_equal(sum(fstat$tokens), 1, tolerance=toler)
})

fstat <- characterStatistics(rksp.0, segment="Scene", normalize=TRUE)
test_that("characterStatistics(rksp.0, segment='Scene', normalize=TRUE) 
          has correct dimensions and produces correct output", {
  expect_length(colnames(fstat), 12)
  expect_length(rownames(fstat), 94)
  expect_equal(sum(fstat$tokens), 1, tolerance=toler)
})


# isolateCharacterSpeech()

context("isolateCharacterSpeech()")

test_that("isolateFigureSpeech(rksp.0, writeToFiles=FALSE)
          produces correct output", {
            l <- isolateCharacterSpeech(rksp.0, writeToFiles=FALSE)
            expect_length(l, 13)
            expect_equal(names(l[1]), "rksp.0_der.prinz")
            expect_length(l[[1]], 5553)
            expect_equal(l[[1]][1], "Klagen")
            expect_equal(l[[1]][2], ",")
            })

test_that("isolateCharacterSpeech(rksp.0, segment='Act', minTokenCount=1000, writeToFiles=FALSE)
          produces correct output", {
            l <- isolateCharacterSpeech(rksp.0, segment="Act", minTokenCount=1000, writeToFiles=FALSE)
            expect_length(l, 9)
            expect_equal(names(l[3]), "claudia.galotti_2")
            expect_length(l[[3]], 1264)
            expect_equal(l[[3]][1], "Wer")
            expect_equal(l[[3]][2], "sprengte")
            })

l <- isolateCharacterSpeech(combine(rksp.0, rjmw.0), 
                            segment='Scene',
                            minTokenCount=700, 
                            countPunctuation=FALSE, 
                            writeToFiles=FALSE)
test_that("isolateCharacterSpeech(rbind(rksp.0, rjmw.0), segment='Scene', minTokenCount=700, countPunctuation=FALSE, writeToFiles=FALSE)
          produces correct output", {
            expect_length(l, 13)
            expect_equal(names(l[1]), "rjmw.0_mellefont_1_7")
            expect_length(l[[1]], 996)
            expect_equal(l[[1]][1], "Sie")
            expect_equal(names(l[6]), "rjmw.0_sara_4_8")
            expect_length(l[[6]], 1468)
            expect_equal(l[[6]][1], "Mein")
          })


# rankFiguresByDramatisPersonae()

#fstat_ranked <- rankFiguresByDramatisPersonae(figureStatistics(rksp.0$mtext))
# test_that("rankFiguresByDramatisPersonae(figureStatistics(rksp.0$mtext)) 
#           has correct dimensions and produces correct output", {
#   expect_equal(ncol(fstat_ranked), 12)
#   expect_equal(colnames(fstat_ranked)[12], "Rank (dramatis personae)")
#   expect_equal(as.integer(fstat_ranked[1,12]), 1)
# })


# TODO: add tests for rankFiguresByAppearance() ?