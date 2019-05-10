context("save.R")

data(rksp.0)
data(rjmw.0)


# isolateFigureSpeech()

l <- isolateFigureSpeech(rksp.0, write_to_files=FALSE)
test_that("isolateFigureSpeech(rksp.0, write_to_files=FALSE)
          produces correct output", {
  expect_length(l, 13)
  expect_equal(names(l[1]), "rksp.0_der.prinz")
  expect_length(l[[1]]$Token.surface, 5553)
  expect_equal(l[[1]]$Token.surface[1], "Klagen")
  expect_equal(l[[1]]$Token.surface[2], ",")
})

l <- isolateFigureSpeech(rksp.0, segment="Act", min_token_count=1000, write_to_files=FALSE)
test_that("isolateFigureSpeech(rksp.0, segment='Act', min_token_count=1000, write_to_files=FALSE)
          produces correct output", {
  expect_length(l, 9)
  expect_equal(names(l[3]), "claudia.galotti_2")
  expect_length(l[[3]]$Token.surface, 1264)
  expect_equal(l[[3]]$Token.surface[1], "Wer")
  expect_equal(l[[3]]$Token.surface[2], "sprengte")
})

l <- isolateFigureSpeech(rbind(rksp.0, rjmw.0), segment='Scene', min_token_count=700, count_punctuation=FALSE, write_to_files=FALSE)
test_that("saveFigureSpeech(rbind(rksp.0, rjmw.0), segment='Scene', min_token_count=700, count_punctuation=FALSE, write_to_files=FALSE)
          produces correct output", {
  expect_length(l, 13)
  expect_equal(names(l[1]), "rksp.0_marinelli_1_6")
  expect_length(l[[1]]$Token.surface, 1062)
  expect_equal(l[[1]]$Token.surface[1], "Gnädiger")
  expect_equal(names(l[6]), "rjmw.0_mellefont_1_7")
  expect_length(l[[6]]$Token.surface, 996)
  expect_equal(l[[6]]$Token.surface[1], "Sie")
})


# newCollection()

l <- newCollection(rbind(rksp.0, rjmw.0), write_to_file=FALSE)
test_that("newCollection(rbind(rksp.0, rjmw.0), write_to_file=FALSE)
          produces correct output", {
  expect_length(l, 2)
  expect_equal(l[1], "rksp.0")
  expect_equal(l[2], "rjmw.0")
})

l <- newCollection(c("a", "b", "c"), write_to_file=FALSE)
test_that("newCollection(c('a', 'b', 'c'), write_to_file=FALSE)
          produces correct output", {
  expect_length(l, 3)
  expect_equal(l[1], "a")
  expect_equal(l[2], "b")
  expect_equal(l[3], "c")
})
