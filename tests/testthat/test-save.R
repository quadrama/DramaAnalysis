context("save.R")

data(rksp.0)
data(rjmw.0)


# isolateFigureSpeech()

test_that("isolateFigureSpeech(rksp.0$mtext, write_to_files=FALSE)
          produces correct output", {
  l <- isolateFigureSpeech(rksp.0$mtext, write_to_files=FALSE)
  # TODO
})

test_that("isolateFigureSpeech(rksp.0$mtext, segment='Act', min_token_count=1000, write_to_files=FALSE)
          produces correct output", {
  l <- isolateFigureSpeech(rksp.0$mtext, segment="Act", min_token_count=1000, write_to_files=FALSE)
  # TODO
})

test_that("saveFigureSpeech(rbind(rksp.0$mtext, rjmw.0$mtext), segment='Scene', min_token_count=1000, count_punctuation=FALSE, write_to_files=FALSE)
          produces correct output", {
  l <- isolateFigureSpeech(rbind(rksp.0$mtext, rjmw.0$mtext), segment='Scene', min_token_count=1000, count_punctuation=FALSE, write_to_files=FALSE)
  # TODO
})


# newCollection()

test_that("newCollection(rbind(rksp.0$mtext, rjmw.0$mtext), write_to_file=FALSE)
          produces correct output", {
  l <- newCollection(rbind(rksp.0$mtext, rjmw.0$mtext), write_to_file=FALSE)
  expect_length(l, 2)
  expect_equal(l[1], "rksp.0")
  expect_equal(l[2], "rjmw.0")
})

test_that("newCollection(c('a', 'b', 'c'), write_to_file=FALSE)
          produces correct output", {
  l <- newCollection(c("a", "b", "c"), write_to_file=FALSE)
  expect_length(l, 3)
  expect_equal(l[1], "a")
  expect_equal(l[2], "b")
  expect_equal(l[3], "c")
})
