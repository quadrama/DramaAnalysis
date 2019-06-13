context("save.R")

data(rksp.0)
data(rjmw.0)


context("isolateCharacterSpeech()")

l <- isolateCharacterSpeech(rksp.0, writeToFiles=FALSE)
test_that("isolateFigureSpeech(rksp.0, writeToFiles=FALSE)
          produces correct output", {
  expect_length(l, 13)
  expect_equal(names(l[1]), "rksp.0_der.prinz")
  expect_length(l[[1]]$Token.surface, 5303)
  expect_equal(l[[1]]$Token.surface[1], "Klagen")
  expect_equal(l[[1]]$Token.surface[2], ",")
})

l <- isolateCharacterSpeech(rksp.0, segment="Act", minTokenCount=1000, writeToFiles=FALSE)
test_that("isolateCharacterSpeech(rksp.0, segment='Act', minTokenCount=1000, writeToFiles=FALSE)
          produces correct output", {
  expect_length(l, 9)
  expect_equal(names(l[3]), "claudia.galotti_2")
  expect_length(l[[3]]$Token.surface, 1252)
  expect_equal(l[[3]]$Token.surface[1], "Wer")
  expect_equal(l[[3]]$Token.surface[2], "sprengte")
})

l <- isolateCharacterSpeech(combine(rksp.0, rjmw.0), 
                         segment='Scene',
                         minTokenCount=700, 
                         countPunctuation=FALSE, 
                         writeToFiles=FALSE)
test_that("isolateCharacterSpeech(rbind(rksp.0, rjmw.0), segment='Scene', minTokenCount=700, countPunctuation=FALSE, writeToFiles=FALSE)
          produces correct output", {
  expect_length(l, 9)
  expect_equal(names(l[1]), "rjmw.0_mellefont_1_7")
  expect_length(l[[1]]$Token.surface, 996)
  expect_equal(l[[1]]$Token.surface[1], "Sie")
  expect_equal(names(l[6]), "rjmw.0_sara_4_8")
  expect_length(l[[6]]$Token.surface, 1460)
  expect_equal(l[[6]]$Token.surface[1], "Mein")
})


# newCollection()


l <- newCollection(combine(rksp.0, rjmw.0), writeToFile=FALSE)
test_that("newCollection(rbind(rksp.0, rjmw.0), writeToFile=FALSE)
          produces correct output", {
  expect_length(l, 2)
  expect_equal(l[1], "rksp.0")
  expect_equal(l[2], "rjmw.0")
})

l <- newCollection(c("a", "b", "c"), writeToFile=FALSE)
test_that("newCollection(c('a', 'b', 'c'), writeToFile=FALSE)
          produces correct output", {
  expect_length(l, 3)
  expect_equal(l[1], "a")
  expect_equal(l[2], "b")
  expect_equal(l[3], "c")
})
