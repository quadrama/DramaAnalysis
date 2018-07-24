context("frequencytable()")

data(rksp.0)
toler <- 1e-4
testtext <- data.table(drama=c("a","a","a"),
                       Speaker.figure_id=c(1, 1, 2),
                       Token.surface=c("b","b","a"))

ft <- frequencytable(testtext, byFigure = TRUE, acceptedPOS=c(), sortResult=FALSE, normalize = FALSE)
test_that("frequencytable(testtext, byFigure = TRUE, acceptedPOS=c(), sortResult=FALSE, normalize = FALSE) 
          produces correct output", {
  expect_equal(ft[1,1], 0, tolerance=toler)
  expect_equal(ft[1,2], 2, tolerance=toler)
  expect_equal(ft[2,1], 1, tolerance=toler)
  expect_equal(ft[2,2], 0, tolerance=toler)
})

ft <- frequencytable(rksp.0$mtext)
test_that("frequencytable(rksp.0$mtext) 
          has correct dimensions and produces correct output", {
  expect_length(ft, 3474)
  expect_equal(colnames(ft)[4], "ab")
  expect_equal(ft[4], 22)
  expect_equal(colnames(ft)[2000], "Mörder")
  expect_equal(ft[2000], 12)
})

ft <- frequencytable(rksp.0$mtext, byFigure=TRUE, by="Act")
test_that("frequencytable(rksp.0$mtext, byFigure=TRUE, by='Act') 
          has correct dimensions and produces correct output", {
  expect_length(colnames(ft), 3474)
  expect_length(rownames(ft), 28)
  expect_equal(rownames(ft)[4], "rksp.0|1|der_prinz")
  expect_equal(colnames(ft)[4], "ab")
  expect_equal(ft[4,4], 6)
  expect_equal(colnames(ft)[2000], "Mörder")
  expect_equal(ft[4,2000], 0)
})

ft <- frequencytable(rksp.0$mtext, by="Scene", normalize=TRUE)
test_that("frequencytable(rksp.0$mtext, by='Scene', normalize=TRUE) 
          has correct dimensions and produces correct output", {
  expect_length(colnames(ft), 3474)
  expect_length(rownames(ft), 43)
  expect_equal(colnames(ft)[4], "ab")
  expect_equal(rownames(ft)[1], "rksp.0|1|1")
  expect_equal(ft[4,4], 0.0008354219, tolerance=toler)
  expect_equal(ft[30,2000], 0)
})

ft <- frequencytable(rksp.0$mtext, byFigure=TRUE, by="Scene", sep=";")
test_that("frequencytable(rksp.0$mtext, byFigure=TRUE, by='Scene', sep=';') 
          has correct dimensions and produces correct output", {
  expect_length(colnames(ft), 3474)
  expect_length(rownames(ft), 94)
  expect_equal(colnames(ft)[4], "ab")
  expect_equal(rownames(ft)[1], "rksp.0;1;1;der_kammerdiener")
  expect_equal(ft[4,4], 0)
})

ft <- frequencytable(rksp.0$mtext, byFigure=TRUE, sortResult=TRUE, names=TRUE)
test_that("frequencytable(rksp.0$mtext, byFigure=TRUE, sortResult=TRUE, names=TRUE) 
          has correct dimensions and produces correct output", {
  expect_length(colnames(ft), 3474)
  expect_length(rownames(ft), 13)
  expect_equal(colnames(ft)[4], "nicht")
  expect_equal(rownames(ft)[1], "rksp.0|ANGELO")
  expect_equal(ft[1,1], 30)
})