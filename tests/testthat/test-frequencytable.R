context("frequencytable()")

data(rksp.0)
toler <- 1e-4

test_that("frequencytable(rksp.0) 
          has correct dimensions and produces correct output", {
  ft <- frequencytable(rksp.0)
  expect_length(ft, 2350)
  expect_equal(colnames(ft)[4], ">>")
  expect_equal(ft[4], 4)
  expect_equal(colnames(ft)[2000], "verkummern")
  expect_equal(ft[2000], 1)
})

test_that("frequencytable(rksp.0, byCharacter=TRUE, segment='Act') 
          has correct dimensions and produces correct output", {
  ft <- frequencytable(rksp.0, byCharacter=TRUE, segment="Act")
  expect_length(colnames(ft), 2350)
  expect_length(rownames(ft), 28)
  expect_equal(rownames(ft)[4], "rksp.0|1|der_prinz")
  expect_equal(colnames(ft)[4], ">>")
  expect_equal(ft[4,4], 1)
  expect_equal(colnames(ft)[2000], "verkummern")
  expect_equal(ft[4,2000], 0)
})

ft <- frequencytable(rksp.0, segment="Scene", normalize=TRUE)
test_that("frequencytable(rksp.0, segment='Scene', normalize=TRUE) 
          has correct dimensions and produces correct output", {
  expect_length(colnames(ft), 2350)
  expect_length(rownames(ft), 43)
  expect_equal(colnames(ft)[4], ">>")
  expect_equal(rownames(ft)[1], "rksp.0|1|1")
  expect_equal(ft[4,4], 0, tolerance=toler)
  expect_equal(ft[30,2000], 0)
})

ft <- frequencytable(rksp.0, byCharacter=TRUE, segment="Scene", sep=";")
test_that("frequencytable(rksp.0, byCharacter=TRUE, segment='Scene', sep=';') 
          has correct dimensions and produces correct output", {
  expect_length(colnames(ft), 2350)
  expect_length(rownames(ft), 94)
  expect_equal(colnames(ft)[4], ">>")
  expect_equal(rownames(ft)[1], "rksp.0;1;1;der_kammerdiener")
  expect_equal(ft[4,4], 0)
})

ft <- frequencytable(rksp.0, byCharacter=TRUE, sortResult=TRUE)
test_that("frequencytable(rksp.0, byCharacter=TRUE, sortResult=TRUE) 
          has correct dimensions and produces correct output", {
  expect_length(colnames(ft), 2350)
  expect_length(rownames(ft), 13)
  expect_equal(colnames(ft)[4], "sein")
  expect_equal(rownames(ft)[1], "rksp.0|angelo")
  expect_equal(ft[1,1], 35)
})

ft <- frequencytable(rksp.0, byCharacter = TRUE, acceptedPOS = postags$de$n)
test_that("frequencytable(..., byCharacter=TRUE, acceptedPOS = postags$de$n) has correct dimensions and produces correct output", {
  expect_equal(ncol(ft), 914)
  expect_equal(nrow(ft), 13)
})
