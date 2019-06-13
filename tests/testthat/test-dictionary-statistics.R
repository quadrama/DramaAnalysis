context("Dictionary Statistics")

data(rksp.0)


# dictionaryStatisticsSingle()
context("dictionaryStatisticsSingle()")

dstat <- dictionaryStatisticsSingle(rksp.0, wordfield = c("schon"))
test_that("dictionaryStatisticsSingle(rksp.0, wordfield = c('schon'))
          has correct dimensions and produces correct output", {
  expect_length(dstat, 4)
  expect_equal(as.integer(dstat[6,4]), 2)
  expect_equal(as.character(dstat[6,3]), "conti")
})

dstat <- dictionaryStatisticsSingle(rksp.0, wordfield = c("schon"), normalizeByFigure = TRUE)
test_that("dictionaryStatisticsSingle(rksp.0, wordfield = c('schon'), normalizeByFigure = TRUE) has correct dimensions and produces correct output", {
  expect_length(dstat, 4)
  expect_equal(as.numeric(dstat[6,4]), 0.0026178010)
})

dstat <- dictionaryStatisticsSingle(rksp.0, wordfield = c("schon","gut"), 
                                    normalizeByFigure = FALSE,
                                    normalizeByField = TRUE)
test_that("dictionaryStatisticsSingle(rksp.0, wordfield = c('schon','gut'), normalizeByFigure = FALSE, normalizeByField = TRUE) has correct dimensions and produces correct output", {
  expect_length(dstat, 4)
  expect_equal(as.numeric(dstat[6,4]), 1.0)
})


test_that("dictionaryStatisticsSingle(..., wordfield = c('schon','gut'), byFigure=FALSE) has correct dimensions and produces correct output", {
  dstat <- dictionaryStatisticsSingle(rksp.0, wordfield = c("schon","gut"), 
                                      byFigure = FALSE)
  expect_length(dstat, 3)
  expect_equal(as.numeric(dstat[1,3]), 132)

  dstat <- dictionaryStatisticsSingle(rksp.0, wordfield = c("schon","gut"), 
                                      byFigure = FALSE, normalizeByField = TRUE)
  expect_length(dstat, 3)
  expect_equal(as.numeric(dstat[1,3]), 66)
  
})


# dictionaryStatistics()
context("dictionaryStatistics()")


dstat <- dictionaryStatistics(rksp.0, fields=list(Familie=list("aber")))
test_that("dictionaryStatistics(rksp.0, fields=list(Familie=list('aber'))) 
          produces correct output", {
  expect_equal(as.numeric(dstat[8,4]), 29)
  expect_equal(as.character(dstat[8,3]), "der_prinz")
})

dstat <- dictionaryStatistics(rksp.0)
test_that("dictionaryStatistics(rksp.0) 
          has correct dimensions and produces correct output", {
  expect_length(dstat, 4)          
  expect_equal(as.integer(dstat[6,4]), 11)
  expect_equal(colnames(dstat)[4], "Liebe")
})

dstat <- dictionaryStatistics(rksp.0, segment="Act")
test_that("dictionaryStatistics(rksp.0, segment='Act') 
          has correct dimensions and produces correct output", {
    expect_length(dstat, 5)
    expect_equal(nrow(dstat), 65)
    expect_equal(as.integer(dstat[7,5]), 9)
    expect_equal(colnames(dstat)[3], "Number.Act")
})

dstat <- dictionaryStatistics(rksp.0, segment="Scene")
test_that("dictionaryStatistics(rksp.0, segment='Scene') has correct dimensions and produces correct output", {
    expect_length(dstat, 6)   
    expect_equal(nrow(dstat), 559)
    expect_equal(as.integer(dstat[6,6]), 0)
    expect_equal(as.integer(dstat[8,6]), 2)
    expect_equal(colnames(dstat)[3], "Number.Act")
    expect_equal(colnames(dstat)[4], "Number.Scene")
})


dstat <- dictionaryStatistics(rksp.0, fieldnames=c("Ratio", "Religion"), normalizeByFigure = TRUE)
test_that("dictionaryStatistics(rksp.0, fieldnames=c('Ratio', 'Religion'), normalizeByFigure = TRUE) has correct dimensions and produces correct output" ,{
  expect_length(dstat, 5)
  expect_equal(colnames(dstat)[4], "Ratio")  
  expect_equal(colnames(dstat)[5], "Religion")
  expect_equal(as.numeric(dstat[5,4]), 0.0042898)
  expect_equal(as.numeric(dstat[5,5]), 0.004766444)
})



context("filterByDictionary()")

filtered <- filterByDictionary(frequencytable(rksp.0, byFigure = TRUE))
test_that("filterByDictionary(frequencytable(rksp.0, byFigure = TRUE) has correct dimensions and produces correct output", {
  expect_length(filtered, 143)
  expect_length(filtered[,1], 13)
  expect_equal(FALSE %in% (colnames(filtered) %in% base_dictionary$Liebe), FALSE)
})

filtered <- filterByDictionary(frequencytable(rksp.0, byFigure = TRUE), fieldnames = c("Krieg"))
test_that("filterByDictionary(frequencytable(rksp.0, byFigure = TRUE, fieldnames = c('Krieg'))  
          has correct dimensions and produces correct output", {
  expect_length(filtered, 78)
  expect_length(filtered[,1], 13)
  expect_equal(FALSE %in% (colnames(filtered) %in% base_dictionary$Krieg), FALSE)
})



# TODO: enrichDictionary()?