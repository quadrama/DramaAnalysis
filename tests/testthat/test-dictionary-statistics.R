context("Dictionary Statistics")

data(rksp.0)


# dictionaryStatisticsSingle()

dstat <- dictionaryStatisticsSingle(rksp.0, wordfield = c("schön"))
test_that("dictionaryStatisticsSingle(rksp.0, wordfield = c('schön'))
          has correct dimensions and produces correct output", {
  expect_length(dstat, 4)
  expect_equal(as.integer(dstat[6,4]), 1)
})

dstat <- dictionaryStatisticsSingle(rksp.0, wordfield = c("schön"), normalizeByFigure = TRUE)
test_that("dictionaryStatisticsSingle(rksp.0, wordfield = c('schön'), normalizeByFigure = TRUE) 
          has correct dimensions and produces correct output", {
  expect_length(dstat, 4)
  expect_equal(as.numeric(dstat[6,4]), 0.0013089005)
})

dstat <- dictionaryStatisticsSingle(rksp.0, wordfield = c("schön","gut"), normalizeByFigure = FALSE, normalizeByField = TRUE)
test_that("dictionaryStatisticsSingle(rksp.0, wordfield = c('schön','gut'), normalizeByFigure = FALSE, normalizeByField = TRUE) 
          has correct dimensions and produces correct output", {
  expect_length(dstat, 4)
  expect_equal(as.numeric(dstat[6,4]), 0.5)
})


# dictionaryStatistics()

dstat <- dictionaryStatistics(rksp.0, fields=list(Familie=list("aber")))
test_that("dictionaryStatistics(rksp.0, fields=list(Familie=list('aber'))) 
          produces correct output", {
  expect_equal(as.numeric(dstat[10,4]), 29)
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
    expect_equal(as.integer(dstat[6,5]), 11)
    expect_equal(colnames(dstat)[3], "Number.Act")
})

dstat <- dictionaryStatistics(rksp.0, segment="Scene")
test_that("dictionaryStatistics(rksp.0, segment='Scene') has correct dimensions and produces correct output", {
    expect_length(dstat, 6)          
    expect_equal(as.integer(dstat[6,6]), 0)
    expect_equal(colnames(dstat)[3], "Number.Act")
    expect_equal(colnames(dstat)[4], "Number.Scene")
})


dstat <- dictionaryStatistics(rksp.0, fieldnames=c("Ratio", "Religion"), normalizeByFigure = TRUE)
test_that("dictionaryStatistics(rksp.0, fieldnames=c('Ratio', 'Religion'), normalizeByFigure = TRUE) has correct dimensions and produces correct output" ,{
  expect_length(dstat, 5)
  expect_equal(colnames(dstat)[4], "Ratio")  
  expect_equal(colnames(dstat)[5], "Religion")
  expect_equal(as.numeric(dstat[5,4]), 0.0042898)
  expect_equal(as.numeric(dstat[5,5]), 0.003336511)
})


# filterByDictionary()

filtered <- filterByDictionary(frequencytable(rksp.0, byFigure = TRUE))
test_that("filterByDictionary(frequencytable(rksp.0, byFigure = TRUE)  
          has correct dimensions and produces correct output", {
  expect_length(filtered, 403)
  expect_length(filtered[,1], 13)
  expect_equal(FALSE %in% (colnames(filtered) %in% base_dictionary$Liebe), FALSE)
})

filtered <- filterByDictionary(frequencytable(rksp.0, byFigure = TRUE), fieldnames = c("Krieg"))
test_that("filterByDictionary(frequencytable(rksp.0, byFigure = TRUE, fieldnames = c('Krieg'))  
          has correct dimensions and produces correct output", {
  expect_length(filtered, 338)
  expect_length(filtered[,1], 13)
  expect_equal(FALSE %in% (colnames(filtered) %in% base_dictionary$Krieg), FALSE)
})



# TODO: enrichDictionary()?