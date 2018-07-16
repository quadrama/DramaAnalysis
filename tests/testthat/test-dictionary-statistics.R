context("Dictionary Statistics")

data(rksp.0)


# dictionaryStatisticsSingle()

dstat <- dictionaryStatisticsSingle(rksp.0$mtext, wordfield = c("schön"), names=TRUE)
test_that("dictionaryStatisticsSingle(rksp.0$mtext, wordfield = c('schön'), names=TRUE)
          has correct dimensions and produces correct output", {
  expect_length(dstat, 4)
  expect_equal(as.integer(dstat[6,4]), 1)
})

dstat <- dictionaryStatisticsSingle(rksp.0$mtext, wordfield = c("schön"), names=TRUE, normalizeByFigure = TRUE)
test_that("dictionaryStatisticsSingle(rksp.0$mtext, wordfield = c('schön'), names=TRUE, normalizeByFigure = TRUE) 
          has correct dimensions and produces correct output", {
  expect_length(dstat, 4)
  expect_equal(as.numeric(dstat[6,4]), 0.0013089005)
})

dstat <- dictionaryStatisticsSingle(rksp.0$mtext, wordfield = c("schön","gut"), names=TRUE, normalizeByFigure = FALSE, normalizeByField = TRUE)
test_that("dictionaryStatisticsSingle(rksp.0$mtext, wordfield = c('schön','gut'), names=TRUE, normalizeByFigure = FALSE, normalizeByField = TRUE) 
          has correct dimensions and produces correct output", {
  expect_length(dstat, 4)
  expect_equal(as.numeric(dstat[6,4]), 0.5)
})


# dictionaryStatistics()

dstat <- dictionaryStatistics(rksp.0$mtext, fields=list(Familie=list("aber")))
test_that("dictionaryStatistics(rksp.0$mtext, fields=list(Familie=list('aber'))) 
          produces correct output", {
  expect_equal(as.numeric(dstat[10,4]), 29)
})

dstat <- dictionaryStatistics(rksp.0$mtext)
test_that("dictionaryStatistics(rksp.0$mtext) 
          has correct dimensions and produces correct output", {
  expect_length(dstat, 4)          
  expect_equal(as.integer(dstat[6,4]), 10)
  expect_equal(colnames(dstat[,4]), "Liebe")
})

dstat <- dictionaryStatistics(rksp.0$mtext, fieldnames=c("Ratio", "Religion"), normalizeByFigure = TRUE)
test_that("dictionaryStatistics(rksp.0$mtext, fieldnames=c('Ratio', 'Religion'), normalizeByFigure = TRUE) 
          has correct dimensions and produces correct output" ,{
  expect_length(dstat, 5)
  expect_equal(colnames(dstat[,4]), "Ratio")  
  expect_equal(colnames(dstat[,5]), "Religion")
  expect_equal(as.numeric(dstat[5,4]), 0.004211511)
  expect_equal(as.numeric(dstat[5,5]), 0.0028076743)
})

# TODO: add test for boost-option when implemented
# dstat <- dictionaryStatistics(rksp.0$mtext, normalizeByField = TRUE, boost = 2)
# test_that("dictionaryStatistics(rksp.0$mtext, normalizeByField = TRUE, boost = 2) 
#           produces correct output", {
#
# })


# filterByDictionary()

filtered <- filterByDictionary(frequencytable(rksp.0$mtext, byFigure = TRUE))
test_that("filterByDictionary(frequencytable(rksp.0$mtext, byFigure = TRUE)  
          has correct dimensions and produces correct output", {
  expect_length(filtered, 403)
  expect_length(filtered[,1], 13)
  expect_equal(FALSE %in% (colnames(filtered) %in% base_dictionary$Liebe), FALSE)
})

filtered <- filterByDictionary(frequencytable(rksp.0$mtext, byFigure = TRUE), fieldnames = c("Krieg"))
test_that("filterByDictionary(frequencytable(rksp.0$mtext, byFigure = TRUE, fieldnames = c('Krieg'))  
          has correct dimensions and produces correct output", {
  expect_length(filtered, 338)
  expect_length(filtered[,1], 13)
  expect_equal(FALSE %in% (colnames(filtered) %in% base_dictionary$Krieg), FALSE)
})


# TODO: add tests for regroup()

# TODO: add tests for enricht_Dictionary()