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


# regroup()

dslr <- regroup(dictionaryStatistics(rksp.0, fieldnames = c("Liebe", "Familie"), segment = "Scene", normalizeByFigure = TRUE, asList = TRUE), 
                by = "Field")
test_that("regroup(dictionaryStatistics(rksp.0, fieldnames = c('Liebe', 'Familie'), segment = 'Scene', normalizeByFigure = TRUE, asList = TRUE), 
                by = 'Field') 
          has correct dimensions and produces correct output", {
  expect_length(dslr, 2)
  expect_length(dslr$Liebe, 43)
  expect_length(dslr$Familie, 43)
  expect_equal(dslr$Liebe[1][8,], 0.005882353) # Act 1, Scene 1, der_prinz
  expect_equal(dslr$Familie[1][8,], 0) # Act 1, Scene 1, der_prinz
})

dslr <- regroup(dictionaryStatistics(rksp.0, fieldnames = c("Liebe", "Familie"), normalizeByFigure = TRUE, asList = TRUE), 
                by = "Field")
test_that("regroup(dictionaryStatistics(rksp.0, fieldnames = c('Liebe', 'Familie'), normalizeByFigure = TRUE, asList = TRUE), 
                by = 'Field') 
          has correct dimensions and produces correct output", {
  expect_length(dslr, 2)
  expect_length(dslr$Liebe, 1)
  expect_length(dslr$Familie, 1)
  expect_equal(dslr$Liebe[8,1], 0.005222402) # der_prinz
  expect_equal(dslr$Familie[8,1], 0.005042319) # der_prinz
})

dslr <- regroup(dictionaryStatistics(rksp.0, fieldnames = c("Krieg"), normalizeByFigure = TRUE, asList = TRUE), 
                by = "Character")
test_that("regroup(dictionaryStatistics(rksp.0, fieldnames = c('Krieg'), normalizeByFigure = TRUE, asList = TRUE), 
                by = 'Character') 
          has correct dimensions and produces correct output", {
  expect_length(dslr, 13)
  expect_length(dslr$der_prinz, 1)
  expect_equal(dslr$der_prinz$mat, 0.002881325)
})


# TODO: enrichDictionary()?