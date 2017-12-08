data(rksp.0)

dstat <- dictionaryStatisticsSingle(rksp.0$mtext, wordfield = c("schön"), names=TRUE)

expect_equal(length(dstat), 4)
expect_that(as.integer(dstat[6,4]), equals(1))

dstat <- dictionaryStatisticsSingle(rksp.0$mtext, wordfield = c("schön"), names=TRUE, normalizeByFigure = TRUE)
expect_that(length(dstat), equals(4))
expect_that(as.numeric(dstat[6,4]), equals(0.0013089005))

dstat <- dictionaryStatisticsSingle(rksp.0$mtext, wordfield = c("schön","gut"), names=TRUE, normalizeByFigure = FALSE, normalizeByField = TRUE)
expect_that(length(dstat), equals(4))
expect_that(as.numeric(dstat[6,4]), equals(0.5))

dstat <- dictionaryStatistics(rksp.0$mtext, fields=list(Familie=list("aber")))
expect_that(as.numeric(dstat[10,4]), equals(29))
