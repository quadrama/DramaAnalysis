data(rksp.0.text)

dstat <- dictionaryStatisticsSingle(rksp.0.text, wordfield = c("schön"), names=TRUE)

expect_that(length(dstat), equals(3))
expect_that(as.integer(dstat[6,3]), equals(1))

dstat <- dictionaryStatisticsSingle(rksp.0.text, wordfield = c("schön"), names=TRUE, normalizeByFigure = TRUE)
expect_that(length(dstat), equals(3))
expect_that(as.numeric(dstat[6,3]), equals(0.0013089005))

dstat <- dictionaryStatisticsSingle(rksp.0.text, wordfield = c("schön","gut"), names=TRUE, normalizeByFigure = FALSE, normalizeByField = TRUE)
expect_that(length(dstat), equals(3))
expect_that(as.numeric(dstat[6,3]), equals(0.5))
