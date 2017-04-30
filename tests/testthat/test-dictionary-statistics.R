data(rksp.0.text)

dstat <- dictionary.statistics.single(rksp.0.text, wordfield = c("schön"), names=TRUE)

expect_that(length(dstat), equals(3))
expect_that(dstat[6,3], equals(1))

dstat <- dictionary.statistics.single(rksp.0.text, wordfield = c("schön"), names=TRUE, normalize.by.figure = TRUE)
expect_that(length(dstat), equals(3))
expect_that(dstat[6,3], equals(0.0013089005))

dstat <- dictionary.statistics.single(rksp.0.text, wordfield = c("schön","gut"), names=TRUE, normalize.by.figure = FALSE, normalize.by.field = TRUE)
expect_that(length(dstat), equals(3))
expect_that(dstat[6,3], equals(0.5))
