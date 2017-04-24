data(rksp.0.text)

dstat <- dictionary.statistics.single(rksp.0.text, wordfield = c("schön"), names=TRUE)

expect_that(length(dstat), equals(3))
expect_that(dstat[7,3], equals(1))