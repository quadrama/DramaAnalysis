data(rksp.0)
data(vndf.0)

expect_that(length(rksp.0.text), equals(9))
expect_that(length(vndf.0), equals(9))
expect_that(length(rksp.0.text$begin), equals(25365))
expect_that(length(vndf.0$begin), equals(28542))
