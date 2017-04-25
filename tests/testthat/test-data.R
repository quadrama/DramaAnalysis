data(rksp.0.text)
data(vndf.0.text)

expect_that(length(rksp.0.text), equals(9))
expect_that(length(vndf.0.text), equals(9))
expect_that(length(rksp.0.text$begin), equals(25365))
expect_that(length(vndf.0.text$begin), equals(28542))
