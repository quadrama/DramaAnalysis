data(rksp.0)
data(vndf.0)

expect_that(length(rksp.0$mtext), equals(18))
expect_that(length(vndf.0$mtext), equals(18))
expect_that(length(rksp.0$mtext$begin), equals(25365))
expect_that(length(vndf.0$mtext$begin), equals(28524))
