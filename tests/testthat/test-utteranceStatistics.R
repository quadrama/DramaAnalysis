data(rksp.0)
data(vndf.0)

ustat <- utteranceStatistics(rksp.0$mtext, numberOfFigures = 100, 
                             normalizeByDramaLength = FALSE)

expect_that(length(ustat), equals(5))
expect_that(length(ustat$drama), equals(835))
expect_that(median(ustat$utteranceLength), equals(17))
expect_that(median(ustat[ustat$figure=="Odoardo G",5]), equals(14))

ustat <- utteranceStatistics(rksp.0$mtext, numberOfFigures = 10, 
                             normalizeByDramaLength = TRUE)

expect_that(length(ustat), equals(6))
expect_that(length(ustat$drama), equals(785))
expect_equal(median(ustat$utteranceLength), 0.000733, tolerance=0.00001)
expect_equal(median(ustat[ustat$figure=="Odoardo G",5]), 0.00057, tolerance=0.00001)
