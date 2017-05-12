data(rksp.0.text)
data(vndf.0.text)

ustat <- utteranceStatistics(rksp.0.text, numberOfFigures = 10, normalizeByDramaLength = FALSE)

expect_that(length(ustat), equals(5))
expect_that(length(ustat$drama), equals(812))
expect_that(median(ustat$utteranceLength), equals(17.5))
expect_that(median(ustat[ustat$figure=="ODOARDO GALOTTI",5]), equals(14))

ustat <- utteranceStatistics(rksp.0.text, numberOfFigures = 10, normalizeByDramaLength = TRUE)

expect_that(length(ustat), equals(5))
expect_that(length(ustat$drama), equals(812))
expect_that(median(ustat$utteranceLength), equals(0.0006899271))
expect_that(median(ustat[ustat$figure=="ODOARDO GALOTTI",5]), equals(0.0005519417))
