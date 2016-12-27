data(rksp.0)
data(vndf.0)

ustat <- utterance_statistics(rksp.0.text, num.figures = 10, normalize.by.drama.length=FALSE)

expect_that(length(ustat), equals(5))
expect_that(length(ustat$drama), equals(812))
expect_that(median(ustat$utterance_length), equals(17.5))
expect_that(median(ustat[ustat$figure=="Odoardo",5]), equals(14))

ustat <- utterance_statistics(rksp.0.text, num.figures = 10, normalize.by.drama.length=TRUE)

expect_that(length(ustat), equals(5))
expect_that(length(ustat$drama), equals(812))
expect_that(median(ustat$utterance_length), equals(0.0006899271))
expect_that(median(ustat[ustat$figure=="Odoardo",5]), equals(0.0005519417))
