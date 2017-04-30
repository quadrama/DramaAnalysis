data(rksp.0.text)
data(vndf.0.text)

ustat <- utterance_statistics(rksp.0.text, num.figures = 10, normalize.by.drama.length=FALSE)

expect_that(length(ustat), equals(5))
expect_that(length(ustat$drama), equals(781))
expect_that(median(ustat$utterance_length), equals(18))
expect_that(median(ustat[ustat$figure=="Odoardo",5]), equals(13.5))

ustat <- utterance_statistics(rksp.0.text, num.figures = 10, normalize.by.drama.length=TRUE)

expect_that(length(ustat), equals(5))
expect_that(length(ustat$drama), equals(781))
expect_that(median(ustat$utterance_length), equals(0.0007096393))
expect_that(median(ustat[ustat$figure=="Odoardo",5]), equals(0.0005322295))
