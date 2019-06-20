context("combine()")

data(rksp.0)
data(rjmw.0)

combinedPlay <- combine(rksp.0, rjmw.0)

test_that("combine(rksp.0, rjmw.0) produces the correct output", 
          {
            expect_s3_class(combinedPlay, "QDDrama")
            expect_s3_class(combinedPlay$text, "QDHasUtteranceBE")
            expect_s3_class(combinedPlay$segments, "QDHasSegments")
            expect_s3_class(combinedPlay$mentions, "QDHasUtteranceBE")

          })

context("numberOfPlays()")
test_that("numberOfPlays() counts correctly", 
          {
            expect_equal(numberOfPlays(rksp.0), 1)
            expect_equal(numberOfPlays(rjmw.0), 1)
            expect_equal(numberOfPlays(combinedPlay), 2)
          })