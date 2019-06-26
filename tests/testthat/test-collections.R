context("newCollection()")

data(rksp.0)
data(rjmw.0)


# newCollection()

l <- newCollection(combine(rksp.0, rjmw.0), writeToFile=FALSE)
test_that("newCollection(rbind(rksp.0, rjmw.0), writeToFile=FALSE)
          produces correct output", {
            expect_length(l, 2)
            expect_equal(l[1], "rksp.0")
            expect_equal(l[2], "rjmw.0")
          })

l <- newCollection(c("a", "b", "c"), writeToFile=FALSE)
test_that("newCollection(c('a', 'b', 'c'), writeToFile=FALSE)
          produces correct output", {
            expect_length(l, 3)
            expect_equal(l[1], "a")
            expect_equal(l[2], "b")
            expect_equal(l[3], "c")
          })
