data("rksp.0.text")
data("vndf.0.text")

require(data.table)

testtext <- data.table(drama=c("a","a","a"),Speaker.figure_id=c(1, 1, 2),Token.surface=c("b","b","a"))

ft <- frequencytable(testtext, byFigure = TRUE, acceptedPOS=c())
expect_equal(ft[1,1],0)
expect_equal(ft[1,2],0.666667, tolerance=1e-4)
expect_equal(ft[2,1],0.3333, tolerance=1e-4)
expect_equal(ft[2,2],0)

text <- rbind(vndf.0.text,rksp.0.text)

ft <- frequencytable(text,normalize=FALSE,byFigure = FALSE)

expect_that(length(ustat), equals(5))
