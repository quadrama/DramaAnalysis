data("rksp.0")
data("vndf.0")



toler <- 1e-4

testtext <- data.table(drama=c("a","a","a"),
                       Speaker.figure_id=c(1, 1, 2),
                       Token.surface=c("b","b","a"))

ft <- frequencytable(testtext, byFigure = TRUE, acceptedPOS=c(), sortResult=FALSE, normalize = FALSE)
expect_equal(ft[1,1], 0, tolerance=toler)
expect_equal(ft[1,2], 2, tolerance=toler)
expect_equal(ft[2,1], 1, tolerance=toler)
expect_equal(ft[2,2], 0, tolerance=toler)

text <- rbind(vndf.0$mtext,rksp.0$mtext)

ft <- frequencytable(text,normalize=FALSE,byFigure = FALSE)

