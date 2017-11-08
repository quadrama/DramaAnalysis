data(rksp.0)

t <- dramaTail(rksp.0$mtext, n=1)
expect_length(unique(t$begin.Act), 1)

t <- dramaTail(rksp.0$mtext, n=3)
expect_length(unique(t$begin.Act), 1)

t <- dramaTail(rksp.0$mtext, n=6)
expect_length(unique(t$begin.Act), 0)
expect_that(nrow(t), equals(0))

t <- dramaTail(rksp.0$mtext, n=1, op=">=")
expect_length(unique(t$begin.Act), 1)

t <- dramaTail(rksp.0$mtext, n=3, op=">=")
expect_length(unique(t$begin.Act), 3)

t <- dramaTail(rksp.0$mtext, n=6, op=">=")
expect_length(unique(t$begin.Act), 0)

t <- dramaTail(rksp.0$mtext, by="Scene", n=1)
expect_length(unique(t$begin.Scene), 1)

t <- dramaTail(rksp.0$mtext, by="Scene", n=12)
expect_length(unique(t$begin.Scene), 1)

t <- dramaTail(rksp.0$mtext, by="Scene", n=30)
expect_length(unique(t$begin.Scene), 1)