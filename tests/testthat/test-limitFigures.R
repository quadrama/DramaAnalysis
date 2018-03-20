data(rksp.0)

limited <- limitFigures(rksp.0$mtext, by="rank", threshold=1)
expect_equal(length(unique(limited$Speaker.figure_id)), 1)
expect_equal(as.character(unique(limited$Speaker.figure_surface)), "MARINELLI")

limited <- limitFigures(rksp.0$mtext, by="tokens", threshold=1)
expect_equal(length(unique(limited$Speaker.figure_id)), 13)
