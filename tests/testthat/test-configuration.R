data("rksp.0")


cfg <- configuration(rksp.0$mtext, onlyPresence = TRUE)
expect_length(cfg, 3)

expect_equal(mode(cfg$matrix), "logical")
expect_equal(class(cfg$figure), "factor")
expect_true("data.table" %in% class(cfg$drama))

expect_equal(ncol(cfg$matrix),5)
expect_equal(nrow(cfg$matrix),13)

cfg <- configuration(rksp.0$mtext, onlyPresence = TRUE, by="Scene")
expect_length(cfg, 3)

expect_equal(mode(cfg$matrix), "logical")
expect_equal(class(cfg$figure), "factor")
expect_true("data.table" %in% class(cfg$drama))

expect_equal(ncol(cfg$matrix),43)
expect_equal(nrow(cfg$matrix),13)
