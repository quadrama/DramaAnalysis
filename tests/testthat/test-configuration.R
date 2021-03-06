context("Configuration")

data("rksp.0")

cfg <- configuration(rksp.0, onlyPresence = TRUE)

test_that("configuration() has correct types", {
  expect_true(inherits(cfg, "QDConfiguration"))
  expect_true(inherits(cfg, "data.frame"))
})
test_that("configuration (onlyPresence = TRUE) has correct length and height", {
  expect_equal(ncol(cfg), 8) 
  expect_equal(nrow(cfg), 13) 
})


cfg <- configuration(rksp.0, onlyPresence = TRUE, segment="Scene")

test_that("configuration() has correct types", {
  expect_true(inherits(cfg, "QDConfiguration"))
  expect_true(inherits(cfg, "data.frame"))
})

test_that("configuration (onlyPresence = TRUE, segment = 'Scene') has correct length 3", {
  expect_equal(nrow(cfg), 13)
  expect_equal(ncol(cfg), 46)
})

test_that("configuration() stops if called with multiple plays", {
  expect_error(configuration(combine(rksp.0, rjmw.0)))
})


test_that("configuration (onlyPresence = TRUE, mode = 'Passive') produces correct results", {
  cfg <- configuration(rksp.0, onlyPresence = TRUE, mode = "Passive")
  cfgm <- as.matrix(cfg)

  expect_equal(as.vector(cfgm[1,]), c(TRUE, TRUE, TRUE, TRUE, TRUE)) # "DER PRINZ"
  expect_equal(as.vector(cfgm[2,]), c(TRUE, TRUE, TRUE, TRUE, TRUE)) # "EMILIA"
  expect_equal(as.vector(cfgm[3,]), c(TRUE, FALSE, FALSE, FALSE, FALSE)) # "DER KAMMERDIENER"
})
