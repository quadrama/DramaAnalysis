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

cfgl <- as.list(cfg)

test_that("as.list() has correct types", {
  expect_true(inherits(cfgl, "list"))
})

test_that("elements of configuration (onlyPresence = TRUE) have correct type", {
  expect_equal(mode(cfgl$matrix), "logical")
  expect_equal(class(cfgl$character), "factor")
  expect_true("data.frame" %in% class(cfgl$drama))
})

test_that("configuration$matrix (onlyPresence = TRUE) has correct dimensions", {
  expect_equal(ncol(cfgl$matrix),5)
  expect_equal(nrow(cfgl$matrix),13)
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

cfgl <- as.list(cfg)

test_that("elements of configuration (onlyPresence = TRUE, segment = 'Scene') have correct type", {
  expect_equal(mode(cfgl$matrix), "logical")
  expect_equal(class(cfgl$character), "factor")
  expect_true("data.frame" %in% class(cfgl$drama))
  
})

test_that("configuration$matrix (onlyPresence = TRUE, segment = 'Scene') has correct dimensions", {
  expect_equal(ncol(cfgl$matrix),43)
  expect_equal(nrow(cfgl$matrix),13)
})


cfg <- configuration(rksp.0, onlyPresence = TRUE, mode = "Passive")
cfgm <- as.matrix(cfg)
test_that("configuration (onlyPresence = TRUE, mode = 'Passive') uses mentions for configuration$matrix", {
  expect_equal(as.vector(cfgm[1,]), c(TRUE, TRUE, TRUE, TRUE, TRUE)) # "DER PRINZ"
  expect_equal(as.vector(cfgm[2,]), c(TRUE, TRUE, TRUE, TRUE, TRUE)) # "EMILIA"
  expect_equal(as.vector(cfgm[3,]), c(TRUE, FALSE, FALSE, FALSE, FALSE)) # "DER KAMMERDIENER"
})