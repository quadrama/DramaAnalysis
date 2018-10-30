context("Configuration")

data("rksp.0")

cfg <- configuration(rksp.0$mtext, onlyPresence = TRUE)

test_that("configuration (onlyPresence = TRUE) has correct length 3", {
  expect_length(cfg, 3) 
})

test_that("elements of configuration (onlyPresence = TRUE) have correct type", {
  expect_equal(mode(cfg$matrix), "logical")
  expect_equal(class(cfg$figure), "factor")
  expect_true("data.table" %in% class(cfg$drama))
})

test_that("configuration$matrix (onlyPresence = TRUE) has correct dimensions", {
  expect_equal(ncol(cfg$matrix),5)
  expect_equal(nrow(cfg$matrix),13)
})

cfg <- configuration(rksp.0$mtext, onlyPresence = TRUE, by="Scene")

test_that("configuration (onlyPresence = TRUE, by = 'Scene') has correct length 3", {
  expect_length(cfg, 3)
})

test_that("elements of configuration (onlyPresence = TRUE, by = 'Scene') have correct type", {
  expect_equal(mode(cfg$matrix), "logical")
  expect_equal(class(cfg$figure), "factor")
  expect_true("data.table" %in% class(cfg$drama))
  
})

test_that("configuration$matrix (onlyPresence = TRUE, by = 'Scene') has correct dimensions", {
  expect_equal(ncol(cfg$matrix),43)
  expect_equal(nrow(cfg$matrix),13)
})

cfg <- configuration(rksp.0$mtext, onlyPresence = TRUE, useCharacterId = TRUE)

test_that("configuration (onlyPresence = TRUE, useCharacterID = TRUE) uses characterID", {
  expect_equal(as.character(cfg$figure[1]), "der_prinz")
  expect_equal(as.character(cfg$figure[2]), "der_kammerdiener")
})

cfg <- configuration(rksp.0$mtext, onlyPresence = TRUE, mode = "Passive")

test_that("configuration (onlyPresence = TRUE, mode = 'Passive') uses mentions for configuration$matrix", {
  expect_equal(as.vector(cfg$matrix[1,]), c(TRUE, TRUE, TRUE, TRUE, TRUE)) # "DER PRINZ"
  expect_equal(as.vector(cfg$matrix[2,]), c(TRUE, TRUE, TRUE, TRUE, TRUE)) # "EMILIA"
  expect_equal(as.vector(cfg$matrix[4,]), c(TRUE, FALSE, FALSE, FALSE, FALSE)) # "DER KAMMERDIENER"
})