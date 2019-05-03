context("presence()")

data(rksp.0)

pres <- presence(rksp.0)
test_that("presence(rksp.0) has correct dimensions and produces correct output", {
  row <- pres[pres$character=="battista",]
  expect_equal(row$actives,  4)
  expect_equal(row$passives, 2)
  row <- pres[pres$character=="emilia",]
  expect_equal(row$actives,  7)
  expect_equal(row$passives, 16)
  row <- pres[pres$character=="marinelli",]
  expect_equal(row$actives,  19)
  expect_equal(row$passives, 5)
})

pres <- presence(rksp.0, passiveOnlyWhenNotActive = FALSE)
test_that("presence(rksp.0, passiveOnlyWhenNotActive = FALSE) has correct dimensions and produces correct output", {
  row <- pres[pres$character=="battista",]
  expect_equal(row$actives,  4)
  expect_equal(row$passives, 6)
  row <- pres[pres$character=="emilia",]
  expect_equal(row$actives,  7)
  expect_equal(row$passives, 23)
  row <- pres[pres$character=="marinelli",]
  expect_equal(row$actives,  19)
  expect_equal(row$passives, 24)
})