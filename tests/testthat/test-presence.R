context("presence()")

data(rksp.0)

test_that("presence(rksp.0) has correct dimensions and produces correct output", {
  pres <- presence(rksp.0)
  row <- pres[pres$character=="battista",]
  expect_equal(row$actives,  4)
  expect_equal(row$passives, 7)
  expect_lte(row$passives+row$actives, row$scenes)
  row <- pres[pres$character=="emilia",]
  expect_equal(row$actives,  7)
  expect_equal(row$passives, 29)
  expect_lte(row$passives+row$actives, row$scenes)
  row <- pres[pres$character=="marinelli",]
  expect_equal(row$actives,  19)
  expect_equal(row$passives, 10)
  expect_lte(row$passives+row$actives, row$scenes)
  
})

test_that("presence(rksp.0, passiveOnlyWhenNotActive = FALSE) has correct dimensions and produces correct output", {
  pres <- presence(rksp.0, passiveOnlyWhenNotActive = FALSE)
  row <- pres[pres$character=="battista",]
  expect_equal(row$actives,  4)
  expect_equal(row$passives, 11)
  row <- pres[pres$character=="emilia",]
  expect_equal(row$actives,  7)
  expect_equal(row$passives, 36)
  row <- pres[pres$character=="marinelli",]
  expect_equal(row$actives,  19)
  expect_equal(row$passives, 29)
})