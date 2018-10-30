context("presence()")

data(rksp.0)

pres <- presence(rksp.0$mtext)
test_that("presence(rksp.0$mtext) 
          has correct dimensions and produces correct output", {
  row <- pres[figure=="battista",]
  expect_equal(row$actives,  4)
  expect_equal(row$passives, 2)
  row <- pres[figure=="emilia",]
  expect_equal(row$actives,  7)
  expect_equal(row$passives, 16)
  row <- pres[figure=="marinelli",]
  expect_equal(row$actives,  19)
  expect_equal(row$passives, 5)
})

pres <- presence(rksp.0$mtext, passiveOnlyWhenNotActive = FALSE)
test_that("presence(rksp.0$mtext, passiveOnlyWhenNotActive = FALSE) 
          has correct dimensions and produces correct output", {
  row <- pres[figure=="battista",]
  expect_equal(row$actives,  4)
  expect_equal(row$passives, 6)
  row <- pres[figure=="emilia",]
  expect_equal(row$actives,  7)
  expect_equal(row$passives, 23)
  row <- pres[figure=="marinelli",]
  expect_equal(row$actives,  19)
  expect_equal(row$passives, 24)
})