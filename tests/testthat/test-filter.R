context("filter()")

data("rksp.0")


# utteranceStatistics()

ustat <- utteranceStatistics(rksp.0)

filtered <- filter(ustat, rksp.0, by="rank", n=1)
test_that("filter(by=rank,n=1) produces correct results", {
  expect_equal(nrow(filtered), 221)
  expect_equal(ncol(ustat), ncol(filtered))
})

filtered <- filter(ustat, rksp.0, by="rank", n=2)
test_that("filter(by=rank,n=2) produces correct results", {
  expect_equal(nrow(filtered), 378)
  expect_equal(ncol(ustat), ncol(filtered))
})

# dictionaryStatistics()

dstat <- dictionaryStatistics(rksp.0, segment="Drama")

filtered <- filter(dstat, rksp.0, by="rank", n=1)
test_that("filter(by=rank, n=1) produces correct results", {
  expect_equal(nrow(filtered), 1)
  expect_equal(ncol(dstat), ncol(filtered))
})

filtered <- filter(dstat, rksp.0, by="rank", n=2)
test_that("filter(by=rank, n=2) produces correct results", {
  expect_equal(nrow(filtered), 2)
  expect_equal(ncol(dstat), ncol(filtered))
})

# configuration()

hc <- configuration(rksp.0, segment = "Act")

filtered <- filter(hc, rksp.0, by="rank", n = 3)
test_that("filter(by=rank, n=3) produces correct results", {
  expect_equal(nrow(filtered), 3)
  expect_equal(ncol(filtered), ncol(hc))
})

filtered <- filter(hc, rksp.0, by="tokens", n = 2000)
test_that("filter(by=tokens, n=2000) produces correct results", {
  expect_equal(nrow(filtered), 6)
  expect_equal(ncol(filtered), ncol(hc))
})

filtered <- filter(hc, rksp.0, by="name", n = c("emilia", "der_prinz"))
test_that("filter(by=tokens, n=c(emilia, der_prinz), ) produces correct results", {
  expect_equal(nrow(filtered), 2)
  expect_equal(ncol(filtered), ncol(hc))
})

hc <- dictionaryStatistics(combine(rksp.0, rjmw.0))
filtered <- filter(hc, rksp.0, by="rank", n = 3)
test_that("filter(by=rank, n=3) produces correct results", {
  expect_equal(nrow(filtered), 3)
  expect_equal(ncol(filtered), ncol(hc))
})

