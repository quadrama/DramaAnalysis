context("filterCharacters()")

data("rksp.0")


# utteranceStatistics()

ustat <- utteranceStatistics(rksp.0)

filtered <- filterCharacters(ustat, rksp.0, by="rank", n=1)
test_that("filterCharacters(by=rank,n=1) produces correct results", {
  expect_equal(nrow(filtered), 221)
  expect_equal(ncol(ustat), ncol(filtered))
})

filtered <- filterCharacters(ustat, rksp.0, by="rank", n=2)
test_that("filterCharacters(by=rank,n=2) produces correct results", {
  expect_equal(nrow(filtered), 378)
  expect_equal(ncol(ustat), ncol(filtered))
})

# dictionaryStatistics()

dstat <- dictionaryStatistics(rksp.0, segment="Drama")

filtered <- filterCharacters(dstat, rksp.0, by="rank", n=1)
test_that("filterCharacters(by=rank, n=1) produces correct results", {
  expect_equal(nrow(filtered), 1)
  expect_equal(ncol(dstat), ncol(filtered))
})

filtered <- filterCharacters(dstat, rksp.0, by="rank", n=2)
test_that("filterCharacters(by=rank, n=2) produces correct results", {
  expect_equal(nrow(filtered), 2)
  expect_equal(ncol(dstat), ncol(filtered))
})

# configuration()

hc <- configuration(rksp.0, segment = "Act")

filtered <- filterCharacters(hc, rksp.0, by="rank", n = 3)
test_that("filterCharacters(by=rank, n=3) produces correct results", {
  expect_equal(nrow(filtered), 3)
  expect_equal(ncol(filtered), ncol(hc))
})

filtered <- filterCharacters(hc, rksp.0, by="tokens", n = 2000)
test_that("filterCharacters(by=tokens, n=2000) produces correct results", {
  expect_equal(nrow(filtered), 6)
  expect_equal(ncol(filtered), ncol(hc))
})

filtered <- filterCharacters(hc, rksp.0, by="name", n = c("emilia", "der_prinz"))
test_that("filterCharacters(by=tokens, n=c(emilia, der_prinz), ) produces correct results", {
  expect_equal(nrow(filtered), 2)
  expect_equal(ncol(filtered), ncol(hc))
})

hc <- dictionaryStatistics(combine(rksp.0, rjmw.0))
filtered <- filterCharacters(hc, rksp.0, by="rank", n = 3)
test_that("filterCharacters(by=rank, n=3) produces correct results", {
  expect_equal(nrow(filtered), 3)
  expect_equal(ncol(filtered), ncol(hc))
})

