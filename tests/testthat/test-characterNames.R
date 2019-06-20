context("characterNames()")

data(rksp.0)

# characterStatistics()

hc <- characterStatistics(rksp.0)
f <- characterNames(hc, rksp.0)
test_that("characterNames() produces correct output", {
  expect_equal(as.character(f$character), c("Der Prinz", "Der Kammerdiener", "Conti", "Marinelli", "Camillo Rota", "Claudia", "Pirro", "Odoardo Galotti", "Angelo", "Emilia", "Appiani", "Battista", "Orsina"))
  expect_s3_class(f$character, "factor")
})

f <- characterNames(hc, rksp.0, sort=1)
test_that("characterNames(sort=1) produces correct output", {
  expect_equal(as.character(f$character), c("Angelo", "Appiani", "Battista", "Camillo Rota", "Claudia",  "Conti", "Der Kammerdiener", "Der Prinz", "Emilia","Marinelli", "Odoardo Galotti", "Orsina", "Pirro"))
  expect_s3_class(f$character, "factor")
})

f <- characterNames(hc, rksp.0, sort=-1)
test_that("characterNames(sort=-1) produces correct output", {
  expect_equal(as.character(f$character), c("Pirro", "Orsina", "Odoardo Galotti", "Marinelli", "Emilia", "Der Prinz", "Der Kammerdiener", "Conti", "Claudia", "Camillo Rota", "Battista", "Appiani", "Angelo"))
  expect_s3_class(f$character, "factor")
})

f <- characterNames(hc, rksp.0, sort=c(13, 1, 12, 2, 11, 3, 10, 4, 9, 5, 8, 6, 7))
test_that("characterNames(sort=c(13, 1, 12, 2, 11, 3, 10, 4, 9, 5, 8, 6, 7)) produces correct output", {
  expect_equal(as.character(f$character), c("Orsina", "Der Prinz", "Battista", "Der Kammerdiener", "Appiani", "Conti", "Emilia", "Marinelli", "Angelo", "Camillo Rota", "Odoardo Galotti", "Claudia", "Pirro"))
  expect_s3_class(f$character, "factor")
})





