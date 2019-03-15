require(DramaAnalysis)

setup()

installData("test")


rksp.0 <- loadDrama("test:rksp.0")
usethis::use_data(rksp.0, overwrite=TRUE)

rjmw.0 <- loadDrama("test:rjmw.0")
usethis::use_data(rjmw.0, overwrite=TRUE)
