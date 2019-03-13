require(DramaAnalysis)

setup()

installData("test")

rksp.0 <- list(mtext=loadSegmentedText("test:rksp.0"),
               char=loadCharacters("test:rksp.0"))
usethis::use_data(rksp.0, overwrite=TRUE)

rjmw.0 <- list(mtext=loadSegmentedText("test:rjmw.0"),
               char=loadCharacters("test.rjmw.0"))
usethis::use_data(rjmw.0, overwrite=TRUE)
