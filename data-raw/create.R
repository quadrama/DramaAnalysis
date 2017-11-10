require(DramaAnalysis)

setup()

installData("gdc")

rksp.0 <- list(mtext=loadSegmentedText("test:rksp.0"))
devtools::use_data(rksp.0, overwrite=TRUE)

rjmw.0 <- list(mtext=loadSegmentedText("test:rjmw.0"))
devtools::use_data(rjmw.0, overwrite=TRUE)
