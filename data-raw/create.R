require(DramaAnalysis)

setup()

rksp.0 <- list(mtext=loadSegmentedText("test:rksp.0"))
devtools::use_data(rksp.0, overwrite=TRUE)
