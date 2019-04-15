require(DramaAnalysis)

setup()

installData("test")

rksp.0 <- list(mtext=loadSegmentedText("test:rksp.0"),
               char=loadCharacters("test:rksp.0"))
usethis::use_data(rksp.0, overwrite=TRUE)

rjmw.0 <- list(mtext=loadSegmentedText("test:rjmw.0"),
               char=loadCharacters("test.rjmw.0"))
usethis::use_data(rjmw.0, overwrite=TRUE)

base_dictionary <- loadFields(fieldnames=c("Liebe", "Familie", "Krieg", "Ratio", "Religion"))
Encoding(base_dictionary$Familie) <- "UTF-8"
Encoding(base_dictionary$Krieg) <- "UTF-8"
Encoding(base_dictionary$Ratio) <- "UTF-8"
Encoding(base_dictionary$Liebe) <- "UTF-8"
Encoding(base_dictionary$Religion) <- "UTF-8"
usethis::use_data(base_dictionary, overwrite=TRUE)
