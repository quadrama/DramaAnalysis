require(DramaAnalysis)

setup()

installData("test")

removeSpecialCharacters <- function(x) {
  x$text$Token.surface <- stringi::stri_trans_general(x$text$Token.surface, "latin-ascii")
  x$text$Token.lemma <- stringi::stri_trans_general(x$text$Token.lemma, "latin-ascii")
  
  x$stageDirections$Token.surface <- stringi::stri_trans_general(x$stageDirections$Token.surface, "latin-ascii")
  x$stageDirections$Token.lemma <- stringi::stri_trans_general(x$stageDirections$Token.lemma, "latin-ascii")
  
  x
}

rksp.0 <- loadDrama("test:rksp.0")
rksp.0 <- removeSpecialCharacters(rksp.0)
usethis::use_data(rksp.0, overwrite=TRUE)

rjmw.0 <- loadDrama("test:rjmw.0")
rjmw.0 <- removeSpecialCharacters(rjmw.0)
usethis::use_data(rjmw.0, overwrite=TRUE)
