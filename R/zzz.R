.onLoad <- function(libname, pkgname) {
  dataDirectory <- file.path(path.expand("~"),"QuaDramA","Data2")
  collectionDirectory <- file.path(dataDirectory,"collections")
  options(qd.datadir=dataDirectory)
  options(qd.collectionDirectory=collectionDirectory)
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste("DramaAnalysis",packageVersion("DramaAnalysis")))
}