.onLoad <- function(libname, pkgname) {
  if (!Sys.getenv(c("QUADRAMA_DIR")) == "") {
    dataDirectory <- Sys.getenv(c("QUADRAMA_DIR"))
  } else {
    dataDirectory <- file.path(path.expand("~"),"QuaDramA","Data2")
  }
  collectionDirectory <- file.path(dataDirectory,"collections")
  options(qd.datadir=dataDirectory)
  options(qd.collectionDirectory=collectionDirectory)
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste("DramaAnalysis",utils::packageVersion("DramaAnalysis")))
}
