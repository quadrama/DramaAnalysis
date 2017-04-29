.onLoad <- function(libname, pkgname) {
  .jinit(parameters=c("-Xmx2g"))
  rJava::.jpackage(pkgname, jars="*", lib.loc = libname)
}