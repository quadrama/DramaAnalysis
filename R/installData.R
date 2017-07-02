#' @title Download and install collection data
#' @description Function to download collection data (grouped texts) from github
#' @param dataDirectory The data directory in which collection and data files are stored
#' @param branchOrCommit The git branch, commit id, or tag that we want to download
#' @param repository The repository
#' @param baseUrl The github user (or group)
#' @importFrom utils download.file unzip
#' @export
installCollectionData <- function(dataDirectory=getOption("qd.datadir"),branchOrCommit="master",repository="metadata",baseUrl="https://github.com/quadrama/") {
  tf <- tempfile()
  utils::download.file(paste0(baseUrl,repository,"/archive/",branchOrCommit,".zip"), destfile=tf)
  utils::unzip(tf,exdir=dataDirectory)
  repoDirectory = file.path(dataDirectory,paste(repository,branchOrCommit,sep="-"))
  unlink(file.path(dataDirectory,"collections"))
  file.rename(from=file.path(repoDirectory,"collections"),to=file.path(dataDirectory,"collections"))
  unlink(repoDirectory,recursive = TRUE)
}