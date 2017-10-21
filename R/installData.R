#' @title Download and install collection data
#' @description Function to download collection data (grouped texts) from github. 
#' Overwrites (!) the current collections.
#' @param dataDirectory The data directory in which collection and data files are stored
#' @param branchOrCommit The git branch, commit id, or tag that we want to download
#' @param repository The repository
#' @param baseUrl The github user (or group)
#' @importFrom utils download.file unzip
#' @export
installCollectionData <- function(dataDirectory=getOption("qd.datadir"),
                                  branchOrCommit="master",
                                  repository="metadata",
                                  baseUrl="https://github.com/quadrama/") {
  # create temporary file to download
  tf <- tempfile()
  
  # download repo as zip file
  utils::download.file(paste0(baseUrl,
                              repository,
                              "/archive/",
                              branchOrCommit,".zip"), 
                       destfile=tf)
  
  # unzip for temp file
  utils::unzip(tf,exdir=dataDirectory)
  
  repoDirectory = file.path(dataDirectory, 
                            paste(repository, branchOrCommit,sep="-"))
  # remove old collections-directory
  unlink(file.path(dataDirectory,"collections"))
  
  # move new dir into place
  file.rename(from=file.path(repoDirectory,"collections"),
              to=getOption("qd.collectionDirectory"))
  
  # remove repo directory
  unlink(repoDirectory,recursive = TRUE)
  
  # remove temp file
  unlink(tf)
}