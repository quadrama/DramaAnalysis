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

#' @title Download preprocessed drama data
#' @description This function downloads pre-processed dramatic texts via http and stores them locally in your data directory
#' @param dataSource Currently, only "tg" (textgrid) is supported
#' @param dataDirectory The directory in which the data is to be stored
#' @param downloadSource No longer used.
#' @param removeZipFile No longer used.
#' @param baseUrl The remote repository owner (e.g., https://github.com/quadrama)
#' @param remoteUrl The URL of the remote repository.
#' @importFrom git2r clone pull in_repository repository
#' @export
installData <- function(dataSource="tg", 
                        dataDirectory=getOption("qd.datadir"),
                        downloadSource="ims", 
                        removeZipFile = TRUE,
                        baseUrl = "https://github.com/quadrama",
                        remoteUrl = paste0(baseUrl,"/data_",dataSource,".git")) {
  dir.create(dataDirectory, recursive = TRUE, showWarnings = FALSE)
  localDirectory <- file.path(dataDirectory, dataSource)
  sourceFilename <- paste(dataSource, "git", sep=".")


  if (dir.exists(localDirectory) && git2r::in_repository(localDirectory)) {
    repo <- git2r::repository(localDirectory)
    message("Pulling new data from ", remoteUrl, ".")
    git2r::pull(repo)
  } else {
    message("Cloning ", remoteUrl, ".")
    git2r::clone(remoteUrl,localDirectory)
  }
}




#' @importFrom utils read.csv
getInstalledDate <- function(dataDirectory=getOption("qd.datadir"),filename) {
  versionsFilename <- file.path(dataDirectory,"versions.csv")
  if (file.exists(versionsFilename)) {
    versions <- utils::read.csv(versionsFilename,stringsAsFactors = FALSE)
    v <- versions[versions$file == filename,2]
    if (length(v)>0) {
      as.Date(v)
    } else {
      NA
    }
  } else {
    NA
  }
}

#' @importFrom utils write.csv read.csv
saveInstalledDate <- function(dataDirectory, filename, date) {
  versionsFilename <- file.path(dataDirectory,"versions.csv")
  if (file.exists(versionsFilename)) {
    versions <- utils::read.csv(versionsFilename,stringsAsFactors = FALSE)
    if (length(versions[versions$file==filename,"date"])>0) {
      versions[versions$file==filename,"date"] <- format(date)
    } else {
      versions[nrow(versions) + 1,] = c(filename,format(date))
    }
  } else {
    versions <- data.frame(file=c(filename),date=c(format(date)))
  }
  utils::write.csv(versions,file=versionsFilename,row.names=FALSE)
  
}

#' @importFrom httr HEAD headers
lastModifiedDate <- function(url) {
  h <- httr::HEAD(url)
  lm <- httr::headers(h)$`last-modified`
  as.Date(lm, "%a, %d %b %Y %H:%M:%S")
}

createIMSUrl <- function(filename) {
  paste0("https://www2.ims.uni-stuttgart.de/gcl/reiterns/quadrama/res/",filename)
}

createZenodoUrl <- function(id,filename) {
  paste0("https://zenodo.org/record/",id,"/files/",filename)
}
