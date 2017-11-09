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
                              branchOrCommit,".csv.zip"), 
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
#' @param downloadSource The server from which to download
#' @param removeZipFile If true (the default), the downloaded zip file is removed after unpacking
#' @importFrom utils download.file unzip
#' @export
installData <- function(dataSource="tg", dataDirectory=getOption("qd.datadir"),downloadSource="ims", removeZipFile = TRUE) {
  dir.create(dataDirectory, recursive = TRUE, showWarnings = FALSE) 
  sourceFilename <- switch(dataSource,
                           tg="tg.zip",
                           gdc="gdc.zip",
                           tc="tc.zip",
                           gbd="gbd.zip")
  
  if (downloadSource == "ims") {
    sourceUrl <- createIMSUrl(sourceFilename)
  } else if (downloadSource == "zenodo") {
    sourceUrl <- createZenodoUrl(803280, sourceFilename)
  }
  lm <- lastModifiedDate(sourceUrl)
  message("Version on server: ", format(lm))
  installedV <- getInstalledDate(dataDirectory,sourceFilename)
  
  message("Locally installed version: ", format(installedV))
  
  
  if (is.na(installedV) | installedV < lm) {
    message("Downloading new version.")
    tf <- tempfile()
    utils::download.file(sourceUrl,destfile = tf)
    utils::unzip(tf,exdir=file.path(dataDirectory,"xmi"))
    if (removeZipFile == TRUE) {
      file.remove(tf)
    }
    saveInstalledDate(dataDirectory, sourceFilename, lm)
  } else {
    message("No download necessary.")
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
