[![release](https://img.shields.io/badge/release-1.2.0-blue.svg)](https://github.com/quadrama/DramaAnalysis/releases/tag/v1.2.0)
[![Build Status](https://travis-ci.org/quadrama/DramaAnalysis.svg?branch=master)](https://travis-ci.org/quadrama/DramaAnalysis)
[![DOI](https://zenodo.org/badge/64286398.svg)](https://zenodo.org/badge/latestdoi/64286398)
[![license](https://img.shields.io/badge/license-Apache%202-blue.svg)](LICENSE)

# DramaAnalysis with R

This package contains a number of functions to support the analyis of dramatic texts. 

## Features
- Summary statistics about the number of words and utterances per figure
- Statistics about word use from a dictionary per figure
- Support for stylometric analyses by figure or text
- Generation of a report for a dramatic text
- Reads in preprocessed texts 

# Requirements
DramaAnalysis *should* work on Mac OS X, Windows 10 and Linuxes, but the JVM needs a bit of memory, so we assume to be working on the **64bit** variants.

- R (64 bit)
- Java
- [rJava](https://cran.r-project.org/web/packages/rJava/index.html)
   - Please make sure rJava finds the correct Java installation on your machine. Windows: [This answer](http://stackoverflow.com/a/7604469) worked when testing.

# Updating
In some cases, the old java code is not replaced if you just install the new version. Please restart R *before* and *do not* run `setup()` before installation.

# Installation
```R
install.packages("https://github.com/quadrama/DramaAnalysis/releases/download/v1.2.0/DramaAnalysis_1.2.0.tar.gz",
   repos=NULL, type="source", INSTALL_opts="--no-multiarch")
```

## Usage and Howto
Please see the [wiki](https://github.com/quadrama/DramaAnalysis/wiki)

