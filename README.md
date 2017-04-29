[![Build Status](https://travis-ci.org/quadrama/DramaAnalysis.svg?branch=master)](https://travis-ci.org/quadrama/DramaAnalysis)
[![DOI](https://zenodo.org/badge/64286398.svg)](https://zenodo.org/badge/latestdoi/64286398)
[![license](https://img.shields.io/badge/license-Apache%202-blue.svg)](LICENSE)

# DramaAnalysis with R

This package contains a number of functions that work with the QuaDramA web service.

## Download and Installation
Please check [this page](https://github.com/quadrama/DramaAnalysis/releases).

## Usage and Howto
Please see the [wiki](https://github.com/quadrama/DramaAnalysis/wiki)

## Release workflow
- Make a new branch for the release (e.g. `release/1.0.0`)
- Set all version numbers to the correct version
  - `DESCRIPTION` file for the R package
  - `pom.xml` for the Java code
- Compile the Java code by running `mvn -f java/pom.xml clean package`
- Go one directory up `cd ..`
- Check that the R package is well: `R CMD check DramaAnalysis`
- Make a source package by running `R CMD build DramaAnalysis`
- If all works well, merge the branch into master, tag it with `v1.0.0`
- Create a new release on the [github release page](https://github.com/quadrama/DramaAnalysis/releases)
- Upload the source package
- Copy the following installations instructions in release note (with fixed version numbers)
   ```R
   install.packages("https://github.com/quadrama/DramaAnalysis/releases/download/v1.0.0/DramaAnalysis_1.0.0.tar.gz",
   repos=NULL, type="source")
   ```
- Add the PDF documentation for the R package to the release page
- Update zenodo-DOI in README.md on master branch
