# DramaAnalysis with R

This package contains a number of functions that work with the QuaDramA web service.

## Download and Installation
Please check [this page](https://github.com/quadrama/DramaAnalysis/releases).

## Usage and Howto
Please see the [wiki](quadrama/DramaAnalysis/wiki)

## Release workflow
- Make a new branch for the release (e.g. `release/1.0.0`)
- Set all version numbers to the correct version
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
