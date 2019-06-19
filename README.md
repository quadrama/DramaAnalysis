[![release](https://img.shields.io/badge/release-2.0.1-blue.svg)](https://github.com/quadrama/DramaAnalysis/releases/tag/v2.0.1)
[![Build Status](https://travis-ci.org/quadrama/DramaAnalysis.svg?branch=master)](https://travis-ci.org/quadrama/DramaAnalysis)
[![DOI](https://zenodo.org/badge/64286398.svg)](https://zenodo.org/badge/latestdoi/64286398)
[![license](https://img.shields.io/badge/license-GPL%20v3-blue.svg)](LICENSE) 
[![Join the chat at https://gitter.im/quadrama/DramaAnalysis](https://badges.gitter.im/quadrama/DramaAnalysis.svg)](https://gitter.im/quadrama/DramaAnalysis?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

# DramaAnalysis with R

This package contains a number of functions to support the analyis of dramatic texts. 

## Features
- Summary statistics about the number of words and utterances per character
- Statistics about word use from a dictionary per character
- Support for stylometric analyses by character or text
- Generation of a report for a dramatic text
- Reads in preprocessed texts 

# Requirements
DramaAnalysis works on Mac OS X, Windows 10 and Linuxes, and requires R 3.3 or later.


# Installation
```R
# this is only necessary once per system
install.packages("devtools") 
library(devtools)

# Install newest stable version
install_github("quadrama/DramaAnalysis") 

# Install newest development version
install_github("quadrama/DramaAnalysis", ref="develop/3.x")

# Install specific version (3.0.0)
install_github("quadrama/DramaAnalysis", ref="v3.0.0") 
```

# Usage and Howto
Please see the [wiki](https://github.com/quadrama/DramaAnalysis/wiki).

