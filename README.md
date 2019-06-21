[![release](https://img.shields.io/badge/release-3.0.0-blue.svg)](https://github.com/quadrama/DramaAnalysis/releases/tag/v3.0.0)
[![Build Status](https://travis-ci.org/quadrama/DramaAnalysis.svg?branch=master)](https://travis-ci.org/quadrama/DramaAnalysis)
[![DOI](https://zenodo.org/badge/64286398.svg)](https://zenodo.org/badge/latestdoi/64286398)
[![license](https://img.shields.io/badge/license-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html) 
[![Join the chat at https://gitter.im/quadrama/DramaAnalysis](https://badges.gitter.im/quadrama/DramaAnalysis.svg)](https://gitter.im/quadrama/DramaAnalysis?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

# DramaAnalysis with R

This package contains a number of functions to support the analysis of dramatic texts. 

## Features
- Summary statistics about the number of words and utterances per character
- Statistics about word use from a dictionary per character
- Support for stylometric analyses by character or text
- Generation of a report for a dramatic text
- Reads in preprocessed texts 

## Example

```r
# loads a specific, pre-packaged play
data(rksp.0)

# calculates standard stats about a character
charStat <- characterStatistics(rksp.0)

# fix character names
charStat <- characterNames(charStat, rksp.0)

# plot them as a bar plot
barplot(charStat)
```

# Requirements
DramaAnalysis works on Mac OS X, Windows 10 and Linuxes, and requires R 3.3 or later.


# Installation
```R
# installation from CRAN
install.packages("DramaAnalysis")

# Install newest development version, requires devtools to be installed
devtools::install_github("quadrama/DramaAnalysis", ref="develop/3.x")
```

# Usage and Howto
Please refer to the [tutorial](https://quadrama.github.io/DramaAnalysis/tutorial/3/).

