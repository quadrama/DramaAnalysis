## Resubmission 

This is a resubmission. In this version we have:

* Added documentation on return values of all functions

* Replaced \dontrun{} by \donttest{} in the examples

* Removed examples for unexported functions

* Fixed several bugs in the function loadDramaTEI()

* R CMD check reports one doi that currently can't 
  be resolved, because the German computer science
  association (GI) is currently restructuring their
  web page (http://dx.doi.org/10.18420/in2017_119).

## Resubmission

This is a resubmission. In this version I have:

* Added a reference paper to the Description

* Replaced the LICENSE link in README.md by an actual link

* Added CRAN-based installation instruction to README.md

## Test environments
* local OS X 10.14.5 install, R 3.6.0
* ubuntu 14.04 (on travis-ci), R 3.3.3, R 3.4.4, oldrel, release, devel
* OS X 10.13.3 (on travis-ci), R 3.3.3, oldrel, release
* win-builder (release, oldrel, devel)

## R CMD check results
There were no ERRORs or WARNINGs.

There was 1 NOTE:
* checking CRAN incoming feasibility ... NOTE
  Maintainer: ‘Nils Reiter <nils.reiter@ims.uni-stuttgart.de>’
  New submission
    
  The package has been developed and used outside of CRAN for two years now,
  this is the first CRAN submission.

## Downstream dependencies
There are currently no downstream dependencies for this package.