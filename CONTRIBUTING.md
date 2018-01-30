## Git branching scheme

We follow the scheme described [here](http://nvie.com/posts/a-successful-git-branching-model/), which can be summarised like this:

- `master`: only stable, released code
- `develop`: latest updates, new features integrated here

Three additional *types* of branches are used:

- `release`: to prepare a release. Branches off of `develop`, do the final things, merge into `master`.
- `feature`: Should be prefixed with `feature/` and ideally correspond to an issue id. Branches off of `develop` and is integrated into `develop`
- `fix`: For bug fixes

## Release workflow
- Make a new branch for the release (e.g. `release/1.0.0`)
- Set all version numbers to the correct version
  - `DESCRIPTION` file for the R package
  - `README.md` for badge
- Create vignettes by running `devtools::build_vignettes()` in R
- Check that the R package is well: `make check`
- Make a source package by running `make build`
- If all works well, merge the branch into master, tag it with `v1.0.0`
- Create a new release on the [github release page](https://github.com/quadrama/DramaAnalysis/releases)
- Copy the following installations instructions in release note (with fixed version numbers)
  ```R
  install_github("quadrama/DramaAnalysis", ref="v2.0.0") 
  ```
- Add the PDF documentation for the R package to the release page
- Post the updated vignettes on the wiki: `make wiki`
