## Git branching scheme

We follow the scheme described [here](http://nvie.com/posts/a-successful-git-branching-model/), which can be summarised like this:

- `master`: only stable, released code
- `develop`: latest updates, new features integrated here

Three additional *types* of branches are used:

- `release`: to prepare a release. Branches off of `develop`, do the final things, merge into `master`.
- `feature`: Should be prefixed with `feature/` and ideally correspond to an issue id. Branches off of `develop` and is integrated into `develop`
- `fix`: For bug fixes
