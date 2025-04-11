This section provides additional details that are unnecessary for the general user. That being said, it will not contain all of the high level details that are outlined in the [User Documentation](#user-documentation).

This document is about how to contribute to the LMChla repository. This process looks a bit different depending on whether you’re a member of the public or a member of the development team. Here’s what you can find in this doc:

* [Environment and style](#environment-and-style)
* [Public contributions](#Contributions)
    - [Branches](#branches)
* [Public domain](#public-domain)

No matter who you are, if you spot an error, omission, or bug, you're welcome to open an issue in this repo!

# Environment and style
The development environment is managed via "renv". To set up the development environment after cloning the repository run "renv::restore()". This will then ask to install and update packages necesssary for the development environment. If you make changes that alter the environment (i.e. add a new package dependency) please update the renv by running "renv::snapshot()"

For scripts/function organization and style see the [Google r style guide](https://google.github.io/styleguide/Rguide.html). 
For package layout see the [R-exts](https://cran.r-project.org/doc/manuals/R-exts.pdf)
For function documentation [roxygen notes](https://roxygen2.r-lib.org/articles/rd.html)

Lint the project files using R linter package (we are using the default linting choices which enforces tidyverse standards)
```{r}
lintr::lint(filename = "R/filename.R") 
lintr::lint_dir(path = "R")
```

Tags in code are supported to make it easier to search when searching for things to do etc: 
- BUG - if a bug is known to occur from a certain chunk of code
- [ ] - For TODO's
- [x] - to mark that necessary changes are done
- DOCTHIS - highlight sections of code that are important to write up in the documentation for the package
- XXX - Something else of note for developers

# Testing
In order to cut down time tests take to run, tests for a given data source should all be run together. To accomplish this, the data should be stored as a fixture and then tested. Tests are ran through ['testthat' R package](https://testthat.r-lib.org/). Code coverage is provided by [covr](https://covr.r-lib.org/).

## Contributions
We're so glad you're thinking about contributing to an LMChla! If you're unsure about anything, just (see contacts in the [description file](DESCRIPTION.md)) with your question — or submit the issue or pull request anyway. The worst that can happen is you'll be politely asked to change something. We love all friendly contributions.

We encourage you to read this project's CONTRIBUTING policy (you are here), its [LICENSE](LICENSE.md), and its [README](README.md).

* If you see an error or have feedback, the best way to let us know is to file an issue.

## Contributions

There is a team actively working on the site. You can find us in Slack in the #18f-site or #beta-18F-site channels (access is limited to 18F employees).

### Branches

Any team member should be able to make a branch of the site and submit a pull request. 

* Submit **documentation edits** as pull requests to the `master` branch.
* Submit **new design work, content changes, and features** as pull requests with branch names which directly correspond to the issue from which they were spawned.


## Public domain

For detailed license information, see [LICENSE](LICENSE.md).

All contributions to this project will be released under the CC0 dedication. By submitting a pull request, you are agreeing to comply with this waiver of copyright interest.