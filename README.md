
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pkgtemplate

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Travis build
status](https://travis-ci.com/poissonconsulting/pkgtemplate.svg?branch=master)](https://travis-ci.com/poissonconsulting/pkgtemplate)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/poissonconsulting/pkgtemplate?branch=master&svg=true)](https://ci.appveyor.com/project/poissonconsulting/pkgtemplate)
[![Codecov test
coverage](https://codecov.io/gh/poissonconsulting/pkgtemplate/branch/master/graph/badge.svg)](https://codecov.io/gh/poissonconsulting/pkgtemplate?branch=master)
[![License:
MIT](https://img.shields.io/badge/License-MIT-green.svg)](https://opensource.org/licenses/MIT)
[![CRAN
status](https://www.r-pkg.org/badges/version/pkgtemplate)](https://cran.r-project.org/package=pkgtemplate)
![CRAN downloads](http://cranlogs.r-pkg.org/badges/pkgtemplate)
<!-- badges: end -->

pkgtemplate provides a template for a new R package.

## Instructions

In order to create a new package the user should

1)  Go to the pkgtemplate [GitHub
    repository](https://github.com/poissonconsulting/pkgtemplate) and
    choose ‘Use this template’.
2)  Clone the new repository and replace ‘pkgtemplate’ with the name of
    the new package in `DESCRIPTION`, `NEWS.md`, `tests/testthat.R` and
    this `README.Rmd` file.
3)  `devtools::check()` the package and fix any Errors, Warnings or
    Notes.
4)  Knit this `README.Rmd` file and `pkgdown::build_site()`.
5)  Add the project to
    [Travis](https://www.travis-ci.com/poissonconsulting/pkgtemplate)
    and in the
    [Settings](https://www.travis-ci.com/poissonconsulting/pkgtemplate/settings)
    add a Cron Job to run the master branch daily if there hasn’t been a
    build in the last 24h.
6)  Add the project to
    [Appveyor](https://ci.appveyor.com/project/poissonconsulting/pkgtemplate).
7)  Rename the `pkgtemplate.Rproj` file.
8)  Push the changes to the new repository.
9)  Go to the repository GitHub
    [settings](https://github.com/poissonconsulting/pkgtemplate/settings)
    and set the GitHub Pages Source to be the master branch /docs
    folder.
10) Edit the GitHub repository
    [description](https://github.com/poissonconsulting/pkgtemplate/) and
    set the website to be
    <https://poissonconsulting.github.io/pkgtemplate/>.

## Installation

You can install the latest development version of pkgtemplate from
[GitHub](https://github.com/poissonconsulting/pkgtemplate) with:

``` r
# install.packages("remotes")
remotes::install_github("poissonconsulting/pkgtemplate")
```

## Contribution

Please report any
[issues](https://github.com/poissonconsulting/pkgtemplate/issues).

[Pull requests](https://github.com/poissonconsulting/pkgtemplate/pulls)
are always welcome.

Please note that the ‘pkgtemplate’ project is released with a
[Contributor Code of
Conduct](https://poissonconsulting.github.io/pkgtemplate/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
