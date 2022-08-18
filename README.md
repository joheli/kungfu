[![R-CMD-check](https://github.com/joheli/kungfu/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/joheli/kungfu/actions/workflows/R-CMD-check.yaml)
# kungfu

This R-package contains functions that I use for the cleaning and transforming of "dirty" data. 

## Installation

Please install package [remotes](https://cran.r-project.org/web/packages/remotes/index.html). After successful installation, type `remotes::install_github("joheli/kungfu")`. Alternatively, download the most recent tagged compressed package from [tags](https://github.com/joheli/kungfu/tags) and install from the R command line by typing `install.packages(vX.X.X.tar.gz, repos = NULL, type ="source")` (where `vX.X.X` is to be replaced by the most recent version).

## Contents

Presently, the package contains the following functions (in alphabetic order):

  - `cleaner`: removes duplicates in a `data.frame`
  - `df_pattern_subset`: subsets a `data.frame` given two regex patterns marking the upper left and lower right corners of the returned `data.frame`.
  - `dfilter`: filters a vector of type *numeric*, *integer*, *Date*, or *POSIXt*, only allowing entries with distances less than a user-specified value to each other
  - `pattern_join`: joins two tables according to a regex pattern; it is similar to function `regex_join` in package [fuzzyjoin](https://github.com/dgrtwo/fuzzyjoin), which I discovered only after writing `pattern_join`
  - `postgresql_uploader`: uploads a `data.frame` into an existing PostgreSQL table
  - `rbinder`: function for importing and joining of multiple csv-like files with identical headers
  - `seamless`: converts a table of intervals into a "seamless" succession of intervals
  
## Help

Please use `help(*function*)` or `?*function*` to access the help pages of above functions after installation.



