[![R-CMD-check](https://github.com/joheli/kungfu/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/joheli/kungfu/actions/workflows/R-CMD-check.yaml)
# kungfu

This R-package contains functions that I use for the cleaning and transforming of "dirty" data. 

## Install

Please install package [devtools](https://cran.r-project.org/web/packages/devtools/index.html). After successful installation type `devtools::install_github("joheli/kungfu")`. Alternatively, download the most recent tagged compressed package from [tags](https://github.com/joheli/kungfu/tags) and install from the R CLI by typing `install.packages(vX.X.X.tar.gz, repos = NULL, type ="source")` (where X.X.X is to be replaced by the version).

## Contents

Presently, the package contains the following functions:
  -  `pattern_join`: joins two tables according to a regex pattern; it is similar, and probably inferior to function `regex_join` in package [fuzzyjoin](https://github.com/dgrtwo/fuzzyjoin), which I discovered only after writing `pattern_join`
  - `postgresql_uploader`: helps with the uploading of tables to a PostgreSQL database
  - `rbinder`: convenience function for importing and joining of multiple csv-like files that have identical headers
  - `seamless`: attempts to convert a table of intervals into a "seamless" succession of intervals
  - `dfilter`: distance filter - filters a vector of type *numeric*, *integer*, *Date*, or *POSIXt*, only allowing entries up to a given maximal distance.
  - `df_pattern_subset`: subsets a `data.frame` given two regex patterns which identify the upper left and lower right corner of the new `data.frame`.
  
## Help

Please use `help(*function*)` or `?*function*` to access the help pages of above functions after installation.



