# function grepcolumns correctly processes regex patterns starting with '^' or ending with '$'
grepcolumns <- function(pattern, d) {
  # indicate columns with matches
  m <- mapply(function(x) length(grep(pattern, x)) > 0, d)
  # pattern found?
  grep(TRUE, m)
}

#' @rdname df_pattern_subset
#'
#' @param d a \code{data.frame}
#' @param pattern a \code{regex} pattern
#' @param strategy character specifying if upmost left ("ul") or lowermost right ("lr") occurrence
#' is to be sought.
#'
#' @return \code{df_pattern_locate} returns an integer vector of length 2 (i.e. a tuple)
#' specifying row and column number, or \code{c(NA, NA)} if no occurrence of \code{pattern} found
#' @export
#'
df_pattern_locate <- function(d, pattern, strategy = c("ul", "lr")) {
  # is the upper left or lower right corner to be located?
  strategy = match.arg(strategy)

  # grep applied to a data.frame checks if the pattern is present in a column!
  columnswithpattern = grepcolumns(pattern, d)

  # if length == 0, the pattern is not to be found within d
  if (length(columnswithpattern) == 0) {
    message(paste0("Pattern `", pattern, "` not found!"))
    return(c(NA_integer_, NA_integer_))
  }

  # colwithp is a scalar containing the column number corresponding to the pattern and strategy
  colwithp <- ifelse(strategy == "ul", min(columnswithpattern), max(columnswithpattern))

  # rowswithpattern is generated from column colwithp (as this one contains the appropriate pattern)
  rowswithpattern <- grep(pattern, pull(d, colwithp))
  # rowwithp is a scalar containing the row number corresponding to the pattern and strategy
  rowwithp <- ifelse(strategy == "ul", min(rowswithpattern), max(rowswithpattern))

  # return the result as a tuple
  c(rowwithp, colwithp)
}

#' @name df_pattern_subset
#' @title Subset a data frame given two patterns
#'
#' @description \code{df_pattern_locate} locates the up- and leftmost or lower- and rightmost occurrence of a
#' given pattern within a given \code{data.frame}.
#'
#' \code{df_pattern_subset} returns a subset of a \code{data.frame} given two patterns, which correspond to
#' the upper left and lower right corner of the subset.
#'
#' @param d a \code{data.frame}
#' @param pattern_ul \code{regex} pattern specifying upper left corner
#' @param pattern_lr \code{regex} pattern specifying lower left corner
#' @param ignore_columns logical; if \code{TRUE}, filtering of columns is omitted, irrespective of columns where patterns were found
#' @param ignore_rows logical; if \code{TRUE}, filtering of rows is omitted, irrespective of rows where patterns were found
#' @param which_header character, specifying if a new header based on the row above ("row above", default) the upper left corner
#' or if old table headers ("table header") be used
#' @param adjust_ul integer vector of length 2 specifying addends to row and column to determine upper left corner
#' @param adjust_lr integer vector of length 2 specifying addends to row and column to determine lower right corner
#'
#' @return \code{df_pattern_subset} returns a new \code{data.frame} extending from the upper left to the
#' lower right corner previously located (using \code{pattern_ul} and \code{pattern_lr}, respectively).
#' @export
#'
#' @examples
#' # # load fantasy data from an excel file (generation of data see below):
#' # # fantasy <- read_xlsx(paste0(system.file("extdata", package = "kungfu"), "/fnts.xlsx"))
#' # fantasy_subset <- df_pattern_subset(fnts, "43", "^dun.*dun$")
#' #
#' # # locate pattern
#' # df_pattern_locate(fnts, "dunbumm", "ul")
#' # # returns c(3, 2)
#' #
#' # # generate fantasy excel file fnts.xlxs
#' # library(openxlsx)
#' # syllables <- c("arg", "ton", "hun", "kon", "dun", "bumm", "samm", "kur", "dem", "schim", "to")
#' # word <- function(s = syllables, range = c(1,3), clps = "") {
#' #   paste(sample(s, round(runif(1, min(range), max(range))), replace = TRUE), collapse = clps)
#' # }
#' # words <- function(n, ...) mapply(function(x) word(...), 1:n)
#' # set.seed(1)
#' # fnts <- data.frame(id = sample(1:200, 100),
#' #                    bumka = words(100),
#' #                    humka = words(100, range = c(3,6), clps = " "),
#' #                    group = sample(LETTERS, 100, replace = TRUE),
#' #                    groko = words(100, range = c(2,4), clps = ", "))
#' #
#' # write.xlsx(fnts, "../inst/extdata/fnts.xlsx", overwrite = TRUE)

df_pattern_subset <- function(d,
                         pattern_ul = NULL,
                         pattern_lr = NULL,
                         ignore_columns = FALSE,
                         ignore_rows = FALSE,
                         which_header = c("row above", "table header"),
                         adjust_ul = c(0, 0),
                         adjust_lr = c(0, 0)) {
  # validate input
  if (is.null(pattern_ul) & is.null(pattern_lr)) stop("Please provide either 'pattern_ul' or 'pattern_lr'!")

  which_header <- match.arg(which_header)

  # initialize locations of upper left and lower rigth corners
  location_ul <- c(NA, NA)
  location_lr <- c(NA, NA)

  # locate new corners according to pattern_ul and pattern_lr
  if (!is.null(pattern_ul)) location_ul = df_pattern_locate(d, pattern_ul, strategy = "ul")
  if (!is.null(pattern_lr)) location_lr = df_pattern_locate(d, pattern_lr, strategy = "lr")

  # "ignore columns" means: erase information pertaining to column selection, i.e. have all columns returned
  if (ignore_columns) {
    location_ul[2] <- NA # column no. upper left corner
    location_lr[2] <- NA # column no. lower right corner
  }

  # "ignore rows" means: erase information pertaining to row selection, i.e. have all rows returned
  if (ignore_rows) {
    location_ul[1] <- NA # row no. upper left corner
    location_lr[1] <- NA # row no. lower right corner
  }

  # adjust, if so specified
  location_ul <- location_ul + adjust_ul
  location_lr <- location_lr + adjust_lr

  # create indices
  index_rows <- rep(TRUE, nrow(d))
  index_cols <- rep(TRUE, ncol(d))

  # trim data.frame row- and columnwise
  if (!is.na(location_ul[1])) { # row no. of upper left corner
    if (location_ul[1] > 1 & location_ul[1] <= nrow(d))
      index_rows[1:(location_ul[1] - 1)] <- FALSE
  }
  if (!is.na(location_ul[2])) { # col no. of upper left corner
    if (location_ul[2] > 1 & location_ul[2] <= ncol(d))
      index_cols[1:(location_ul[2] - 1)] <- FALSE
  }
  if (!is.na(location_lr[1])) { # row no. of lower left cornder
    if (location_lr[1] > 0 & location_lr[1] < nrow(d))
      index_rows[(location_lr[1] + 1):nrow(d)] <- FALSE
  }
  if (!is.na(location_lr[2])) { # col no. of lower left corner
    if (location_lr[2] > 0 & location_lr[2] < ncol(d))
      index_cols[(location_lr[2] + 1):ncol(d)] <- FALSE
  }

  # generate first subset ss1
  ss1 <- d[index_rows, index_cols] # ss1 lacks header adjustment
  ss <- ss1

  # if no row above available, or rows are ignored (i.e. all rows selected), take table header
  if (!is.na(location_ul[1])) {
    if (location_ul[1] < 2) which_header = "table header" # no rows above available!
  } else {
    which_header = "table header" # location_ul[1] NA!
  }

  # adjust header if specified (and if possible)
  if (which_header == "row above") {
    colnames(ss) <- d[location_ul[1] - 1, index_cols]
  }

  # return final data.frame
  ss
}
