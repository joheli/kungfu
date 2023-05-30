## Helper functions

# rescale
rescale <- function(x, min = 0, max = 1) {
  if (all(diff(x) == 0)) { # if all values are the same ...
    rep(0, length(x))      # ... return vector of zeroes
  } else {
    # difference between max and min
    mmd <- max - min
    result0 <- (x - min(x))/(max(x)-min(x))
    result <- result0
    if (mmd != 1) result = result0 * mmd
    if (min != 0) result = result + min
    return(result)
  }
}

# rescale value from 0 to 1
rescale01 <- function(x) rescale(x, min = 0, max = 1)

# rescale value from 1 to 0
rescale10 <- function(x) 1 - rescale01(x)

# massage optionally rescaled value by condensing values closer to maximal (pull up) or minimal (push down) value
rescale_modify <- function(x, modification = c("none", "pull up", "push down")) {
  # record min and max of x
  min.x <- min(x)
  max.x <- max(x)
  x01 <- x
  x.rescale <- FALSE
  # check if argument between 0 and 1
  if (min.x < 0 | max.x > 1) {
    x.rescale <- TRUE
    x01 <- rescale(x, min = 0, max = 1)
  }
  # if all values the same, return x unaltered
  if (all(diff(x) == 0)) return(x)
  # define helper function 'massage' to deal with 0 and 1
  massage <- function(x) {
    # define arbitrary small number
    s <- min(abs(diff(unique(x01))))/1e6
    # replace zeroes with s
    x[x == 0] <- s
    # replace ones with 1-s
    x[x == 1] <- 1 - s
    # return
    x
  }
  # which modification was requested?
  md <- match.arg(modification)
  # return based on md (i.e. modification requested)
  result0 <- switch (md,
                     none = x,
                     `pull up` = rescale01(log(massage(x01))),
                     `push down` = rescale01(-log(1 - massage(x01))))
  # if necessary, rescale
  result <- result0
  if (x.rescale) result <- rescale(result0, min = min.x, max = max.x)
  # return result
  return(result)
}

# custom rescale functions used by pattern_join below
rescale_adist <- function(x) rescale_modify(rescale10(x), "push down")
rescale_pattern_complexity <- function(x) rescale(x, min = 0.01, max = 1)

# function matchLen determines match lengths between pattern and target
matchLen <- function(pattern, target) {
  rgx <- regexpr(pattern, target)
  result0 <- attr(rgx, "match.length")
  result <- ifelse(result0 == -1, 0, result0) # replace -1 by 0
  return(result)
}

# function matchLenMatrix determines match lengths between patterns and targets and puts them in a matrix
matchLenMatrix <- function(patterns, targets) {
  mp <- purrr::map(targets, function(y) purrr::map(patterns, function(x) matchLen(x, y)))
  matrix(unlist(mp), nrow = length(patterns))
}

# function matchLenMatrix determines match lengths between patterns and targets and puts them in a matrix
matchLenMatrix2 <- function(patterns, targets) {
  x <- expand.grid(patterns = patterns, targets = targets)
  mp <- purrr::map2(x$patterns, x$targets, function(x, y) matchLen(x, y))
  matrix(unlist(mp), nrow = length(patterns))
}

# split large data.frame into parts using dplyr::group_split
splitter <- function(d, maxrows = 1000) {
  parts <- ceiling(nrow(d)/maxrows)
  # to avoid errors of type "Undefined global functions or variables", set variables to NULL
  gr <- NULL
  g <- d %>% mutate(gr = rep_len(1:parts, length.out = nrow(d))) %>%
    group_by(gr)
  group_split(g, .keep = FALSE)
}

# joiner
# function that does the actual joining based on x1 and x2
#     IMPORTANT:  x1 represents the dirty raw data,
#                 x2 represents the reference (e.g. pattern, dictionary, etc.)
# arguments
#     x_part  represents a subset of x (table 1) with identical column names as x
#     y       table 2 containing x2
#     f_x     field name (column name) of table x_part passed from "by" in joining process
#     x2      extracted field form table 2 used for joining
#     matcher a function returning a matrix of a custom matching metric between
#             x1 (field from table 1) and x2 (field from table 2)
joiner <- function(x_part, y, f_x, x2, matcher) {
  # extract field x1 (i.e. the "realworld sample")
  x1 <- pull(x_part, f_x) %>% as.character

  # replace non-breaking whitespace with regular space
  x1 <- gsub("\U00A0", " ", x1)

  # process x1 and x2; x2 ends up in rows, x1 in columns
  matches <- matcher(x1, x2)

  # assign integer column names (colums represent x1, that's why it's 1 to nrow(x_part))
  colnames(matches) <- as.character(1:nrow(x_part))

  # to avoid errors of type "Undefined global functions or variables", set variables to NULL
  uniquerownumber <- uniquepatternnumber <- matching_metric <- n <- pattern <- NULL

  # convert matrix to data.frame
  matches_d <- matches %>%
    as.data.frame %>%
    mutate(unique_x2_number = as.character(1:nrow(y))) %>%
    tidyr::pivot_longer(-unique_x2_number, # convert to long format
                        names_to = "unique_x1_number",
                        values_to = "matching_metric") %>%
    group_by(unique_x1_number) %>%
    slice_max(matching_metric, n = 1, with_ties = FALSE) %>% # do not allow more than one match
    arrange(as.numeric(unique_x1_number)) %>%
    inner_join(mutate(x_part, unique_x1_number = as.character(1:n())), by = "unique_x1_number") %>% # inner_join to x_part
    inner_join(mutate(y, unique_x2_number = as.character(1:n())), by = "unique_x2_number") %>% # inner_join to y
    ungroup() %>%
    select(-unique_x1_number, -unique_x2_number) # get rid of temporary columns

  # return
  matches_d
}

# common join: a common function that is used by pattern_join and similarity_join
common_join <- function(x, y, by, nomatch_label = NA, nomatch_cutoff = 0.2,
                        x_split_cutoff = 500, multicore = TRUE, matcher = NULL) {
  # check arguments
  for (d in list(x, y)) if (!inherits(d, "data.frame")) stop("Please supply 'data.frame' objects for arguments 'x' and 'y'.")
  if (!inherits(by, "character") | length(by) != 1) stop("Please supply a character of length 1 as argument 'by'.")

  # extract field names specified by 'by'
  f_x <- f_y <- NULL
  if (is.null(names(by))) {
    f_x <- f_y <- by
  } else {
    f_x <- names(by)
    f_y <- by
  }

  # extract x2 (i.e. the "references")
  x2 <- pull(y, f_y) %>% as.character
  # replace non-breaking whitespace with regular space; failing to do may prevent successful matching.
  x2 <- gsub("\U00A0", " ", x2)

  # stop if no matcher provided
  if (is.null(matcher)) stop("Function 'matcher' was not provided!")

  # join function to be passed to lapply or pblapply below
  join <- function(x_part) joiner(x_part, y, f_x, x2, matcher)

  # split a big table 'x' into smaller ones to be processed in a parallel fashion
  x_split <- splitter(x, maxrows = x_split_cutoff)

  # if 'multicore' evaluates to TRUE, use parallel processing
  if (!multicore) {
    # join
    result <- lapply(x_split, join)
  } else {
    # determine no. of cores
    # TODO: replace with parallelly::availableCores()?
    n.cores <- parallel::detectCores()

    # helper function for pbapply
    prl <- function(cl) pbapply::pblapply(X = x_split, FUN = join, cl = cl)

    # detect if OS is windows
    onwindows = grepl("windows", .Platform$OS.type, ignore.case = TRUE)

    # process differs by OS
    if (onwindows) {
      cl <- parallel::makeCluster(n.cores)
      result <- prl(cl = cl)
      parallel::stopCluster(cl)
    } else {
      ##  on other os 'cl' can be an integer:
      result <- prl(cl = n.cores)
    }
  }

  # rbind pieces together
  result0 <- Reduce(rbind, result, data.frame())
  # now remove those matches with unacceptably low matching metric
  #     first create an index
  index_below_nomatch_cutoff <- pull(result0, matching_metric) < nomatch_cutoff
  #     second set to 'nomatch_label' if below nomatch_cutoff
  result0[index_below_nomatch_cutoff, f_y] <- nomatch_label
  # finally remove matching metric from result
  result <- result0 %>% select(-matching_metric)
  #result <- result0
  # return result
  return(result)
}

# specific matchers
# functions that accept x1 and x2 (see joiner) and return a matrix with a specific matching metric
similarity_join_matcher <- function(x1, x2) stringdist::stringsimmatrix(a = x2, b = x1, method = "osa")

#' @rdname x_join
#'
#' @param nomatch_cutoff numeric between 0 and 1 specifying the similarity
#' (using metric \emph{optimal string alignment}, see \link[stringdist]{stringsim}) below which `nomatch_label` is
#' joined to orginal data (meaning: the entry is treated as a "no match")
#'
#' @export
#'
#' @examples
#' # test data
#' dirty <- data.frame(sample = 1:6, description = c("Bergerx", "Mueler", "Horsst", "Kinga", "Mannn", "Schneemann"))
#' reference <- data.frame(reference = c("Berger", "Mueller", "Horst", "King", "Mann", "Mustermann"))
#' # similarity_join with default nomatch_cutoff
#' dirty %>% similarity_join(reference, by = c("description" = "reference"))
#' # to avoid mapping "Schneemann" to "Mustermann", increase nomatch_cutoff (default 0.2) to at least 0.51
#' dirty %>% similarity_join(reference, by = c("description" = "reference"), nomatch_cutoff = 0.51)
similarity_join <- function(...) common_join(..., matcher = similarity_join_matcher)

# pattern join
pattern_join_matcher <- function(x1, x2) {
  # calculate pattern (character) lengths
  x2_nchars <- nchar(x2)

  # calculate "pattern complexity", assumed to correspond to pattern length (0.1 ~ lowest, 1 ~ highest complexity)
  x2_complexity <- rescale_pattern_complexity(x2_nchars)

  # calculate approximate string distances
  # fixed = FALSE means that x2 (the first argument) is treated as a regular expression
  ad <- adist(x = x2, y = x1, fixed = F, ignore.case = T)

  # 1st weight "rs" corresponds to string similarities:
  # rescale string distances (ad); rs ~ 1 and rs ~ 0 mean little and great distances, respectively.
  rs <- apply(ad, 2, rescale_adist)

  # 2nd weight (match overlaps) and 3rd weight (pattern complexity)
  # calculate match lengths
  mlm <- matchLenMatrix(x2, x1)

  # calculate match overlaps, i.e. what proportion of targets are captured by the patterns
  # create a matrix with target lengths (i.e. lengths of x1)
  tl <- matrix(rep(nchar(x1), length(x2)), byrow = TRUE, nrow = length(x2))

  # the overlaps are match lengths divided by target lengths; i.e. the proportions of targets captured by the
  # regex patterns
  mlm_ol <- mlm/tl

  # adjust for pattern complexity, i.e. favor complex (=long) patterns over simple (=short) patterns
  mlm_ol_pc <- apply(mlm_ol, 2, function(column) column * x2_complexity)

  # compute rescaled and weighted approximate string distances (from here on abbreviated as 'rwasd')
  # by multiplying rs with mlm_ol_pc
  rs * mlm_ol_pc
}

## pattern_join, similarity_join

#' @title pattern_join, similarity_join
#' @name x_join
#'
#' `pattern_join` and `similarity_join` join two `data.frame` objects based on \emph{regex} patterns or similarities to a reference, respectively. The \emph{first} `data.frame`
#' contains a \emph{dirty} column (i.e. "real-world" data originating from a free text field) that needs grouping, categorizing, or classifying based on its content.
#' The \emph{second} `data.frame` maps its rows to above \emph{dirty} column. It achieves this using the unique patterns (`patter_join`) or references (`similarity_join`) given in
#' one of its own columns (specified by parameter `by`, see below).
#'
#' @param x the first `data.frame`
#' @param y the second `data.frame` containing a column with \emph{regex} patterns
#' @param by character of length 1, specifying either names of corresponding field names in a
#' named (e.g. `c("field name in x" = "field name containing patterns in y")`) or
#' unnamed (e.g. `"field name in both x and y"`; here, both `x` and `y` contain the same column name) character.
#' @param nomatch_label character or `NA`, specifying values joined to entries in `x` that do not
#' have corresponding match in `y`
#' @param x_split_cutoff integer specifying number of rows above which `x` is split into smaller
#' `data.frame` objects; this is necessary, as the joining algorithm cannot handle data.frames with
#' many thousand rows.
#' @param multicore logical specifying if multiple cores should be used or not; it defaults to `TRUE`, although
#' benefits in speed only arise if `nrow(x)` \emph{is substantially greater} than `x_split_cutoff`.
#' @return `data.frame` of merged `x` and `y` based on found similarities columns specified by argument `by`.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr pull
#' @importFrom dplyr mutate
#' @importFrom dplyr group_by
#' @importFrom dplyr slice_max
#' @importFrom dplyr arrange
#' @importFrom dplyr select
#' @importFrom dplyr ungroup
#' @importFrom dplyr inner_join
#' @importFrom tidyr pivot_longer
#' @importFrom utils adist
#' @import nycflights13
#'
#' @seealso \code{pattern_join} is similar to \link[fuzzyjoin]{regex_join}
#'
#' @export
#'
#' @examples
#' # pattern_join 'airplanes' with 'model_type' by columns 'model' and 'pattern'
#' airplanes_model_type <- pattern_join(airplanes, model_type, c("model" = "pattern"), multicore = FALSE)

pattern_join <- function(...) common_join(..., nomatch_cutoff = -Inf, matcher = pattern_join_matcher)

