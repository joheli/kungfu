## Helper functions

# assess length of characters (optionally also digits) in a string
alphaLen <- function(x, excludeDigits = TRUE) {
  # retain original x and record number of characters
  originalX <- x
  originalLength <- nchar(x)
  # optionally replace digits by empty space in x
  if (!excludeDigits) x <- gsub("[0-9]", "", x)
  # replace characters and space by empty space in x
  x <- gsub("[[:space:] a-z]", "", x, ignore.case = TRUE)
  # return length of string with all non-characters (and optionally non-digits) removed
  originalLength - nchar(x)
}

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
rescale_pattern_length <- function(x) rescale(x, min = 0.1, max = 1)

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
  matching_metric <- n <- pattern <- unique_x1_number <- unique_x2_number <- NULL

  # convert matrix to data.frame
  matches_d <- matches %>%
    as.data.frame %>%
    mutate(unique_x2_number = as.character(1:nrow(matches))) %>%
    # mutate(unique_x2_number = as.character(1:nrow(y))) %>%
    tidyr::pivot_longer(-unique_x2_number, # convert to long format
                        names_to = "unique_x1_number",
                        values_to = "matching_metric") %>%
    group_by(unique_x1_number) %>%
    slice_max(matching_metric, n = 1, with_ties = FALSE) %>% # do not allow more than one match
    arrange(as.numeric(unique_x1_number)) %>%
    inner_join(mutate(x_part, unique_x1_number = as.character(1:n())), by = "unique_x1_number") %>% # inner_join to x_part
    # inner_join(mutate(y, unique_x2_number = as.character(1:n())), by = "unique_x2_number") %>% # inner_join to y
    # change to left_join to also include pattern .* added by pattern_join_matcher
    left_join(mutate(y, unique_x2_number = as.character(1:n())), by = "unique_x2_number") %>%
    ungroup() %>%
    select(-unique_x1_number, -unique_x2_number) # get rid of temporary columns

  # return
  matches_d
}

# common join: a common trunk that is used by pattern_join and similarity_join

#' @title common_join, pattern_join, similarity_join
#' @name pattern_join
#' @rdname pattern_join
#'
#' @description `pattern_join` and `similarity_join` join two `data.frame` objects based on \emph{regex} patterns or similarities to a reference, respectively. The \emph{first} `data.frame`
#' contains a \emph{dirty} column (i.e. "real-world" data originating from a free text field) that needs grouping, categorizing, or classifying based on its content.
#' The \emph{second} `data.frame` maps its rows to above \emph{dirty} column. It achieves this using the unique patterns (`pattern_join`) or references (`similarity_join`) given in
#' one of its own columns (specified by parameter `by`, see below). `common_join` is a "common trunk" used by both
#' `pattern_join` and `similarity_join` and can be used to create custom join functions (provided a custom `matcher`
#' function is given, see below).
#'
#' @param x the first `data.frame`
#' @param y the second `data.frame` containing a column with \emph{regex} patterns
#' @param by character of length 1, specifying either names of corresponding field names in a
#' named (e.g. `c("field name in x" = "field name containing patterns in y")`) or
#' unnamed (e.g. `"field name in both x and y"`; here, both `x` and `y` contain the same column name) character.
#' @param nomatch_cutoff used by `similarity_match`: a numeric between 0 and 1 specifying the similarity
#' (using metric \emph{optimal string alignment}, see \link[stringdist]{stringsim}) below which `NA` is
#' joined to orginal data (meaning: the entry is treated as a "no match")
#' @param x_split_cutoff integer specifying number of rows above which `x` is split into smaller
#' `data.frame` objects; this is necessary, as the joining algorithm cannot handle data.frames with
#' many thousand rows.
#' @param multicore logical specifying if multiple cores should be used or not; it defaults to `TRUE`, although
#' benefits in speed only arise if `nrow(x)` \emph{is substantially greater} than `x_split_cutoff`.
#' @param matcher to create a custom join function using `common_join`, specify here a function accepting two
#' character vectors and returning a matrix with a custom matching metric; e.g. for `similarity_join` the custom matching
#' function is `function(x1, x2) stringdist::stringsimmatrix(a = x2, b = x1, method = "osa")`.
#'
#' @return a `tibble` of merged `x` and `y` based on found similarities columns specified by argument `by`.
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
common_join <- function(x, y, by, nomatch_cutoff = 0.2,
                        x_split_cutoff = 500, multicore = TRUE, matcher = NULL) {
  # check arguments
  for (d in list(x, y)) if (!inherits(d, "data.frame")) stop("Please supply 'data.frame' objects for arguments 'x' and 'y'.")
  if (!inherits(by, "character") | length(by) != 1) stop("Please supply a character of length 1 as argument 'by'.")

  # set nomatch_label to NA
  nomatch_label <- NA

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
  # to avoid errors of type "Undefined global functions or variables", set variables to NULL
  matching_metric <- NULL

  # rbind pieces together
  result0 <- Reduce(rbind, result, data.frame())
  # now remove those matches with unacceptably low matching metric
  #     first create an index
  index_below_nomatch_cutoff <- pull(result0, matching_metric) < nomatch_cutoff
  #     second set to 'nomatch_label' if below nomatch_cutoff
  result0[index_below_nomatch_cutoff, names(y)] <- nomatch_label
  # finally remove matching metric from result
  result <- result0 %>% select(-matching_metric)
  #result <- result0
  # return result
  return(result)
}

# specific matchers
# functions that accept x1 and x2 (see joiner) and return a matrix with a specific matching metric
similarity_join_matcher <- function(x1, x2) stringdist::stringsimmatrix(a = x2, b = x1, method = "osa")

# pattern join
pattern_join_matcher <- function(x1, x2) {
  # add joker pattern .* to x2 if not already present
  if (!any(grepl("^\\.\\*$", x2))) x2 <- c(x2, ".*")

  # calculate pattern (character) lengths; these serve as a proxy for pattern specificity
  x2_lengths <- alphaLen(x2)

  # rescale pattern lengths (0.1 ~ lowest, 1 ~ highest complexity)
  x2_lengths_rs <- rescale_pattern_length(x2_lengths)

  # calculate string distances treating x2 as regular expressions (fixed = FALSE)
  ad <- adist(x = x2, y = x1, fixed = FALSE, ignore.case = TRUE)

  # calculate target (x1) lengths
  x1_lenghts <- nchar(x1)

  # normalize string distances (ad) through division by x1_lenghts
  # this operation mostly generates values between 0 (no distance) to 1 (great distance)
  # as usually number of characters of targets exceed pattern lengths
  adn <- t(t(ad)/x1_lenghts)
  # in case values are greater than 1 enforce upper bound of 1
  adn1 <- adn
  adn1[adn1 > 1] <- 1

  # invert normalized string distances (adn1) to get 0 for great distance and 1 for no distance
  # then, push values down to increase contrast (i.e. make good matches stand out)
  adni <- apply(1 - adn1, 2, rescale_modify, modification = "push down")

  # finally, multiply by rescaled pattern lengths (x2_lengths_rs) to give more weight
  # to long rather than short patterns
  adnip <- adni * x2_lengths_rs

  # return
  adnip
}

## pattern_join, similarity_join

#' @rdname pattern_join
#'
#' @export
#'
#' @examples
#' # pattern_join 'airplanes' with 'model_type' by columns 'model' and 'pattern'
#' airplanes_model_type <- pattern_join(airplanes, model_type, c("model" = "pattern"), multicore = FALSE)

pattern_join <- function(x, y, by, nomatch_cutoff = 0.2, x_split_cutoff = 500,
                         multicore = TRUE) {
  common_join(x = x, y = y, by = by, nomatch_cutoff = nomatch_cutoff,
              x_split_cutoff = x_split_cutoff, multicore = multicore, matcher = pattern_join_matcher)
}

#' @rdname pattern_join
#'
#' @export
#'
#' @examples
#' # test data for similarity_join
#' dirty <- data.frame(sample = 1:6, description = c("Bergerx", "Mueler", "Horsst", "Kinga", "Mannn", "Schneemann"))
#' reference <- data.frame(reference = c("Berger", "Mueller", "Horst", "King", "Mann", "Mustermann"))
#' # similarity_join with default nomatch_cutoff
#' dirty %>% similarity_join(reference, by = c("description" = "reference"), multicore = FALSE)
#' # to avoid mapping "Schneemann" to "Mustermann", increase nomatch_cutoff (default 0.4) to at least 0.51
#' dirty %>% similarity_join(reference, by = c("description" = "reference"), nomatch_cutoff = 0.51, multicore = FALSE)
similarity_join <- function(x, y, by, nomatch_cutoff = 0.4,
                            x_split_cutoff = 500, multicore = TRUE) {
  common_join(x = x, y = y, by = by, nomatch_cutoff = nomatch_cutoff,
              x_split_cutoff = x_split_cutoff, multicore = multicore, matcher = similarity_join_matcher)
}
