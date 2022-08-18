## Helper functions

# rescale value from 0 to 1
rescale01 <- function(x) {
  if (all(diff(x) == 0)) { # if all values are the same ...
    rep(0, length(x))      # ... return vector of zeroes
  } else {
    (x - min(x))/(max(x)-min(x))
  }
}

# rescale value from 1 to 0
rescale10 <- function(x) 1 - rescale01(x)

# massage rescaled value by condensing values closer to one (pull up) or closer to zero (push down)
rescale_modify <- function(x, modification = c("none", "pull up", "push down")) {
  # check if argument between 0 and 1
  if (min(x) < 0 | max(x) > 1) stop("Please provide numeric between 0 and 1.")
  # if all values the same, return x unaltered
  if (all(diff(x) == 0)) return(x)
  # define helper function to deal with 0 and 1
  massage <- function(x) {
    # define arbitrary small number
    s <- 1e-8
    # replace zeroes with s
    x[x == 0] <- s
    # replace ones with 1-s
    x[x == 1] <- 1 - s
    # return
    x
  }
  # check modification
  md <- match.arg(modification)
  # return based on md
  switch (md,
          none = x,
          `pull up` = rescale01(log(massage(x))),
          `push down` = rescale01(-log(1 - massage(x)))
  )
}

# the rescale function for matchmaker
rescale_adist <- function(x) rescale_modify(rescale10(x), "push down")

# split large data.frame into parts using dplyr::group_split
splitter <- function(d, maxrows = 1000) {
  parts <- ceiling(nrow(d)/maxrows)
  # to avoid errors of type "Undefined global functions or variables", set variables to NULL
  gr <- NULL
  g <- d %>% mutate(gr = rep_len(1:parts, length.out = nrow(d))) %>%
    group_by(gr)
  group_split(g, .keep = FALSE)
}

## Function pattern_join

#' pattern_join
#'
#' Function to join two `data.frame` objects based on \emph{regex} patterns. The \emph{first} `data.frame`
#' contains a \emph{dirty} column (e.g. "real-world" data originating from a free text field) that needs grouping.
#' The \emph{second} `data.frame` maps its rows to above mentioned \emph{dirty} column using the unique patterns contained in
#' one of its own columns.
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

pattern_join <- function(x, y, by, nomatch_label = NA, x_split_cutoff = 500, multicore = TRUE) {

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

  # TODO: check, if a "nomatch" entry (i.e. one starting with the all-inclusive pattern ".*" exists
  # in table 'y'
  # x1 is dealt with below
  x2 <- pull(y, f_y) %>% as.character

  if (!any(grepl("^\\.\\*$", x2))) { # if "nomatch" entry does not exist, add one to 'y'
    jokerpattern <- ".*" # this pattern matches all
    number_cols_y <- ncol(y)
    names_cols_y <- colnames(y)
    nomatch_entry <- c(jokerpattern, rep(nomatch_label, number_cols_y - 1))
    # y is added the row 'nomatch_entry':
    y <- rbind(y, nomatch_entry)
    # x2 also has to be updated:
    x2 <- c(x2, jokerpattern)
  } else {
    message("'y' already contains a 'nomatch' entry.")
  }

  # function that does the joining
  joiner <- function(x_part) {
    # extract fields
    x1 <- pull(x_part, f_x) %>% as.character

    # calculate approximate string distances
    ad <- adist(x2, x1, fixed = F, ignore.case = T)

    # rescale string distances
    rs <- apply(ad, 2, rescale_adist)

    # calculate weights of string distances (smaller the shorter a possible string overlap)
    w <- matrix(nchar(x2), ncol=1) %*% (1/nchar(x1))

    # compute rescaled and weighted approximate string distances (from here on abbreviated as 'rwasd')
    matches <- rs * w
    colnames(matches) <- as.character(1:nrow(x_part)) # columns represent row numbers of x

    # to avoid errors of type "Undefined global functions or variables", set variables to NULL
    uniquerownumber <- uniquepatternnumber <- rwasd <- n <- pattern <- NULL

    # convert matrix to data.frame
    matches_d <- matches %>%
      as.data.frame %>%
      mutate(uniquepatternnumber = as.character(1:nrow(y))) %>%
      tidyr::pivot_longer(-uniquepatternnumber, # convert to long format
                   names_to = "uniquerownumber",
                   values_to = "rwasd") %>%
      group_by(uniquerownumber) %>%
      slice_max(rwasd, n = 1, with_ties = FALSE) %>% # do not allow more than one match
      arrange(as.numeric(uniquerownumber)) %>%
      inner_join(mutate(x_part, uniquerownumber = as.character(1:n())), by = "uniquerownumber") %>% # inner_join to x_part
      inner_join(mutate(y, uniquepatternnumber = as.character(1:n())), by = "uniquepatternnumber") %>% # inner_join to y
      ungroup() %>%
      select(-uniquerownumber, -uniquepatternnumber, -rwasd, -pattern) # get rid of temporary columns

    # return
    matches_d
  }

  # split a big table 'x' into smaller ones.
  x_split <- splitter(x, maxrows = x_split_cutoff)

  # if 'multicore' evaluates to TRUE, use parallel processing
  if (!multicore) {
    # join
    result <- lapply(x_split, joiner)
  } else {
    # determine no. of cores
    n.cores <- parallel::detectCores()

    # helper function for pbapply
    prl <- function(cl) pbapply::pblapply(X = x_split, FUN = joiner, cl = cl)

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
  return(Reduce(rbind, result, data.frame()))
}

# for comparing parallel vs. single core computing:

pattern_join_p <- function(x, y, by, nomatch_label = NA, x_split_cutoff = 1000) {
  pattern_join(x = x, y = y, by = by, nomatch_label = nomatch_label, x_split_cutoff = x_split_cutoff, multicore = TRUE)
}

pattern_join_m <- function(x, y, by, nomatch_label = NA, x_split_cutoff = 1000) {
  pattern_join(x = x, y = y, by = by, nomatch_label = nomatch_label, x_split_cutoff = x_split_cutoff, multicore = FALSE)
}

# # Test usefulness of parallel computing:
# airplanes3 <- nycflights13::planes # contains 3322 rows
# bench::mark(pattern_join_m(airplanes3, model_type, c("model" = "pattern")),
#             pattern_join_p(airplanes3, model_type, c("model" = "pattern")), memory = FALSE)
# # # OUTPUT
# # # A tibble: 2 x 13
# # expression                                                      min median `itr/sec` mem_alloc `gc/sec` n_itr  n_gc total_time
# # <bch:expr>                                                   <bch:> <bch:>     <dbl> <bch:byt>    <dbl> <int> <dbl>   <bch:tm>
# #   1 pattern_join_m(airplanes3, model_type, c(model = "pattern"))  607ms  607ms      1.65        NA     9.89     1     6      607ms
# #   2 pattern_join_p(airplanes3, model_type, c(model = "pattern"))  249ms  252ms      3.96        NA    17.8      2     9      505ms
