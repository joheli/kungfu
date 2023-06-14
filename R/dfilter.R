#library(tidyr)
#library(dplyr)

# Test df
set.seed(1)
test <- data.frame(group = sample(letters[1:4], 20, TRUE),
                   pos = runif(20, 0, 100),
                   time = as.POSIXct(runif(20, 0, as.numeric(Sys.time())),
                                     origin = as.POSIXct("1970-01-01 00:00.00 UTC")))

# convert distance object to long data.frame
dist2df <- function(x) {
  # Argument validation: check if 'dist'
  if (!inherits(x, "dist")) stop("Please supply an object of class 'dist'!")

  # Attempt conversion to matrix
  m <- NULL # initialize placeholder variable
  try(m <- as.matrix(x), silent = TRUE)
  if (is.null(m)) stop("Conversion to matrix was not successful.")

  # Convert matrix to data.frame
  df <- as.data.frame(m)
  df[, "from"] <- rownames(df)

  # Convert to long format
  dl <- pivot_longer(df,
                     cols = !starts_with("from"),
                     names_to = "to",
                     values_to = "distance",
                     names_transform = list(from = as.integer, to = as.integer))

  # return result
  return(dl)
}

# 'chunks' is the heart of distance filter; it returns clusters of sizes determined by max.distance
chunks <- function(x, max.distance = NULL,
                   which.chunk = c("all", "biggest", "first", "random")) {
  # empty result objects
  single.chunk.empty <- numeric()
  multi.chunk.empty <- data.frame(x = numeric(), chunk = numeric())
  # Argument validation
  which.chunk <- match.arg(which.chunk)
  if (!inherits(x, c("numeric", "integer"))) stop("Please supply a numeric or integer.")
  if (length(x) < 2) {
    if (which.chunk == "all") {
      return(multi.chunk.empty)
    } else {
      return(single.chunk.empty)
    }
  }
  # throw error if no max.distance supplied
  if (is.null(max.distance)) stop("Please supply a maximal distance!")

  # if max.distance is Inf, return all values
  if (max.distance == Inf) {
    if (which.chunk == "all") return(data.frame(x = x, chunk = 1))
    return(x)
  }

  # If duplicates present: stop
  if (length(unique(x)) < length(x)) stop("Please supply a vector without duplicates.")

  # Generate distance matrix
  ds <- NULL # initialize ds (variable containing dist result)
  try(ds <- stats::dist(x), silent = TRUE)
  if (is.null(ds)) stop("Generation of distance matrix by function 'dist' failed.")

  # Apply hierarchical clustering: hclust
  tr <- stats::hclust(ds)

  # Create groups based on max.distance
  cuts <- stats::cutree(tree = tr, h = max.distance)

  # Single chunk ("first", "random", "biggest") options
  tc <- table(cuts)
  #         which chunks have at least two entries?
  chunks.2.index <- tc >= 2
  #         Index of those x and cuts that belong to a chunk with at least two entries
  i2 <- cuts %in% as.numeric(names(tc[chunks.2.index]))
  #         x2 holds those x that belong to a chunk with at least two entries
  x2 <- x[i2]
  #         cuts2 hold those cuts that belong to a chunk with at least two entries
  cuts2 <- cuts[i2]
  #         how many chunks with at least two entries are available?
  chunks.2.sum <- sum(as.integer(chunks.2.index))

  #         if *no* chunk has two or more entries
  if (chunks.2.sum == 0) {
    if (which.chunk == "all") {
      return(multi.chunk.empty)
    } else {
      return(single.chunk.empty)
    }

  } else {
    #         first: return first chunk with two or more entries
    first.cut <- min(as.numeric(names(tc[chunks.2.index])))
    #         biggest: return chunk with most entries (in case of a tie the first is chosen)
    biggest.cut <- min(grep("TRUE", tc == max(tc)))
    #         random: return any one
    random.cut <- sample(names(tc[chunks.2.index]), 1)
  }

  # return (if not already done)
  switch(which.chunk,
         all = data.frame(x = x2, chunk = cuts2),
         biggest = x[cuts == biggest.cut],
         first = x[cuts == first.cut],
         random = x[cuts == random.cut])
}

### COPY FROM HERE

# this function returns a logical marking entries in a vector that have a distance *below* min.dist
below_min.dist <- function(x, min.dist = 0) {
  xn <- x
  nxn <- 1:length(x)
  names(xn) <- nxn

  # to avoid errors of type "Undefined global functions or variables", set variables to NULL
  to <- from <- distance <- NULL

  uncompatibleindices <- function(x) {
    dist2df(dist(x)) %>%
      filter(from != to) %>%
      filter(distance < min.dist) %>%
      select(-distance)
  }

  df <- uncompatibleindices(xn)

  selecao <- rep(TRUE, length(x))

  while (nrow(df) > 0) {
    selecao[nxn == df$to[1]] <- FALSE
    k <- xn[selecao]
    df <- uncompatibleindices(k)
  }

  selecao
}

#' Distance filter
#'
#' Filter entries of a vector \code{d} of class \code{numeric}, \code{integer}, \code{Date}, or \code{POSIXt}
#' according to maximal distance \code{max.dist}.
#'
#' @param d a \code{numeric}, \code{integer}, \code{Date}, or \code{POSIXt}
#' @param min.dist \code{numeric} specifying minimal distance that entries in \code{d} are allowed to have to be retained
#' @param max.dist \code{numeric} specifying maximal distance that entries in \code{d} are allowed to have to be retained
#' @param temporal.unit only applies, if \code{d} is of class \code{Date} or \code{POSIXt}; accepted values are "days", "seconds", "minutes", "hours", "weeks", "years".
#' @param which.chunk controls output and display of chunks satisfying the restriction imposed by \code{max.dist}; accepted
#' accepted values are all", "biggest", "first", and "random"
#'
#' @return depending on \code{which.chunk} a \code{logical} ("biggest", "first", and "random") labeling
#' entries in \code{d} to be retained or a \code{data.frame} ("all") containing retained entries and the chunk number.
#' @importFrom stats dist
#' @importFrom stats hclust
#' @importFrom stats cutree
#' @importFrom stats runif
#' @export
#'
#' @examples
#' # test data
#' set.seed(1)
#' test <- data.frame(group = sample(letters[1:4], 20, TRUE),
#'                   pos = runif(20, 0, 100),
#'                   time = as.POSIXct(runif(20, 0, as.numeric(Sys.time())),
#'                                     origin = as.POSIXct("1970-01-01 00:00.00 UTC")))
#'
#' # indicate first chunk satisfying the condition set by max.dist
#' dfilter(test$pos, max.dist = 15, which.chunk = "first")
#' # show all chunks
#' dfilter(test$pos, max.dist = 15, which.chunk = "all")
#' # apply in a grouped tibble
#' test %>% group_by(group) %>% filter(dfilter(time, max.dist = 1000, which.chunk = "first"))
#' # please note that in a grouped tibble, you cannot use dfilter inside filter()
#' test %>% group_by(group) %>% group_modify(~ dfilter(.x[, "pos"], max.dist = 15, which.chunk = "all"))
dfilter <- function(d, min.dist = 0, max.dist = Inf,
                     temporal.unit = c("days", "seconds", "minutes", "hours", "weeks", "years"),
                     which.chunk = "all") {
  # Housekeeping: retain original copy of d
  d.orig <- d
  d.orig2 <- d # backup copy used for special case
  d.name <- class(d)[1] # use class to create a column name, if needed
  is_tbl <- inherits(d, c("tbl_df", "data.frame")) # check if table

  # extract values
  e <- d
  e.orig <- d.orig
  if (is_tbl) {
    e <- pull(d, 1)
    e.orig <- e
  }

  # only one entry?
  only_one <- FALSE
  if (is_tbl) {
    if (nrow(d) < 2) only_one <- TRUE
    # btw: ajdust d.name for tables
    if (dim(d)[2] == 1) {
      d.name <- names(d)
    } else {
      stop("Sorry cannot deal with more than one column.")
    }
  } else {
    if (length(d) < 2) only_one <- TRUE
  }

  # only proceed here if more than one entry
  if (!only_one) {
    # Internal functions
    # function to transform to unix timestamp
    uts <- function(x) as.numeric(as.POSIXct(x))
    # substract smallest value from all
    red <- function(x) x - min(x)
    # combine uts and red
    uts_red <- function(x) red(uts(x))
    # transform time to distance according to temporal.unit
    time2dist <- function(x) {
      switch(temporal.unit,
             days = x * 60 * 60 * 24,
             seconds = x,
             hours = x * 60 * 60,
             minutes = x * 60,
             weeks = x * 60 * 60 * 24 * 7,
             years = x * 60 * 60 * 24 * 365)
    }

    # Content validation
    if (!inherits(e, c("numeric", "integer", "Date", "POSIXt"))) {
      stop("Please supply a numeric, integer, Date, or POSIXt.")
    }

    # determine temporal unit
    temporal.unit <- match.arg(temporal.unit)

    # transform e, min.dist, and max.dist in case d is a temporal vector
    if (inherits(e, c("Date", "POSIXt"))) {
      e <- uts_red(e)
      min.dist <- time2dist(min.dist)
      max.dist <- time2dist(max.dist)
    }

    # apply min.dist (if min.dist == 0, all values in d are retained)
    at_least.min.dist <- below_min.dist(e, min.dist = min.dist)

    # otherwise, continue
    e <- e[at_least.min.dist]

    # Are values in e unique?
    # If not, they need to be made unique by adding a small random number!
    # Why? Because function chunk() only accepts unique entries.
    # Without making values unique, duplicates would be filtered out, which is not desired!
    if (length(unique(e)) < length(e)) {
      # Ok, d does contain duplicates, i.e. we must make values unique; d.x is to contain ("uniquified") entries only
      # remove duplicates
      e.u <- unique(e)
      # if d.u contains only one entry, set small_value (required for noise generation) to arbitrary value
      if (length(e.u) == 1) {
        small_value = 1/1000
      } else {
        # what is the smallest difference between unique entries
        min.diff.u <- min(abs(diff(e.u)))
        # divide that by 1000
        small_value = min.diff.u/1000
      }
      # function to create small random noise
      noise <- function() runif(n = length(e), min = -small_value, max = small_value)
      # now add noise to d ...
      e.x <- e + noise()
      # ... and make sure each value is in fact unique!
      while (length(unique(e.x)) < length(e)) e.x <- e + noise()
      # correct max.dist according to noise added;
      # as noise added can be positive or negative, add twice maximal absolute value of noise added
      noise.added <- e.x - e # first, calculate noise added
      max.dist.corr <- max.dist + 2 * max(abs(noise.added))
    } else {
      # no duplicates found! d.x is equal to d
      e.x <- e
      # no correction necessary
      max.dist.corr <- max.dist
    }

    # finally, create translation matrix
    e.trans <- data.frame(e.orig = e.orig[at_least.min.dist], e = e, e.x = e.x)

    # Chunks (only accepts unique values!)
    ch <- chunks(e.x, max.distance = max.dist.corr, which.chunk = which.chunk)
  } else {
    # if only one entry
    ch <- chunks(0, max.distance = 0, which.chunk = which.chunk)
    e.trans <- data.frame(e.orig = e.orig[1], e = 0, e.x = 0) # e.orig[1] has to be passed to conserve type!
  }

  # to avoid errors of type "Undefined global functions or variables", set variables to NULL
  chunk <- NULL

  # process according to return type of chunks
  if (inherits(ch, "data.frame")) {
    res <- ch %>%
      inner_join(e.trans, by = c("x" = "e.x")) %>%
      select(e.orig, chunk) %>%
      rename(!!d.name := e.orig)
  } else {
    res <- e.trans$e.x %in% ch
  }

  return(res)
}
