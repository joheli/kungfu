library(tidyr)
library(dplyr)

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
  df[, "from"] <- 1:nrow(df)

  # Convert to long format
  dl <- pivot_longer(df,
                     cols = !starts_with("from"),
                     names_to = "to",
                     values_to = "distance",
                     names_transform = list(from = as.integer, to = as.integer))

  # return result
  return(dl)
}

chunks <- function(x, max.distance = NULL,
                   which.chunk = c("all", "biggest", "first", "random")) {
  # empty result objects
  single.chunk.empty <- numeric()
  multi.chunk.empty <- data.frame(x = numeric(), chunk = numeric())
  # Argument validation
  if (!inherits(x, c("numeric", "integer"))) stop("Please supply a numeric or integer.")
  if (length(x) < 2) {
    if (which.chunk == "all") {
      return(multi.chunk.empty)
    } else {
      return(single.chunk.empty)
    }
  }
  if (is.null(max.distance)) stop("Please supply a maximal distance!")
  which.chunk <- match.arg(which.chunk)

  # If duplicates present: stop
  if (length(unique(x)) < length(x)) stop("Please supply a vector without duplicates.")

  # Generate distance matrix
  ds <- NULL # initialize ds (variable containing dist result)
  try(ds <- dist(x), silent = TRUE)
  if (is.null(ds)) stop("Generation of distance matrix by function 'dist' failed.")

  # Apply hierarchical clustering: hclust
  tr <- hclust(ds)

  # Create groups based on max.distance
  cuts <- cutree(tree = tr, h = max.distance)

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

#' Distance filter
#'
#' Filter entries of a vector \code{d} of class \code{numeric}, \code{integer}, \code{Date}, or \code{POSIXt}
#' according to maximal distance \code{max.dist}.
#'
#' @param d a \code{numeric}, \code{integer}, \code{Date}, or \code{POSIXt}
#' @param max.dist \code{numeric} specifying maximal distance entries in \code{d} are allowed to have to be retained
#' @param temporal.unit only applies, if \code{d} is of class \code{Date} or \code{POSIXt}; accepted values are "day", "second", "minute", "week", "year".
#' @param which.chunk controls output and display of chunks satisfying the restriction imposed by \code{max.dist}; accepted
#' accepted values are all", "biggest", "first", and "random"
#'
#' @return depending on \code{which.chunk} a \code{logical} ("biggest", "first", and "random") labeling
#' entries in \code{d} to be retained or a \code{data.frame} ("all") containing retained entries and the chunk number.
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
#' # indicate firs chunk satisfying the condition set by max.dist
#' dfilter(test$pos, max.dist = 15, which.chunk = "first")
#' # show all chunks
#' dfilter(test$pos, max.dist = 15, which.chunk = "all")
#' # apply in a grouped tibble
#' test %>% group_by(group) %>% filter(dfilter(time, max.dist = 1000, which.chunk = "first"))
#' # please note that in a grouped tibble, you cannot use dfilter inside filter()
#' test %>% group_by(group) %>% group_modify(~ dfilter(.x[, "pos"], max.dist = 15, which.chunk = "all"))
dfilter <- function(d, max.dist = Inf,
                    temporal.unit = c("day", "second", "minute", "week", "year"),
                    which.chunk = "all") {
  # Housekeeping: retain original copy of d
  d.orig <- d
  d.name <- class(d)[1] # use class to create a column name, if needed

  # Internal functions
  uts <- function(x) as.numeric(as.POSIXct(x))  # function to transform to unix timestamp

  # with group_map, a tibble with one column can be passed as d
  # in such a case, additional extraction is required for argument validation (see below) to work
  if (inherits(d, "tbl_df")) {
    if (dim(d)[2] == 1) {
      d.name <- names(d)
      d <- pull(d, 1)
      d.orig <- d
    }
  }

  # Argument validation
  if (!inherits(d, c("numeric", "integer", "Date", "POSIXt"))) {
    stop("Please supply a numeric, integer, Date, or POSIXt.")
  }

  # Return d unchanged if max.dist is Inf!
  if (max.dist == Inf) return(d)

  temporal.unit <- match.arg(temporal.unit)

  if (inherits(d, c("Date", "POSIXt"))) {
    d <- uts(d)
    max.dist <- switch(temporal.unit,
                       day = max.dist * 60 * 60 * 24,
                       second = max.dist,
                       minute = max.dist * 60,
                       week = max.dist * 60 * 60 * 24 * 7,
                       year = max.dist * 60 * 60 * 24 * 365)
  }

  # remove duplicates
  d <- unique(d)
  # record names of unique entries
  d.orig.unique <- d.orig[!duplicated(d)]
  # translate d to d.orig.unique
  d.trans <- data.frame(d = d, d.orig = d.orig.unique)

  # Chunks
  ch <- chunks(d, max.distance = max.dist, which.chunk = which.chunk)

  # Return
  res0 <- data.frame(d.orig = d.orig) %>% inner_join(d.trans, by = "d.orig")

  if (inherits(ch, "data.frame")) {
    res <- res0 %>%
      inner_join(ch, by = c("d" = "x")) %>%
      select(-d, ) %>%
      rename(!!d.name := d.orig)
    } else {
    res1 <- res0 %>%
      inner_join(data.frame(d = ch), by = "d") %>%
      pull(d.orig)
    res <- d.orig %in% res1
  }

  return(res)
}
