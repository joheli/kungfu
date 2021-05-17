library(dplyr)
library(tidyr)

# df1 <- tibble(
#   gurken = c("Bungas (gut)", "Bungas", "Heinz", "Heinz Maky", "Birgit", "Bungas Hungas", "Birgit", "Kokos")
# )
#
# mx <- tibble(
#   typ = c("Bungas", "Heinz", "Heinz Maky", "Birgit", ".*")
# )

rescale01 <- function(x) {
  if (all(diff(x) == 0)) { # if all values are the same ...
    rep(0, length(x))      # ... return vector of zeroes
  } else {
    (x - min(x))/(max(x)-min(x))
  }
}

rescale10 <- function(x) 1 - rescale01(x)
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

rescale_adist <- function(x) rescale_modify(rescale10(x), "push down")

#' matchmaker
#'
#' Function to match rows in a given data.frame to unique patterns contained in a second data.frame.
#'
#' @param t1 data.frame containing rows to be matched
#' @param f1 character specifying field name on which matching is attempted
#' @param t2 data.frame containing unique patterns
#' @param f2 character specifying field name carrying unique patterns
#'
#' @return data.frame of merged t1 and t2 based on found similarities between f1 and f2
#' @export
#'
#' @examples
matchmaker <- function(t1,
                       f1,
                       t2,
                       f2) {

  # extract fields
  x1 <- pull(t1, f1)
  x2 <- pull(t2, f2)

  # approximate string distances
  ad <- adist(x2, x1, fixed = F, ignore.case = T)

  # rescaled string distances
  rs <- t(apply(ad, 1, rescale_adist))

  # weights of string distances (the smaller the shorter the possible string overlap)
  w <- matrix(nchar(x2), ncol=1) %*% (1/nchar(x1))

  # rescaled and weighted approximate string distances (rwasd)
  matches <- rs * w
  colnames(matches) <- 1:nrow(t1) # columns represent row numbers of t1

  matches_d <- matches %>%
    as.data.frame %>%
    mutate(type = 1:nrow(t2)) %>%
    pivot_longer(-type,
                 names_to = "row",
                 values_to = "rwasd") %>%
    group_by(row) %>%
    slice_max(rwasd, n = 1, with_ties = FALSE) %>% # do not allow more than one match
    arrange(row) %>%
    inner_join(mutate(t1, row = as.character(1:n()))) %>%
    inner_join(mutate(t2, type = 1:n()))

  matches_d
}

# m <- matchmaker(df1, "gurken", mx, "typ")
