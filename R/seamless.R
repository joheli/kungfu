#' @name seamless
#' @aliases is_seamless
#'
#' @title is_seamless, seamless
#'
#' @description \code{is_seamless} checks if a given history (i.e. a succession of intervals) is
#' seamless, i.e. \emph{stop} timepoints of preceding coincide with \emph{start} timepoints of
#' succeeding intervals. Conversion of a not seamless into a seamless history can be attempted with
#' \code{seamless}.
#'
#' @param d table
#' @param start \code{tidyr} selector specifying column containing \emph{start} timepoints
#' @param stop \code{tidyr} selector specifying column containing \emph{stop} timeponts
#'
#' @return \code{is_seamless} returns a logical stating whether history is seamless or not
#'
#' @import dplyr
#' @import lubridate
#' @import fuzzyjoin
#'
#' @rdname seamless
#'
#' @export
#'
#' @examples
#' # set seed to make it reproducible
#' set.seed(1)
#' # generate three random numbers between 0 and 1e6
#' rn <- runif(3, 0, 1e6)
#' # generate intervals in seconds
#' ir <- rn + runif(3, 0, 1e6)
#' # generate random history `hx` (table)
#' hx <- data.frame(unit = letters[1:3], start = as.POSIXlt(rn, origin = "2021-01-01 00:00:00"), stop = as.POSIXlt(ir, origin = "2021-01-01 00:00:00"))
#' # hx is expected to not be seamless
#' is_seamless(hx) # returns FALSE
#' # hx can, however, be converted
#' sl_hx <- seamless(hx)
#' is_seamless(sl_hx) # returns TRUE
is_seamless <- function(d, start = start, stop = stop) {
  if (nrow(d) < 2) return(TRUE) # Any table with only one row is seamless.
  ro <- arrange(d, {{start}}, {{stop}})
  a <- pull(ro, {{start}})
  b <- pull(ro, {{stop}})
  return(all(a[-1] == b[-length(b)]))
}


#' @rdname seamless
#' @return \code{seamless} returns a \code{tibble} that is seamless, i.e. satisfies \code{is_seamless}
seamless <- function(d, start = start, stop = stop) {
  r <- nrow(d)
  if (r < 2) return(d) # return 'd' for tables containing only one row
  clnms <- colnames(d) # save order of columns for later use
  start_tp <- pull(d, {{start}})
  stop_tp <- pull(d, {{stop}})
  if (!all((stop_tp - start_tp) > 0)) stop("{{start}} has to be smaller than {{stop}}")

  # Calculate duration 'dur'
  dd <- mutate(d, rownumber = 1:nrow(d)) %>%
    arrange({{start}}, {{stop}}) %>%
    rowwise() %>%
    mutate(dur = {{stop}} - {{start}})

  # Extract all timepoints and convert to seamless intervals
  i <- sort(unique(c(start_tp, stop_tp))) %>% int_diff()

  # Create table containing seamless intervals
  id <- data.frame(a = int_start(i), b = int_end(i)) %>%
    transmute("{{start}}" := a, "{{stop}}" := b)

  # Join 'id' with 'd', prioritizing matches according to shortest duration 'dur'
  d3 <- mutate(id, movement = 1:nrow(id)) %>% # movement is a temporary field
    interval_left_join(dd, minoverlap = 2) %>% # minoverlap is necessary to avoid matching with contiguous intervals
    group_by(movement) %>%
    slice_min(dur) %>% # prioritizing according to 'dur'
    ungroup() %>% # necessary to remove temporary field 'movement'
    select(-ends_with("y"), -movement, -rownumber, -dur) # remove not needed columns
  colnames(d3) <- gsub("\\.x$", "", colnames(d3)) # remove suffixes '.x' from column names
  d3 %>% select(all_of(clnms)) # ensure identical column ordering as in 'd'

  # TODO: merge contiguous movements to same unit
}
