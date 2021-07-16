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
#' @param begin \code{tidyr} selector specifying column containing \emph{begin} timepoints
#' @param end \code{tidyr} selector specifying column containing \emph{end} timeponts
#'
#' @return \code{is_seamless} returns a logical stating whether history is seamless or not
#'
#' @import dplyr
#' @import lubridate
#' @import fuzzyjoin
#' @importFrom rlang :=
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
#' hx <- data.frame(unit = letters[1:3],
#' begin = as.POSIXlt(rn, origin = "2021-01-01 00:00:00"),
#' end = as.POSIXlt(ir, origin = "2021-01-01 00:00:00"))
#' # hx is expected to not be seamless
#' is_seamless(hx) # returns FALSE
#'
is_seamless <- function(d, begin = begin, end = end) {
  if (nrow(d) < 2) return(TRUE) # Any table with only one row is seamless.
  ro <- arrange(d, {{begin}}, {{end}})
  a <- pull(ro, {{begin}})
  b <- pull(ro, {{end}})
  return(all(a[-1] == b[-length(b)]))
}


#' @rdname seamless
#'
#' @return \code{seamless} returns a \code{tibble} that is seamless, i.e. satisfies \code{is_seamless}
#'
#' @examples
#' # hx can, however, be converted
#' \dontrun{
#' sl_hx <- seamless(hx)
#' is_seamless(sl_hx) # returns TRUE
#' }
seamless <- function(d, begin = begin, end = end) {
  r <- nrow(d)
  if (r < 2) return(d) # return 'd' for tables containing only one row
  if (ncol(d) < 3) stop("Please supply at least one additional column in addition to 'begin' and 'end'.")
  clnms <- colnames(d) # save order of columns for later use
  clnms_group <- d %>% select(-c({{begin}}, {{end}})) %>% colnames()
  start_tp <- pull(d, {{begin}})
  stop_tp <- pull(d, {{end}})
  if (!all((stop_tp - start_tp) > 0)) stop("'begin' has to be smaller than 'end'!")

  # to avoid errors of type "Undefined global functions or variables", set variables to NULL
  a <- b <- rownumber <- movement <- dur <- NULL

  # Calculate duration 'dur'
  dd <- mutate(d, rownumber = 1:nrow(d)) %>%
    arrange({{begin}}, {{end}}) %>%
    rowwise() %>%
    mutate(dur = {{end}} - {{begin}})

  # Extract all timepoints and convert to seamless intervals
  i <- sort(unique(c(start_tp, stop_tp))) %>% int_diff()

  # Create table containing seamless intervals
  id <- data.frame(a = int_start(i), b = int_end(i)) %>%
    transmute("{{begin}}" := a, "{{end}}" := b)

  # Join 'id' with 'd', prioritizing matches according to shortest duration 'dur'
  d3 <- mutate(id, movement = 1:nrow(id)) %>% # movement is a temporary field
    interval_left_join(dd, minoverlap = 2) %>% # minoverlap is necessary to avoid matching with contiguous intervals
    group_by(movement) %>%
    slice_min(dur) %>% # prioritizing according to 'dur'
    ungroup() %>% # necessary to remove temporary field 'movement'
    select(-ends_with("y"), -movement, -rownumber, -dur) # remove not needed columns
  colnames(d3) <- gsub("\\.x$", "", colnames(d3)) # remove suffixes '.x' from column names

  # are there congiguous intervals that need merging?
  d4 <- d3 %>% mutate(z = 1:nrow(d3))
  to_be_merged <- d4 %>%
    get_dupes(-c({{begin}}, {{end}}, z)) %>%
    mutate(k = c(0, diff(z))) %>%
    filter(k <= 1) %>%
    group_by(across(all_of(clnms_group)))

  d5 <- d4 %>%
    filter(!(z %in% to_be_merged$z)) %>%
    select(all_of(clnms))

  d6 <- to_be_merged %>%
    summarise("{{begin}}" := min({{begin}}, na.rm = TRUE), "{{end}}" := max({{end}}, na.rm = T)) %>%
    select(all_of(clnms))

  rbind(d5, d6)
}
