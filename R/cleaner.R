#' @name cleaner
#' @title cleaner
#'
#' @description `cleaner` removes duplicate entries from a `data.frame`.
#'
#' @param d a `data.frame`
#' @param ufn character, contains column names identifying unique entries (`ufn` is an abbreviation of "unique field names")
#' @param orderAlsoBy optional character, specifies column name that `d` is ordered by prior to cleaning
#' @param decr logical, specifying if `orderAlsoBy` is to be sorted in a decreasing fashion (defaults to `FALSE`)
#' @return `cleaner` returns a cleaned (i.e. deduplicated) `data.frame`
#' @examples
#' # clean heart rate data, only allow first measurement per person and condition
#' onlyfirst <- cleaner(heartrate, c("person", "condition"), "timestamp")
#' # alternatively, only allow last measurement
#' onlylast <- cleaner(heartrate, c("person", "condition"), "timestamp", TRUE)
#' @export
cleaner <- function(d, ufn, orderAlsoBy = character(), decr = FALSE) {
  # TODO: if d is a tibble, it has to be converted to a data.frame to avoid warning message: In xtfrm.data.frame(x) : cannot xtfrm data frames
  ufn_ <- ufn
  colns <- colnames(d) # save column names
  # if "orderAlsoBy" provided, add column ".order"
  if (length(orderAlsoBy) > 0) {
    d$.order <- order(d[, orderAlsoBy], decreasing = decr)
    ufn_ <- c(ufn, ".order")
  }
  d %>% arrange(across(all_of(ufn_))) %>%
    group_by(across(all_of(ufn))) %>%
    slice_head(n = 1) %>%
    select(all_of(colns)) %>% # do not include column name ".order"
    as.data.frame
}

#' @rdname cleaner
#'
#' @description `is_clean` checks whether a `data.frame` has duplicate entries.
#'
#' @param d a `data.frame`
#'
#' @return `is_clean` returns a logical indicating whether duplicate entries were found
#' @export
#'
#' @examples
#' # show whether 'heartrate' contains duplicates
#' is_clean(heartrate, c("person", "condition")) # returns false
is_clean <- function(d, ufn = names(d)) {
  # convert to data.frame
  # this is necessary, as tibbles with one column have length 1 regardless of number of entries
  d <- as.data.frame(d)
  result = FALSE
  if (length(ufn) == 1) {
    result = nrow(d) == length(unique(d[, ufn]))
  } else {
    result = nrow(d) == nrow(unique(d[, ufn]))
  }
  result
}

#' @rdname cleaner
#'
#' @description `cleaned` shows which rows have been cleaned from a `data.frame` with `cleaner`
#'
#' @param before a `data.frame`
#' @param after a `data.frame` that has been cleaned with `cleaner`
#'
#' @return `cleaned` returns a `data.frame` containing the entries in `before` that have been cleaned.
#' @export
#'
#' @examples
#' # show entries cleaned from 'heartrate' compared to 'onlyfirst'
#' cleaned(heartrate, onlyfirst) # contains rows that were removed
cleaned <- function(before, after) {
  k.before <- key2(before)
  k.after <- key2(after)
  i <-
    !(k.before %in% k.after) |
    duplicated(k.before) # warning: assumption!
  # it is assumed, that 'after' was cleaned with cleaner(), i.e. duplicates were removed!
  # this assumption (i.e. that duplicates were removed) does not hold for other functions
  # this function is not agnostic to the function performing the cleaning
  before[i, ]
}
