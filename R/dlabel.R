#' @name dlabel
#' @title dlabel
#' 
#' @description 
#' This function adds a label to a data frame based on the distance between records as 
#' defined by the `distance` column and calculated by \link[kungfu]{dfilter}. The label
#' is a boolean stating whether the distance between records is within the specified 
#' range.
#' 
#' @param df A data frame.
#' @param id A character string specifying the column name of the id.
#' @param category A character string specifying the column name of the category.
#' @param distance A character string specifying the column name of the distance column; see \link[kungfu]{dfilter} 
#' for details.
#' @param min.dist A numeric value specifying the minimum distance between records; see \link[kungfu]{dfilter}
#' for details.
#' @param max.dist A numeric value specifying the maximum distance between records; see \link[kungfu]{dfilter}
#' for details.
#' @param temporal.unit A character string specifying the temporal unit of the distance; see \link[kungfu]{dfilter}
#' for details.
#' @param label_name A character string specifying the name of the label column.
#' @param invert A logical value specifying whether to invert the label.
#' @param df_filter A character string specifying the filter expression to be applied to the data frame before
#' the distance label is calculated.
#' 
#' @import dplyr
#' @import rlang
#' @import glue
#' @import tidyr
#' 
#' @return A data frame with the distance label added.
#' 
#' @export 
#' 
#' @examples
#' # create test data
#' set.seed(123)
#' dl.test <- data.frame(id = sample(1:10, 30, replace = TRUE), 
#'            category = sample(letters[1:4], 30, replace = TRUE), 
#'            timestamp = as.POSIXct(runif(30, 1704063600, 1711922400), 
#'                                         origin = "1970-01-01"))
#'
#' # test: dlabel will reveal three id-category combinations with temporal 
#' # distances within the range of 2 to 40 days pertaining to category 'a'
#' test <- dlabel(dl.test, id = "id", category = "category", 
#'         distance = "timestamp", 
#'         min.dist = 2, max.dist = 40,
#'         temporal.unit = "days",
#'         label_name = "within_range",
#'         df_filter = "category == 'a'")
#' 

dlabel <- function(df, id, category, distance, min.dist, max.dist, temporal.unit, 
            label_name = "dlabel",
            invert = FALSE, df_filter = NULL) {
  # save original df for debugging
  df0 <- df

  # avoid error messages of the variety 'no visible binding for global variable'
  chunk <- dlabel_row_number <- NULL

  # add row number to df
  df <- df %>% 
    arrange(pick(!!id, !!category, !!distance)) %>% 
    mutate(dlabel_row_number = row_number())

  # if df_filter is not NULL, filter df
  if (!is.null(df_filter)) {
    df_filtered <- df %>% filter(eval(parse(text = df_filter)))
    # df_filter <- rlang::expr(filter(data, !!rlang::enquo(df_filter)))
    # df_filtered <- eval(df_filter, envir = list(data = df))
  } else {
    df_filtered <- df
  }

  # list of id-category combinations with more than one record
  id_category_list <- df_filtered %>%
    group_by(pick(!!id, !!category)) %>%
    summarize(n = n(), .groups = "drop") %>%
    filter(n > 1)

  # filter df according to id_category_list
  df_records <- df %>%
    inner_join(id_category_list,
               by = c(id, category)) %>%
    select(-n) %>%
    arrange(pick(!!id, !!category))

  # apply dfilter to df_records
  df_records_dfltr <- df_records %>%
    group_by(pick(!!id, !!category)) %>%
    group_modify(~ dfilter(d = .x[, distance], which.chunk = "all", min.dist = min.dist, max.dist = max.dist, temporal.unit = temporal.unit)) %>%
    # add row number to df_records_dfltr
    left_join(df_filtered %>% select(all_of(c(!!id, !!category, !!distance, "dlabel_row_number"))), 
              by = c(id, category, distance)) %>%
    # apply ruthless kungfu power to enforce unique row numbers
    cleaner(ufn = c(id, category, distance, "dlabel_row_number"))
  
  # modifier according to invert
  modf <- function(x) !x # the default; !is.na(chunk), see below 
  if (invert) modf <- function(x) x # leave as is; is.na(chunk), see below
  
  # add dlabel column to df_filtered
  df_dlabel_0 <- df_filtered %>%
    left_join(df_records_dfltr, by = "dlabel_row_number") %>%
    mutate(!!label_name := modf(is.na(chunk))) %>%
    select(-chunk) %>% # remove chunk from dfilter
    select(-starts_with(distance)) # remove distance from dfilter (not needed anymore

  # rejoin to unfiltered df
  df_dlabel <- df %>%
    left_join(select(df_dlabel_0, all_of(c("dlabel_row_number", label_name))), by = "dlabel_row_number") %>%
    select(-dlabel_row_number) %>% # remove dlabel_row_number
    mutate(!!sym(label_name) := replace_na(!!sym(label_name), FALSE)) 

  # return bc_contam
  return(df_dlabel)
}

#' @rdname dlabel
#' @description `bc_contamination` calls \code{dlabel} with default parameters for the
#' calculation of contamination in blood cultures.
#'
#' @import dplyr
#' @import rlang
#' @import glue
#' @import tidyr
#' 
#' @export 
#' 
#' @examples 
#' set.seed(123)
#'
#' bugs <- data.frame(species = c("S. epidermidis", "C. acnes", "S. aureus", "E. coli"),
#'                   category = c("skin flora", "skin flora", "pathogen", "pathogen"))
#' 
#' samples <- data.frame(lab_no = 1:50,
#'                        patient = sample(1:10, 50, replace = TRUE),
#'                        species = sample(bugs$species, 50, replace = TRUE),
#'                        timestamp = as.POSIXct(runif(50, 1704063600, 1711000000), 
#'                                                origin = "1970-01-01"))
#' 
#' samples <- samples %>% left_join(bugs, by = "species")
#' 
#' bc_conta <- bc_contamination(samples, 
#'                             id = "patient", 
#'                             category = "species", 
#'                             distance = "timestamp",
#'                             df_filter = "category == 'skin flora'")
#' 
#' # Patient 9 has 5 cultures with skin flora, which, despite revealing
#' # skin flora, could correspond to infection (field contamination equals
#' # to FALSE), as these cultures satisfy the temporal distance criterion
#' # given by min.dist and max.dist.
#'
#' # check:
#'
#' # bc_conta %>% filter(category=="skin flora" & !contamination)
#' 
#' # The remaining samples yielding skin flora likely represent 
#' # contamination, as their temporal occurrence is outside the range
#' # given by min.dist and max.dist.
#' 
 
bc_contamination <- function(..., min.dist = 5, 
                            max.dist = 7 * 60 * 24, 
                            temporal.unit = "minutes",
                            label_name = "contamination",
                            invert = TRUE) {
    if (missing(...)) stop("Please supply arguments to dlabel()!")
    dlabel(..., min.dist = min.dist, max.dist = max.dist, 
           temporal.unit = temporal.unit, label_name = label_name, invert = invert)
}
