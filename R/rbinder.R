# # create test data
# set.seed(1)
# .dts <- seq.Date(as.Date("2002-01-01"), as.Date("2002-12-31"), 1) # sequence of dates
# fakedata <- function(start, rows) {
#   data.frame(id = start:(start + rows - 1), group = sample(letters[1:4], rows, replace = TRUE),
#              date = sample(.dts, rows, replace = TRUE),
#              car = sample(rownames(mtcars), rows, replace = TRUE))
# }
# test1 <- fakedata(1,10)
# test2 <- fakedata(7, 12)
# test3 <- fakedata(16, 10)
#
# write.csv(test1, "inst/extdata/test1.csv", row.names = FALSE)
# write.csv(test2, "inst/extdata/test2.csv", row.names = FALSE)
# write.csv(test3, "inst/extdata/test3.csv", row.names = FALSE)

#' rbinder
#'
#' Batch read and unite multiple data files into a single `data.frame`.
#'
#' @param file.pattern character; specifies regex pattern of file names to be processed.
#' @param readf function; specifies function (which has to return a `data.frame`) used for reading in of data
#' (defaults to `read.csv2`)
#' @param path character; specifies directory from which to collect files corresponding to `file.pattern`
#' (defaults to ".").
#' @param unique.field.name character; column names designating unique entries that are not to be duplicated
#' after uniting of files; use to avoid duplicates introduced by reading in of files with same content.
#' @param result character; increases verbosity ("summary") or additionally persists intermediary results to file `debug.rds` ("debug").
#' @param ... arguments passed to function `readf`.
#'
#' @import utils
#'
#' @return a united `data.frame` with unique entries not duplicated despite possible multiple occurrence in files
#' @seealso [df_pattern_subset()] for subsetting a \code{data.frame}, useful for creating custom readers, see examples
#' @examples
#' # - Folder 'extdata' (system.file("extdata", package = "kungfu") contains three csv-files:
#' #> dir(system.file("extdata", package = "kungfu"), pattern = "csv")
#' #[1] "data01.csv" "data02.csv" "data03.csv"
#' #
#' # - To join them, run:
#' data_combined <- rbinder("^data", read.csv, path = system.file("extdata", package = "kungfu"), unique.field.name = "id")
#' #
#' # - You can also create your own custom "dirty reader" and supply that to "rbinder"
#' # - (Please see "?df_pattern_subset" for information regarding that function)
#' # - e.g.:
#' # my_dirty_excel_reader <- function(path) {
#' #    read_xlsx(path) %>%
#' #    df_pattern_subset("^mySpecial.*Pattern$", ignore_columns = TRUE) %>%
#' #    select(id, size, -timestamp, etc, anothercolumn)
#' # }
#' @export
#'
rbinder <- function(file.pattern,
                    readf = read.csv2,
                    path = ".",
                    unique.field.name,
                    result = c("default", "summary", "debug"),
                    ...) {
  result = match.arg(result)

  # step1: a list of data.frames, read in using 'readf' on files matching 'file.pattern'
  step1 <- batchread(file.pattern,
                     readf,
                     path,
                     ...)

  # step2: a combined data.frame created from the list supplied through step1
  step2 <- rbinder2(
    df.list = step1,
    unique.field.name = unique.field.name,
    verbose = result != "default"
  )
  rtn <- step2$rbound

  # add info depending on 'result'
  if (result != "default") {
    cat(step2$summary_sentence)
    print(step2$summary, row.names = FALSE)

    if (result == "debug")
      saveRDS(step2, file = "debug.rds")
  }

  return(rtn)
}

# Helper function to read files into a list of data.frames
batchread <- function (file.pattern,
                       readf = read.csv2,
                       path = ".",
                       ...) {
  # file.names: a vector of matching file names
  file.names <-
    dir(path = path,
        pattern = file.pattern,
        full.names = TRUE)
  # Stop if no matching files found
  if (length(file.names) < 1)
    stop(paste0("No files matching the pattern '", file.pattern, "' found!"))
  # create list of data.frames
  df.list <- purrr::map(file.names, readf, ...)
  names(df.list) <- fs::path_file(file.names)
  # return
  return(df.list)
}

#' cleaner
#'
#' This function "cleans" data.frames, i.e. removes duplicate entries.
#'
#' @param d a `data.frame`
#' @param ufn character, contains column names identifying unique entries
#' @param orderAlsoBy optional character, contains column name(s) that `d` is ordered by prior to cleaning
#'
#' @return a cleaned (i.e. deduplicated) `data.frame`
#' @export
#'
#' @examples
cleaner <- function(d, ufn, orderAlsoBy = character()) {
  ufn_ <- c(ufn, orderAlsoBy)
  d %>% arrange(across(all_of(ufn_))) %>%
    group_by(across(all_of(ufn))) %>%
    slice(n = 1)
}

key <- function(d, ufn = NULL) {
  if (is.null(ufn))
    ufn = names(d)
  d %>% as.data.frame() %>% # a tibble has to be converted to a data.frame!
    select(all_of(ufn)) %>%
    mutate(across(everything(), as.character)) %>%
    rowwise() %>% mutate(k = paste(c_across(), collapse = "")) %>%
    ungroup() %>% pull(k)
}

key2 <- function(d, ufn = NULL) {
  #d <- as.data.frame(d)
  if (is.null(ufn)) ufn <- names(d)
  ds <- d[,ufn]
  trimws(apply(ds, 1, paste, collapse = ""))
}

cleaned <- function(before, after) {
  k.before <- key2(before)
  k.after <- key2(after)
  i <-
    !(k.before %in% k.after) |
    duplicated(k.before) # warning: assumption!
  # it is assumed, that 'after' was cleaned with cleaner(), i.e. duplicates were removed!
  # this assumption (i.e. that duplicates were removed) does not hold for other functions
  # this function is not agnostic of the function performing the cleaning
  before[i,]
}

excluded <- function(d, reference, ufn) {
  k1 <- key2(d, ufn = ufn)
  k2 <- key2(reference, ufn = ufn)
  is <- intersect(k1, k2)
  i <- (k1 %in% is) | duplicated(k1) # warning: assumption!
  # it is assumed, that duplicates were in fact excluded; see comment in function cleaned().
  d[i,]
}

combine_unique <- function(a, b, ufn) {
  i <- !(key2(a, ufn = ufn) %in% key2(b, ufn = ufn))
  rbind(a[i,], b)
}

# Helper function to join list of data.frames into a single data.frame
rbinder2 <- function(df.list,
                     unique.field.name,
                     verbose = FALSE) {
  # stop if df.list is empty
  if (length(df.list) < 1)
    stop(paste0(deparse(substitute(df.list)), " appears to be empty!"))

  # initialize optional objects depending on 'verbose'
  smmr_df <-
    df.list.cleaned <-
    df.excluded <- number_of_rows_comb <- summary_sentence <- NULL

  # first, clean each data.frame, i.e. exclude duplicates
  # remember: 'df.list.clean' is essential, 'df.list.cleaned' is not
  df.list.clean <-
    purrr::map(df.list, cleaner, ufn = unique.field.name)
  len.dlc <- length(df.list.clean)
  # for transparency, save cleaned out rows
  if (verbose)
    df.list.cleaned <- purrr::map2(df.list, df.list.clean, cleaned)

  # second, reduce df.clean.list to single data.frame, successively excluding already present entries
  df.combined <-
    purrr::reduce(df.list.clean, combine_unique, ufn = unique.field.name) %>% ungroup # I don't understand why grouping is introduced in the first place - irritating - now removed

  # assemble a list of excluded entries
  if (verbose)
    df.excluded <-
    purrr::map2(df.list.clean[2:len.dlc], df.list.clean[1:(len.dlc - 1)],
                excluded, ufn = unique.field.name)

  # summary data (only if 'verbose' is TRUE)
  if (verbose) {
    names_df0 <- names(df.list)
    names_df <- names_df0
    if (is.null(names_df0))
      names_df <- 1:length(df.list)
    number_of_rows_pre <- unlist(purrr::map(df.list, nrow))
    sum_rows_pre <- sum(number_of_rows_pre)
    number_of_rows_cleaned <-
      unlist(purrr::map(df.list.cleaned, nrow))
    sum_rows_cleaned <- sum(number_of_rows_cleaned)
    number_of_rows_excluded <- unlist(purrr::map(df.excluded, nrow))
    sum_rows_excluded <- sum(number_of_rows_excluded)
    sum_filtered <- sum_rows_cleaned + sum_rows_excluded
    number_of_rows_comb <- nrow(df.combined)
    smmr_df <- data.frame(
      names_df,
      number_of_rows_pre,
      number_of_rows_cleaned,
      c(0, number_of_rows_excluded)
    )
    summary_sentence <-
      paste0(
        "\nS u m m a r y\n\n",
        sum_rows_pre,
        " rows from ",
        length(df.list),
        " files",
        " have been read in. The combined data.frame retained ",
        number_of_rows_comb,
        " rows.\n\nIn total, ",
        sum_filtered,
        " rows of the original tables have been denied inclusion into the combined data.frame. ",
        "\nOf those, ",
        sum_rows_cleaned,
        " rows have been 'cleaned', i.e. filtered out due to ",
        "being duplicates of entries within the same file, and ",
        sum_rows_excluded,
        " rows have been 'excluded', i.e. filtered out due to being duplicates of ",
        "entries previously added from preceding files.\nPlease see the summary table ",
        "below for a more complete breakdown:\n\n"
      )
    names(smmr_df) <-
      c("table name",
        "initial number of rows",
        "rows cleaned",
        "rows excluded")
  }


  # return
  list(
    rbound = df.combined,
    summary = smmr_df,
    cleaned = df.list.cleaned,
    excluded = df.excluded,
    nrow_combined = number_of_rows_comb,
    summary_sentence = summary_sentence
  )
}
