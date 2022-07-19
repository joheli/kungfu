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
#' @param result character; only used for debugging (in that case, pass "debug").
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
                          result = c("default",
                                     "debug"),
                          ...) {
  result = match.arg(result)
  step1 <- batchread(file.pattern,
                           readf,
                           path,
                           ...)
  step2 <- rbinder2(step1, unique.field.name)
  rtn <- step2
  if (result == "default") rtn <- step2[[1]]
  return(rtn)
}

# Helper function to read files into a list of data.frames
batchread <- function (file.pattern,
                             readf = read.csv2,
                             path = ".",
                             ...) {
  # file.names: a vector of matching file names
  file.names <- dir(path, file.pattern)
  # Stop if no matching files found
  if (length(file.names) < 1) stop(paste0("No files matching the pattern '", file.pattern, "' found!"))
  # create list to save data.frames
  df.list <- list()
  # loop, read in files to df.list
  for (i in 1:length(file.names)) {
    f.name <- file.names[i]
    f.path <- f.name
    if (path != ".") f.path <- fs::path(path, f.name)
    d <- readf(f.path, ...)
    df.list[[f.name]] <- d
  }
  return(df.list)
}

# Helper function to join list of data.frames into a single data.frame
rbinder2 <- function(df.list,
                     unique.field.name) {
  # stop if df.list is empty
  if (length(df.list) < 1)
    stop(paste0(deparse(substitute(df.list)), " appears to be empty!"))
  # stop if df.list contents are not named
  if (length(names(df.list)) < 1)
    stop(paste0("Contents of ", deparse(substitute(df.list)), "are not named!"))
  # stop if names of df.list contains NA
  if (any(is.na(names(df.list))))
    stop(paste0(deparse(substitute(df.list)), " has unnamed entries!"))

  # d is the dataframe returned at the end
  d <- data.frame()

  # nrows holds the number of rows contained within each assessed data.frame
  nrows <- numeric()

  # uniquerows lists the number of unique rows contributed by each assessed data.frame
  uniquerows  <- numeric()

  # function for temporary key generation
  tk <- function(df, u)
    return(eval(parse(text = paste0("with(df, paste(", paste(u, collapse = ", "), "))"))))

  # loop through the list of data.frames
  for (df.name in names(df.list)) {
    df.current <- df.list[[df.name]]      # current data.frame in loop
    nrows  <- c(nrows, nrow(df.current))  # how much rows does the current data.frame have?
    if (nrow(d) == 0) {
      # if d is empty (at start), set d to df.current
      d <- df.current
      uniquerows <- nrows
    } else {
      # if d is not empty (after first iteration) reduce df.current to rows that are unique
      df.current <- df.current[!(tk(df.current, unique.field.name) %in% tk(d, unique.field.name)), ]
      uniquerows <- c(uniquerows, nrow(df.current))
      d <- rbind(d, df.current)
    }
  }
  report <- data.frame(`file name` = names(df.list), `number of rows` = nrows, `unique rows` = uniquerows)
  report  <- rbind(report,
                   data.frame(
                     `file name` = "merged data.frame",
                     `number of rows` = nrow(d),
                     `unique rows` = length(unique(d[, unique.field.name]))
                   ))
  return(list(`merged data.frame` = d, report = report))
}
