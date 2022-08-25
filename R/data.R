#' airplanes
#'
#' A `tibble` representing a subset of `nycflights13` for the demonstration of function `pattern_join`
#'
#' @docType data
#'
#' @note created thus:
#'
#' ```
#' # set seed
#' set.seed(1)
#' # extract twenty rows of nycflights13::planes
#' airplanes <- nycflights13::planes %>% slice(sample(1:nrow(nycflights13::planes), 20))
#' ```
#'
#' @format A data.frame with 20 rows and 9 columns:
#' \describe{
#'   \item{tailnum}{tail number}
#'   \item{year}{year}
#'   \item{type}{type}
#'   \item{manufacturer}{manufacturer}
#'   \item{model}{model}
#'   \item{engines}{number of engines}
#'   \item{seats}{number of seats}
#'   \item{speed}{speed}
#'   \item{engine}{engine}
#' }
"airplanes"

#' model_type
#'
#' A `tibble` representing a pattern map for the demonstration of function `pattern_join`
#'
#' @docType data
#'
#' @note created thus:
#'
#' ```
#' model_type <- tibble(pattern = c("PA-32", "EMB-145", "7[2-8]7", "A320"),
#'                      model_type = c("Piper 32", "Embraer 145", "Boeing 7X7", "Airbus 320"))
#' ```
#' @format A data.frame with 4 rows and 2 columns:
#' \describe{
#'   \item{pattern}{regex pattern}
#'   \item{model_type}{airplane model type}
#' }
"model_type"

#' heart rate
#'
#' Fantasy heart rate data for the demonstration of function `cleaner`.
#'
#' @docType data
#'
#' @note created thus:
#' ```
#' set.seed(2)
#' heartrate <- data.frame(person = sample(1:100, size = 100, replace = TRUE),
#'                         condition = sample(letters[1:6], size = 100, replace = TRUE),
#'                         heartrate = round(rnorm(100, 80, 10)),
#'                         timestamp = seq.POSIXt(from = as.POSIXct("2022-08-24 08:00"),
#'                                                to = as.POSIXct("2022-08-24 10:00"),
#'                                                length.out = 100))
#' ```
#' @format A data.frame with 100 rows and 4 columns:
#' \describe{
#'   \item{person}{person identifier}
#'   \item{condition}{fantasy condition a-e in which the person is in at the time of heart rate measurement}
#'   \item{hearrate}{the heart rate of the person}
#'   \item{timestamp}{time point of heart rate measurement}
#' }
"heartrate"
