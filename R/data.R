#' airplanes
#'
#' A `tibble` representing a subset of `nycflights13` for the demonstration of function `pattern_join`
#'
#' @docType data
#'
#' @note It is created thus:
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
#' @note It is created thus:
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


