% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{heartrate}
\alias{heartrate}
\title{heart rate}
\format{
A data.frame with 100 rows and 4 columns:
\describe{
\item{person}{person identifier}
\item{condition}{fantasy condition a-e in which the person is in at the time of heart rate measurement}
\item{hearrate}{the heart rate of the person}
\item{timestamp}{time point of heart rate measurement}
}
}
\usage{
heartrate
}
\description{
Fantasy heart rate data for the demonstration of function \code{cleaner}.
}
\note{
created thus:

\if{html}{\out{<div class="sourceCode">}}\preformatted{set.seed(2)
heartrate <- data.frame(person = sample(1:100, size = 100, replace = TRUE),
                        condition = sample(letters[1:6], size = 100, replace = TRUE),
                        heartrate = round(rnorm(100, 80, 10)),
                        timestamp = seq.POSIXt(from = as.POSIXct("2022-08-24 08:00"),
                                               to = as.POSIXct("2022-08-24 10:00"),
                                               length.out = 100))
}\if{html}{\out{</div>}}
}
\keyword{datasets}
