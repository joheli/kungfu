% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cleaner.R
\name{cleaner}
\alias{cleaner}
\alias{is_clean}
\alias{cleaned}
\title{cleaner}
\usage{
cleaner(d, ufn, orderAlsoBy = character(), decr = FALSE)

is_clean(d, ufn = names(d))

cleaned(before, after)
}
\arguments{
\item{d}{a \code{data.frame}}

\item{ufn}{character, contains column names identifying unique entries (\code{ufn} is an abbreviation of "unique field names")}

\item{orderAlsoBy}{optional character, specifies column name that \code{d} is ordered by prior to cleaning}

\item{decr}{logical, specifying if \code{orderAlsoBy} is to be sorted in a decreasing fashion (defaults to \code{FALSE})}

\item{before}{a \code{data.frame}}

\item{after}{a \code{data.frame} that has been cleaned with \code{cleaner}}
}
\value{
\code{cleaner} returns a cleaned (i.e. deduplicated) \code{data.frame}

\code{is_clean} returns a logical indicating whether duplicate entries were found

\code{cleaned} returns a \code{data.frame} containing the entries in \code{before} that have been cleaned.
}
\description{
\code{cleaner} removes duplicate entries from a \code{data.frame}.

\code{is_clean} checks whether a \code{data.frame} has duplicate entries.

\code{cleaned} shows which rows have been cleaned from a \code{data.frame} with \code{cleaner}
}
\examples{
# clean heart rate data, only allow first measurement per person and condition
onlyfirst <- cleaner(heartrate, c("person", "condition"), "timestamp")
# alternatively, only allow last measurement
onlylast <- cleaner(heartrate, c("person", "condition"), "timestamp", TRUE)
# show whether 'heartrate' contains duplicates
is_clean(heartrate, c("person", "condition")) # returns false
# show entries cleaned from 'heartrate' compared to 'onlyfirst'
cleaned(heartrate, onlyfirst) # contains rows that were removed
}
