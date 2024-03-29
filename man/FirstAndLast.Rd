% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/firstlast.R
\name{First}
\alias{First}
\alias{Last}
\title{First and last elements in an object}
\usage{
First(x, keep = 1L, unit = NULL, calendar = TRUE, ...)

Last(x, keep = 1L, unit = NULL, calendar = TRUE, ...)
}
\arguments{
\item{x}{An object such as a \code{vector}, \code{matrix}, \code{array}, \code{list} etc.}

\item{keep}{A scalar or vector of the number of first or last elements to
keep, or if \code{unit} is a unit of time, the number initial or final time
periods to consider when retaining elements. If \code{x} is multi-dimensional,
e.g. a matrix, \code{keep} can be a vector whose entries correspond to
the dimensions, which allows the first or last elements to be kept for
multiple dimensions, e.g. both rows and columns. If all elements in a
dimension are to be kept, the corresponding value for \code{keep} should
be \code{NA}. If a value in keep is negative, the magnitude of the number
indicates the number of elements/time periods to remove from the end (First)
or the start (Last).}

\item{unit}{If \code{keep} is a scalar, a string that is either
\code{"Row"}, \code{"Column"}, or a time unit: \code{"Year"},
\code{"Quarter"}, \code{"Month"}, \code{"Week"}, \code{"Day"},
\code{"Hour"}, \code{"Minute"}, \code{"Second"}. If \code{keep} is a
vector, then \code{unit} is a string that is either \code{"Element"} or a
time unit.}

\item{calendar}{A logical scalar that indicates whether to consider calendar
periods when \code{unit} is a unit of time. See details for more information
on calendar and non-calendar periods.}

\item{...}{Additional arguments for \code{head} and \code{tail}.}
}
\value{
A subset containing the first or last elements in x.
}
\description{
Return first or last elements in an object, or if the data is
  named with dates and \code{unit} is a time period such as "year" or "month",
  the first or last elements within a specified number of time periods.
}
\details{
When calendar periods are considered, a year is counted as the
  12-month period from January to December; a quarter is one of the
  following 3-month periods: January-March, April-June, July-September,
  October-December; a month is the period from the first to last day in the
  12 months in the calendar; a week is the 7 day period from Sunday to
  Saturday; a day is the 24 hour period from midnight to midnight; and hours,
  minutes and seconds are the fixed periods as indicated on a clock. For
  example, if the first 2 calendar years are to be kept and the earliest
  date in the data is Nov 6, 2020, all data labeled in 2020 and 2021 will be
  kept.

  When non-calendar periods are considered instead, all periods are compared
  against a reference date-time, which is the earliest date-time in the
  labels for \code{First}, and the latest date-time in the labels for
  \code{Last}. For periods that do not have a fixed duration, i.e., years,
  quarters and months, a period is defined using the day and month of the
  reference date-time. For example if the reference date-time is
  Nov 6 2020 12:34:56pm, and the first 2 years of data are to be kept,
  all data up to (but not including) Nov 6 2022 12:34:56pm would be retained.
  If the reference date-time falls on a day for which the corresponding
  end-of-period date-time does not exist, then all actual date-times up to
  this mock end-of-period date-time are considered. For example, 1 month
  from March 31 12:34:56pm includes all date-times up to and including
  April 30, and similarly, 1 year before Feb 29 2020 12:34:56pm includes all
  date-times down to and including March 1 2019.

  If \code{x} is multidimensional, \code{unit} is a date period,
  \code{keep} is a scalar and \code{dimension} is not specified,
  then \code{First} and \code{Last} will apply to the first dimension that
  is labeled with dates.
}
\examples{
  First(1:10, 6) # 1:6
  x <- 1:10
  names(x) <- Sys.Date() + 1:10
  First(x, keep = 1, unit = "Week", calendar = FALSE) # next 7 days
}
