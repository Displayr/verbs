% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities.R
\name{ValidateFilterForEachColumnVariants}
\alias{ValidateFilterForEachColumnVariants}
\title{Manage the filter variable for the "Each Column" variants of
the verbs functions.}
\usage{
ValidateFilterForEachColumnVariants(data, filter)
}
\arguments{
\item{data}{The table to be filtered.}

\item{filter}{The filter to be checked against table.}
}
\description{
This function is intended to be used on the Standard R pages
for the verbs "Each Column" variants, e.g. AverageEachColumn.
While the user is permitted to supply a filter to these tools,
the filter only ever makes sense if the input table is a raw
data table from the same data set as the filter.
This function intercepts the filter, and if it does not have
the same number of cases as the table then it warns the user
and returns a value of NULL so that the verbs function does
not attempt to apply the filter.
}
