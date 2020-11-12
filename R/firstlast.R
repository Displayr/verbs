#' @rdname FirstAndLast
#' @title First and last elements in an object
#' @description Return first or last elements in an object, or if the data is
#'   named with dates and \code{by} is a time period such as "year" or "month,
#'   the first or last elements within a specified number of time periods.
#' @param x An object such as a \code{vector}, \code{matrix}, \code{array}, \code{list} etc.
#' @param keep A scalar or vector of the number of first or last elements to
#'   keep, or if \code{by} is a unit of time, the number initial or final time
#'   periods to consider when retaining elements. If \code{x} is multi-dimensional,
#'   e.g. a matrix, \code{keep} can be a vector whose entries correspond to
#'   the dimensions, which allows the first or last elements to be kept for
#'   multiple dimensions, e.g. both rows and columns. If all elements in a
#'   dimension are to be kept, the corresponding value for \code{keep} should
#'   be \code{NA}. If a value in keep is negative, the magnitude of the number
#'   indicates the number of elements/time periods to remove from the end (First)
#'   or the start (Last).
#' @param by A scalar or vector that contains either \code{"element"}
#'   or a unit of time: \code{"year"}, \code{"quarter"}, \code{"month"},
#'   \code{"week"}, \code{"day"}, \code{"hour"}, \code{"minute"},
#'   \code{"second"}. If \code{keep} is a vector, then \code{by} can also be a
#'   vector with elements corresponding to keep, allowing different dimensions
#'   to have different values specified.
#' @param calendar A logical scalar that indicates whether to consider calendar
#'   periods when \code{by} is a unit of time. See details for more information
#'   on calendar and non-calendar periods.
#' @param dimension A string with values "Automatic", "Rows" or "Columns"
#'   which indicate whether to apply the function to rows or columns, or let
#'   the function automatically determine which dimension to use.
#' @param ... Additional arguments for \code{head} and \code{tail}.
#' @details When calendar periods are considered, a year is counted as the
#'   12-month period from January to December; a quarter is one of the
#'   following 3-month periods: January-March, April-June, July-September,
#'   October-December; a month is the period from the first to last day in the
#'   12 months in the calendar; a week is the 7 day period from Sunday to
#'   Saturday; a day is the 24 hour period from midnight to midnight; and hours,
#'   minutes and seconds are the fixed periods as indicated on a clock. For
#'   example, if the first 2 calendar years are to be kept and the earliest
#'   date in the data is Nov 6, 2020, all data labeled in 2020 and 2021 will be
#'   kept.
#'
#'   When non-calendar periods are considered instead, all periods are compared
#'   against a reference date-time, which is the earliest date-time in the
#'   labels for \code{First}, and the latest date-time in the labels for
#'   \code{Last}. For periods that do not have a fixed duration, i.e., years,
#'   quarters and months, a period is defined using the day and month of the
#'   reference date-time. For example if the reference date-time is
#'   Nov 6 2020 12:34:56pm, and the first 2 years of data are to be kept,
#'   all data up to (but not including) Nov 6 2022 12:34:56pm would be retained.
#'   If the reference date-time falls on a day for which the corresponding
#'   end-of-period date-time does not exist, then all actual date-times up to
#'   this mock end-of-period date-time are considered. For example, 1 month
#'   from March 31 12:34:56pm includes all date-times up to and including
#'   April 30, and similarly, 1 year before Feb 29 2020 12:34:56pm includes all
#'   date-times down to and including March 1 2020.
#'
#'   If \code{x} is multidimensional, \code{by} is a date period,
#'   \code{keep} is a scalar and \code{dimension} is not specified,
#'   then \code{First} and \code{Last} will apply to the first dimension that
#'   is labeled with dates.
#' @return A subset containing the first or last elements in x.
#' @examples
#'   First(1:10, 6) # 1:6
#'   x <- 1:10
#'   names(x) <- Sys.Date() + 1:10
#'   First(x, keep = 1, by = "week", calendar = FALSE) # next 7 days
#' @importFrom flipU CopyAttributes
#' @importFrom utils head
#' @export
First <- function(x, keep = 6L, by = "element", calendar = TRUE,
                  dimension = "Automatic", ...)
{
    firstLast(x, keep, by, calendar, dimension, TRUE, ...)
}

#' @rdname FirstAndLast
#' @importFrom utils tail
#' @export
Last <- function(x, keep = 6L, by = "element", calendar = TRUE,
                 dimension = "Automatic", ...)
{
    firstLast(x, keep, by, calendar, dimension, FALSE, ...)
}

firstLast <- function(x, keep, by, calendar, dimension,
                      is.first, ...)
{
    by <- tolower(by)
    checkFirstLastInputs(x, keep, by, calendar, dimension)
    keep <- updateKeepWithDimensions(x, keep, by, dimension)
    result <- if (any(by %in% allowed.time.units))
        firstLastByPeriod(x, keep, by, calendar, is.first, ...)
    else if (is.first)
        head(x, keep, ...)
    else
        tail(x, keep, ...)
    result
}

checkFirstLastInputs <- function(x, keep, by, calendar, dimension)
{
    dim.x <- dim(x)

    # Check 'keep'
    if (!is.atomic(keep) || is.array(keep) || !allIntegers(keep))
        stop("The input 'keep' needs to be an integer scalar or vector ",
             "containing the number of entries to keep corresponding to ",
             "the dimensions of the input data.")

    # Check lengths of x and 'keep'
    if ((is.null(dim.x) && length(keep) > 1))
        stop("The input 'keep' is a vector with more than one value. It needs to ",
             "be a scalar when 'x' is a vector.")
    if (!is.null(dim.x) && length(keep) > length(dim.x))
        stop("The input 'keep' is a vector with length greater than the number of ",
             "dimensions of 'x'. Its length needs to be less than or equal to ",
             "the number of dimensions of 'x'.")

    if (length(keep) > 1 && all(is.na(keep)))
        stop("The input 'keep' cannot have values that are all missing.")

    # Check 'by'
    by.msg.1D <- paste0("The input 'by' needs to be one of ",
                        paste0(paste0("'", allowed.for.by, "'"),
                               collapse = ", "))

    if (is.null(dim.x) || length(keep) == 1)
    {
        if (length(by) != 1 || invalidSingleValueBy(by))
            stop(by.msg.1D, ".")
    }
    else if ((length(by) == 1 && invalidSingleValueBy(by)) ||
              length(by) != 1 && invalidMultiValueBy(by, keep))
        stop(by.msg.1D, " or a vector of these strings (and NA) corresponding ",
             "to the elements in 'keep'")

    # Check 'calendar'
    if (by %in% allowed.time.units && !is.logical(calendar))
        stop("The input 'calendar' needs to be either TRUE or FALSE")

    # Check 'dimension'
    if (!(dimension %in% c("Automatic", "Rows", "Columns")))
        stop("The dimension input needs to be either 'Automatic', 'Rows' or ",
             "'Columns'.")
    if ((is.null(dim.x) || length(dim.x) == 1)
        && dimension != "Automatic")
        warning("The dimension input has been ignored as the input ",
                "data is 1-dimensional.")
    if (length(keep) > 1 && dimension != "Automatic")
        stop("The input keep needs to be a scalar when the ",
             "dimension input is not 'Automatic'.")
}

# Permitted time period units
allowed.time.units <- c("year", "quarter", "month", "week", "day", "hour",
                        "minute", "second")

# Permitted values for the 'by' input
allowed.for.by <- c("element", allowed.time.units)

# Check that 'by' input value is permitted
invalidSingleValueBy <- function(by)
{
    !(by %in% allowed.for.by)
}

# Check that non-missing values in 'keep' and 'by' coincide, and that 'by'
# contains permitted values
invalidMultiValueBy <- function(by, keep)
{
    !identical(which(!is.na(keep)), which(!is.na(by))) ||
        !all(by %in% c(NA, allowed.for.by))
}

# Are all numbers integers or NA
allIntegers <- function(v)
{
    is.numeric(v) && all(is.na(v) | round(v) == v)
}

isData1D <- function(x)
{
    is.null(dim(x)) || length(dim(x)) == 1
}

updateKeepWithDimensions <- function(x, keep, by, dimension)
{
    dim.x <- dim(x)
    n.dim <- ifelse(is.null(dim.x), 1, length(dim(x)))
    if (n.dim == 1 || length(keep) > 1)
        keep
    else if (dimension == "Automatic")
    {
        if (by %in% allowed.time.units)
            keepForDateDimension(x, keep, by)
        else # choose first dimension that has length > 1
        {
            ind <- which(dim.x > 1)
            if (length(ind) > 0)
                c(rep(NA, ind[1] - 1), keep)
            else
                keep
        }
    }
    else if (dimension == "Columns")
    {
        if (!isData1D(x))
            c(NA, keep)
        else
            keep
    }
    else # dimension is 'Rows'
        keep
}

# Modify the keep variable so that it is a vector that is not NA for the
# dimension containing dates
keepForDateDimension <- function(x, keep, by)
{
    n.dim <- length(dim(x))
    date.dim <- which(vapply(seq_len(n.dim), function(i) {
        nms <- dimnames(x)[[i]]
        !is.null(nms) && !any(is.na(AsDateTime(nms)))
    }, logical(1)))
    if (length(date.dim) == 0)
        stop("The duration '", by, "' cannot be applied as the input ",
             "data is not labeled with dates.")
    if (identical(date.dim, 1:2))
        warning("Both the rows and columns of the input data are labeled ",
                "with dates. The duration '", by,
                "' will be applied to the dates in the row labels. For column ",
                "labels instead, set the row.or.columns parameter to 'columns'.")
    else if (length(date.dim) > 1)
        warning("Multiple dimensions of the input data are labeled with ",
                "dates. The duration '", by,
                "' will be applied to the dates in dimension ", date.dim[1],
                ". Use the keep parameter to specify a different dimension.")
    original.keep <- keep
    keep <- rep(NA, n.dim)
    keep[date.dim[1]] <- original.keep
    keep
}

firstLastByPeriod <- function(x, keep, by, calendar, is.first, ...)
{
    # If 'by' contains "element" entries, apply first/last on those dimensions
    # first before considering the date-time dimensions
    if (any(by %in% "element"))
    {
        x <- firstLastElements(x, keep, by, is.first, ...)
        is.time.units <- by %in% allowed.time.units
        keep[!is.time.units] <- NA
        by[!is.time.units] <- NA
    }

    dim.x <- if (is.array(x)) dim(x) else length(x)
    n.dim <- length(dim.x)

    if (length(by) == 1)
        by <- rep(by, n.dim)

    indices <- lapply(seq_len(n.dim), function(i) {
        if (i > length(keep) || is.na(keep[i]))
            return(seq_len(dim.x[i]))
        nms <- if (is.array(x)) dimnames(x)[[i]] else names(x)
        date.times <- parseDateTime(nms, by[i], i, n.dim)

        period.integers <- periodIntegers(date.times, by[i], calendar, is.first)
        ind <- indicesToKeep(period.integers, keep[i], is.first)
        ind[order(date.times[ind])] # return indices in ascending date order
    })
    do.call(`[`, c(list(x), indices, drop = FALSE))
}

firstLastElements <- function(x, keep, by, is.first, ...)
{
    is.time.units <- by %in% allowed.time.units
    keep.element <- keep
    keep.element[is.time.units] <- NA
    if (is.first)
        First(x, keep.element, "element", ...)
    else
        Last(x, keep.element, "element", ...)
}

#' @importFrom flipTime AsDateTime
parseDateTime <- function(date.time.strings, by, dimension.index,
                          n.dimensions)
{
    if (is.null(date.time.strings))
        stop(dateLabelErrorPrefix(by, dimension.index, n.dimensions),
             "not labeled with dates.")
    date.times <- AsDateTime(date.time.strings, on.parse.failure = "")
    if (any(is.na(date.times)))
        stop(dateLabelErrorPrefix(by, dimension.index, n.dimensions),
             "labeled with invalid date(s).")
    date.times
}

dateLabelErrorPrefix <- function(by, dimension.index, n.dimensions)
{
    if (n.dimensions == 1)
        paste0("The duration '", by,
               "' cannot be applied as the input data is ")
    else if (n.dimensions == 2)
    {
        dimension.label <- if (dimension.index == 1) "rows" else "columns"
        paste0("The duration '", by,
               "' cannot be applied as the ", dimension.label,
               " in the input data are ")
    }
    else
        paste0("The duration '", by,
               "' cannot be applied as dimension ", dimension.index,
               " in the input data is ")
}

# Represent periods in date.times as integers, where the smallest integer
# corresponds to the earliest period and vice versa. The parameter 'from.start'
# indicates whether to count periods from the first date as opposed to the last
# date when calendar is FALSE.
periodIntegers <- function(date.times, by, calendar, from.start)
{
    if (calendar)
    {
        start.date <- structure(0, class = c("POSIXct", "POSIXt")) # unix epoch
        # Add 3 days to unix epoch so that it starts on a Sunday, so that we
        # count weeks starting from Sunday
        if (by == "week")
            start.date <- start.date + 3 * 24 * 60 *60
        intervalLength(start.date, date.times, by)
    }
    else if (from.start)
        intervalLength(min(date.times), date.times, by)
    else
        # negation causes latest date to have the greatest integer
        -intervalLength(date.times, max(date.times), by)
}

# The number of time periods in units of 'by' between the start and end dates
# as a integer (rounded down)
#' @importFrom lubridate interval time_length
intervalLength <- function(start.date.time, end.date.time, by)
{
    if (by == "quarter")
        floor(intervalLength(start.date.time, end.date.time, "month") / 3)
    else
        floor(time_length(interval(start.date.time, end.date.time), by))
}

# The indices of the period integers to keep
indicesToKeep <- function(period.integers, keep, is.first)
{
    if (keep == 0)
        integer(0)
    else if (keep > 0)
    {
        if (is.first)
            which(period.integers < min(period.integers) + keep)
        else
            which(period.integers > max(period.integers) - keep)
    }
    else # keep < 0
    {
        if (is.first)
            which(period.integers <= max(period.integers) - abs(keep))
        else
            which(period.integers >= min(period.integers) + abs(keep))
    }
}
