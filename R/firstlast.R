#' @rdname FirstAndLast
#' @title First and last elements in an object
#' @description Return first or last elements in an object, or if the data is
#'   named with dates and \code{unit} is a time period such as "year" or "month,
#'   the first or last elements within a specified number of time periods.
#' @param x An object such as a \code{vector}, \code{matrix}, \code{array}, \code{list} etc.
#' @param keep A scalar or vector of the number of first or last elements to
#'   keep, or if \code{unit} is a unit of time, the number initial or final time
#'   periods to consider when retaining elements. If \code{x} is multi-dimensional,
#'   e.g. a matrix, \code{keep} can be a vector whose entries correspond to
#'   the dimensions, which allows the first or last elements to be kept for
#'   multiple dimensions, e.g. both rows and columns. If all elements in a
#'   dimension are to be kept, the corresponding value for \code{keep} should
#'   be \code{NA}. If a value in keep is negative, the magnitude of the number
#'   indicates the number of elements/time periods to remove from the end (First)
#'   or the start (Last).
#' @param unit If \code{keep} is a scalar, a string that is either
#'   \code{"Row"}, \code{"Column"}, or a time unit: \code{"Year"},
#'   \code{"Quarter"}, \code{"Month"}, \code{"Week"}, \code{"Day"},
#'   \code{"Hour"}, \code{"Minute"}, \code{"Second"}. If \code{keep} is a
#'   vector, then \code{unit} is a string that is either \code{"Element"} or a
#'   time unit.
#' @param calendar A logical scalar that indicates whether to consider calendar
#'   periods when \code{unit} is a unit of time. See details for more information
#'   on calendar and non-calendar periods.
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
#'   If \code{x} is multidimensional, \code{unit} is a date period,
#'   \code{keep} is a scalar and \code{dimension} is not specified,
#'   then \code{First} and \code{Last} will apply to the first dimension that
#'   is labeled with dates.
#' @return A subset containing the first or last elements in x.
#' @examples
#'   First(1:10, 6) # 1:6
#'   x <- 1:10
#'   names(x) <- Sys.Date() + 1:10
#'   First(x, keep = 1, unit = "Week", calendar = FALSE) # next 7 days
#' @export
First <- function(x, keep = 1L, unit = NULL, calendar = TRUE, ...)
{
    firstLast(x, keep, unit, calendar, TRUE, ...)
}

#' @rdname FirstAndLast
#' @export
Last <- function(x, keep = 1L, unit = NULL, calendar = TRUE, ...)
{
    firstLast(x, keep, unit, calendar, FALSE, ...)
}

#' @importFrom flipU CopyAttributes
#' @importFrom utils head tail
firstLast <- function(x, keep, unit, calendar, is.first, ...)
{
    checkFirstLastInputs(x, keep, unit, calendar)
    # Possibly convert keep to a vector to specify which dimension to operate on
    if (!is.null(dim(x)) && !is.null(unit) && length(keep) == 1)
        keep <- scalarKeepToVector(x, keep, unit)
    result <- if (!is.null(unit) && unit %in% allowed.time.units)
        firstLastByPeriod(x, keep, unit, calendar, is.first, ...)
    else if (is.first)
        head(x, keep, ...)
    else
        tail(x, keep, ...)
    attr(result, "statistic") <- attr(x, "statistic")
    result
}

checkFirstLastInputs <- function(x, keep, unit, calendar)
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

    # Check 'unit'
    if (!is.null(unit))
    {
        if (length(keep) == 1)
        {
            if (is.null(dim(x)) && unit == "Column")
                warning("The unit 'Column' could not be applied as the data ",
                        "is 1-dimensional. Rows have been considered instead.")
            else if (is.null(dim(x)) && !(unit %in% allowed.units.vector))
                stop("The input 'unit' needs to be one of ",
                     paste0(paste0("'", allowed.units.vector, "'"), collapse = ", "),
                     " when the input is 1-dimensional.")
            else if (!is.null(dim(x)) && !(unit %in% allowed.units.with.dim))
                stop("The input 'unit' needs to be one of ",
                     paste0(paste0("'", allowed.units.with.dim, "'"), collapse = ", "),
                     ".")
        }
        else if (length(unit) > 1 || !(unit %in% allowed.units.multi.keep))
            stop("The input 'unit' needs to be one of ",
                 paste0(paste0("'", allowed.units.multi.keep, "'"), collapse = ", "),
                 ".")
    }

    # Check 'calendar'
    if (unit %in% allowed.time.units && !is.logical(calendar))
        stop("The input 'calendar' needs to be either TRUE or FALSE")
}

# Permitted time period units
allowed.time.units <- c("Year", "Quarter", "Month", "Week", "Day", "Hour",
                       "Minute", "Second")

# Permitted values for the 'unit' input
allowed.units.vector <- c("Row", allowed.time.units)
allowed.units.with.dim <- c("Row", "Column", allowed.time.units)
allowed.units.multi.keep <- c("Element", allowed.time.units)

# Are all numbers integers or NA
allIntegers <- function(v)
{
    is.numeric(v) && all(is.na(v) | round(v) == v)
}

scalarKeepToVector <- function(x, keep, unit)
{
    if (length(dim(x)) > 1 && unit == "Column")
        c(NA, keep)
    else if (unit %in% allowed.time.units)
        keepForDateDimension(x, keep, unit)
    else
        keep
}

# Modify the keep variable so that it is a vector that is not NA for the
# dimension containing dates
keepForDateDimension <- function(x, keep, unit)
{
    n.dim <- length(dim(x))
    date.dim <- which(vapply(seq_len(n.dim), function(i) {
        nms <- dimnames(x)[[i]]
        !is.null(nms) && !any(is.na(AsDateTime(nms, on.parse.failure = "")))
    }, logical(1)))
    if (length(date.dim) == 0)
        stop("The duration '", unit, "' cannot be applied as the input ",
             "data is not labeled with dates.")
    if (identical(date.dim, 1:2))
        warning("Both the rows and columns of the input data are labeled ",
                "with dates. The duration '", unit,
                "' will be applied to the dates in the row labels. For column ",
                "labels instead, set the row.or.columns parameter to 'columns'.")
    else if (length(date.dim) > 1)
        warning("Multiple dimensions of the input data are labeled with ",
                "dates. The duration '", unit,
                "' will be applied to the dates in dimension ", date.dim[1],
                ". Use the keep parameter to specify a different dimension.")
    original.keep <- keep
    keep <- rep(NA, n.dim)
    keep[date.dim[1]] <- original.keep
    keep
}

firstLastByPeriod <- function(x, keep, unit, calendar, is.first, ...)
{
    dim.x <- if (is.array(x)) dim(x) else length(x)
    n.dim <- length(dim.x)

    indices <- lapply(seq_len(n.dim), function(i) {
        if (i > length(keep) || is.na(keep[i]))
            return(seq_len(dim.x[i]))
        nms <- if (is.array(x)) dimnames(x)[[i]] else names(x)
        date.times <- parseDateTime(nms, unit, i, n.dim)

        period.integers <- periodIntegers(date.times, unit, calendar, is.first)
        ind <- indicesToKeep(period.integers, keep[i], is.first)
        ind[order(date.times[ind])] # return indices in ascending date order
    })
    do.call(`[`, c(list(x), indices, drop = FALSE))
}

#' @importFrom flipTime AsDateTime
parseDateTime <- function(date.time.strings, unit, dimension.index,
                          n.dimensions)
{
    if (is.null(date.time.strings))
        stop(dateLabelErrorPrefix(unit, dimension.index, n.dimensions),
             "not labeled with dates.")
    date.times <- AsDateTime(date.time.strings, on.parse.failure = "")
    if (any(is.na(date.times)))
        stop(dateLabelErrorPrefix(unit, dimension.index, n.dimensions),
             "not labeled with valid dates.")
    date.times
}

dateLabelErrorPrefix <- function(unit, dimension.index, n.dimensions)
{
    if (n.dimensions == 1)
        paste0("The duration '", unit,
               "' cannot be applied as the input data is ")
    else if (n.dimensions == 2)
    {
        dimension.label <- if (dimension.index == 1) "rows" else "columns"
        paste0("The duration '", unit,
               "' cannot be applied as the ", dimension.label,
               " in the input data are ")
    }
    else
        paste0("The duration '", unit,
               "' cannot be applied as dimension ", dimension.index,
               " in the input data is ")
}

# Represent periods in date.times as integers, where the smallest integer
# corresponds to the earliest period and vice versa. The parameter 'from.start'
# indicates whether to count periods from the first date as opposed to the last
# date when calendar is FALSE.
periodIntegers <- function(date.times, unit, calendar, from.start)
{
    if (calendar)
    {
        start.date <- structure(0, class = c("POSIXct", "POSIXt")) # unix epoch
        # Add 3 days to unix epoch so that it starts on a Sunday, so that we
        # count weeks starting from Sunday
        if (unit == "Week")
            start.date <- start.date + 3 * 24 * 60 *60
        intervalLength(start.date, date.times, unit)
    }
    else if (from.start)
        intervalLength(min(date.times), date.times, unit)
    else
        # negation causes latest date to have the greatest integer
        -intervalLength(date.times, max(date.times), unit)
}

# The number of time periods in units of 'unit' between the start and end dates
# as a integer (rounded down)
#' @importFrom lubridate interval time_length
intervalLength <- function(start.date.time, end.date.time, unit)
{
    if (unit == "Quarter")
        floor(intervalLength(start.date.time, end.date.time, "month") / 3)
    else
        floor(time_length(interval(start.date.time, end.date.time), tolower(unit)))
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
