#' @title First elements in an object
#' @description Return first elements in an object, when the number of
#'   initial elements can be defined in terms of the number of elements, or
#'   if the data is named with dates, the first elements within a specified
#'   number of time periods.
#' @param x An object such as a \code{vector}, \code{matrix}, \code{array}, \code{list} etc.
#' @param keep An \code{integer} of the number of first elements to keep, or
#'   if \code{by} is a unit of time, the number initial time periods to
#'   consider when retaining elements.
#' @param by A string that is either \code{"element"} or a unit of time:
#'   \code{"year"}, \code{"quarter"}, \code{"month"}, \code{"week"},
#'   \code{"day"}, \code{"hour"}, \code{"minute"}, \code{"second"}.
#' @param calendar Whether to consider calendar periods, e.g., with calendar months,
#'   actual months are counted, i.e., January, February etc. Otherwise months are
#'   counted as time periods of 28 to 31 days (based on the calendar months
#'   between start and end dates).
#' @return A subset containing the first elements in x
#' @examples
#'   First(1:10, 6) # 1:6
#'   x <- 1:10
#'   names(x) <- Sys.Date() + 1:10
#'   First(x, keep = 1, by = "week", calendar = FALSE) # next 7 days
#' @importFrom flipU CopyAttributes
#' @export
First <- function(x, keep = 6L, by = "element", calendar = TRUE, ...)
{
    checkFirstLastInputs(x, keep, by, calendar)
    result <- if (any(by %in% allowed.time.units))
        firstLastByPeriod(x, keep, by, calendar, TRUE, ...)
    else
        head(x, keep, ...)
    result <- CopyAttributes(result, x)
    result
}

#' @title Last elements in an object
#' @description Return last elements in an object, when the number of
#'   final elements can be defined in terms of the number of elements, or
#'   if the data is named with dates, the last elements within a specified
#'   number of time periods.
#' @param x An object such as a \code{vector}, \code{matrix}, \code{array}, \code{list} etc.
#' @param keep An \code{integer} of the number of last elements to keep, or
#'   if \code{by} is a unit of time, the number final time periods to
#'   consider when retaining elements.
#' @param by A string that is either \code{"element"} or a unit of time:
#'   \code{"year"}, \code{"quarter"}, \code{"month"}, \code{"week"},
#'   \code{"day"}, \code{"hour"}, \code{"minute"}, \code{"second"}.
#' @param calendar Whether to consider calendar periods, e.g., with calendar months,
#'   actual months are counted, i.e., January, February etc. Otherwise months are
#'   counted as time periods of 28 to 31 days (based on the calendar months
#'   between start and end dates).
#' @return A subset containing the last elements in x
#' @examples
#'   Last(1:10, 6) # 4:10
#'   x <- 1:10
#'   names(x) <- Sys.Date() - 1:10
#'   Last(x, keep = 1, by = "week", calendar = FALSE) # previous 7 days
#' @export
Last <- function(x, keep = 6L, by = "element", calendar = TRUE, ...)
{
    checkFirstLastInputs(x, keep, by, calendar)
    result <- if (any(by %in% allowed.time.units))
        firstLastByPeriod(x, keep, by, calendar, FALSE, ...)
    else
        tail(x, keep, ...)
    result <- CopyAttributes(result, x)
    result
}

checkFirstLastInputs <- function(x, keep, by, calendar)
{
    dim.x <- dim(x)

    # Check 'keep'
    if (!is.atomic(keep) || is.array(keep) || !allWholeNumbers(keep))
        stop("The input 'keep' needs to be an integer scalar or vector ",
             "containing the number of entries to keep corresponding to ",
             "the dimensions of the input data.")

    # Check lengths of x and 'keep'
    if ((is.null(dim.x) && length(keep) > 1))
        stop("The input 'keep' is a vector with more than one value. It needs to ",
             "be scalar when 'x' is non-dimensioned, e.g., when it is a ",
             "vector.")
    if ((is.null(dim.x) && length(keep) > 1) ||
        (!is.null(dim.x) && length(keep) > length(dim.x)))
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
    if (!is.logical(calendar))
        stop("The input 'calendar' needs to be either TRUE or FALSE")
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
    !setequal(which(!is.na(keep)), which(!is.na(by))) ||
        !all(by %in% c(NA, allowed.for.by))
}

# Are all numbers in 0,1,2,3,..., ignoring NA
allWholeNumbers <- function(v)
{
    is.numeric(v) && all(is.na(v) | (round(v) == v & v >= 0))
}

# Is valid duration, ignoring NA
validDuration <- function(x)
{
    pattern <- paste0("^[[:blank:]]*[[:digit:]]*[[:blank:]]*(",
                      paste0(allowed.time.units, collapse = "|"),
                      ")s*[[:blank:]]*$")
    is.character(x) && all(is.na(x) | grepl(pattern, x, ignore.case = TRUE))
}

firstLastByPeriod <- function(x, keep, by, calendar, is.first, ...)
{
    if (any(by %in% "element"))
    {
        is.time.units <- by %in% allowed.time.units
        keep.element <- keep
        keep.element[is.time.units] <- NA
        x <- if (is.first)
            First(x, keep.element, "element", ...)
        else
            Last(x, keep.element,"element", ...)
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
        ind <- if (is.first)
            which(period.integers <= min(period.integers) + keep[i] - 1)
        else
            which(period.integers >= max(period.integers) - keep[i] + 1)
        ind[order(date.times[ind])] # return indices in ascending date order
    })
    do.call(`[`, c(list(x), indices, drop = FALSE))
}

#' @importFrom flipTime AsDateTime
parseDateTime <- function(date.time.strings, by, dimension.index,
                          n.dimensions)
{
    common.msg <- if (n.dimensions == 1)
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

    if (is.null(date.time.strings))
        stop(common.msg, "not labeled with dates.")
    date.times <- AsDateTime(date.time.strings, on.parse.failure = "")
    if (any(is.na(date.times)))
        stop(common.msg, "labeled with invalid date(s).")
    date.times
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
        if (by == "week")
            start.date <- start.date + 3 * 24 * 60 *60 # add 3 days to unix epoch
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
