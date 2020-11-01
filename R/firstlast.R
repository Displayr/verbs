#' @param x to be completed
#' @param keep to be completed
#' @param by to be completed
#' @param calendar to be completed
#' @return to be completed
#' @importFrom flipU CopyAttributes
#' @export
First <- function(x, keep = 6L, by = "element", calendar = TRUE, ...)
{
    checkFirstLastInputs(x, keep, by)
    result <- if (by != "element")
        firstLastByPeriod(x, keep, by, calendar, TRUE)
    else
        head(x, keep, ...)
    result <- CopyAttributes(result, x)
    result
}

#' @export
Last <- function(x, keep = 6L, by = "elements", calendar = TRUE, ...)
{
    checkFirstLastInputs(x, keep, by)
    result <- if (by != "element")
        firstLastByPeriod(x, keep, by, calendar, FALSE)
    else
        head(x, keep, ...)
    result <- CopyAttributes(result, x)
    result
}

checkFirstLastInputs <- function(x, keep, by)
{
    dim.x <- dim(x)

    # Check 'keep'
    if (!is.atomic(keep) || is.array(keep) || !allNaturalNumbers(keep))
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
              length(by) != 1 && invalidMultivalueBy(by, keep))
        stop(by.msg.1D, " or a vector of these strings (and NA) corresponding ",
             "to the elements in 'keep'")
}

allowed.for.by <- c("element", "year", "quarter", "month", "week", "day")

invalidSingleValueBy <- function(by)
{
    !(by %in% allowed.for.by)
}

invalidMultiValueBy <- function(by, keep)
{
    !setequal(which(!is.na(keep)), which(!is.na(by))) ||
        !all(by %in% allowed.for.by)
}

# Are all numbers in 1,2,3,..., ignoring NA
allNaturalNumbers <- function(v)
{
    is.numeric(v) && all(is.na(v) | (round(v) == v & v > 0))
}

# Is valid duration, ignoring NA
validDuration <- function(x)
{
    valid.units <- c("year", "quarter", "month", "week", "day")
    pattern <- paste0("^[[:blank:]]*[[:digit:]]*[[:blank:]]*(",
                      paste0(valid.units, collapse = "|"),
                      ")s*[[:blank:]]*$")
    is.character(x) && all(is.na(x) | grepl(pattern, x, ignore.case = TRUE))
}

firstLastByPeriod <- function(x, keep, by, calendar, is.first)
{
    dim.x <- if (is.array(x)) dim(x) else length(x)
    n.dim <- length(dim.x)
    indices <- lapply(seq_len(n.dim), function(i) {
        if (i > length(keep) || is.na(keep[i]))
            seq_len(dim.x[i])
        nms <- if (is.array(x)) dimnames(x)[[i]] else names(x)
        date.times <- parseDateTime(nms, by[i], i, n.dim)

        period.indices <- periodIndices(date.times, by, calendar, is.first)
        if (is.first)
            which(period.indices <= min(period.indices) + keep[i] - 1)
        else
            which(period.indices >= max(period.indices) - keep[i] + 1)
    })
    do.call(`[`, c(list(x), indices))
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
    invalid.date.times <- date.time.strings[is.na(date.times)]
    n.invalid.date.times <- length(invalid.date.times)
    if (n.invalid.date.times > 0)
    {
        invalid.date.time.strings <- if (n.invalid.date.times > 5)
            paste0(c(invalid.date.times[1:5], "..."), collapse = ", ")
        else
            paste0(invalid.date.times, collapse = ", ")
        stop(common.msg, "labeled with ",
             ngettext(n.long.cases, "an invalid date: ", "invalid dates: "),
             invalid.date.time.strings, ".")
    }
    date.times
}

#' @noRd
periodIndices <- function(date.times, by, calendar, from.start)
{
    duration.function <- if (by == "year")
        durationInYears
    else if (by == "quarter")
        durationInQuarters
    else if (by == "month")
        durationInMonths
    else if (by == "week")
        durationInWeeks
    else if (by == "day")
        durationInDays
    else if (by == "hour")
        durationInHours
    else if (by == "minute")
        durationInMinutes
    else if (by == "second")
        durationInSeconds
    else # we do not expect user to see this error as 'by' is checked earlier
        stop("Unhandled 'by': ", by)

    if (calendar)
    {
        start.date <- structure(0, class = c("POSIXct", "POSIXt")) # unix epoch
        if (by == "week")
            start.date <- start.date + 3 * 24 * 60 *60 # add 3 days to unix epoch
        duration.function(start.date, date.times)
    }
    else if (from.start)
        duration.function(min(date.times), date.times)
    else
        # negation causes latest date to have the greatest integer
        -duration.function(date.times, max(date.times))
}

# to be moved to flipTime
#' @importFrom lubridate interval years
durationInYears <- function(start.date.time, end.date.time)
{
    interval(start.date.time, end.date.time) %/% years(1)
}

durationInQuarters <- function(start.date.time, end.date.time)
{
    floor(durationInMonths(start.date.time, end.date.time) / 3)
}

#' @importFrom lubridate months
durationInMonths <- function(start.date.time, end.date.time)
{
    interval(start.date.time, end.date.time) %/% months(1)
}

durationInWeeks <- function(start.date.time, end.date.time)
{
    floor(difftime(end.date.time, start.date.time, "weeks"))
}

durationInDays <- function(start.date.time, end.date.time)
{
    floor(difftime(end.date.time, start.date.time, "days"))
}

durationInHours <- function(start.date.time, end.date.time)
{
    floor(difftime(end.date.time, start.date.time, "hours"))
}

durationInMinutes <- function(start.date.time, end.date.time)
{
    floor(difftime(end.date.time, start.date.time, "mins"))
}

durationInSeconds <- function(start.date.time, end.date.time)
{
    floor(difftime(end.date.time, start.date.time, "secs"))
}
