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
Last <- function(x, n.entries = 6L, by = "elements", calendar = TRUE, ...)
{
    checkFirstLastInputs(x, keep)
    result <- if (by != "element")
        firstLastByPeriod(x, keep, by, calendar, TRUE)
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
        dates <- parseDates(nms, keep[i], by[i], i, n.dim)

        period.indices <- periodIndices(dates, by, calendar)
        if (is.first)
            which(period.indices <= min(period.indices) + keep[i] - 1)
        else
            which(period.indices >= max(period.indices) - keep[i] + 1)
    })
    do.call(`[`, c(list(x), indices))
}

#' @importFrom flipTime AsDateTime
parseDates <- function(date.strings, by, dimension.index,
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

    if (is.null(date.strings))
        stop(common.msg, "not labeled with dates.")
    dates <- AsDateTime(date.strings, on.parse.failure = "")
    invalid.dates <- date.strings[is.na(dates)]
    n.invalid.dates <- length(invalid.dates)
    if (n.invalid.dates > 0)
    {
        invalid.dates.string <- if (n.valid.dates > 5)
            paste0(c(invalid.dates[1:5], "..."), collapse = ", ")
        else
            paste0(invalid.dates, collapse = ", ")
        stop(common.msg, "labeled with ",
             ngettext(n.long.cases, "an invalid date: ", "invalid dates: "),
             invalid.dates.string, ".")
    }
    dates
}

#' @noRd
periodIndices <- function(dates, by, calendar)
{
    if (by == "year")
        periodIndicesByYear(dates)
    else if (by == "quarter")
        year(dates) * 4 + quarter(dates)
    else if (by == "month")
        year(dates) * 12 + month(dates)
    else if (by == "week")
        weeksSinceEpoch(dates)
    else if (by == "day")
        daysSinceEpoch(dates)
    else # we do not expect user to see this error as 'by' is checked earlier
        stop("Unhandled 'by': ", by)
}

periodIndicesByYear <- function(dates, calendar)
{
    if (calendar)
        year(dates)
    else
        -vapply(dates, durationInYears, numeric(1), max(dates))
}

periodIndicesByQuarter <- function(dates, calendar)
{
    if (calendar)
        year(dates) * 4 + quarter(dates)
    else
    {

    }
}

periodIndicesByMonth <- function(dates, calendar)
{
    if (calendar)
        year(dates) * 12 + month(dates)
    else
    {

    }
}

periodIndicesByWeek <- function(dates, calendar)
{
    if (calendar)
        weeksSinceEpoch(dates)
    else
    {

    }
}

periodIndicesByDay <- function(dates, calendar)
{
    if (calendar)
        daysSinceEpoch(dates)
    else
    {

    }
}

daysSinceEpoch <- function(dates)
{
    floor(as.numeric(dates) / (24 * 60 * 60))
}

weeksSinceEpoch <- function(dates)
{
    # Add 4 so that we count weeks starting from Sunday
    floor((daysSinceEpoch(dates) + 4) / 7)
}

# to be moved to flipTime
#' @importFrom flipTime AsDateTime
#' @importFrom lubridate make_datetime year quarter month day
durationInYears <- function(start.date, end.date)
{
    if (start.date > end.date)
        stop("Start date cannot be after end date")

    result <- year(end.date) - year(start.date)

    # # Make a copy of the start date but with the year of the end date
    # dt <- make_datetime(year(end.date), month(start.date), day(start.date),
    #                     hour(start.date), minute(start.date), second(start.date))
    # # Happens if we ask for 29 Feb on a non leap year, in which case we
    # # rollover to the next valid date
    # if (is.na(dt))
    #     dt <- make_datetime(year(end.date), month(start.date) + 1, 1,
    #                         hour(start.date), minute(start.date), second(start.date))
    #
    # # Rounding down if
    # if (dt > end.date)
    #     result - 1


    result
}


compareInYear(date.1, date.2)
{
    order(c(month(start.date), month(end.date)),
          c(day(start.date), day(end.date)),
          c(hour(start.date), hour(end.date)),
          c(minute(start.date), minute(end.date)),
          c(second(start.date), second(end.date)))[1] == 1
}

durationInQuarters <- function(start.date, end.date)
{
    if (start.date > end.date)
        stop("Start date cannot be after end date")


}

