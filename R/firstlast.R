#' @param data to be completed
#' @param n.entries to be completed
#' @return to be completed
#' @export
First <- function(data, n.entries = 6L, ...)
{
    checkFirstLastInputs(data, n.entries)
    if (validDuration(n.entries))
        firstLastInDuration(data, n.entries, TRUE)
    else
        head(data, n.entries, ...)
}

#' @export
Last <- function(data, n.entries = 6L, ...)
{
    checkFirstLastInputs(data, n.entries)
    if (validDuration(n.entries))
        firstLastInDuration(data, n.entries, TRUE)
    else
        head(data, n.entries, ...)
}

checkFirstLastInputs <- function(data, n.entries)
{
    if (!is.atomic(n.entries) || is.array(n.entries) ||
        (!allNaturalNumbers(n.entries) && !validDuration(n.entries)))
        stop("The input 'n.entries' needs to be an integer scalar or vector ",
             "containing the number of entries to retain, or string or vector ",
             "of strings containing the time period to retain, e.g., '8 days'.")

    dim.data <- dim(data)
    if ((is.null(dim.data) && length(n.entries) > 1))
        stop("The input 'n.entries' is a vector with more than one value. It needs to ",
             "be scalar when 'data' is non-dimensioned, e.g., when it is a ",
             "vector.")
    if ((is.null(dim.data) && length(n.entries) > 1) ||
        length(n.entries) > length(dim.data))
        stop("The input 'n.entries' is a vector with length greater than the number of ",
             "dimensions of 'data'. Its length needs to be less than or equal to ",
             "the number of dimensions of 'data'.")
}

# Are all numbers in 1,2,3,..., ignoring NA
allNaturalNumbers <- function(x)
{
    is.numeric(x) && all(is.na(x) | (round(x) == x & x > 0))
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

firstLastInDuration <- function(data, duration, is.first)
{
    dim.data <- if (is.array(data)) dim(data) else length(data)
    n.dim <- length(dim.data)
    indices <- lapply(seq_len(n.dim), function(i) {
        if (i > length(duration) || is.na(duration[i]))
            seq_len(dim.data[i])
        nms <- if (is.array(data)) dimnames(data)[[i]] else names(data)
        dates <- parseDates(nms, duration[i], i, n.dim)
        period.indicies <- periodIndices(dates, duration[i])
        quantity <- durationQuantity(duration[i])
        if (is.first)
            which(period.indicies <= min(period.indices) + quantity - 1)
        else
            which(period.indicies >= max(period.indices) - quantity + 1)
    })
    do.call(`[`, c(data, indices))
}

# getDimensionLabel <- function(dim.index, n.dimensions)
# {
#     if (n.dimensions == 1)
#         ""
#     else if (n.dimensions == 2)
#     {
#         if (dim.index == 1)
#             "the rows"
#         else
#             "the columns"
#     }
#     else # n.dimensions > 2
#     {
#         paste0("dimension ", dim.index)
#     }
# }

#' @importFrom flipTime AsDateTime
parseDates <- function(date.strings, duration.string, dimension.index,
                       n.dimensions)
{
    common.msg <- if (n.dimensions == 1)
        paste0("The duration '", duration.string,
               "' cannot be applied to this data as ",
               "the input data is ")
    else if (n.dimensions == 2)
    {
        dimension.label <- if (dimension.index == 1) "rows" else "columns"
        paste0("The duration '", duration.string,
               "' cannot be applied to this data as the",
               dimension.label, " in the input data are ")
    }
    else
        paste0("The duration '", duration.string,
               "' cannot be applied to this data as dimension ",
               dimension.index, " in the input data is ")

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
#' @importFrom flipTime AsDateTime
#' @importFrom lubridate year quarter month day
periodIndices <- function(dates, duration.string)
{
    if (grepl("year", duration.string, ignore.case = TRUE))
        year(dates)
    else if (grepl("quarter", duration.string, ignore.case = TRUE))
        year(dates) * 4 + quarter(dates)
    else if (grepl("month", duration.string, ignore.case = TRUE))
        year(dates) * 12 + month(dates)
    else if (grepl("week", duration.string, ignore.case = TRUE))
        weeksSinceEpoch(dates)
    else if (grepl("day", duration.string, ignore.case = TRUE))
        daysSinceEpoch(dates)
}

durationQuantity <- function(duration.string)
{
    m <- regexpr("[[:digit:]]+", duration.string)
    if (m != -1)
        as.integer(substr(duration.string, m, m + attr(m, "match.length") - 1))
    else
        1
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

