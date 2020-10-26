#' @export
First <- function(x, n = 6L, period = NULL, ...)
{
    checkFirstLastInputs(x, n)
    if (!is.null(period) && hasDateNames(x, n))
        n <- nIndicesInPeriod(x, n, period)
    head(x, n, ...)
}

# #' @export
# Last <- function(x, n = 6L, ...)
# {
#     hasDateNames(x, ...)
#     tail(x, ...)
# }

checkFirstLastInputs <- function(x, n)
{
    dim.x <- dim(x)
    if ((is.null(dim.x) && length(n) > 1))
        stop("The input n is a vector with more than one value. It needs to ",
             "be scalar when input x is non-dimensioned, e.g., when it is a ",
             "vector.")
    if ((is.null(dim.x) && length(n) > 1) ||
        length(n) > length(dim.x))
        stop("The input n is a vector with length greater than the number of ",
             "dimensions of x. Its length needs to be less than or equal to ",
             "the number of dimensions of x")
}

#' @importFrom flipTime AsDateTime
hasDateNames(x, n)
{
    dim.x <- dim(x)
    if (is.null(dim.x) && !is.null(names(x)))
    {
        AsDateTime(names(x), on.parse.failure = "")
    }
    else if (!is.null(dim.x))
    {

    }
}

nIndicesInPeriod <- function(x, n, period)
{

}

