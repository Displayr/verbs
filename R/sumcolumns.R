#' @rdname SumOperations
#' @return The \code{SumColumns} function returns the summation of all the elements in each column
#'   index provided in the input, possibly after the elements have been pre-processed similar
#'   to \code{Sum}.
#' @examples
#' # Examples using SumColumns
#' input.matrix <- matrix(1:8, nrow = 4)
#' SumColumns(input.matrix) #= c(10, 26)
#' named.matrix <- input.matrix
#' dimnames(named.matrix) <- list(LETTERS[1:4], letters[1:2])
#' SumColumns(named.matrix)
#' SumColumns(named.matrix, remove.rows = c("A", "C"))
#' SumColumns(named.matrix, remove.columns = "a")
#' SumColumns(named.matrix, subset = c(TRUE, FALSE, TRUE, FALSE))
#' SumColumns(named.matrix, remove.rows = c("B", "D"))
#' # Each element is summed individually
#' # The order of input determines the order of output.
#' w <- c(a = 1, b = 2, c = 3, d = 4)
#' x <- c(a = 1, b = 2)
#' y <- c(b = 3, c = 10)
#' z <- c(c = -1, d = 3)
#' SumColumns(w, x, y, z)
#' SumColumns(z, y, x, w)
#' SumColumns(sample(w), sample(x), sample(y), sample(z))
#' @export
SumColumns <- function(...,
                       remove.missing = TRUE,
                       remove.rows = c("NET", "SUM", "Total"),
                       remove.columns = c("NET", "SUM", "Total"),
                       subset = NULL,
                       weights = NULL,
                       warn = FALSE)
{
    function.name <- sQuote(match.call()[[1]])
    x <- list(...)
    n.inputs <- length(x)
    if (n.inputs > 1)
        lapply(x, checkIfSuitableVectorType, function.name = function.name)
    x <- processArguments(...,
                          remove.missing = remove.missing,
                          function.name = function.name,
                          remove.rows = remove.rows,
                          remove.columns = remove.columns,
                          subset = subset,
                          weights = weights,
                          check.statistics = FALSE,
                          warn = warn)
    output.names <- lapply(x, getColumnNames)
    sum.output <- lapply(x, sumCols, remove.missing = remove.missing)
    sum.output <- joinOutputs(sum.output, output.names)
    if (warn && any(nan.outputs <- is.nan(sum.output)))
    {
        if (n.inputs == 1 && NCOL(x[[1L]]) > 1)
            x <- split(as.matrix(x[[1L]]), col(x[[1L]]))
        opposite.infinities <- logical(length(nan.outputs))
        opposite.infinities[nan.outputs] <- vapply(x[nan.outputs],
                                                   checkForOppositeInfinites,
                                                   logical(1))
        warnAboutOppositeInfinities(opposite.infinities, function.name)
    }
    sum.output
}

sumCols <- function(x, remove.missing = TRUE)
{
    # 2D Table with Multiple statistics is stored as a 3d array
    # and handled as a special case here.
    if (isQTable(x) && length(dim(x)) > 2)
        sumWithin3Darray(x, summing.function = colSums, remove.missing = remove.missing)
    else if (NCOL(x) == 1)
    {
        y <- sum(x, na.rm = remove.missing)
        names(y) <- getColumnNames(x)
        y
    } else if (NROW(x) == 1)
        x
    else
        colSums(x, na.rm = remove.missing)
}

getColumnNames <- function(x)
{
    if (getDim(x) == 1)
        attr(x, "label")
    else
        colNames(x)
}

listInputIncludesMatrices <- function(x)
{
    result <- FALSE
    for(i in seq_along(x))
        if (is.matrix(x[[i]]) || is.array(x[[i]]))
        {
            result <- TRUE
            break;
        }
    result
}

joinOutputs <- function(x, output.names)
{
    elements.are.matrices <- listInputIncludesMatrices(x)
    if (!elements.are.matrices)
    {
        x <- unlist(x)
        names(x) <- unlist(output.names)
    } else
        x <- do.call(cbind, x)
    x
}
