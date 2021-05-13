#' @rdname SumOperations
#' @description Also, \code{SumEachRow} is a generalization of \code{\link{rowSums}} supporting
#'  column removal before calculation but not supporting filters, weights nor multiple inputs.
#' @details For \code{SumEachRow} the sum is computed within the row dimension of the input.
#'  E.g. a n x p matrix supplied to \code{SumEachRow} will produce a vector of of length \code{n}.
#'  If names are provided in the row dimension of the input then the output will have the same
#'  row names.
#'
#' @return The \code{SumEachRow} function returns the summation of all the elements in each row
#'   index provided in the input, possibly after some rows have been removed via the \code{remove.rows}
#'   argument.
#' @examples
#' # Examples using SumEachRow
#' input.matrix <- matrix(runif(6), nrow = 3, dimnames = list(letters[1:3], c("Q1", "Q2")))
#' SumEachRow(input.matrix)
#' input.matrix.with.total <- cbind(input.matrix, "Total" = rowSums(input.matrix))
#' SumEachRow(input.matrix.with.total) # The total column is removed by default
#' colnames(input.matrix.with.total) <- c("Q1", "Q2", "tot")
#' SumEachRow(input.matrix.with.total) # This will be double due to the non-standard Total label.
#' SumEachRow(input.matrix.with.total, remove.columns = "tot")
#' @export
SumEachRow <- function(x,
                       remove.missing = TRUE,
                       remove.rows = NULL,
                       remove.columns = c("NET", "SUM", "Total"),
                       warn = FALSE)
{
    sumRowsInputs(x,
                  remove.missing = remove.missing,
                  remove.rows = remove.rows,
                  remove.columns = remove.columns,
                  return.column.counts = FALSE,
                  warn = warn,
                  function.name = sQuote(deparse(sys.call()[[1]])))
}

#' @rdname SumOperations
#' @export
SumRows <- SumEachRow

sumRowsInputs <- function(x,
                          remove.missing = TRUE,
                          remove.rows = NULL,
                          remove.columns = c("NET", "SUM", "Total"),
                          return.column.counts = FALSE,
                          warn = FALSE,
                          function.name)
{
    higher.dim.array <- isQTable(x) && getDimensionLength(x) > 2L
    x <- processArguments(list(x),
                          remove.missing = FALSE, # This is only used to trigger a warning
                          remove.rows = remove.rows, remove.columns = remove.columns,
                          subset = NULL, weights = NULL,
                          check.statistics = !higher.dim.array,
                          return.total.element.weights = "No",
                          warn = warn,
                          function.name = function.name)
    input <- x[[1L]]
    output <- sumRows(input, remove.missing = remove.missing)
    if (warn)
    {
        if (NCOL(input) == 1L)
            throwWarningAboutCalculationWithSingleElement(input, dimension = 2L, function.name)
        checkOppositeInifinitiesByRow(output, input, function.name)
        warnIfDataHasMissingValues(x, remove.missing = remove.missing)
    }
    if (return.column.counts)
        attr(output, "n.sum") <- computeSingleInputSampleSizeByRows(input)
    output
}

#' @importFrom stats setNames
sumRows <- function(x, remove.missing)
{
    x.names <- rowNames(x)
    # Higher dimensional arrays that can occur in some Q Tables
    # are handled as a special case here.
    if (isQTable(x) && getDimensionLength(x) > 2)
    {
        y <- apply(x, c(1L, 3L), sum, na.rm = remove.missing)
        if (NCOL(y) == 1L)
        {
            y <- as.vector(y)
            names(y) <- x.names
        }
        y
    } else if (NCOL(x) == 1)
    {
        if (remove.missing && anyNA(x))
            x[is.na(x)] <- 0
        if (is.data.frame(x))
            x
        else
            setNames(as.vector(x), nm = x.names)
    } else
        setNames(as.vector(rowSums(x, na.rm = remove.missing)), nm = x.names)
}

computeSingleInputSampleSizeByRows <- function(x)
{
    if (is.data.frame(x) || is.matrix(x))
        apply(!is.na(x), 1L, sum, na.rm = TRUE)
    else if (is.array(x) && length(dim(x)) == 3L)
        apply(!is.na(x), c(1L, if (dim(x)[3L] == 1L) NULL else 3L), sum)
    else
        (!is.na(x)) * 1L
}

throwWarningAboutCalculationWithSingleElement <- function(input, dimension, function.name)
{
    dimension <- switch(dimension, "row", "column")
    input.type <- if (isVariable(input)) "a single variable" else paste0("an input with a single ", dimension)
    suffix <- if (isTRUE(attr(input, "missing.removed"))) " with missing values replaced with zeros." else "."
    warning("Only ", input.type, " was provided to ", function.name, " and consequently ",
            "the same input was returned", suffix)
}

checkOppositeInifinitiesByRow <- function(output, input, function.name)
{
    if (is.data.frame(output))
        output <- as.matrix(output)
    if (any(nan.output <- is.nan(output)))
    {
        if (getDimensionLength(output) == 2L)
        {
            input <- ftable(input, row.vars = c(1L, 3L))
            split.x <- split(input, row(input))
        } else
            split.x <- split(as.matrix(input), row(input))
        opposite.infinities <- logical(length(nan.output))
        opposite.infinities[nan.output] <- vapply(split.x[nan.output],
                                                  checkForOppositeInfinites,
                                                  logical(1))
        warnAboutOppositeInfinities(opposite.infinities, function.name)
    }
}
