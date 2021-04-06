#' @rdname SumOperations
#' @description In a similar way, \code{SumForEachColumn} is a generalization of \code{\link{colSums}} supporting
#'  row removal and the application of filters and weights before calculation but not supporting row
#'  or column matching for multiple inputs.
#' @details If a single input is provided to \code{SumForEachRow} and \code{SumForEachColumn}, it is
#'  permissible to be a \code{numeric} vector, \code{data.frame}, Q Table, \code{matrix} or
#'  other possible structure that has well defined rows or columns. In the case of a vector of length n, it
#'  is interpreted to be an input with n rows and a single column. An array is only
#'  permissible if it has 2 dimensions. Higher order arrays are only allowed in the form of
#'  a Q Table. Multiple inputs are allowed but only if each input is a single \code{numeric}
#'  vector with the same number of rows (a vector with n elements is interpreted as a matrix with
#'  n rows and 1 column) or the input elements can be reduced to that situation. For example,
#'  an n x p \code{matrix} or \code{data.frame} can be converted to p separate vectors with n rows.
#'
#'  For \code{SumForEachColumn}, the column names of the inputs if provided are used to define the names
#'  in the output vector.
#' @return The \code{SumForEachColumn} function returns the summation of all the elements in each column
#'   index provided in the input, possibly after the elements have been via filtering, application of
#'   weights or rows removed via the provided calling arguments.
#' @examples
#' # Examples using SumForEachColumn
#' input.matrix <- matrix(1:8, nrow = 4)
#' SumForEachColumn(input.matrix) #= c(10, 26)
#' named.matrix <- input.matrix
#' dimnames(named.matrix) <- list(LETTERS[1:4], letters[1:2])
#' SumForEachColumn(named.matrix)
#' SumForEachColumn(named.matrix, remove.rows = c("A", "C"))
#' SumForEachColumn(named.matrix, subset = c(TRUE, FALSE, TRUE, FALSE))
#' SumForEachColumn(named.matrix, remove.rows = c("B", "D"))
#' # Each element is summed individually
#' # The order of input determines the order of output.
#' w <- c(a = 1, b = 2, c = 3, d = 4)
#' x <- c(a = 1, b = 2)
#' y <- c(b = 3, c = 10)
#' z <- c(c = -1, d = 3)
#' SumForEachColumn(w, x, y, z)
#' SumForEachColumn(z, y, x, w)
#' SumForEachColumn(sample(w), sample(x), sample(y), sample(z))
#' @export
SumForEachColumn <- function(...,
                             remove.missing = TRUE,
                             remove.rows = c("NET", "SUM", "Total"),
                             subset = NULL, weights = NULL,
                             warn = FALSE)
{
    sumColumns(...,
               remove.missing = remove.missing,
               remove.rows = remove.rows,
               subset = subset, weights = weights,
               return.total.element.weights = "No",
               warn = warn,
               function.name = sQuote("SumForEachColumn"))

}

sumColumns <- function(...,
                       remove.missing = TRUE,
                       remove.rows = c("NET", "SUM", "Total"),
                       subset = NULL, weights = NULL,
                       return.total.element.weights = "No",
                       warn = FALSE,
                       function.name)
{
    calling.arguments <- match.call(expand.dots = FALSE)
    x <- list(...)
    input <- processArguments(x,
                              remove.missing = remove.missing,
                              remove.rows = remove.rows, remove.columns = NULL,
                              subset = NULL, weights = NULL,
                              check.statistics = FALSE,
                              warn = warn,
                              function.name = function.name)
    # input <- addSymbolAttributeIfPossible(calling.arguments[[2L]], input)
    n.inputs <- length(input)
    if (n.inputs == 1L)
    {
        input <- subsetAndWeightInputsIfNecessary(input,
                                                  subset = subset, weights = weights,
                                                  return.total.element.weights = return.total.element.weights,
                                                  warn = warn,
                                                  function.name = function.name)
        output <- sumCols(input[[1L]],
                          remove.missing = remove.missing,
                          remove.rows = remove.rows)
        if (warn)
        {
            if (any(nan.output <- is.nan(output)))
            {
                split.x <- split(as.matrix(input[[1L]]), col(input[[1L]]))
                opposite.infinities <- logical(length(nan.output))
                opposite.infinities[nan.output] <- vapply(split.x[nan.output],
                                                          checkForOppositeInfinites,
                                                          logical(1))
                warnAboutOppositeInfinities(opposite.infinities, function.name)
            }
        }
    } else
    {
        checkPossibleToSplitIntoNumericVectors(input, function.name)
        input <- splitIntoOneDimensionalVariables(input)
        input <- subsetAndWeightInputsIfNecessary(input,
                                                  subset = subset, weights = weights,
                                                  return.total.element.weights = return.total.element.weights,
                                                  function.name = function.name)
        output <- vapply(input, sum, numeric(1L), na.rm = remove.missing)
        candidate.names <- lapply(x, getColumnNames)
        all.names.found <- identical(Filter(is.null, lapply(x, getColumnNames)), list())
        if (all.names.found)
            names(output) <- unlist(candidate.names)
        if (warn)
        {
            if (any(nan.output <- is.nan(output)))
            {
                opposite.infinities <- logical(length(nan.output))
                opposite.infinities[nan.output] <- vapply(input[nan.output],
                                                          checkForOppositeInfinites,
                                                          logical(1L))
                warnAboutOppositeInfinities(opposite.infinities, function.name)
            }
        }
    }
    if (return.total.element.weights != "No")
    {
        if (!is.null(attr(input[[1L]], "sum.weights")))
            n.sum <- unlist(lapply(input, attr, "sum.weights"))
        else
        {
            if (length(input) == 1L)
                n.sum <- computeSingleInputSampleSizeByColumns(input[[1L]])
            else
                n.sum <- unlist(lapply(input, computeSingleInputSampleSizeByColumns))
        }
        attr(output, "n.sum") <- unlist(n.sum)
    }
    output
}

#' @importFrom stats setNames
#' @noRd
sumCols <- function(x, remove.missing = TRUE, remove.rows)
{
    if (NCOL(x) == 1)
    {
        y <- sum(x, na.rm = remove.missing)
        if (isVariable(x) || isQTable(x))
            y <- setNames(y, getInputNames(x))
        y
    } else
        colSums(x, na.rm = remove.missing)
}

computeSingleInputSampleSizeByColumns <- function(x)
{
    if (is.data.frame(x))
        vapply(x, numberNonMissingObservations, integer(1L))
    else if (is.matrix(x))
        apply(x, 2, numberNonMissingObservations)
    else if (is.array(x) && length(dim(x)) == 3L)
        apply(x, 2:3, numberNonMissingObservations)
    else
        numberNonMissingObservations(x)
}
