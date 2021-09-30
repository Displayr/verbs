#' @rdname SumOperations
#' @param x A single input to be used in \code{SumEachRow} or \code{SumEachColumn}.
#' @description In a similar way, \code{SumEachColumn} is a generalization of \code{\link{colSums}} supporting
#'  row removal and the application of filters and weights before calculation but not supporting multiple inputs.
#'  The functions \code{SumColumns} and \code{SumRows} are aliases for \code{SumEachColumn} and \code{SumEachRow}.
#' @details The  \code{SumEachRow} and \code{SumEachColumn}, only support a single input \code{x}. The permissible
#'  input is a \code{numeric} vector, \code{array} with at most 2 dimensions, a \code{data.frame},
#'  Q Table, \code{matrix} or other possible structure that has well defined rows or columns.
#'
#'  In the case of a vector of length n, it is interpreted to be an input with n rows and
#'  a single column. An array is only permissible if it has 2 dimensions, the exception being
#'  higher order arrays are only allowed in the form of a Q Table.
#'
#'  For \code{SumEachColumn}, the column names of the inputs if provided are used to define the names
#'  in the output vector.
#' @return The \code{SumEachColumn} function returns the summation of all the elements in each column
#'   index provided in the input, possibly after the elements have been via filtering, application of
#'   weights or rows removed via the provided calling arguments.
#' @examples
#' # Examples using SumEachColumn
#' input.matrix <- matrix(1:8, nrow = 4)
#' SumEachColumn(input.matrix) #= c(10, 26)
#' named.matrix <- input.matrix
#' dimnames(named.matrix) <- list(LETTERS[1:4], letters[1:2])
#' SumEachColumn(named.matrix)
#' SumEachColumn(named.matrix, remove.rows = c("A", "C"))
#' SumEachColumn(named.matrix, subset = c(TRUE, FALSE, TRUE, FALSE))
#' SumEachColumn(named.matrix, remove.rows = c("B", "D"))
#' @export
SumEachColumn <- function(x,
                          remove.missing = TRUE,
                          remove.rows = c("NET", "SUM", "Total"),
                          remove.columns = NULL,
                          subset = NULL, weights = NULL,
                          warn = FALSE)
{
    sumColumns(x,
               remove.missing = remove.missing,
               remove.rows = remove.rows,
               remove.columns = remove.columns,
               subset = subset, weights = weights,
               return.total.element.weights = "No",
               warn = warn,
               function.name = sQuote(deparse(sys.call()[[1]])))
}

#' @rdname SumOperations
#' @export
SumColumns <- SumEachColumn

sumColumns <- function(x,
                       remove.missing = TRUE,
                       remove.rows = c("NET", "SUM", "Total"),
                       remove.columns = NULL,
                       subset = NULL, weights = NULL,
                       return.total.element.weights = "No",
                       warn = FALSE,
                       function.name)
{
    x <- processArguments(list(x),
                          remove.missing = remove.missing,
                          remove.rows = remove.rows, remove.columns = remove.columns,
                          subset = NULL, weights = NULL,
                          check.statistics = FALSE,
                          warn = warn,
                          function.name = function.name)
    input <- subsetAndWeightInputsIfNecessary(x,
                                              subset = subset, weights = weights,
                                              return.total.element.weights = return.total.element.weights,
                                              warn = warn,
                                              function.name = function.name)[[1L]]
    output <- sumCols(input, remove.missing = remove.missing)
    if (warn)
    {
        if (NROW(input) == 1L)
            throwWarningAboutCalculationWithSingleElement(input, dimension = 1L, function.name)
        checkOppositeInifinitiesByColumn(output, input, function.name)
    }
    if (return.total.element.weights != "No")
    {
        if (is.null(n.sum <- attr(input, "sum.weights")))
            n.sum <- computeSingleInputSampleSizeByColumns(input)
        attr(output, "n.sum") <- n.sum
    }
    output
}

#' @importFrom stats setNames
#' @noRd
sumCols <- function(x, remove.missing = TRUE, remove.rows)
{
    if (NCOL(x) == 1)
    {
        y <- baseSum(x, remove.missing = remove.missing)
        if (isVariable(x) || isQTable(x))
            y <- setNames(y, getInputNames(x))
    } else
    {
        y <- colSums(x, na.rm = remove.missing)
        if (any(all.missing.in.cols <- apply(x, 2:length(DIM(x)), allNA)))
            y[all.missing.in.cols] <- NA
        if (is.data.frame(x) && any(variables.inside <- vapply(x, isVariable, logical(1L))))
            names(y)[variables.inside] <- vapply(x[variables.inside],
                                                 getInputNames,
                                                 character(1L))
    }
    y
}

getInputNames <- function(x)
{
    if (!is.null(label <- attr(x, "label")))
        return(label)
    if (!is.null(name <- attr(x, "name", exact = TRUE)))
        return(name)
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

checkOppositeInifinitiesByColumn <- function(output, input, function.name)
{
    if (any(nan.output <- is.nan(output)))
    {
        if (is.data.frame(input))
            opposite.infinities <- vapply(input, checkForOppositeInfinites, logical(1L))
        else {
            n.col <- ncol(input)
            if (!is.null(n.col) && !is.na(n.col))
            {
                if (getDimensionLength(output) == 2L)
                {
                    input <- ftable(input, col.vars = c(2L, 3L))
                    split.x <- split(input, col(input))
                } else
                    split.x <- split(as.matrix(input), col(input))
                opposite.infinities <- logical(length(nan.output))
                opposite.infinities[nan.output] <- vapply(split.x[nan.output],
                                                          checkForOppositeInfinites,
                                                          logical(1))
            } else
                opposite.infinities <- checkForOppositeInfinites(input)
        }
        warnAboutOppositeInfinities(opposite.infinities, function.name)
    }
}

allNA <- function(x)
{
    all(is.na(x))
}
