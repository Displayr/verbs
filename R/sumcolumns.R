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
                       subset = NULL,
                       weights = NULL,
                       call. = FALSE,
                       warn = FALSE)
{
    calling.arguments <- match.call(expand.dots = FALSE)
    if ((parent.frame.index <- sys.parent(1L)) != 0L)
    {
        called.from.average <- identical(sys.function(parent.frame.index), AverageColumns)
        function.name <- sQuote(if(called.from.average) "AverageColumns" else calling.arguments[[1L]])
    } else
    {
        called.from.average <- FALSE
        function.name <- sQuote(calling.arguments[[1L]])
    }
    x <- list(...)
    input <- processArguments(x,
                              remove.missing = remove.missing,
                              remove.rows = remove.rows, remove.columns = NULL,
                              subset = NULL, weights = NULL,
                              check.statistics = FALSE,
                              warn = warn,
                              function.name = function.name)
    input <- addSymbolAttributeIfPossible(calling.arguments[[2L]], input)
    n.inputs <- length(input)
    if (n.inputs == 1L)
    {
        input <- subsetAndWeightInputsIfNecessary(input,
                                                  subset = subset, weights = weights,
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
    if (called.from.average)
    {
        if (!is.null(attr(input[[1L]], "sum.weights")))
            n.sum <- vapply(input, attr, numeric(1L), "sum.weights")
        else
        {
            if (length(input) == 1L)
                n.sum <- computeSingleInputSampleSizeByColumns(input[[1L]])
            else
                n.sum <- unlist(lapply(input, computeSingleInputSampleSizeByColumns))
        }
        n.sum <- setNames(n.sum, names(output))
        attr(output, "n.sum") <- unlist(n.sum)
    }
    output
}

#' @importFrom stats setNames
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
