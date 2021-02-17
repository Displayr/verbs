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
    function.name <- sQuote(calling.arguments[[1L]])
    x <- list(...)
    x <- addSymbolAttributeIfPossible(calling.arguments[[2L]], x)
    n.inputs <- length(x)
    if (n.inputs == 1L)
    {
        input <- processArguments(x,
                                  remove.missing = remove.missing,
                                  remove.rows = remove.rows, remove.columns = NULL,
                                  subset = subset, weights = weights,
                                  check.statistics = FALSE,
                                  warn = warn,
                                  function.name = function.name)
        if (remove.missing)
            input <- lapply(input, removeMissing)
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
        x <- extractChartDataIfNecessary(x)
        x <- checkInputsAtMost2DOrQTable(x, function.name = function.name)
        x <- removeRowsAndColsFromInputs(x,
                                         remove.rows = remove.rows,
                                         remove.columns = NULL,
                                         function.name = function.name)
        checkInputTypes(x, function.name = function.name)
        checkPossibleToSplitIntoNumericVectors(x, function.name)
        x <- convertToNumeric(x)
        x <- subsetAndWeightInputsIfNecessary(x,
                                              subset = subset,
                                              weights = weights,
                                              warn = warn,
                                              function.name = function.name)
        input <- splitIntoOneDimensionalVariables(x)
        output <- vapply(input, Sum, numeric(1L),
                         remove.missing = remove.missing,
                         remove.rows = NULL, remove.columns = NULL,
                         match.rows = "No", match.columns = "No",
                         subset = NULL, weights = NULL,
                         warn = FALSE)
        candidate.names <- lapply(x, getColumnNames)
        all.names.found <- identical(Filter(is.null, lapply(x, getColumnNames)), list())
        if (all.names.found)
            names(output) <- unlist(candidate.names)
        if (warn)
        {
            warnIfDataHasMissingValues(x, remove.missing = remove.missing)
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
        if (!is.null(sum.w <- attr(input[[1L]], "sum.weights")))
        {
            n.sum <- rep(sum.w, length(output))
            n.sum <- setNames(n.sum, names(output))
            attr(output, "n.sum") <- n.sum
        } else
        {
            n.sum <- lapply(input, function(x) {
                y <- rep(NROW(x), NCOL(x))
                setNames(y, colNames(x))
            })
            attr(output, "n.sum") <- unlist(n.sum)
        }
    }
    output
}

#' @importFrom stats setNames
sumCols <- function(x, remove.missing = TRUE, remove.rows)
{
    # 2D Table with Multiple statistics is stored as a 3d array
    # and handled as a special case here.
    if (isQTable(x) && length(dim(x)) > 2)
        sumColumnsWithinArray(x,
                              remove.missing = remove.missing,
                              remove.rows = remove.rows)
    else if (NCOL(x) == 1)
    {
        y <- sum(x, na.rm = remove.missing)
        if (isVariable(x) || isQTable(x))
            y <- setNames(y, getInputNames(x))
        y
    } else
        colSums(x, na.rm = remove.missing)
}

#' Used to sum out the appropriate dimension when a 2D table with multiple statistics is used
#' @noRd
sumColumnsWithinArray <- function(x, remove.missing, remove.rows)
{
    n.dims <- length(dim(x))
    apply(x, 2:3, Sum,
          remove.missing = remove.missing,
          remove.rows = remove.rows)
}
