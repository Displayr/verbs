#' @rdname SumOperations
#' @details If a single input is provided to \code{SumRows} and \code{SumColumns}, it is
#'  permissible to be a \code{numeric} vector, \code{data.frame}, \code{Q Table}, \code{matrix} or
#'  other possible structure that has well defined rows or columns. An array is only
#'  permissible if it has 2 dimensions. Higher order arrays are only allowed in the form of
#'  a \code{Q Table}. Multiple inputs are allowed but only if each input is a single \code{numeric}
#'  vector. i.e. multiple \code{data.frame}s or matrices etc. are not allowed.
#'
#'  For \code{SumRows} the summation only occurs separately for each row of the
#'  provided input. E.g. a n x p matrix supplied to \code{SumRows} will produce a vector
#'  of length \code{n} and possible be named with the same names as the row names of the
#'  input matrix. A named vector of length \code{n} is interpreted as a structure with
#'  \code{n} rows and 1 column. Meaning that a single vector input to \code{SumRows} will
#'  typically return the same input vector (assuming no rows are removed or the vector is
#'  filtered by the \code{subset} argument).
#'
#'  \code{SumRows} has an extra feature in that it supports matching rows by name. If multiple
#'  named vectors are given as input, their indices could be matched using their names via an
#'  exact match or fuzzy match (see the \code{match.elements} argument). These elements are
#'  matched before calculation occurs.
#' @return The \code{SumRows} function returns the summation of all the elements in each row
#'   index provided in the input, possibly after the elements have been pre-processed similar
#'   to \code{Sum}. However, \code{SumRows} also allows elements to be matched by name.
#' @examples
#' # Examples using SumRows
#' SumRows(input.matrix) #= c(6, 8, 10, 12)
#' SumRows(named.matrix)
#' SumRows(named.matrix, remove.rows = c("A", "C"))
#' SumRows(named.matrix, remove.columns = "a")
#' SumRows(named.matrix, subset = c(TRUE, FALSE, TRUE, FALSE))
#' SumRows(named.matrix, remove.rows = c("B", "D"))
#' # Due to the default row matching all the below
#' # will evaluate to the equivalent of c(a = 2, b = 7, c = 12, d = 7)
#' # It will differ on the order of the elements, not the evaluated content
#' SumRows(w, x, y, z)
#' SumRows(z, y, x, w)
#' SumRows(sample(w), sample(x), sample(y), sample(z))
#' # This will error since there are elements with no match
#' # SumRows(x, y, match.elements = 'Yes - error if unmatched')
#' X <- x
#' names(X) <- toupper(names(x))
#' Z <- z
#' names(Z) <- toupper(names(z))
#' # In the exact match case, it is case sensitive
#' SumRows(w, X, y, Z, match.elements = "Yes - ignore if unmatched")
#' SumRows(w, X, y, Z, match.elements = "Fuzzy - ignore if unmatched")
#' SumRows(w, x, y, z)
#' @export
SumRows <- function(...,
                    remove.missing = TRUE,
                    remove.rows = c("NET", "SUM", "Total"),
                    remove.columns = c("NET", "SUM", "Total"),
                    match.rows = "Yes",
                    subset = NULL,
                    weights = NULL,
                    warn = FALSE)
{
    function.name <- sQuote(match.call()[[1]])
    x <- list(...)
    n.inputs <- length(x)
    if (n.inputs == 1)
    {
        # x <- lapply(x, extractChartDataIfNecessary)
        # x <- subsetAndWeightInputsIfNecessary(x,
        #                                       subset = subset,
        #                                       weights = weights,
        #                                       warn = warn,
        #                                       function.name = function.name)
        # x <- lapply(x, removeRowsAndCols,
        #             remove.rows = remove.rows,
        #             remove.columns = remove.columns,
        #             function.name = function.name)
        x <- processArguments(x,
                              remove.missing = remove.missing,
                              remove.rows = remove.rows, remove.columns = remove.columns,
                              match.rows = match.rows, match.columns = match.columns,
                              subset = subset, weights = weights, check.statistics = FALSE,
                              function.name = function.name)
        x.in <- x[[1L]]
        if (warn)
            checkForMultipleStatistics(x.in, function.name = function.name)
        output <- sumRowsSingleInput(x.in,
                                     remove.missing = remove.missing,
                                     subset = subset,
                                     weights = weights,
                                     warn = warn,
                                     function.name = function.name)
    }
    else
    {
        browser()
        # x <- lapply(x, splitIntoOneDimensionalVariables)
        new.arguments <- x
        called.args <- match.call(expand.dots = FALSE)
        function.args <- formals(as.character(called.args[[1L]]))
        called.args[[1L]] <- as.name('list')
        called.args <- eval(called.args, parent.frame())
        called.args[["..."]] <- function.args[["..."]] <- NULL
        matched.args <- match(names(called.args), names(function.args), nomatch = 0L)
        if (length(matched.args))
            function.args[matched.args] <- called.args
        new.arguments <- c(x, function.args)
        new.arguments[["match.columns"]]  <- "No"
        output <- do.call("Sum", new.arguments)
    }
    if (warn)
    {
        checkMissingData(x, remove.missing = TRUE)
        if (any(nan.output <- is.nan(output)))
        {
            if (n.inputs == 1 && NCOL(x[[1L]]) > 1)
                x <- split(as.matrix(x[[1L]]), row(x[[1L]]))
            opposite.infinities <- logical(length(nan.output))
            opposite.infinities[nan.output] <- vapply(x[nan.output],
                                                      checkForOppositeInfinites,
                                                      logical(1))
            warnAboutOppositeInfinities(opposite.infinities, function.name)
        }
    }
    output
}

sumRowsSingleInput <- function(x,
                               remove.missing,
                               subset,
                               weights,
                               warn,
                               function.name)
{
    checkIfCharacter(x, function.name = function.name)
    checkIfDateTime(x, function.name = function.name)
    x <- AsNumeric(x, binary = FALSE)
    sum.output <- sumRowsSingleCalculation(x, remove.missing = remove.missing)
    sum.output
}

sumRowsSingleCalculation <- function(x, remove.missing)
{
    x.names <- rowNames(x)
    # 2D Table with Multiple statistics is stored as a 3d array
    # and handled as a special case here.
    if (isQTable(x) && length(dim(x)) > 2)
    {
        y <- sumWithin3Darray(x, summing.function = rowSums, remove.missing = remove.missing)
        if (NCOL(y) == 1)
        {
            y <- as.vector(y)
            names(y) <- rowNames(x)
        }
        y
    } else if (NCOL(x) == 1)
        setRowNames(as.vector(x), x.names)
    else
        setRowNames(as.vector(rowSums(x, na.rm = remove.missing)), x.names)
}


setRowNames <- function(x, names.to.use)
{
    names(x) <- names.to.use
    x
}

splitIntoMultipleVariablesIfPossible <- function(x, function.name)
{
    browser()
}
