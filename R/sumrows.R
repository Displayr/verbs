#' @rdname SumOperations
#' @details If a single input is provided to \code{SumRows} and \code{SumColumns}, it is
#'  permissible to be a \code{numeric} vector, \code{data.frame}, \code{Q Table}, \code{matrix} or
#'  other possible structure that has well defined rows or columns. An array is only
#'  permissible if it has 2 dimensions. Higher order arrays are only allowed in the form of
#'  a \code{Q Table}. Multiple inputs are allowed but only if each input is a single \code{numeric}
#'  vector. i.e. multiple \code{data.frame}s or matrices etc. are not allowed.
#'
#'  For \code{SumRows} the sum is computed not elementwise but across the whole row dimension
#'  E.g. a n x p matrix supplied to \code{SumRows} will produce a vector
#'  of length \code{n} with all \code{p} column elements being used to compute each row sum.
#'  If names are provided in the row dimension of the input then the output will have the same
#'  row names. A named vector of length \code{n} is interpreted as a structure with
#'  \code{n} rows and 1 column. Meaning that a single vector input to \code{SumRows} will
#'  typically return the same input vector (assuming no rows are removed or the vector is
#'  filtered by the \code{subset} argument).
#'
#' @return The \code{SumRows} function returns the summation of all the elements in each row
#'   index provided in the input, possibly after the elements have been pre-processed similar
#'   to \code{Sum}. However, \code{SumRows} also allows elements to be matched by name.
#' @examples
#' # Examples using SumRows
#' @export
SumRows <- function(...,
                    remove.missing = TRUE,
                    remove.columns = c("NET", "SUM", "Total"),
                    warn = FALSE)
{
    calling.arguments <- match.call(expand.dots = FALSE)
    function.name <- sQuote(calling.arguments[[1]])
    symbol.input <- vapply(calling.arguments[[2]], is.symbol, logical(1L))
    x <- list(...)
    symbol.names <- rep(NA, length(x))
    if (any(symbol.input))
    {
        symbol.names[symbol.input] <- vapply(calling.arguments[[2]][symbol.input],
                                             as.character, character(1L))
        inds.with.symbol.names <- which(symbol.input)
        x[inds.with.symbol.names] <- mapply(function(x, symbol.name) {
            attr(x, "symbol") <- symbol.name
            x
        },
        x[inds.with.symbol.names],
        symbol.names[inds.with.symbol.names],
        SIMPLIFY = FALSE)
    }
    n.inputs <- length(x)
    single.QTable.with.multiple.stats <- isQTable(x[[1L]]) && length(dim(x[[1L]])) == 3L
    three.dim.array <- n.inputs == 1 && single.QTable.with.multiple.stats
    # If a 3D array via a 2D QTable with multiple statistics
    # Don't check for multiple statistics since they are not summed in
    # SumRows, also compute the result directly here as a special case and not call
    # Sum instead
    if (n.inputs == 1L)
    {
        x <- processArguments(x,
                              remove.missing = remove.missing,
                              remove.rows = NULL, remove.columns = remove.columns,
                              subset = NULL, weights = NULL,
                              check.statistics = !three.dim.array,
                              warn = warn,
                              function.name = function.name)
        if (remove.missing)
            x <- lapply(x, removeMissing)
        output <- sumRowsSingleCalculation(x[[1L]],
                                           remove.missing = remove.missing)
        ncols <- NCOL(x[[1L]])
        single.column <- ncols == 1L
        no.column.names <- ncols > 1L && is.null(colnames(x[[1L]]))
        colnames.not.required <- single.column || no.column.names
        if (warn)
        {
            if (any(nan.output <- is.nan(output)))
            {
                split.x <- split(as.matrix(x[[1L]]), row(x[[1L]]))
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
        x <- lapply(x, removeRowsAndCols,
                    remove.rows = NULL,
                    remove.columns = remove.columns,
                    function.name = function.name)
        checkInputTypes(x, function.name = function.name)
        checkMultipleInputsAppropriateForSumRows(x, function.name = function.name)
        y <- splitIntoOneDimensionalVariables(x)
        new.arguments <- y
        called.args <- match.call(expand.dots = FALSE)
        function.args <- formals(as.character(called.args[[1L]]))
        called.args[[1L]] <- as.name('list')
        called.args <- eval(called.args, parent.frame())
        called.args[["..."]] <- function.args[["..."]] <- NULL
        matched.args <- match(names(called.args), names(function.args), nomatch = 0L)
        if (length(matched.args))
            function.args[matched.args] <- called.args
        new.arguments <- c(new.arguments, function.args)
        new.arguments[["match.columns"]]  <- new.arguments[["match.rows"]] <- "No"
        new.arguments[["remove.rows"]]  <- new.arguments[["remove.columns"]] <- NULL
        output <- do.call("Sum", new.arguments)
    }
    input.colnames <- lapply(x, getColnames)
    colnames.required <- !(three.dim.array || (n.inputs == 1L && colnames.not.required))
    if (colnames.required && identical(Filter(is.null, input.colnames), list()))
    {
        output.colname <- paste0(unique(unlist(input.colnames)), collapse = " + ")
        output <- array(output,
                        dim = c(length(output), 1L),
                        dimnames = list(names(output), output.colname))
    }
    output
}

getColnames <- function(x)
{
    x.names <- if (length(d <- dim(x)) && length(d) == 2L) colnames(x)
    if (!is.null(x.names))
        return(x.names)
    if (!is.null(label <- attr(x, "label")))
        return(label)
    if (!is.null(name <- attr(x, "name")))
        return(name)
    if (!is.null(question <- attr(x, "question")))
        return(question)
    if (!is.null(symbol <- attr(x, "symbol")))
        return(symbol)
}

sumRowsSingleCalculation <- function(x, remove.missing)
{
    x.names <- rowNames(x)
    # 2D Table with Multiple statistics is stored as a 3d array
    # and handled as a special case here.
    if (isQTable(x) && length(dim(x)) > 2)
    {
        y <- sumWithin3Darray(x, summing.function = rowSums, remove.missing = remove.missing)
        if (NCOL(y) == 1L)
        {
            y <- as.vector(y)
            names(y) <- x.names
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

flattenToSingleList <- function(input.list)
{
    args <- lapply(input.list, function(x) if (is.list(x)) flattenToSingleList(x) else list(x))
    do.call(c, args)
}

splitIntoOneDimensionalVariables <- function(x, function.name)
{
    y <- lapply(x, splitIntoVariables)
    listed.vars <- vapply(y, is.list, logical(1L))
    if (any(listed.vars))
        y <- flattenToSingleList(y)
    y
}

splitIntoVariables <- function(x, function.name)
{
    if (NCOL(x) == 1L)
        return(x)
    else if ((is.df <- is.data.frame(x)) || is.array(x))
    {
        x.names <- rowNames(x)
        if (is.df)
            x <- as.list(x)
        else
            x <- split(x, col(x))
        if (!is.null(x.names))
            x <- lapply(x, setRowNames, names.to.use = x.names)
    }
    x
}

checkMultipleInputsAppropriateForSumRows <- function(x, function.name)
{
    checkNumericOrMatrixInput(x, function.name)
    checkNumberRowsAgree(x, function.name)
}

checkNumericOrMatrixInput <- function(x, function.name)
{
    for (elem in x)
    {
        one.dim.vector <- (is.factor(elem) || is.numeric(elem) || is.array(elem)) && NCOL(elem) == 1L
        if (!(is.matrix(elem) || is.data.frame(elem) || one.dim.vector))
            stop(function.name, " requires all input elements to be numeric ",
                 "vectors or a matrix")
    }
}

checkNumberRowsAgree <- function(x, function.name)
{
    nrows <- vapply(x, NROW, integer(1L))
    if (!all(nrows == nrows[1L]))
    {
        unique.nrows <- unique(nrows)
        n.unique <- length(unique.nrows)
        unique.nrows <- paste0(c(paste0(unique.nrows[1:(n.unique - 1L)], collapse = ", "),
                                 unique.nrows[n.unique]),
                               collapse = " and ")
        stop(function.name, " requires all input elements to have the same number of ",
             "rows. In this case there are input elements with ", unique.nrows, " rows. ",
             "Please ensure that all inputs have the same number of rows before attempting ",
             "to call ", function.name, " again.")
    }
}

