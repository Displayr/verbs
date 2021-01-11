#' @rdname SumOperations
#' @description In a similar way, \code{SumRows} is a generalization of \code{\link{rowSums}} but
#'  not supporting row or column matching for multiple inputs.
#' @details If a single input is provided to \code{SumRows} and \code{SumColumns}, it is
#'  permissible to be a \code{numeric} vector, \code{data.frame}, \code{Q Table}, \code{matrix} or
#'  other possible structure that has well defined rows or columns. An array is only
#'  permissible if it has 2 dimensions. Higher order arrays are only allowed in the form of
#'  a \code{Q Table}. Multiple inputs are allowed but only if each input is a single \code{numeric}
#'  vector. i.e. multiple \code{data.frame}s or matrices etc. are not allowed.
#'
#'  For \code{SumRows} the sum is computed not element-wise but across the whole row dimension
#'  E.g. a n x p matrix supplied to \code{SumRows} will produce a vector or column vector of
#'  of length \code{n}. If names are provided in the row dimension of the input then the output will have the same
#'  row names. The output will be a simple vector if the input is a single input with no column names.
#'  If the input has column names or there are multiple inputs, then the output will be a column vector
#'  where the single column name is determined by collating all the input column names or variables names
#'  into a single string. E.g. if two input vectors are provided, \code{x1} and \code{x2}, then the
#'  output column name will be \code{"x1 + x2"}.
#'
#' @return The \code{SumRows} function returns the summation of all the elements in each row
#'   index provided in the input, possibly after the elements have been pre-processed similar
#'   to \code{Sum}. However, \code{SumRows} also allows elements to be matched by name.
#' @examples
#' # Examples using SumRows
#' input.matrix <- matrix(runif(6), nrow = 3, dimnames = list(letters[1:3], c("Q1", "Q2")))
#' SumRows(input.matrix)
#' input.matrix.with.total <- cbind(input.matrix, "Total" = rowSums(input.matrix))
#' SumRows(input.matrix.with.total) # The total column is removed by default
#' colnames(input.matrix.with.total) <- c("Q1", "Q2", "tot")
#' SumRows(input.matrix.with.total) # This will be double since the non-standard Total label is used.
#' SumRows(input.matrix.with.total, remove.columns = "tot")
#' v3 <- matrix(runif(3), nrow = 3, dimnames = list(letters[1:3], "Q3"))
#' SumRows(input.matrix, v3)
#' input.df <- data.frame(V1 = runif(3), V2 = runif(3))
#' SumRows(input.matrix, input.df)
#' @export
SumRows <- function(...,
                    remove.missing = TRUE,
                    remove.columns = c("NET", "SUM", "Total"),
                    warn = FALSE)
{
    calling.arguments <- match.call(expand.dots = FALSE)
    function.name <- sQuote(calling.arguments[[1L]])
    x <- list(...)
    x <- addSymbolAttributeIfPossible(calling.arguments[[2L]], x)
    n.inputs <- length(x)
    single.QTable.with.multiple.stats <- isQTable(x[[1L]]) && length(dim(x[[1L]])) == 3L
    three.dim.array <- n.inputs == 1L && single.QTable.with.multiple.stats
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
        output <- sumRows(x[[1L]], remove.missing = remove.missing)
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
    input.colnames <- lapply(x, getColumnNames)
    colnames.required <- !(three.dim.array || (n.inputs == 1L && colnames.not.required))
    if (colnames.required && identical(Filter(is.null, input.colnames), list()))
    {
        output.colname <- paste0(unique(unlist(input.colnames)), collapse = " + ")
        output <- array(output,
                        dim = c(length(output), 1L),
                        dimnames = list(rowNames(output), output.colname))
    }
    output
}

getColumnNames <- function(x)
{
    x.names <- if (length(d <- dim(x)) && length(d) == 2L) colnames(x)
    if (!is.null(x.names))
        return(x.names)
    if (!is.null(label <- attr(x, "label")))
        return(label)
    if (!is.null(name <- attr(x, "name", exact = TRUE)))
        return(name)
    if (!is.null(question <- attr(x, "question", exact = TRUE)))
        return(question)
    if (!is.null(symbol <- attr(x, "symbol")))
        return(symbol)
}

sumRows <- function(x, remove.missing)
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

splitIntoOneDimensionalVariables <- function(x)
{
    y <- lapply(x, splitIntoVariables)
    listed.vars <- vapply(y, is.list, logical(1L))
    if (any(listed.vars))
        y <- flattenToSingleList(y)
    y
}

splitIntoVariables <- function(x)
{
    if (NCOL(x) == 1L)
        return(x)
    else if ((is.df <- is.data.frame(x)) || is.array(x))
    {
        x.rownames <- rowNames(x)
        x.colnames <- colNames(x)
        if (is.df)
            x <- as.list(x)
        else
            x <- split(x, col(x))
        if (!is.null(x.rownames))
            x[[1L]] <- setRowNames(x[[1L]], names.to.use = x.rownames)
        if (!is.null(x.colnames))
            names(x) <- x.colnames
        else
            names(x) <- NULL
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

