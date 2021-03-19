#' @rdname SumOperations
#' @description Also, \code{SumRows} is a generalization of \code{\link{rowSums}} supporting
#'  column removal before calculation but not supporting filters, weights and row or column matching
#'  for multiple inputs.
#' @details For \code{SumRows} the sum is computed within the row dimension of the input.
#'  E.g. a n x p matrix supplied to \code{SumRows} will produce a vector of of length \code{n}.
#'  If names are provided in the row dimension of the input then the output will have the same
#'  row names.
#'
#' @return The \code{SumRows} function returns the summation of all the elements in each row
#'   index provided in the input, possibly after some rows have been removed via the \code{remove.rows}
#'   argument.
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
    sumRowsInputs(...,
                  remove.missing = remove.missing,
                  remove.columns = remove.columns,
                  return.column.counts = FALSE,
                  warn = warn,
                  function.name = sQuote("SumRows"))
}

sumRowsInputs <- function(...,
                          remove.missing = TRUE,
                          remove.columns = c("NET", "SUM", "Total"),
                          return.column.counts = FALSE,
                          warn = FALSE,
                          function.name)
{
    x <- list(...)
    n.inputs <- length(x)
    single.higher.dim.array <- n.inputs == 1L && isQTable(x[[1L]]) && length(dim(x[[1L]])) > 2L
    variables.or.variable.sets <- vapply(x, containsAVariableInEachColumn, logical(1L))
    x <- processArguments(x,
                          remove.missing = FALSE, # This is only used to trigger a warning
                          remove.rows = NULL, remove.columns = remove.columns,
                          subset = NULL, weights = NULL,
                          check.statistics = !single.higher.dim.array,
                          return.total.element.weights = "No",
                          warn = warn,
                          function.name = function.name)
    if (n.inputs == 1L)
    {
        input <- x[[1L]]
        output <- sumRows(x[[1L]], remove.missing = remove.missing)
        colnames.required <- variables.or.variable.sets[1L] && NCOL(x[[1L]]) > 1L
        if (colnames.required)
        {
            input.names <- getColumnNames(x[[1L]])
            null.input.names <- Filter(is.null, input.names)
            output.rownames <- rowNames(x[[1L]])
        }
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
            warnIfDataHasMissingValues(x, remove.missing = remove.missing)
        }
        if (return.column.counts)
            attr(output, "n.sum") <- computeSingleInputSampleSizeByRows(x[[1L]])

    } else
    {
        checkMultipleInputsAppropriateForSumRows(x, function.name = function.name)
        input <- splitIntoOneDimensionalVariables(x)
        input.names <- if (all(variables.or.variable.sets)) lapply(x, getInputNames) else NULL
        null.input.names <- Filter(is.null, input.names)
        input.rownames <- lapply(x, rowNames)
        output.rownames <- if (Reduce(identical, input.rownames)) input.rownames[[1L]] else NULL
        called.args <- match.call(expand.dots = FALSE)
        function.args <- formals()
        called.args[[1L]] <- function.args[[1L]] <- as.name('list')
        called.args <- eval.parent(called.args)
        called.args[["..."]] <- function.args[["..."]] <- NULL
        matched.args <- match(names(called.args), names(function.args), nomatch = 0L)
        if (length(matched.args))
            function.args[matched.args] <- called.args
        function.args[["return.column.counts"]] <- NULL
        function.args[["match.columns"]]  <- function.args[["match.rows"]] <- "No"
        return.total.element.weights <- if (return.column.counts) "Yes" else "No"
        output <- do.call(sumInputs, c(input, function.args,
                                       return.total.element.weights = return.total.element.weights))
    }
    if ((n.inputs > 1L && identical(null.input.names, list())) ||
        (n.inputs == 1L && colnames.required && identical(null.input.names, character(0L))))
    {
        output.colname <- paste0(unique(unlist(input.names)), collapse = " + ")
        dims.req <- c(length(output), 1L)
        dimnames.req <- list(output.rownames, output.colname)
        if (return.column.counts)
            n.sum <- attr(output, "n.sum")
        output <- array(output, dim = dims.req, dimnames = dimnames.req)
        if (return.column.counts)
            attr(output, "n.sum") <- array(n.sum, dim = dims.req, dimnames = dimnames.req)
    }
    output
}

containsAVariableInEachColumn <- function(x)
{
    isVariable(x) || isVariableSetWithSingleVariableInEachColumn(x)
}

isVariableSetWithSingleVariableInEachColumn <- function(x)
{
    if (!isVariableSet(x))
        return(FALSE)
    question.type <- attr(x, "questiontype")
    !(endsWith(question.type, "Grid") || endsWith(question.type, "Compact"))
}

getInputNames <- function(x)
{
    if (!is.null(label <- attr(x, "label")))
        return(label)
    if (!is.null(name <- attr(x, "name", exact = TRUE)))
        return(name)
    if (!is.null(question <- attr(x, "question", exact = TRUE)))
        return(question)
    if (!is.null(symbol <- attr(x, "symbol")))
        return(symbol)
}

getColumnNames <- function(x)
{
    x.names <- if (length(d <- dim(x)) && length(d) == 2L) colnames(x)
    if (!is.null(x.names))
        return(x.names)
    getInputNames(x)
}

#' @importFrom stats setNames
sumRows <- function(x, remove.missing)
{
    x.names <- rowNames(x)
    # Higher dimensional arrays that can occur in some Q Tables
    # are handled as a special case here.
    if (isQTable(x) && length(dim(x)) > 2)
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

flattenToSingleList <- function(input.list)
{
    args <- lapply(input.list, function(x) if (is.list(x)) flattenToSingleList(x) else list(x))
    do.call(c, args)
}

#' @importFrom stats setNames
splitIntoOneDimensionalVariables <- function(x)
{
    x.rownames <- Filter(Negate(is.null), lapply(x, rowNames))
    x.rownames <- if (identical(x.rownames, list())) NULL else x.rownames[[1L]]
    y <- lapply(x, splitIntoVariables)
    listed.vars <- vapply(y, is.list, logical(1L))
    if (any(listed.vars))
        y <- flattenToSingleList(y)
    y[[1L]] <- setNames(y[[1L]], x.rownames)
    y
}

splitIntoVariables <- function(x)
{
    if (NCOL(x) == 1L && !is.data.frame(x))
        return(as.vector(x))
    else if ((is.df <- is.data.frame(x)) || is.array(x))
    {
        if (is.df)
            x <- as.list(x)
        else
            x <- split(x, col(x))
        names(x) <- NULL
    }
    x
}

checkMultipleInputsAppropriateForSumRows <- function(x, function.name)
{
    checkPossibleToSplitIntoNumericVectors(x, function.name)
    checkNumberRowsAgree(x, function.name)
}

canSplitIntoVectors <- function(x, function.name)
{
    if (isQTable(x))
    {
        table.name <- getInputNames(x)
        stop(function.name, " doesn't support Tables when more than one input is provided. ",
             "Either remove the input ", table.name, " and any other Tables from the input ",
             "or call ", function.name, " again with only ", table.name, " as the input.")
    }

    ((is.numeric(x) || is.logical(x) || is.factor(x)) && getDim(x) < 3L) ||
        is.data.frame(x)
}

checkPossibleToSplitIntoNumericVectors <- function(x, function.name)
{
    for (i in seq_along(x))
        if (!canSplitIntoVectors(x[[i]], function.name))
            stop(function.name, " requires all input elements to be numeric vectors ",
                 "or reducible to individual numeric vectors such as a numeric matrix or ",
                 "data frame containing numeric elements. ",
                 "One of the provided input elements is a ", class(x[[i]]))
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

computeSingleInputSampleSizeByRows <- function(x)
{
    if (is.data.frame(x) || is.matrix(x))
        apply(!is.na(x), 1L, sum, na.rm = TRUE)
    else if (is.array(x) && length(dim(x)) == 3L)
        apply(!is.na(x), c(1L, if (dim(x)[3L] == 1L) NULL else 3L), sum)
    else
        (!is.na(x)) * 1L
}
