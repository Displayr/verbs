#' @rdname ExtremeOperations
#' @param x A single input to be used in \code{MinEachRow} or
#'     \code{MinEachColumn}.
#' @description In a similar way, \code{MinEachColumn}
#'     (\code{MaxEachColumn}) is a vectorized generalization of
#'     \code{\link{min}} (\code{\link{max}}) calculating the minimum
#'     (maximum) separately for each column, supporting row removal
#'     and the application of filters before calculation but not
#'     supporting multiple inputs.  The functions \code{MinColumns}
#'     and \code{MinRows} are aliases for \code{MinEachColumn} and
#'     \code{MinEachRow}, and similarly for \code{MaxColumns} and
#'     \code{MaxRows}.
#' @details The permissible types for \code{x} are a \code{numeric}
#'     vector, \code{array} with at most 2 dimensions, a
#'     \code{data.frame}, Q Table, \code{matrix} or other possible
#'     structure that has well defined rows or columns.
#'
#'  In the case of a vector of length n, it is interpreted to be an
#'  input with n rows and a single column. An array is only
#'  permissible if it has 2 dimensions, the exception being higher
#'  order arrays are only allowed in the form of a Q Table.
#'
#' @return The \code{MinEachColumn} (\code{MaxEachColumn}) function
#'     returns the minimum (maximum) of all the elements in each column index
#'     provided in the input, possibly after elements of \code{x} have
#'     been filtered/removed via the other arguments.
#'
#' The names of the output vector are taken from the \code{dimnames}
#' of \code{x}.
#' @examples
#'
#' # Examples using MinEachColumn
#' input.matrix <- matrix(1:8, nrow = 4)
#' MinEachColumn(input.matrix) #= c(10, 26)
#' named.matrix <- input.matrix
#' dimnames(named.matrix) <- list(LETTERS[1:4], letters[1:2])
#' MinEachColumn(named.matrix)
#' MinEachColumn(named.matrix, remove.rows = c("A", "C"))
#' MinEachColumn(named.matrix, subset = c(TRUE, FALSE, TRUE, FALSE))
#' MinEachColumn(named.matrix, remove.rows = c("B", "D"))
#' @export
MinEachColumn <- function(x,
                          remove.missing = TRUE,
                          remove.rows = c("NET", "SUM", "Total"),
                          remove.columns = NULL,
                          subset = NULL,
                          warn = FALSE)
{
    extremeColumns(x,
               remove.missing = remove.missing,
               remove.rows = remove.rows,
               remove.columns = remove.columns,
               subset = subset,
               warn = warn,
               function.name = sQuote(deparse(sys.call()[[1]])))
}

#' @rdname ExtremeOperations
#' @export
MinColumns <- MinEachColumn

#' @rdname ExtremeOperations
#' @examples
#'
#' input.matrix <- matrix(1:8, nrow = 4)
#' MaxEachColumn(input.matrix) #= c(10, 26)
#' named.matrix <- input.matrix
#' dimnames(named.matrix) <- list(LETTERS[1:4], letters[1:2])
#' MaxEachColumn(named.matrix)
#' MaxEachColumn(named.matrix, remove.rows = c("A", "C"))
#' MaxEachColumn(named.matrix, subset = c(TRUE, FALSE, TRUE, FALSE))
#' MaxEachColumn(named.matrix, remove.rows = c("B", "D"))
#' @export
MaxEachColumn <- function(x,
                          remove.missing = TRUE,
                          remove.rows = c("NET", "SUM", "Total"),
                          remove.columns = NULL,
                          subset = NULL,
                          warn = FALSE)
{
    extremeColumns(x,
               remove.missing = remove.missing,
               remove.rows = remove.rows,
               remove.columns = remove.columns,
               subset = subset,
               warn = warn,
               function.name = sQuote(deparse(sys.call()[[1]])))
}

#' @rdname ExtremeOperations
#' @export
MaxColumns <- MaxEachColumn

extremeColumns <- function(x,
                       remove.missing = TRUE,
                       remove.rows = c("NET", "SUM", "Total"),
                       remove.columns = NULL,
                       subset = NULL,
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
                                              subset = subset, weights = NULL,
                                              return.total.element.weights = FALSE,
                                              warn = warn,
                                              function.name = function.name)[[1L]]
    output <- extremeCols(input, function.name, remove.missing = remove.missing)
    if (warn && NROW(input) == 1L)
            throwWarningAboutCalculationWithSingleElement(input, dimension = 1L, function.name)

    return(output)
}

#' @importFrom stats setNames
#' @noRd
extremeCols <- function(x, function.name, remove.missing = TRUE, dims)
{
    function.to.use <- ifelse(grepl("Max", function.name), max, min)
    if (NCOL(x) == 1)
    {
        y <- baseExtreme(x, fun = function.to.use, remove.missing = remove.missing)
        if (isVariable(x) || isQTable(x))
            y <- setNames(y, getInputNames(x))
    } else
    {
        if (isQTable(x) && getDimensionLength(x) > 2)  # calc. for each stat in last dimension
            y <- apply(x, c(2, 3), baseExtreme,
                       fun = function.to.use,
                       remove.missing = remove.missing)
        else
            y <- apply(x, 2L,
                       baseExtreme,
                       fun = function.to.use,
                       remove.missing = remove.missing)
        if (is.data.frame(x) && any(variables.inside <- vapply(x, isVariable, logical(1L))))
            names(y)[variables.inside] <- vapply(x[variables.inside],
                                                 getInputNames,
                                                 character(1L))
    }
    y
}

#' @rdname ExtremeOperations
#' @description Also, \code{MinEachRow} (\code{MaxEachRow}) is a
#'     vectorized generalization of \code{\link{min}}
#'     (\code{\link{max}}) calculating the minimum separately for each
#'     row, supporting column removal before calculation but not
#'     supporting filters, weights nor multiple inputs.
#' @details For \code{MaxEachRow} (\code{MinEachRow}) the maximum
#'     (minimum) is computed within the row dimension of the input.
#'     E.g. a n x p matrix supplied to \code{MaxEachRow} will produce
#'     a vector of of length \code{n}.
#'
#' @examples
#'
#' input.matrix <- matrix(runif(6), nrow = 3, dimnames = list(letters[1:3], c("Q1", "Q2")))
#' MinEachRow(input.matrix)
#' input.matrix.with.total <- cbind(input.matrix, "Total" = rowSums(input.matrix))
#' MaxEachRow(input.matrix.with.total) # The total column is removed by default
#' MaxEachRow(input.matrix.with.total, remove.columns = NULL)
#' @export
MaxEachRow <- function(x,
                       remove.missing = TRUE,
                       remove.rows = NULL,
                       remove.columns = c("NET", "SUM", "Total"),
                       warn = FALSE)
{
    extremeRowsInputs(x,
                  remove.missing = remove.missing,
                  remove.rows = remove.rows,
                  remove.columns = remove.columns,
                  return.column.counts = FALSE,
                  warn = warn,
                  function.name = sQuote(deparse(sys.call()[[1]])))
}

#' @rdname ExtremeOperations
#' @export
MaxRows <- MaxEachRow

#' @rdname ExtremeOperations
#' @export
MinEachRow <- function(x,
                       remove.missing = TRUE,
                       remove.rows = NULL,
                       remove.columns = c("NET", "SUM", "Total"),
                       warn = FALSE)
{
    extremeRowsInputs(x,
                  remove.missing = remove.missing,
                  remove.rows = remove.rows,
                  remove.columns = remove.columns,
                  return.column.counts = FALSE,
                  warn = warn,
                  function.name = sQuote(deparse(sys.call()[[1]])))
}

#' @rdname ExtremeOperations
#' @export
MinRows <- MinEachRow


extremeRowsInputs <- function(x,
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
    output <- extremeRows(input, function.name, remove.missing = remove.missing)
    if (warn)
    {
        if (NCOL(input) == 1L)
            throwWarningAboutCalculationWithSingleElement(input, dimension = 2L,
                                                          function.name)
        warnIfDataHasMissingValues(x, remove.missing = remove.missing)
    }
    return(output)
}

#' @importFrom stats setNames
extremeRows <- function(x, function.name, remove.missing)
{
    function.to.use <- ifelse(grepl("Max", function.name), max, min)
    x.names <- rowNames(x)
    # Higher dimensional arrays that can occur in some Q Tables
    # are handled as a special case here.
    if (isQTable(x) && getDimensionLength(x) > 2)
    {
        y <- apply(x, c(1L, 3L), baseExtreme,
                   fun = function.to.use,
                   remove.missing = remove.missing)
        if (NCOL(y) == 1L)
        {
            y <- as.vector(y)
            names(y) <- x.names
        }
        y
    } else if (NCOL(x) == 1)
    {
        if (is.data.frame(x))
            x
        else
            setNames(as.vector(x), nm = x.names)
    } else
        setNames(as.vector(apply(x, 1,
                                 baseExtreme,
                                 fun = function.to.use,
                                 remove.missing = remove.missing)),
                 nm = x.names)
}

#' This is a simple wrapper for apply that ensures there is only one
#' warning thrown if multiple rows/columns contain all missing values
#' when remove.missing = TRUE
#' @noRd
applyWarnOnce <- function(x, MARGIN, FUN, ...)
{
    dim.str <- ifelse(MARGIN[1] == 1, "rows", "columns")
    replace.str <- ifelse(FUN(0, 1) == 0,
                          "Infinity", "-Infinity")
    no.non.missing.msg <- paste0("There were ", dim.str, " in the input data with entirely ",
                                 "missing values. They are given the value ", replace.str,
                                 " in the output.")

    ## if x is an ftable object, apply first calls as.matrix.ftable, which can
    ## mangle the names of the output if the correct sep argument isn't supplied
    if (inherits(x, "ftable"))
        x <- as.matrix(x, sep = " - ")

    should_warn <- FALSE
    out <- withCallingHandlers(apply(x, MARGIN, FUN, ...),
                               warning = function(w)
                               {
                                   if (grepl("no non-missing", w$message))
                                   {
                                       should_warn <<- TRUE
                                       invokeRestart("muffleWarning")
                                   }
                                   w
                               })
    if (should_warn)
        warning(no.non.missing.msg, call. = FALSE)
    return(out)
}
