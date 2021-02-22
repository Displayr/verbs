#' @rdname AverageOperations
#' @inheritParams SumColumns
#' @return The \code{AverageColumns} function returns the summation of all the elements in each column
#'   index provided in the input, possibly after the elements have been pre-processed similar
#'   to \code{Sum}.
#' @examples
#' # Examples using SumColumns
#' input.matrix <- matrix(1:8, nrow = 4)
#' AverageColumns(input.matrix) #= c(10, 26)
#' named.matrix <- input.matrix
#' dimnames(named.matrix) <- list(LETTERS[1:4], letters[1:2])
#' AverageColumns(named.matrix)
#' AverageColumns(named.matrix, remove.rows = c("A", "C"))
#' AverageColumns(named.matrix, subset = c(TRUE, FALSE, TRUE, FALSE))
#' AverageColumns(named.matrix, remove.rows = c("B", "D"))
#' # Each element is summed individually
#' # The order of input determines the order of output.
#' w <- c(a = 1, b = 2, c = 3, d = 4)
#' x <- c(a = 1, b = 2)
#' y <- c(b = 3, c = 10)
#' z <- c(c = -1, d = 3)
#' AverageColumns(w, x, y, z)
#' AverageColumns(z, y, x, w)
#' AverageColumns(sample(w), sample(x), sample(y), sample(z))
#' @export
AverageColumns <- function(...,
                           remove.missing = TRUE,
                           remove.rows = c("NET", "SUM", "Total"),
                           subset = NULL,
                           weights = NULL,
                           warn = FALSE)
{
    called.args <- match.call(expand.dots = FALSE)
    function.args <- formals()
    called.args[[1L]] <- as.name('list')
    called.args[["..."]] <- function.args[["..."]] <- NULL
    called.args <- eval(called.args, parent.frame())
    matched.args <- match(names(called.args), names(function.args), nomatch = 0L)
    if (length(matched.args))
        function.args[matched.args] <- called.args[]
    inputs <- list(...)
    inputs <- Filter(Negate(is.null), inputs)
    if (identical(inputs, list()))
        return(NaN)
    attr(inputs[[1L]], "called.from.average") <- "AverageColumns"
    new.arguments <- c(inputs, function.args)
    computed.sum <- do.call("SumColumns", new.arguments)
    n.sum <- attr(computed.sum, "n.sum")
    attr(computed.sum, "n.sum") <- NULL
    computed.sum / n.sum
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
