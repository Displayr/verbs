#' @rdname SumOperations
#' @title General-Purpose Summation functions
#'
#' @description A generalization of \code{\link{sum}}, \code{\link{colSums}} and \code{\link{rowSums}}
#'  supporting additional pre-processing and matching of data before calculation.
#' @param ... Objects to be summed; e.g. Q Tables, arrays, vectors, Variables or Variable Sets.
#'   Depending on if \code{Sum}, \code{SumRows} or \code{SumColumns} is used, different possible
#'   multiple input types are allowed. See the details section for more information.
#' @param remove.missing Logical element controlling whether missing values should be
#'   removed in calculations? Defaults to \code{TRUE}. If set to \code{FALSE} and if one of
#'   input elements contains missing values, then the resulting computed sum will also be
#'   \code{NA}.
#' @param remove.rows The labels of named vectors, or, rows of tables and similar
#'   structures to be removed from the row dimension of the input. Any row elements
#'   with the labels specified here will not be included in the resulting sum calculation.
#' @param remove.columns The labels of columns of tables and similar
#'   structures to be removed from the column dimension of the input. Any column elements
#'   with the labels specified here will not be included in the resulting sum calculation.
#' @param match.rows
#' @param match.columns
#' @param warn Logical element to control whether warnings are shown when non-obvious
#'   operations are performed (e.g., removing rows, removing missing values when they are present).
#'   Possible warnings presented include \itemize{
#'     \item Whether missing values were identified and removed if \code{remove.missing}
#'       is set to \code{TRUE}
#'     \item Rows removed due to the options provided in \code{remove.rows}
#'     \item Columns removed due to the options provided in \code{remove.columns}
#'     \item Whether different statistics are being summed in the case of \code{Q Table}.
#'   }
#' @param subset Logical vector of units to use in the calculation. Only applied to variables and
#'   not to \code{Q Table}s that contain statistics since the original variable data is unavailable.
#' @param weights Numeric vector of weights to use in the calculation. Only applies to variables
#'   and not to \code{Q Table}s that contain statistics since the original variable data is unavailable.
#' @details The \code{Sum} function takes all the input elements and processes the inputs according
#'  to the other specified input options above. Then sums together the remaining elements to produce a
#'  single scalar value.
#' @return The \code{Sum} function returns the summation of all the elements provided in the input,
#'   possibly after the elements have had entries removed, filtered out or weighted using the provided
#'   options.
#' @export
#' @examples
#' # Examples using Sum
#' Sum(1:3, 2:4)
#' all.equal(Sum(1:3, 2:4), sum(1:3, 2:4))
#' x <- 1:4
#' names(x) <- c(LETTERS[1:3], "SUM")
#' Sum(x)
#' x <- 1:9
#' desired.subset = rep(c(TRUE, FALSE), c(3, 6))
#' Sum(x, subset = desired.subset)
#' desired.weights <- runif(9)
#' y <- 10:18
#' Sum(x, y, weights = desired.weights)
#' sum(x * desired.weights, y * desired.weights)
#' basic.table <- matrix(1:12, nrow = 4, dimnames = list(letters[1:4], LETTERS[1:3]))
#' table.with.marginals <- rbind(basic.table, SumColumns(basic.table))
#' table.with.marginals <- cbind(table.with.marginals, c(SumRows(basic.table), Sum(basic.table)))
#' dimnames(table.with.marginals) <- list(c(letters[1:4], "SUM"), c(LETTERS[1:3], "SUM"))
#' all.equal(Sum(table.with.marginals), sum(basic.table))
Sum <- function(...,
                remove.missing = TRUE,
                remove.rows = NULL, remove.columns = NULL,
                match.rows = "Yes", match.columns = "Yes",
                subset = NULL, weights = NULL,
                warn = FALSE)
{
    function.name <- sQuote(match.call()[[1]])
    x <- processArguments(...,
                          remove.missing = remove.missing,
                          remove.rows = remove.rows, remove.columns = remove.columns,
                          match.rows = match.rows, match.columns = match.columns,
                          subset = subset, weights = weights,
                          check.statistics = FALSE,
                          warn = warn,
                          function.name = function.name)
    if (length(x) == 1)
        sum.output <- sum(x[[1L]], na.rm = remove.missing)
    else # Remove missing if required to use base::`+`
    {
        if (remove.missing)
            x <- lapply(x, removeMissing)
        sum.output <- as.vector(Reduce(`+`, x))
    }
    if (warn && any(nan.output <- is.nan(sum.output)))
    {
        # If a single input, inspect all the inputs at once
        if (length(x) == 1)
            opposite.infinities <- checkForOppositeInfinites(unlist(x))
        else # If multiple inputs, check the element-wise elements separately
        {
            nan.elements <- which(nan.output)
            elements.calculating.to.nan <- lapply(1:length(nan.elements), function(i) {
                    unlist(lapply(x, '[', nan.elements[i]))
                })
            opposite.infinities <- logical(length(nan.elements))
            opposite.infinities[nan.elements] <- vapply(elements.calculating.to.nan,
                                                        checkForOppositeInfinites,
                                                        logical(1))
        }
        warnAboutOppositeInfinities(opposite.infinities, function.name)
    }
    sum.output
}

removeMissing <- function(x)
{
    if (any(missing.values <- is.na(x)))
        x[missing.values] <- 0
    x
}
