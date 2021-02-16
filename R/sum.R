#' @rdname SumOperations
#' @title General-Purpose Summation functions
#'
#' @description \code{Sum} is a generalization of \code{\link{sum}} and the + function in \code{\link{Arithmetic}}
#'  but supporting additional pre-processing and matching of data before calculation.
#' @param ... Objects to be summed; e.g. Q Tables, arrays, vectors, Variables or Variable Sets.
#' @param remove.missing Logical element controlling whether missing values should be
#'   removed in calculations? Defaults to \code{TRUE}. If set to \code{FALSE} and if one of
#'   input elements contains missing values, then the resulting computed sum will also be
#'   \code{NA}.
#' @param remove.rows The labels of named vectors, or, rows of tables and similar
#'   structures to be removed from the row dimension of the input. Any row elements
#'   with the labels specified here will not be included in the resulting \code{Sum}
#'   calculation.
#' @param remove.columns The labels of columns of tables and similar
#'   structures to be removed from the column dimension of the input. Any column elements
#'   with the labels specified here will not be included in the resulting \code{Sum}
#'   calculation.
#' @param match.rows The names of the row elements of the inputs are inspected and compared.
#'   If matches are found then the input will permute the rows of the elements so that the
#'   elements match in the row dimension. There are five options available for row matching.
#'   \itemize{
#'     \item \code{"Yes"} Perform an exact name match between the row names of input elements.
#'     Any row names that are not matched in the other outputs are permissible, however that
#'     row is then treated as missing values. The resulting row will either be \code{NA} if
#'     \code{remove.missing} is set to \code{FALSE} or zero if \code{remove.missing} is set
#'     to \code{TRUE}.
#'     \item \code{"Yes - hide unmatched"} Performs an exact name match between
#'     input elements in the same manner as \code{"Yes"} option. However, unmatched row names
#'     will cause the entire row to be removed before calculation.
#'     \item \code{"Fuzzy"} Same behaviour as \code{"Yes"} except the matching uses fuzzy name
#'     matching. First an exact name match is performed, any remaining unmatched elements are attempted to
#'     be matched using near matches using the Levenshtein distance. The algorithm will
#'     match elements that have a one-to-one matching with a Levenshtein distance of
#'     at most 1 unit, ignoring case. Following this, some common variants of questionnaire
#'     reponses are attempted to be matched. E.g. \code{"None of these"} could be matched
#'     with \code{"none"}. This occurs for variants of \code{"Other"}, \code{"All of these"}
#'     and variants of \code{"Don't know"}. If further unmatched elements exist, punctuation
#'      and white space are removed and matches attempted. If any unmatched still exist, then
#'     they are appended a value of zero before calculation in the same manner as
#'     \item \code{"Fuzzy - hide unmatched"} Similar to \code{"Fuzzy"} except any unmatched row
#'     names cause the entire row to be removed from the calculation in a similar way to
#'     \code{"Yes - hide unmatched"}.
#'     \item \code{"No"} No matching is to occur with the row names. In this case, all
#'     input elements need to have the same number of rows.
#'     }
#' @param match.columns Performs matching on the column names of the inputs. The behaviour
#'     and argument options are the same as \code{match.rows} except the algorithm performs
#'     them on the column names.
#' @param warn Logical element to control whether warnings are shown when non-obvious
#'   operations are performed (e.g., removing rows, removing missing values when they are present).
#'   Possible warnings presented include \itemize{
#'     \item Whether missing values were identified and removed if \code{remove.missing}
#'       is set to \code{TRUE}
#'     \item Whether different statistics are being summed in the case of \code{Q Table}.
#'     \item Whether unmatched rows or columns have been removed from the calculation if the user
#'     has specified for unmatched elements to be hidden.
#'     \item Whether any inputs have been reshaped.
#'   }
#' @param subset Logical vector of units to use in the calculation. Only applied to variables and
#'   not to \code{Q Table}s that contain statistics since the original variable data is unavailable.
#' @param weights Numeric vector of weights to use in the calculation. Only applies to variables
#'   and not to \code{Q Table}s that contain statistics since the original variable data is unavailable.
#' @details For \code{Sum}, if a single input element is provided, then the element is added in the same
#'   way as \code{\link{sum}}, i.e. all elements added together to give a single scalar value.
#'   If multiple input elements are provided, then elementwise addition is performed in a similar way
#'   to the + function in \code{\link{Arithmetic}}. In the case of multiple inputs, the dimensions need to match before elementwise
#'   addition can occur. i.e. if the first element is a 3 x 2 matrix, then the second element needs to be
#'   a matrix of the same dimension. Partial dimension matching is also supported, so if an n x p matrix is
#'   used as the first input, then the second input could be an n x 1 column vector that is reshaped to an
#'   n x p matrix before calculation.
#' @return The \code{Sum} function returns the summation of all the elements provided in the input,
#'   possibly after the elements have had entries removed, filtered out or weighted using the provided
#'   options.
#' @export
#' @examples
#' # Examples using Sum
#' x <- runif(4)
#' Sum(x)
#' all.equal(Sum(x), sum(x))
#' x <- 1:9
#' desired.subset = rep(c(TRUE, FALSE), c(3, 6))
#' Sum(x, subset = desired.subset)
#' desired.weights <- runif(9)
#' y <- 10:18
#' Sum(x, y, weights = desired.weights)
#' x * desired.weights +  y * desired.weights
#' x <- matrix(1:12, nrow = 4, ncol = 3, dimnames = list(letters[1:4], LETTERS[1:3]))
#' y <- matrix(1:20, nrow = 5, ncol = 4, dimnames = list(letters[1:5], LETTERS[1:4]))
#' Sum(x, y, remove.rows = "e", remove.columns = "D")
#' x <- matrix(1:12, nrow = 4, ncol = 3, dimnames = list(letters[1:4], letters[1:3]))
#' y <- matrix(1:20, nrow = 5, ncol = 4, dimnames = list(LETTERS[1:5], LETTERS[1:4]))
#' Sum(x, y, match.rows = "Yes", match.columns = "Yes")
#' Sum(x, y, match.rows = "Fuzzy", match.columns = "Fuzzy")
Sum <- function(...,
                remove.missing = TRUE,
                remove.rows = NULL, remove.columns = NULL,
                match.rows = "Yes", match.columns = "Yes",
                subset = NULL, weights = NULL,
                warn = FALSE)
{
    function.name <- sQuote("Sum")
    x <- list(...)
    x <- processArguments(x,
                          remove.missing = remove.missing,
                          remove.rows = remove.rows, remove.columns = remove.columns,
                          subset = subset, weights = weights,
                          check.statistics = TRUE,
                          warn = warn,
                          function.name = function.name)
    if (length(x) == 1)
        sum.output <- sum(x[[1L]], na.rm = remove.missing)
    else # Remove missing if required to use base::`+`
    {
        checkMatchingArguments(list(match.rows, match.columns))
        if (remove.missing)
            x <- lapply(x, removeMissing)
        .sumFunction <- function(x, y)
        {
            addTwoElements(x, y,
                           match.rows = match.rows, match.columns = match.columns,
                           remove.missing = remove.missing,
                           function.name = function.name,
                           warn = warn)
        }
        sum.output <- Reduce(.sumFunction, x)
        sum.output <- sanitizeAttributes(sum.output)
    }
    if (warn && any(nan.output <- isNaN(sum.output)))
    {
        opposite.infinities <- determineIfOppositeInfinitiesWereAdded(x, nan.output, match.rows, match.columns)
        warnAboutOppositeInfinities(opposite.infinities, function.name)
    }
    sum.output
}

addTwoElements <- function(x, y,
                           match.rows, match.columns,
                           remove.missing,
                           warn,
                           function.name)
{
    input <- list(x, y)
    # Coerce any vectors to 1d array
    input <- coerceToVectorTo1dArrayIfNecessary(input)
    matching <- list(match.rows, match.columns)
    matching.required <- vapply(matching, function(x) x != "No", logical(1L))
    if (any(matching.required))
        input <- matchDimensionElements(input, match.rows, match.columns, remove.missing,
                                        warn, function.name)
    input <- reshapeIfNecessary(input, warn, function.name = function.name)
    checkDimensionsEqual(input, function.name = function.name)
    if (any(!matching.required))
        input <- assignLabelsIfPossible(input,
                                        dimension = which(!matching.required))
    output <- `+`(input[[1L]], input[[2L]])
    output
}

removeMissing <- function(x)
{
    if (any(missing.values <- is.na(x)))
        x[missing.values] <- 0
    x
}
