#' @rdname SumOperations
#' @param match.elements Character string specifying if elements should be matched
#'   before calculation. This is only possible in the case of multiple single vectors
#'   being given as inputs. In that case, their indices could be matched using their
#'   names via an exact match or fuzzy match before calculation occurs. The details of
#'   the available options are:
#'    \itemize{
#'     \item \code{"Yes - ignore if unmatched"} Perform an exact name match between
#'     input elements. Any elements that are not matched in the other outputs are permissible
#'     and an element with value zero is appended in that case before calculation occurs.
#'     See examples for more information.
#'     \item \code{"Yes - error if unmatched"} Performs an exact name match between
#'     input elements. However, any named elements that are not completely matched will
#'     trigger an error to be thrown.
#'     \item \code{"Fuzzy - ignore if unmatched"} Performs fuzzy name matching. First
#'     an exact name match is performed, any remaining unmatched elements are attempted to
#'     be matched using near matches using the Levenshtein distance. The algorithm will
#'     match elements that have a one-to-one matching with a Levenshtein distance of
#'     at most 1 unit, ignoring case. Following this, some common variants of questionnaire
#'     reponses are attempted to be matched. E.g. \code{"None of these"} could be matched
#'     with \code{"none"}. This occurs for variants of \code{"Other"}, \code{"All of these"}
#'     and variants of \code{"Don't know"}. If further unmatched elements exist, punctuation
#'      and white space are removed and matches attempted. If any unmatched still exist, then
#'     they are appended a value of zero before calculation in the same manner as
#'     \code{"Yes - ignore if unmatched"}.
#'     \item \code{"Fuzzy - error if unmatched"} Performs fuzzy name matching as above.
#'       However, if any unmatched elements remain after the fuzzy matching has been
#'       performed will trigger an error.
#'     \item \code{"No"} No matching is to occur with the input elements to be stacked
#'      together before calculation. Assuming the elements are of the same row length.
#'   }
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
                    match.elements = "Yes - ignore if unmatched",
                    subset = NULL,
                    weights = NULL,
                    warn = FALSE)
{
    function.name <- sQuote(match.call()[[1]])
    x <- list(...)
    x <- lapply(x, extractChartDataIfNecessary)
    x <- subsetAndWeightInputsIfNecessary(x,
                                          subset = subset,
                                          weights = weights,
                                          warn = warn,
                                          function.name = function.name)
    x <- lapply(x, removeRowsAndCols,
                remove.rows = remove.rows,
                remove.columns = remove.columns,
                function.name = function.name)
    n.inputs <- length(x)
    if (n.inputs == 1)
    {
        x.in <- x[[1L]]
        if (warn)
        {
            checkForMultipleStatistics(x.in, function.name = function.name)
            warnAboutRemovedElements(x.in)
        }
        sum.output <- sumRowsSingleInput(x.in,
                                         remove.missing = remove.missing,
                                         subset = subset,
                                         weights = weights,
                                         warn = warn,
                                         function.name = function.name)
    }
    else
    {
        sum.function <- function(x, y) sumRowsTwoInputs(x, y,
                                                        remove.missing = remove.missing,
                                                        remove.rows = remove.rows,
                                                        subset = subset,
                                                        weights = weights,
                                                        match.elements = match.elements,
                                                        warn = warn,
                                                        function.name = function.name)
        sum.output <- Reduce(sum.function, x)
    }
    if (warn)
    {
        warnAboutRemovedElements(x)
        checkMissingData(x, remove.missing = TRUE)
        if (any(nan.output <- is.nan(sum.output)))
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
    sum.output
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

sumRowsTwoInputs <- function(x,
                             y,
                             remove.missing,
                             remove.rows,
                             subset,
                             weights,
                             match.elements,
                             warn,
                             function.name)
{
    inputs <- list(x, y)
    lapply(inputs, checkIfSuitableVectorType, function.name = function.name)
    inputs <- lapply(inputs, AsNumeric, binary = FALSE)
    binded <- matchRows(inputs, match.elements = match.elements, warn = warn, function.name = function.name)
    output <- rowSums(binded, na.rm = remove.missing)
    output
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
