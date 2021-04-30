#' @rdname SumOperations
#' @title General-Purpose Summation functions
#'
#' @description \code{Sum} is a generalization of \code{\link{sum}} and the + function in \code{\link{Arithmetic}}
#'  but supporting additional pre-processing and matching of data before calculation.
#' @param ... Objects to be summed; e.g. vectors, matrices, Variables, Variable Sets or Q Tables
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
#' @param match.elements Either a single string with three possible options or named character vector with two elements. The possible single character options are: \itemize{
#'   \item "No": Ignores names and requires either inputs which the same dimensions
#'         or partially agreeing where recycling can be performed. See details for more information.
#'   \item "Yes - hide unmatched" or "Yes": Performs a matching algorithm that checks row names and column names
#'         of all elements and attempts to match the appropriate elements. The matching
#'         will check both exact matches or fuzzy matches and permutes the order of the elements
#'         so that the names match. It also may transpose an input if,
#'         for example, the column names of one input match the row names of another input.
#'         Any unmatched elements are removed from the input before calculation.
#'   \item "Yes - show unmatched": Performs the same matching algorithm above but any
#'         unmatched elements are kept in the input. The other input without the element has
#'         missing values spliced in.
#'         }
#'    A named character vector is possible but it must have two elements named \code{rows}
#'    or \code{columns} (partial matching names permissible) that specify the matching behavior
#'    for rows and columns.
#'    E.g. \code{match.elements = c(rows = "Yes - hide unmatched", columns = "No")} to specify
#'    that rows are to be matched but unmatched rows are to be removed from the calculation.
#'    The columns are not to be matched. The full set of alternatives for either \code{match.rows}
#'    or \code{match.columns} are given below but are described only for the row scenario.
#'   \itemize{
#'     \item \code{"Yes"} or \code{"Yes - hide unmatched"}: Perform an exact name match between
#'     the row names of input elements. Any unmatched row names will cause the entire row to be
#'     removed before calculation.
#'     \item \code{"Yes - show unmatched"} Performs an exact name match between
#'     input elements in the same manner as \code{"Yes"} option. However, any row names that are
#'     not matched in the other outputs are permissible. The input that doesn't have that row
#'     will have a row of missing values spliced in. The resulting row will either be \code{NA} if
#'     \code{remove.missing} is set to \code{FALSE} or zero if \code{remove.missing} is set
#'     to \code{TRUE}.
#'     \item \code{"Fuzzy"} Same behaviour as \code{"Yes"} except the matching uses fuzzy name
#'     matching. First an exact name match is performed, any remaining unmatched elements are attempted to
#'     be matched using near matches using the Levenshtein distance. The algorithm will
#'     match elements that have a one-to-one matching with a Levenshtein distance of
#'     at most 1 unit, ignoring case. Following this, some common variants of questionnaire
#'     reponses are attempted to be matched. E.g. \code{"None of these"} could be matched
#'     with \code{"none"}. This occurs for variants of \code{"Other"}, \code{"All of these"}
#'     and variants of \code{"Don't know"}. If further unmatched elements exist, punctuation
#'      and white space are removed and matches attempted.
#'     \item \code{"Fuzzy - show unmatched"} Similar to \code{"Fuzzy"} except any unmatched row
#'     names will have a row of missing values spliced in similar to \code{"Yes - show unmatched"}.
#'     \item \code{"No"} No matching is to occur with the row names. In this case, all
#'     input elements need to have the same number of rows.
#'     }
#' @param warn Logical element to control whether warnings are shown when non-obvious
#'   operations are performed (e.g., removing rows, removing missing values when they are present).
#'   Possible warnings presented include \itemize{
#'     \item Whether missing values were identified and removed if \code{remove.missing}
#'       is set to \code{TRUE}
#'     \item Whether different statistics are being summed in the case of Q Table.
#'     \item Whether unmatched rows or columns have been removed from the calculation if the user
#'     has specified for unmatched elements to be hidden.
#'     \item Whether any inputs have been recycled to higher dimensional forms.
#'   }
#' @param subset Logical vector of units to use in the calculation. Only applied to variables and
#'   not to Q Tables that contain statistics since the original variable data is unavailable.
#' @param weights Numeric vector of weights to use in the calculation. It is required to have the same
#'   number of elements as there are rows in the inputs as the weight vector is applied across the row
#'   dimension (elements in different columns but the same row will have the same weight element applied). The
#'   exception to this is that weights will not be applied to Q Tables containing statistics since
#'   the original variable data is unavailable.
#' @details For \code{Sum}, if a single input is provided, then the element is added in the same
#'   way as \code{\link{sum}}, i.e. all elements added together to give a single scalar value.
#'   If multiple input elements are provided, then element-wise addition is performed in a similar way
#'   to the + function in \code{\link{Arithmetic}}. In the case of multiple inputs, the dimensions need to match before elementwise
#'   addition can occur. i.e. if the first element is a 3 x 2 matrix, then the second element needs to be
#'   a matrix of the same dimension. If the inputs have named elements, then these names can be used to
#'   match up each of the elements between inputs via the \code{match.rows} and \code{match.columns}
#'   arguments. If either of \code{match.rows} or \code{match.columns} is set to \code{"No"} then names
#'   are ignored and the length on that dimension needs to agree between inputs. Partial dimension agreement
#'   is also supported. For example if an n x p matrix is used as the first input, then the second input
#'   could be an n x 1 column vector that is recycled to an n x p matrix before calculation.
#'
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
#' Sum(x, weights = desired.weights)
#' sum(x * desired.weights)
#' x <- matrix(1:12, nrow = 4, ncol = 3, dimnames = list(letters[1:4], LETTERS[1:3]))
#' y <- matrix(1:20, nrow = 5, ncol = 4, dimnames = list(letters[1:5], LETTERS[1:4]))
#' Sum(x, y, remove.rows = "e", remove.columns = "D")
#' x <- matrix(1:12, nrow = 4, ncol = 3, dimnames = list(letters[1:4], letters[1:3]))
#' y <- matrix(1:20, nrow = 5, ncol = 4, dimnames = list(LETTERS[1:5], LETTERS[1:4]))
#' Sum(x, y, match.elements = "Yes")
#' Sum(x, y, match.elements = c(rows = "Fuzzy - show unmatched",
#'                              columns = "Fuzzy - show unmatched"))
Sum <- function(...,
                remove.missing = TRUE,
                remove.rows = NULL, remove.columns = NULL,
                match.elements = "Yes",
                subset = NULL, weights = NULL,
                warn = FALSE)
{
    sumInputs(..., remove.missing = remove.missing,
              remove.rows = remove.rows, remove.columns = remove.columns,
              match.elements = match.elements,
              subset = subset, weights = weights,
              return.total.element.weights = "No",
              warn = warn, function.name = sQuote("Sum"))
}

#' Internal function to compute the sum with only a single additional logical argument
#' to control whether the number of elements in the sum should be returned
#' @inheritParams Sum
#' @param return.sample.size Logical element specifying whether the count of elements
#'   used in the sum were used. Used for calls to Average and its variants since the
#'   number of non-missing elements need to be tracked. In the case where weights are
#'   used, this element will return the sum of the weights for non-missing elements.
#' @param function.name Name of the calling function, used for generated warnings or errors.
#' @noRd
sumInputs <- function(...,
                      remove.missing = TRUE,
                      remove.rows = NULL, remove.columns = NULL,
                      match.elements = "Yes",
                      subset = NULL, weights = NULL,
                      return.total.element.weights = "No",
                      warn = FALSE,
                      function.name)
{
    x <- list(...)
    n.inputs <- length(x)
    x <- processArguments(x,
                          remove.missing = remove.missing,
                          remove.rows = remove.rows, remove.columns = remove.columns,
                          subset = subset, weights = weights,
                          return.total.element.weights = return.total.element.weights,
                          check.statistics = TRUE,
                          warn = warn,
                          function.name = function.name)
    keep.counts <- return.total.element.weights == "Yes"
    if (n.inputs == 1)
        sum.output <- sum(x[[1L]], na.rm = remove.missing)
    else
    {
        match.elements[tolower(match.elements) == "yes"] <- "Yes - hide unmatched"
        match.elements <- checkMatchingArguments(match.elements,
                                                 function.name = function.name)
        .sumFunction <- function(x, y)
        {
            addTwoElements(x, y,
                           match.elements = match.elements,
                           remove.missing = remove.missing,
                           function.name = function.name,
                           with.count.attribute = keep.counts,
                           warn = warn)
        }
        sum.output <- Reduce(.sumFunction, x)
        attr.to.keep <- eval(formals(sanitizeAttributes)[["attributes.to.keep"]])
        if (warn)
        {
            throwWarningIfTransposedInput(sum.output, function.name)
            unmatched.elements <- attr(sum.output, "unmatched")
            if (!is.null(unmatched.elements))
                throwWarningAboutUnmatched(unmatched.elements, function.name)
        }

        sum.output <- sanitizeAttributes(sum.output,
                                         attributes.to.keep = if (keep.counts) c(attr.to.keep, "n.sum")
                                                              else attr.to.keep)
    }
    if (warn && any(nan.output <- isNaN(sum.output)))
    {
        opposite.infinities <- determineIfOppositeInfinitiesWereAdded(x, nan.output, match.elements)
        warnAboutOppositeInfinities(opposite.infinities, function.name)
    }
    if (getDimensionLength(sum.output) == 1L)
    {
        n.sum <- attr(sum.output, "n.sum")
        sum.output <- setNames(as.vector(sum.output), nm = names(sum.output))
        if (keep.counts)
            attr(sum.output, "n.sum") <- n.sum
    }
    if (return.total.element.weights != "No" && n.inputs == 1L)
        sum.output <- appendSampleSizeAttribute(sum.output, x)
    sum.output
}

addTwoElements <- function(x, y,
                           match.elements,
                           remove.missing,
                           warn,
                           with.count.attribute,
                           function.name)
{
    input <- list(x, y)
    # Coerce any vectors to 1d array
    input <- coerceToVectorTo1dArrayIfNecessary(input)
    hide.unmatched <- any(endsWith(match.elements, "hide unmatched"))
    if (hide.unmatched && warn)
        attr(input, "unmatched") <- attr(x, "unmatched")
    automatic.matching <- length(match.elements) == 1L
    if (automatic.matching)
        input <- matchInputsUsingAutomaticAlgorithm(input, match.elements, warn, function.name)
    else
        input <- matchInputsUsingCustomArgs(input, match.elements, warn, function.name)
    if (hide.unmatched && warn)
        unmatched <- attr(input, "unmatched")
    if (with.count.attribute)
    {
        if (!is.null(previous.counts <- attr(x, "n.sum")))
        {
            counts.to.sum <- list(previous.counts, (!is.na(input[[2L]])) * 1L)
            dimensions <- lapply(input, dim)
            dimensions.equal <- identical(dimensions[[1L]], dimensions[[2L]])
            matching.required <- if (dimensions.equal) "No" else "Yes - hide unmatched"
            counts.to.sum <- matchDimensionElements(counts.to.sum, match.rows = matching.required,
                                                    match.columns = matching.required, warn = FALSE,
                                                    function.name = function.name)
            current.counts <- `+`(counts.to.sum[[1L]], counts.to.sum[[2L]])
        } else
        {
            non.missing.vals <- lapply(input, function(x) (!(is.na(x))) * 1L)
            current.counts <- `+`(non.missing.vals[[1L]], non.missing.vals[[2L]])
        }
    }

    input <- if (remove.missing) lapply(input, removeMissing) else input
    output <- `+`(input[[1L]], input[[2L]])
    if (with.count.attribute)
        attr(output, "n.sum") <- current.counts
    if (hide.unmatched && warn)
        attr(output, "unmatched") <- unmatched
    output
}

noMatchingButPossiblyRecycle <- function(input, warn, function.name)
{
    matchInputsUsingCustomArgs(input,
                               match.elements = rep("No", 2L),
                               warn = warn, function.name = function.name)
}

matchInputsUsingAutomaticAlgorithm <- function(input, match.elements, warn, function.name)
{
    if (length(match.elements) == 1L && tolower(match.elements) == "no")
        return(noMatchingButPossiblyRecycle(input, warn = warn, function.name = function.name))
    input.names <- lapply(input, getDimensionNamesOfInputs)
    inputs.with.missing.names <- lapply(input.names, checkMissingDimensionNames)
    if (any(unlist(inputs.with.missing.names)))
    {
        input <- mapply(removeElementsWithMissingNames, input, inputs.with.missing.names)
        throwWarningAboutMissingNames(function.name)
        input.names <- lapply(input, getDimensionNamesOfInputs)
    }
    input.names.exist <- lapply(input.names, dimnamesExist)
    input.with.no.names <- vapply(input.names.exist, function(x) all(!x), logical(1L))
    if (any(input.with.no.names))
        return(noMatchingButPossiblyRecycle(input, warn = warn, function.name = function.name))
    duplicate.names.found <- lapply(input.names, checkDuplicatedDimensionNames)
    if (any(unlist(duplicate.names.found)))
    {
        duplicated.names <- getDuplicateNames(input.names, duplicate.names.found)
        throwErrorAboutDuplicatedNamesWhenMatching(duplicated.names, function.name)
    }
    rownames.exist  <- vapply(input.names.exist, "[", logical(1L), i = 1L)
    colnames.exist  <- vapply(input.names.exist, "[", logical(1L), i = 2L)
    input.row.names <- lapply(input.names, "[[", i = 1L)
    input.col.names <- lapply(input.names, "[[", i = 2L)
    match.count <- array(0L, dim = c(4L, 2L),
                         dimnames = list(c("exact", "exact.transposed", "fuzzy", "fuzzy.transposed"),
                                         c("row", "column")))
    names.exist <- c(rows = all(rownames.exist), columns = all(colnames.exist))
    match.count[c(1L, 3L), 1:2] <- computeExactAndFuzzyMatchCounts(input.names, names.exist)

    transposed.names.exist    <- swapRowAndColumnEntries(input.names.exist)
    transposed.names          <- swapRowAndColumnEntries(input.names)
    transposed.rownames.exist <- vapply(transposed.names.exist, "[", logical(1L), i = 1L)
    transposed.colnames.exist <- vapply(transposed.names.exist, "[", logical(1L), i = 2L)
    transposed.names.exist    <- c(rows = all(transposed.rownames.exist),
                                   columns = all(transposed.colnames.exist))

    match.count[c(2L, 4L), 1:2] <- computeExactAndFuzzyMatchCounts(transposed.names, transposed.names.exist)
    total.matches <- rowSums(match.count)
    no.matches.found <- all(total.matches == 0L)
    if (no.matches.found && endsWith(match.elements, "hide unmatched"))
        throwErrorNoMatchingElementsFound(function.name)
    best.match <- total.matches[which.max(total.matches)]
    best.match.name <- names(best.match)
    if (endsWith(best.match.name, "transposed"))
    {
        input[[2L]] <- transposeInput(input[[2L]])
        rownames.exist <- transposed.rownames.exist
        colnames.exist <- transposed.colnames.exist
        attr(input[[1L]], "transposed.input") <- TRUE
    }
    show.unmatched <- endsWith(match.elements, "show unmatched")
    matching.used  <- if (startsWith(best.match.name, "fuzzy")) "Fuzzy - " else "Yes - "
    matching.used  <- paste0(matching.used, if (show.unmatched) "show unmatched" else "hide unmatched")
    match.elements <- ifelse(c(all(rownames.exist), all(colnames.exist)), matching.used, "No")
    matchInputsUsingCustomArgs(input, match.elements, warn, function.name)
}

removeElementsWithMissingNames <- function(input, ind.with.missing.names)
{
    if (any(ind.with.missing.names))
    {
        original.input <- input
        dimension <- which(ind.with.missing.names)
        for (dim in dimension)
        {
            .nameFunction <- switch(dim, rowNames, colNames)
            non.missing.indices <- which(!is.na(.nameFunction(input)))
            input <- reorderDimension(input, non.missing.indices, dim)
        }
        CopyAttributes(input, original.input)
    } else
        input
}

getDuplicatesOnDimension <- function(input.names, dimension)
{
    input.names <- lapply(input.names, "[[", i = dimension)
    unique(unlist(lapply(input.names, function(x) x[duplicated(x)])))
}

getDuplicateNames <- function(input.names, duplicated.indices)
{
    dims.with.duplicates <- duplicated.indices[[1L]] != 0L | duplicated.indices[[2L]] != 0L
    duplicate.names <- list(rows = NULL, columns = NULL)
    if (dims.with.duplicates[[1L]])
        duplicate.names[["rows"]] <- getDuplicatesOnDimension(input.names, 1L)
    if (dims.with.duplicates[[2L]])
        duplicate.names[["columns"]] <- getDuplicatesOnDimension(input.names, 2L)
    duplicate.names
}

throwErrorAboutDuplicatedNamesWhenMatching <- function(duplicated.names, function.name)
{
    duplicated.names <- Filter(Negate(is.null), duplicated.names)
    n.dims <- length(duplicated.names)
    dims.affected <- paste0(names(duplicated.names), collapse = " and ")
    duplicated.names <- lapply(duplicated.names, dQuote)
    if (n.dims == 2L)
        duplicated.names <- lapply(duplicated.names,
                                   function(x)  if (length(x) == 1L) x else paste0("(", paste0(x, collapse = ", "), ")"))
    duplicate.details <- ngettext(n.dims,
                                  paste0("The observed duplicate names along the ", dims.affected,
                                         " were: ", duplicated.names[[1]], ". "),
                                  paste0("The observed duplicate names were ",
                                         paste0(duplicated.names, collapse = " and "),
                                         " along the rows and columns respectively. "))
    stop("Some inputs have duplicated names along the ", dims.affected, ". ",
         "It is not possible to match elements before using ", function.name, " in this situation. ",
         duplicate.details,
         "Ensure there are unique names to match or turn off element matching before attempting ",
         "to compute ", function.name, " again.")
}

throwErrorNoMatchingElementsFound <- function(function.name)
{
    stop("After inspecting the element labels, no matches could be found and no ",
         "matching could be performed. Please ensure there are common labels before ",
         "attempting to recalculate ", function.name, " with element matching. ",
         "Alternatively, give inputs that are the same size or can be recycled to be the same size ",
         "and turn off element matching.")
}

throwWarningIfTransposedInput <- function(x, function.name)
{
    transposed.input.occured <- !is.null(attr(x, "transposed.input"))
    if (transposed.input.occured)
        warning("The automatic name matching algorithm for ", function.name, " ",
                "identified a better match when transposing one of the inputs ",
                "(swapping the rows and columns). If this is not desired, turn off ",
                "the automatic matching of elements or ensure the inputs are the ",
                "appropriate shape.")
}

throwWarningAboutMissingNames <- function(function.name)
{
    warning("Automatic name matching was requested for ", function.name, " but at ",
            "least one of the inputs contained elements that a missing value for its name. ",
            "The elements that had a missing name were removed before calculation.")
}

swapRowAndColumnEntries <- function(input.list)
{
    output.list <- input.list
    output.list[[2L]] <- rev(output.list[[2L]])
    output.list
}

transposeInput <- function(input)
{
    n.dim.input <- length(dim(input))
    switch(n.dim.input,
           array(input, dim = c(1L, length(input)), dimnames = list(NULL, dimnames(input)[[1L]])),
           t(input),
           aperm(input, perm = c(2:1, 3L)))
}

computeExactAndFuzzyMatchCounts <- function(input.names, names.exist)
{
    output <- integer(4L)
    rownames.exist <- names.exist[["rows"]]
    colnames.exist <- names.exist[["columns"]]
    if (rownames.exist)
    {
        input.row.names <- lapply(input.names, "[[", i = 1L)
        output[1L] <- countExactMatches(input.row.names)
        fuzzy.matched <- fuzzyMatchDimensionNames(input.row.names, hide.unmatched = TRUE, warn = FALSE)
        fuzzy.matched <- fuzzy.matched[["mapping.list"]][[1L]]
        output[2L] <- sum(fuzzy.matched > 0L, na.rm = TRUE)
    }
    if (colnames.exist)
    {
        input.col.names <- lapply(input.names, "[[", i = 2L)
        output[3L] <- countExactMatches(input.col.names)
        fuzzy.matched <- fuzzyMatchDimensionNames(input.col.names, hide.unmatched = TRUE, warn = FALSE)
        fuzzy.matched <- fuzzy.matched[["mapping.list"]][[1L]]
        output[4L] <- sum(fuzzy.matched > 0L, na.rm = TRUE)
    }
    output
}

extractDimnamesSettingAllDuplicatedNamesAsNULL <- function(input, dimnameFunction)
{
    dim.names <- dimnameFunction(input)
    non.trivial.names <- !is.null(dim.names) && (length(dim.names) > 1L)
    if (non.trivial.names && (sum(duplicated(dim.names)) == length(dim.names) - 1L))
            dim.names <- NULL
    dim.names
}

getDimensionNamesOfInputs <- function(input, dims = rep(TRUE, 2L))
{
    funs <- list(rowNames, colNames)
    lapply(funs[dims], function(fun) extractDimnamesSettingAllDuplicatedNamesAsNULL(input, fun))
}

checkMissingDimensionNames <- function(input.names)
{
    vapply(input.names, anyNA, logical(1L))
}

checkDuplicatedDimensionNames <- function(input.names)
{
    vapply(input.names, anyDuplicated, integer(1L))
}

countExactMatches <- function(x)
{
    sum(match(x[[1L]], x[[2L]], nomatch = NA_integer_) > 0L, na.rm = TRUE)
}

dimnamesExist <- function(input.dimnames)
{
    vapply(input.dimnames, Negate(is.null), logical(1L))
}

matchInputsUsingCustomArgs <- function(input, match.elements, warn, function.name)
{
    matching.required <- match.elements != "No"
    if (any(matching.required))
    {
        input.names <- lapply(input, getDimensionNamesOfInputs, dims = matching.required)
        input.names.have.missing.vals <- lapply(input.names, checkMissingDimensionNames)
        if (any(unlist(input.names.have.missing.vals)))
        {
            if (all(matching.required))
                inputs.with.missing.names <- input.names.have.missing.vals
            else
            {
                inputs.with.missing.names <- array(logical(4L), dim = c(2L, 2L))
                inputs.with.missing.names[, matching.required] <- unlist(input.names.have.missing.vals)
                inputs.with.missing.names <- split(inputs.with.missing.names, 1:2)
            }
            input <- mapply(removeElementsWithMissingNames, input, inputs.with.missing.names)
            throwWarningAboutMissingNames(function.name)
            input.names <- lapply(input, getDimensionNamesOfInputs, dims = matching.required)
        }
        duplicate.names.found <- lapply(input.names, checkDuplicatedDimensionNames)
        if (any(unlist(duplicate.names.found)))
        {
            duplicated.names <- getDuplicateNames(input.names, duplicate.names.found)
            throwErrorAboutDuplicatedNamesWhenMatching(duplicated.names, function.name)
        }
        input <- matchDimensionElements(input,
                                        match.rows = match.elements[1L],
                                        match.columns = match.elements[2L],
                                        warn, function.name)
    }
    input <- recycleIfNecessary(input, warn = warn, function.name = function.name)
    checkDimensionsEqual(input, function.name = function.name)
    if (any(!matching.required))
    {
        unmatched <- attr(input, "unmatched")
        input <- assignLabelsIfPossible(input,
                                        dimension = which(!matching.required))
        attr(input, "unmatched") <- unmatched
    }
    input
}

removeMissing <- function(x)
{
    if (any(missing.values <- is.na(x)))
        x[missing.values] <- 0
    x
}

#' @param sum.output The calculated output of the call to Sum just before it is returned.
#' @param inputs The inputs to the call in Sum
#' @noRd
appendSampleSizeAttribute <- function(sum.output, inputs)
{
    sum.w <- attr(inputs[[1L]], "sum.weights")
    attr(sum.output, "n.sum") <- if (!is.null(sum.w)) sum.w else numberNonMissingObservations(inputs[[1L]])
    sum.output
}

numberNonMissingObservations <- function(x)
{
    sum(!is.na(x))
}
