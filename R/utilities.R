#' Common processing functions for all inputs.
#'
#' This function will take the arbitrary number of inputs to the function and
#' inspect for appropriate use and then do common tasks. This includes checking
#' the data types are valid for numeric calculations, converting special inputs
#' to numeric, removing unwanted rows and columns, applying subsets and weights
#' @noRd
processArguments <- function(...,
                             remove.missing = TRUE,
                             remove.rows = c("NET", "SUM", "Total"),
                             remove.columns = c("NET", "SUM", "Total"),
                             subset = NULL,
                             weights = NULL,
                             warn = FALSE,
                             function.name)
{
    x <- list(...)
    checkInputTypes(x, function.name = function.name)
    x <- lapply(x, extractChartDataIfNecessary)
    x <- convertToNumeric(x, function.name = function.name)
    x <- lapply(x, removeRowsAndCols,
                remove.rows = remove.rows,
                remove.columns = remove.columns,
                warn = warn,
                function.name = function.name)
    x <- subsetAndWeightInputs(x,
                               subset = subset,
                               weights = weights,
                               function.name = function.name)
    if (warn)
    {
        if (any(qtables <- vapply(x, isQTable, logical(1))))
        {
            statistics <- lapply(x[qtables], lookupStatistics)
            statistics <- if (length(statistics) > 1)
                Reduce(union, statistics)
            else
                unlist(statistics)
            if (length(statistics) > 1)
                throwWarningAboutDifferentStatistics(statistics, function.name)
        }
        checkMissingData(x, remove.missing = remove.missing)
        warnAboutRemovedElements(x)
    }
    x
}

#' @noRd
checkInputTypes <- function(x, function.name)
{
    # Throw error if invalid inputs
    if (any(vapply(x, is.character, logical(1))))
        throwErrorInvalidDataForNumericFunc(invalid.type = "Text",
                                            function.name = function.name)
    if (any(vapply(x, isDateTime, logical(1))))
        throwErrorInvalidDataForNumericFunc(invalid.type = "Date/Time",
                                            function.name = function.name)
    # Check elements of the list are of the same compatible type
    checkInputsDontContainTablesAndVariables(x, function.name = function.name)
}

isDateTime <- function(x)
{
    any(c("QDate", "POSIXct", "POSIXt", "POSIXlt") %in% class(x))
}

isVariableSet <- function(x)
{
    hasQuestionAttribute(x) && is.data.frame(x) && NCOL(x) > 1
}

isVariable <- function(x)
{
    is.dataframe <- is.data.frame(x)
    hasQuestionAttribute(x) &&
        (!is.dataframe || (is.dataframe && NCOL(x) == 1))
}

hasQuestionAttribute <- function(x)
{
    all(c("question", "questiontype") %in% names(attributes(x)))
}

isQTable <- function(x)
{
    all(c("questions", "name") %in% names(attributes(x)))
}

checkInputsDontContainTablesAndVariables <- function(x, function.name)
{
    if (is.list(x) && length(x) > 1)
    {
        qtables <- vapply(x, isQTable, logical(1))
        variable.type <- vapply(x, function(x) isVariable(x) || isVariableSet(x), logical(1))
        if (sum(qtables) > 0 && sum(variable.type) > 0)
        {
            desired.message <- paste0("requires input elements to be of the same type. However, ",
                                      "both QTables and Variables have been used as inputs. ",
                                      "It is not possible to use ", function.name, " ",
                                      "with multiple inputs of different types. ")
            throwErrorContactSupportForRequest(desired.message, function.name = function.name)
        }
    }
}

outputsWithChartData <- function()
{
    c('2Dreduction', 'CART', 'ChoiceEnsemble', 'ConfusionMatrix', 'CorrelationMatrix',
      'CorrespondenceAnalysis', 'DeepLearning', 'DistanceMatrix', 'Ensemble', 'FitChoice',
      'FitMaxDiff', 'flipFactorAnalysis', 'GradientBoost', 'LDA', 'RandomForest', 'Regression')
}

#' @importFrom flipFormat ExtractChartData
extractChartDataIfNecessary <- function(x)
{
    if (any(class(x) %in% outputsWithChartData()))
        x <- ExtractChartData(x)
    x
}

#' @importFrom flipTransformations AsNumeric
convertToNumeric <- function(x, function.name = function.name)
{
    x <- lapply(x, AsNumeric, binary = FALSE)
}

lookupStatistics <- function(x, function.name)
{
    # This test should only be run on a table, to check that it
    # doesnot, say, contain both a mean and a median, and is attempting
    # to sum these.
    possible.stats <- possibleStatistics(x)
    known.statistics <- tolower(possible.stats) %in% statisticNames()
    if (any(known.statistics))
        possible.stats[known.statistics]
    else
        NULL
}

throwWarningAboutDifferentStatistics <- function(multiple.statistics, function.name)
{
    multiple.statistics <- paste0(multiple.statistics, collapse = ", ")
    warning("The input data contains statistics of different types ",
            "(i.e., ",
            multiple.statistics,
            "), it may not be appropriate to compute ",
            function.name, ".")
}

# No doubt there is a better way of storing this than a function
# This vector contains common names for statistics (in Q, Displayr, and elsewhere)
statisticNames <- function()   c('%', '% column responses', '% column share',
                                 '% excluding nan', '% responses', '% row responses',
                                 '% row share', '% share', '% total responses', '% total share',
                                 '25th percentile', '5th percentile', '75th percentile', '95th percentile',
                                 'average', 'base', 'coefficient', 'column %', 'column comparisons',
                                 'column names', 'column sample size', 'column standard error',
                                 'column standard error of mean', 'columns compared', 'corrected p',
                                 'correlation', 'count', 'cumulative %', 'd.f.', 'degrees of freedom',
                                 'df', 'df', 'effective column sample size', 'effective count',
                                 'effective row sample size', 'effective sample size',
                                 'expected average', 'expected correlation', 'expected n',
                                 'expected percent', 'expectedpercent', 'index', 'interquartile range',
                                 'interquartilerange', 'labels', 'maximum', 'mean', 'median',
                                 'minimum', 'missing count', 'mode', 'mode',
                                 'multiple comparison adjustment', 'n observations', 'net',
                                 'not duplicate', 'observation', 'percent', 'probability %',
                                 'range', 'row %', 'row sample size', 's.e.', 'sample size',
                                 'sample size', 'se', 'standard deviation', 'standard error',
                                 'sum', 't-statistic', 'text', 'total', 'total %', 'trimmed average',
                                 'values', 'weighted column sample size', 'weighted count',
                                 'weighted row sample size', 'weighted sample size', 'z', 'z-statistic')


possibleStatistics <- function(x)
{
    if (!is.null(attr(x, "statistic")))
        return(attr(x, "statistic"))
    Last(dimnames(x), 1)[[1]]
}


checkForOppositeInfinites <- function(x)
{
    opposite.infinities <- FALSE
    previous.sign <- 0
    x <- unlist(x)
    for (i in seq_along(x))
        if (!is.finite(x[[i]]))
        {
            if (previous.sign == 0)
                previous.sign <- sign(x[[i]])
            if (sign(x[[i]]) == -previous.sign)
            {
                opposite.infinities <- TRUE
                break
            }
        }
    opposite.infinities
}

removeRowsAndCols <- function(x, remove.rows, remove.columns, warn, function.name)
{
    # Determine rows and columns to keep
    row.names <- rowNames(x)
    keep.rows <- entriesToKeep(row.names,
                               entries.to.remove = remove.rows,
                               dim.length = NROW(x))
    col.names <- colNames(x)
    keep.cols <- entriesToKeep(col.names,
                               entries.to.remove = remove.columns,
                               dim.length = NCOL(x))
    # Subset the input using the appropriate indices
    x <- removeElementsFromArray(x, keep.rows, keep.cols, function.name)
    # Check if warnings are requested and some rows or columns were removed
    # If so, add them as attributes and throw a single warning later.
    removed.rows <- !keep.rows
    removed.cols <- !keep.cols
    if (warn && (any(removed.rows) || any(removed.cols)))
    {
        if (any(removed.rows))
            throwWarningAboutRemovedIndices("rows", row.names[!keep.rows])
        if (any(removed.cols))
            throwWarningAboutRemovedIndices("columns", col.names[!keep.cols])
    }
    x
}

#' @importFrom flipU CopyAttributes
removeElementsFromArray <- function(x, keep.rows, keep.columns, function.name)
{
    n.dim <- getDim(x)
    output <- if (n.dim == 1)
        x[keep.rows, drop = FALSE]
    else if (n.dim == 2)
        x[keep.rows, keep.columns, drop = FALSE]
    else
    {
        if (isQTable(x))
            x[keep.rows, keep.columns, , drop = FALSE]
        else
        {
            desired.msg <- paste0("only supports inputs that have 1 ",
                                  "or 2 dimensions. A supplied input has ", n.dim,
                                  " dimensions. ")
            throwErrorContactSupportForRequest(desired.msg, function.name)
        }
    }
    CopyAttributes(output, x)
}

throwWarningAboutRemovedIndices <- function(index.name, removed.categories)
{
    warning("These categories have been removed from the ", index.name,
            ": ",
            paste(removed.categories, collapse = ", "),
            ".")
}

warnAboutRemovedElements <- function(x)
{
    indices.removed <- lapply(x, function(x) attr(x, "Removed Indices"))
    rows.removed <- unique(unlist(lapply(indices.removed, "[[", "rows")))
    columns.removed <- unique(unlist(lapply(indices.removed, "[[", "columns")))
    if (length(rows.removed))
        throwWarningAboutRemovedIndices("rows", rows.removed)
    if (length(columns.removed))
        throwWarningAboutRemovedIndices("columns", columns.removed)
}

entriesToKeep  <- function(strings, entries.to.remove, dim.length)
{
    if (is.null(strings))
        return(rep(TRUE, dim.length))
    !strings %in% entries.to.remove
}

rowNames <- function(x)
{
    if(getDim(x) == 1)
        return(names(x))
    rownames(x)
}

colNames <- function(x)
{
    if (getDim(x) == 1)
        return(NULL)
    colnames(x)
}

getDim <- function(x)
{
    x.dim <- dim(x)
    n.dim <- if(is.null(x.dim)) 1 else length(x.dim)
    n.dim
}

subsetAndWeightInputs <- function(x, subset = NULL, weights = NULL, function.name)
{
    if (!is.null(subset) && !is.logical(subset))
        stop("The subset argument should be a logical vector")
    if (!is.null(weights) && !is.numeric(weights))
        stop("The weights argument should be a numeric vector")
    subset.required <- !is.null(subset) && !all(subset)
    weighting.required <- !is.null(weights) && !all(weights == 1)
    if (!subset.required && !weighting.required)
        return(x)
    n.rows <- vapply(x, NROW, integer(1))
    if (!all(n.rows == n.rows[1]))
    {
        error.msg <- paste0("requires all input elements to have the same size to be able to ",
                            "apply a filter or weight vector. ")
        throwErrorContactSupportForRequest(error.msg, function.name)
    }
    if (subset.required)
    {
        checkSubset(subset, n.rows[1])
        x <- lapply(x, subsetInputs, subset = subset)
        if (weighting.required)
        {
            weights <- weights[subset]
            n.rows[1] <- sum(subset)
        }
    }
    if (weighting.required)
    {
        checkWeights(weights, n.rows[1])
        x <- lapply(x, function(x) x * weights)
    }
    x
}

subsetAndWeightIfNecessary <- function(x, subset, weights, warn)
{
    if (!is.null(subset) && !is.logical(subset))
        stop("The subset argument should be a logical vector")
    if (!is.null(weights) && !is.numeric(weights))
        stop("The weights argument should be a numeric vector")
    subset.required <- !is.null(subset) && !all(subset)
    weighting.required <- !is.null(weights) && !all(weights == 1)
    if (!subset.required && !weighting.required)
        return(x)
    n.rows <- vapply(x, NROW, integer(1))
    if (!all(n.rows == n.rows[1]))
    {
        error.msg <- paste0("requires all input elements to have the same size to be able to ",
                            "apply a filter or weight vector. ")
        throwErrorContactSupportForRequest(error.msg, function.name)
    }
    if (subset.required)
    {
        checkSubset(subset, n.rows[1])
        x <- lapply(x, subsetInputs, subset = subset)
        if (weighting.required)
        {
            weights <- weights[subset]
            n.rows[1] <- sum(subset)
        }
    }
    if (weighting.required)
    {
        checkWeights(weights, n.rows[1])
        x <- lapply(x, function(x) x * weights)
    }
    x
}

subsetRequired <- function(subset)
{
    if (!is.null(subset) && !is.logical(subset))
        stop("The subset argument should be a logical vector")
    if (!is.null(weights) && !is.numeric(weights))
        stop("The weights argument should be a numeric vector")
    !is.null(subset) && !all(subset)
}

weightsRequired <- function(weights)
{
    if (!is.null(weights) && !is.numeric(weights))
        stop("The weights argument should be a numeric vector")
    !is.null(weights) && !all(weights == 1)
}

checkDimensions <- function(x,
                            by.row,
                            function.name,
                            warn)
{
    dimension.counting.function <- if (by.row) NROW else NCOL
    dimension.sizes.match <- checkDimensionsEqual(x, dimension.counting.function, function.name)
    if (!dimension.sizes.match)
    {
        dimension.string <- if (by.row) "rows" else "columns"
        error.msg <- paste0("requires inputs to have the same number of ",
                            dimension.string, ". ")
        throwErrorContactSupportForRequest(error.msg, function.name)
    }
    if (warn)
    {
        relevant.dimension.name.function <- if (by.row) rowNames else colNames
        all.relevant.dimension.names <- lapply(x, relevant.dimension.name.function)
        simplified.names <- Filter(function(y) !identical(all.relevant.dimension.names[[1]], y),
                                   all.relevant.dimension.names)
        if (length(simplified.names) > 0)
        {
            dimension.string <- if (by.row) "rows" else "columns"
            throwWarningAboutDimensionNames(dimension.string = dimension.string,
                                            function.name = function.name)
        }

    }
}

checkDimensionsEqual <- function(x, dimension.counting.function, function.name)
{
    if (length(x) == 1)
        return(TRUE)
    relevant.dim.sizes <- vapply(x, dimension.counting.function, integer(1))
    relevant.dim.match <- relevant.dim.sizes == relevant.dim.sizes[1]
    all(relevant.dim.match)
}

throwWarningAboutDimensionNames <- function(dimension.string, function.name)
{
    warning("The argument for matching names was set to 'No' in ", function.name, ". ",
            "However, the inputs don't have identical ", dimension.string, " names ",
            "and the calculation in ", function.name, " might not be appropriate. ",
            "The ", dimension.string, " names of the first input element were used ",
            "in the output. Consider changing the name matching options.")
}

checkRowOrColDimensionsEqual <- function(x, function.name)
{
    n.rows <- vapply(x, NROW, integer(1))
    n.cols <- vapply(x, NCOL, integer(1))
    n.rows.match <- n.rows == n.rows[1]
    n.cols.match <- n.cols == n.cols[1]
    if (!all(n.rows.match | n.cols.match))
    {
        error.msg <- paste0("requires inputs to have the same number of rows or the same ",
                            "number of columns. ")
        throwErrorContactSupportForRequest(error.msg, function.name)
    }
}

checkWeights <- function(x, n.required)
{
    if (length(x) != n.required)
        throwErrorSubsetOrWeightsWrongSize("weights", length(x), n.required)
    if (anyNA(x))
    {
        x[is.na(x)] <- 0
        warning("Weights with missing elements were set to have a weight of zero")
    }
    if (any(negative.weights <- x < 0))
    {
        x[negative.weights] <- 0
        warning("Elements with negative weights were set to have weight of zero")
    }
    x
}

checkSubset <- function(x, n.required)
{
    if (length(x) != n.required)
        throwErrorSubsetOrWeightsWrongSize("subset", length(x), n.required)
    if (anyNA(x))
    {
        x[is.na(x)] <- FALSE
        warning("The subset argument contains missing values. ",
                "Data correspondong to these were filtered out.")
    }
    x
}

subsetInputs <- function(x, subset)
{
    n.dim = getDim(x)
    output <- if (n.dim == 1) x[subset, drop = FALSE] else x[subset, , drop = FALSE]
    CopyAttributes(output, x)
}

throwErrorSubsetOrWeightsWrongSize <- function(input.type, input.length, required.length)
{
    stop("The ", input.type, " vector has length ", input.length, ". However, it needs to ",
         "have length ", required.length, " to match the number of cases in the ",
         "supplied input data.")
}

# Needs to be called after the data has been processed to be numeric
# e.g. after conversion from Nominal to numeric
checkMissingData <- function(x, remove.missing = TRUE)
{
    if (remove.missing == TRUE && any(vapply(x, anyNA, logical(1))))
        warning("Missing values have been ignored in calculation.")
}

throwErrorInvalidDataForNumericFunc <- function(invalid.type, function.name = 'Sum')
{
    stop(invalid.type, " data has been supplied but ",
         function.name,
         " requires numeric data.")
}

checkForMultipleStatistics <- function(x, function.name)
{
    different.statistics <- lookupStatistics(x, function.name = function.name)
    if (length(different.statistics) > 1)
        throwWarningAboutDifferentStatistics(different.statistics,
                                             function.name)
}

#' @importFrom flipU IsRServer
throwErrorContactSupportForRequest <- function(desired.message, function.name)
{
    contact.details <- determineAppropriateContact()
    stop.msg <- paste0(function.name, " ",
                       desired.message,
                       contact.details)
    stop(stop.msg)

}

determineAppropriateContact <- function()
{
    contact <- if (IsRServer()) "support@displayr.com" else
        "opensource@displayr.com or raise an issue at https://github.com/Displayr/verbs"
    paste("Contact support at", contact, "if you wish this to be changed.")
}

sumWithin3Darray <- function(x, summing.function, remove.missing)
{
    apply(x, 3, summing.function, na.rm = remove.missing)
}

checkIfSuitableVectorType <- function(x, function.name)
{
    checkIfCharacter(x, function.name)
    checkIfDateTime(x, function.name)
    if (is.list(x) || is.matrix(x) || is.array(x))
    {
        class.name <- if (isQTable(x))
            "is a Q Table"
        else if (isVariableSet(x))
            "is a Variable Set"
        else if (is.data.frame(x))
            "is a data frame"
        else
            paste0("has class : ", paste0(class(x), collapse = ", "))
        error.msg <- paste0("does not support multiple inputs unless they are all ",
                            "individual variables or vectors. One of the inputs here ",
                            class.name, ". ")
        throwErrorContactSupportForRequest(desired.message = error.msg,
                                           function.name = function.name)
    }
}

checkIfCharacter <- function(x, function.name)
{
    if (is.character(x))
        throwErrorInvalidDataForNumericFunc(invalid.type = "Text",
                                            function.name = function.name)
}

checkIfDateTime <- function(x, function.name)
{
    if (isDateTime(x))
        throwErrorInvalidDataForNumericFunc(invalid.type = "Date/Time",
                                            function.name = function.name)
}

checkPartiallyNamedVector <- function(names.to.check, function.name)
{
    if (any(vapply(names.to.check, anyNA, logical(1))))
    {
          output.msg <- paste0("requires either a fully named vector or a vector with no names ",
                               "to calculate output. Some elements of the input vector have names ",
                               "while other elements are not named. Please name all elements if you ",
                               "wish to compute ", function.name, " by matching elements. ")
          throwErrorContactSupportForRequest(output.msg, function.name)
    }
}


matchRows <- function(x, match.elements, warn, function.name)
{
    switch(match.elements,
           `Yes - ignore if unmatched` = exactMatchRowNames(x, ignore.unmatched = TRUE,
                                                            warn = warn, function.name = function.name),
           `Yes - error if unmatched` = exactMatchRowNames(x, ignore.unmatched = FALSE,
                                                           warn = warn, function.name = function.name),
           `Fuzzy - ignore if unmatched` = fuzzyMatchRowNames(x, ignore.unmatched = TRUE),
           `Fuzzy - error if unmatched` = fuzzyMatchRowNames(x, ignore.unmatched = FALSE),
           `No` = cbindAfterCheckingDimensions(x, warn = warn, function.name = function.name))
}

cbindAfterCheckingDimensions <- function(x, warn, function.name)
{
    n.rows <- vapply(x, NROW, integer(1))
    if (diff(n.rows) != 0)
    {
        quoted.function <- sQuote(function.name, q = FALSE)
        stop("Two inputs have a different number of rows and cannot be joined to ",
             "compute ", quoted.function)
    }
    if (warn && !Reduce(identical, lapply(x, rowNames)))
        warning("The argument for matching names was set to 'No' in ", function.name, ". ",
                "However, the inputs don't have identical row names ",
                "and the calculation in ", function.name, " might not be appropriate. ")
    do.call(cbind, x)
}

exactMatchRowNames <- function(x, ignore.unmatched, warn, function.name)
{
    row.names <- lapply(x, rowNames)
    # Handle the case when there are no row names at all
    if (is.null(unlist(row.names)))
        return(cbindInputsIfAppropriate(x,
                                        ignore.unmatched = ignore.unmatched,
                                        warn = warn,
                                        function.name = function.name))
    # Don't allow partially named vectors (completely unnamed is ok)
    checkPartiallyNamedVector(row.names, function.name)
    all.row.names <- Reduce(union, row.names)
    matched.indices <- lapply(row.names, function(x) {
        out <- match(all.row.names, x, nomatch = NA)
        names(out) <- all.row.names
        out
    })
    at.least.one.unmatched.index <- any(vapply(matched.indices, anyNA, logical(1)))
    if (at.least.one.unmatched.index)
    {
        warning.required <- ignore.unmatched && warn
        error.required <- !ignore.unmatched
        an.input.with.no.names <- any(vapply(matched.indices, function(x) all(is.na(x)), logical(1)))
        if (error.required || (warning.required && an.input.with.no.names))
        {
            if (an.input.with.no.names)
                output.msg <- "One of the input elements doesn't have any names and cannot be matched. "
            else
            {
                unmatched.names <- union(setdiff(row.names[[1L]], row.names[[2L]]),
                                         setdiff(row.names[[2L]], row.names[[1L]]))
                quoted.unmatched <- paste0(sQuote(unmatched.names, q = FALSE), collapse = ", ")
                output.msg <- paste0(function.name, " requires inputs to have matching row names. ",
                                     "However, some inputs have names they don't match, i.e. a ",
                                     "named element doesn't occur in all input elements, e.g. ",
                                     "the ", ngettext(length(unmatched.names), "element", "elements"),
                                     " named : ", quoted.unmatched, ". ")
            }
            output.msg <- paste0(output.msg,
                                 "Consider changing the name matching options or ensure all ",
                                 "the names match before recomputing.")
            if (error.required)
                stop(output.msg)
            else
                warning(output.msg)
        }
        n.named <- length(all.row.names)
        template.out <- numeric(n.named)
        names(template.out) <- all.row.names
        mapply(function(x, ind) {
            relevant.names <- names(ind[!is.na(ind)])
            template.out[relevant.names] <- x[relevant.names]
            template.out
        }, x, matched.indices)
    } else
        mapply(function(x, indices) x[indices], x, matched.indices)
}

#' Attempts to resize inputs and cbind them if appropriate
cbindInputsIfAppropriate <- function(x, ignore.unmatched, warn, function.name)
{
    n.rows <- vapply(x, length, integer(1))
    size.difference <- diff(n.rows)
    if (size.difference != 0)
    {
        if (ignore.unmatched)
        {
            smaller.vec <- which.min(n.rows)
            x[[smaller.vec]] <- append(x[[smaller.vec]], rep(0, abs(size.difference)))
        } else
            stop("Two inputs to ", function.name, " were vectors with no names and ",
                 "different lengths, so the elements cannot be matched. ",
                 "Consider changing the name matching options or changing the inputs ",
                 "to have the same size or have matching names.")
    }
    do.call(cbind, x)
}

fuzzyMatchRowNames <- function(x, ignore.unmatched, warn = FALSE, function.name)
{
    row.names <- lapply(x, rowNames)
    n.rows <- vapply(x, length, integer(1))
    # Handle the case when there are no row names at all
    if (is.null(unlist(row.names)))
        return(cbindInputsIfAppropriate(x,
                                        ignore.unmatched = ignore.unmatched,
                                        warn = warn,
                                        function.name = function.name))
    checkPartiallyNamedVector(row.names, function.name)
    exact.matches <- intersect(row.names[[1L]], row.names[[2L]])
    none.match <- identical(exact.matches, character(0))
    if (!none.match)
    {
        exact.matched.indices <- lapply(row.names, function(x) {
            matches <- match(exact.matches, x, nomatch = NA)
            names(matches) <- exact.matches
            matches
        })
        unmatched.row.names <- lapply(row.names, function(x) setdiff(x, exact.matches))
    } else
    {
        exact.matched.indices <- replicate(2, NULL)
        unmatched.row.names <- row.names
    }
    all.matched <- identical(unlist(unmatched.row.names), character(0))
    if (all.matched)
        return(bindUsingMapping(x, exact.matched.indices))
    # Create mapping list and update using Fuzzy checks
    mapping.list <- createMappingList(row.names, exact.matched.indices[[1L]])
    name.distances <- adist(unmatched.row.names[[1L]],
                            unmatched.row.names[[2L]],
                            ignore.case = TRUE)
    # only one match with a distance of 1 or less
    # The or operator is to handle the case when there are two matches,
    # one with dist 0 and another with dist 1
    dimnames(name.distances) <- unmatched.row.names
    min.name.distances <- apply(name.distances, 2, function(x) sum(x <= 0) == 1 || sum(x <= 1) == 1)
    if (any(min.name.distances))
    {
        levenshtein.matches <- apply(name.distances[, min.name.distances, drop = FALSE], 2, which.min)
        mapping.list[[1L]][levenshtein.matches] <- unname(levenshtein.matches)
        mapping.list[[2L]][names(levenshtein.matches)] <- levenshtein.matches
    }
    unmatched.names <- checkRemainingInMappingList(mapping.list)
    all.matched <- identical(unlist(unmatched.names), character(0))
    if (all.matched)
      return(bindUsingFuzzyMapping(x, mapping.list))
    # Cycle through variants
    mapping.list <- checkVariants(mapping.list, isOther, warn = warn)
    mapping.list <- checkVariants(mapping.list, isDontKnow, warn = warn)
    mapping.list <- checkVariants(mapping.list, isNoneOfThese, warn = warn)
    mapping.list <- checkVariants(mapping.list, isAllOfThese, warn = warn)
    unmatched.names <- checkRemainingInMappingList(mapping.list)
    all.matched <- identical(unlist(unmatched.names), character(0))
    if (all.matched)
        return(bindUsingFuzzyMapping(x, mapping.list))
    # Check any remaining and check if any are equal up to case and punctuation.
    fuzzy.mapped <- lapply(unmatched.names, simplifyTextForFuzzyMatching)
    fuzzy.matches <- intersect(names(fuzzy.mapped[[1L]]),
                               names(fuzzy.mapped[[2L]]))
    if (!identical(fuzzy.matches, character(0)))
        mapping.list <- updateMappingListWithFuzzyMatches(mapping.list,
                                                          fuzzy.mapped,
                                                          fuzzy.matches)
    unmatched.names <- checkRemainingInMappingList(mapping.list)
    all.matched <- identical(unlist(unmatched.names), character(0))
    if (all.matched)
        return(bindUsingFuzzyMapping(x, mapping.list))
    ## If here then there are un-matched elements still remaining
    ## Error if appropriate, otherwise ignore the unmatched names
    error.required <- !ignore.unmatched
    warning.required <- ignore.unmatched && warn
    if (warning.required || error.required)
    {
        quoted.unmatched.names <- paste0(sQuote(unlist(unmatched.names), q = FALSE),
                                         collapse = ", ")
        output.msg <- paste0("After a fuzzy matching search there are still names ",
                             "that couldn't be matched. These had the names ",
                             quoted.unmatched.names, ". Consider merging these categories ",
                             "if appropriate or relaxing the matching options to ignore ",
                             "them beforing proceeeding further.")
        if (error.required)
            stop(output.msg)
        else
            warning(output.msg)
    }
    bindUsingMappingAndAppendUnmatched(x, mapping.list, unmatched.names)
}

bindUsingMappingAndAppendUnmatched <- function(x, mapping.list, unmatched.names)
{
    n.unmatched <- length(unlist(unmatched.names))
    n.first <- length(x[[1]])
    unmatched.second <- mapping.list[[2L]][is.na(mapping.list[[2L]])]
    template.out <- numeric(n.first + length(unmatched.second))
    names(template.out) <- c(names(x[[1L]]), names(unmatched.second))
    unmatched.values <- mapply(function(x, nam) x[nam], x, unmatched.names, SIMPLIFY = FALSE)
    mapply(function(x, ind, unmatched) {
        template.out[ind[!is.na(ind)]] <- x[ind[!is.na(ind)]]
        template.out[names(unmatched)] <- unmatched
        template.out
    }, x, mapping.list, unmatched.values)
}

createMappingList <- function(x, indices)
{
    lapply(x, function(x) {
        out <- rep(NA, length(x))
        names(out) <- x
        if (!is.null(indices))
            out[names(indices)] <- indices
        out
    })
}

checkRemainingInMappingList <- function(mapping.list)
{
    lapply(mapping.list, function(x) names(x)[is.na(x)])
}

bindUsingMapping <- function(x, indices)
{
    mapply(function(x, ind) x[unname(ind)], x, indices)
}

bindUsingFuzzyMapping <- function(x, mapping.list)
{
    mapping.list[[2]] <- match(mapping.list[[1]], mapping.list[[2]])
    return(bindUsingMapping(x, mapping.list))
}

simplifyTextForFuzzyMatching <- function(x)
{
    # Coerce to lower case
    x.names <- tolower(x)
    # Remove punctuation and whitespace
    x.names <- gsub("[[:punct:][:blank:]]+", "", x.names)
    names(x) <- x.names
    x
}

isDontKnow <- function(x)
{
    x <- gsub("[^A-Za-z0-9_\\s]", "", x)
    patt <- "dontknow|unsure|notsure|donotknow|noidea|notapplicable"
    grepl(pattern = patt, x = tolower(x))
}

isNoneOfThese <- function(x)
{
    grepl(pattern = "none|nothing", x = tolower(x));
}

isOther <- function(x)
{
    grepl(pattern = "\\sother|^other", x = tolower(x))
}

isAllOfThese <- function(x)
{
    x <- tolower(x)
    patt <- "all of these|any of these|all of them|any of them"
    grepl(pattern = patt, x = x) || x %in% c("any", "all")
}

checkVariants <- function(mapping.list, function.to.check, warn)
{
    potential.matches <- lapply(mapping.list, function(x) which(function.to.check(names(x))))
    n.matches <- vapply(potential.matches, length, integer(1))
    if (all(n.matches == 1))
        mapping.list <- mapply(updateMappingVector,
                               mapping.list, potential.matches,
                               MoreArgs = list(value = potential.matches[[1L]]),
                               SIMPLIFY = FALSE)
    else if (warn && all(n.matches > 0))
    {
        conflicting.matches <- unlist(mapply(function(x, ind) names(x[ind]),
                                             mapping.list, potential.matches,
                                             SIMPLIFY = FALSE))
        conflicting.matches <- paste0(sQuote(conflicting.matches, q = FALSE),
                                      collapse = ", ")
        warning("Multiple fuzzy matches found with rows named ", conflicting.matches, ". ",
                "Considering merging these categories if they are similar measures. ")
    }
    mapping.list
}

updateMappingVector <- function(mapping.vector, index, value)
{
    mapping.vector[index] <- value
    mapping.vector
}

findMappingIndices <- function(mapping.list, mapping.names)
{
    mapply(function(x, nam) {
        which(names(x) == nam)
    }, mapping.list, mapping.names, SIMPLIFY = FALSE)
}

updateMappingListWithFuzzyMatches <- function(mapping.list, fuzzy.mapped, fuzzy.matches)
{
    for (f in fuzzy.matches)
    {
        mapping.names <- lapply(fuzzy.mapped, function(x) unname(x[f]))
        mapping.indices <- findMappingIndices(mapping.list, mapping.names)
        mapping.list <- mapply(updateMappingVector,
                               mapping.list, mapping.indices,
                               MoreArgs = list(value = mapping.indices[[1L]]),
                               SIMPLIFY = FALSE)
    }
    mapping.list
}
