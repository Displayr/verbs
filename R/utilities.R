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

containsOppositeInfinities <- function(x)
    all(c(Inf, -Inf) %in% x)

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
