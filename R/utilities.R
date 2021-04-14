#' Common processing functions for all inputs for Sum.
#'
#' This function will take the arbitrary number of inputs to the function and
#' inspect for appropriate use and then do common tasks. This includes checking
#' the data types are valid for numeric calculations, converting special inputs
#' to numeric, removing unwanted rows and columns, applying subsets and weights
#' Same input parameters as Sum with an additional one to track the function name call.
#' @param check.statistics Logical element to look for multiple statistics in the inputs.
#' @param function.name String used to communicate to the user which parent function
#'  called this function when throwing warnings or errors.
#' @noRd
processArguments <- function(x,
                             remove.missing = TRUE,
                             remove.rows = c("NET", "SUM", "Total"),
                             remove.columns = c("NET", "SUM", "Total"),
                             subset = NULL,
                             weights = NULL,
                             return.total.element.weights = "No",
                             check.statistics = TRUE,
                             warn = FALSE,
                             function.name)
{
    x <- Filter(Negate(is.null), x)
    if (length(x) == 0)
        return(list(NULL))
    x <- removeCharacterStatisticsFromQTables(x)
    x <- lapply(x, extractChartDataIfNecessary)
    checkMultipleDataSets(x, function.name)
    checkInputTypes(x, function.name = function.name)
    x <- convertToNumeric(x)
    x <- subsetAndWeightInputsIfNecessary(x,
                                          subset = subset,
                                          weights = weights,
                                          return.total.element.weights = return.total.element.weights,
                                          warn = warn,
                                          function.name = function.name)
    x <- checkInputsAtMost2DOrQTable(x, function.name = function.name)
    x <- removeRowsAndColsFromInputs(x,
                                     remove.rows = remove.rows,
                                     remove.columns = remove.columns,
                                     function.name = function.name)
    if (warn)
    {
        if (check.statistics)
            warnIfSummingMultipleStatistics(x, function.name = function.name)
        warnIfDataHasMissingValues(x, remove.missing = remove.missing)
    }
    x
}

checkMultipleDataSets <- function(x, function.name)
{
    variables <- lapply(x, function(x) if (isVariable(x)) x)
    variables <- Filter(Negate(is.null), variables)
    if (length(variables) != 0L)
    {
        datasets <- unique(unlist(lapply(variables, attr, which = "dataset")))
        datasets <- unique(datasets)
        if (length(datasets) > 1L)
            throwWarningAboutDifferentDatasets(datasets, function.name)
    }
}

throwWarningAboutDifferentDatasets <- function(datasets, function.name)
{
    n.datasets <- length(datasets)
    datasets <- paste0(c(paste0(datasets[1:(n.datasets - 1)], collapse = ", "),
                         datasets[n.datasets]), collapse = " and ")
    warning("Some inputs to ", function.name, " contain variables from ", n.datasets, " ",
            "different datasets (", datasets, ").")
}

flattenToSingleList <- function(input.list)
{
  args <- lapply(input.list, function(x) if (is.list(x)) flattenToSingleList(x) else list(x))
  do.call(c, args)
}

#' Check statistics present across the inputs and warn if the statistics are being summed
#' @noRd
warnIfSummingMultipleStatistics <- function(x, function.name)
{
    statistics <- lapply(x, lookupStatistics)
    statistics <- Filter(Negate(is.null), statistics)
    if (length(x) == 1L && length(statistics) > 0)
    {
        statistics <- statistics[[1L]]
        throw.warning <- length(statistics) > 1L
    } else if (length(statistics) > 1L)
    {
        throw.warning <- !Reduce(identical, statistics)
        statistics <- Reduce(union, statistics)
    } else
        throw.warning <- FALSE
    if (throw.warning)
        throwWarningAboutDifferentStatistics(statistics, function.name)
}

#' Only be concerned with arrays with more than 2 dimensions
#' Only allow QTables to be an array with more than 2 dimensions, otherwise throw an
#' error that the input is not supported.
#' For QTables with more than 2 dimensions, inspect the QTable and only allow the inputs
#' that have 3 dimensions where the 3rd dimension refers to multiple statistics. In other
#' scenarios where the 3rd dimension doesnt refer to multiple statistics or there are multiple
#' statistics, flatten the QTable to be 2d with a possible 3rd dim for multiple statistics.
#' @noRd
checkInputsAtMost2DOrQTable <- function(x, function.name)
{
    for (i in seq_along(x))
    {
        input <- x[[i]]
        input.dim <- getDim(input)
        if (input.dim > 2L)
        {
            is.qtable <- isQTable(input)
            if (!is.qtable)
            {
                desired.msg <- paste0("only supports inputs that have 1 ",
                                      "or 2 dimensions. A supplied input has ", input.dim,
                                      " dimensions. ")
                throwErrorContactSupportForRequest(desired.msg, function.name)
            }
            else
                x[[i]] <- flattenQTableKeepingMultipleStatistics(input)
        }
    }
    x
}

#' Check if the input is not a text or date/time data type. Also verify
#' that the input doesn't contain a mix of QTables and variable types
#' Error otherwise with an informative message.
#' @noRd
checkInputTypes <- function(x, function.name)
{
    list.elements <- vapply(x, is.list, logical(1L))
    if (any(list.elements))
        x <- flattenToSingleList(x)
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
    all(c("sourcevalues", "values", "codeframe") %in% names(attributes(x)))
}

isVariable <- function(x)
{
    is.dataframe <- is.data.frame(x)
    hasQuestionAttribute(x) &&
        (!is.dataframe || (is.dataframe && NCOL(x) == 1))
}

#' For checking if a data type is a variable set and/or QTable
#' @noRd
hasQuestionAttribute <- function(x)
{
    "questiontype" %in% names(attributes(x))
}

isQTable <- function(x)
{
    "questions" %in% names(attributes(x))
}

#' Used to verify if an input doesnt contain a mix of variables and Q Tables
#' @noRd
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

#' returns a character vector of all the class names that are permissible to ExtractChartData
#' @noRd
outputsWithChartData <- function()
{
    c('2Dreduction', 'CART', 'ChoiceEnsemble', 'ConfusionMatrix', 'CorrelationMatrix',
      'CorrespondenceAnalysis', 'DeepLearning', 'DistanceMatrix', 'Ensemble', 'FitChoice',
      'FitMaxDiff', 'flipFactorAnalysis', 'GradientBoost', 'LDA', 'RandomForest', 'Regression')
}

#' helper function to inspect the class of the input argument and return its chart data
#' if it has chart data.
#' @importFrom flipFormat ExtractChartData
#' @noRd
extractChartDataIfNecessary <- function(x)
{
    if (any(class(x) %in% outputsWithChartData()))
        x <- ExtractChartData(x)
    x
}

#' Helper function to convert nominal variables to numeric via the
#' flipTransformations::AsNumeric function.
#' @importFrom flipTransformations AsNumeric
#' @noRd
convertToNumeric <- function(x)
{
    lapply(x, AsNumeric, binary = FALSE)
}

#' Returns a list of possible statistics by leveraging the possibleStatistics
#' internal function and comparing the extracting strings against the list of
#' known statistics in statisticNames()
#' If any are found, they are returned, otherwise NULL is returned.
#' @noRd
lookupStatistics <- function(x)
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

#' helper function to throw a warning about multiple statistics and informs the
#' user of the R function that was the root parent function call that caused this
#' warning.
#' @noRd
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

#' Inspects the places where statistics are stored in the data structure for
#' Q Tables. That is either as an attribute for a Table with a single statistic
#' or in the names of the highest dimension for tables with multiple statistics.
#' e.g colnames or 3rd dim array names (for a 2d table with multiple statistics)
#' @noRd
possibleStatistics <- function(x)
{
    if (!is.null(attr(x, "statistic")))
        return(attr(x, "statistic"))
    Last(dimnames(x), 1)[[1]]
}

#' Helper function to check if both Inf and -Inf exist
#' Written as a loop that breaks the search if both are found, for speed.
#' @noRd
checkForOppositeInfinites <- function(x)
{
    opposite.infinities <- FALSE
    previous.sign <- 0
    for (i in seq_along(x))
        if (is.infinite(x[i]))
        {
            sign.x <- sign(x[i])
            if (previous.sign == 0)
                previous.sign <- sign.x
            if (sign.x == -previous.sign)
            {
                opposite.infinities <- TRUE
                break
            }
        }
    opposite.infinities
}

#' Helper function that inspects the row and column dimensions and removes the elements
#' slices that correspond to any matches.
#' @param x Structure to be modified
#' @param remove.rows Character vector of names of rows to remove
#' @param remove.cols Character vector of names of columns to remove
#' @param function.name Name of the calling parent function that used this function.
#'   Used to create informative error messages.
#' @noRd
removeRowsAndColsFromInputs <- function(x, remove.rows, remove.columns, function.name)
{
    if (is.null(remove.rows) && is.null(remove.columns))
        return(x)
    lapply(x, removeRowsAndCols,
           remove.rows = remove.rows,
           remove.columns = remove.columns,
           function.name = function.name)
}

flattenQTableKeepingMultipleStatistics <- function(x)
{
    if (!is.null(attr(x, "statistic")))
        return(FlattenTableAndDropStatisticsIfNecessary(x))
    # Inspect the third dimension and check if it is only populated with statistics
    last.dim.names <- Last(dimnames(x), 1L)[[1L]]
    if (all(tolower(last.dim.names) %in% statisticNames()))
    {
        n.dim <- getDim(x)
        n.statistics <- Last(dim(x), 1L)
        cell.indices <- rep(alist(,)[1L], n.dim)
        statistic.names <- dimnames(x)[[n.dim]]
        flattened.table <- lapply(1:n.statistics, function(last.ind) {
            cell.indices[n.dim] <- last.ind
            subsetted.table <- do.call(`[`, c(list(x), cell.indices))
            subsetted.table <- CopyAttributes(subsetted.table, x)
            attr(subsetted.table, "statistic") <- statistic.names[last.ind]
            FlattenTableAndDropStatisticsIfNecessary(subsetted.table)
            })
        flattened.table <- simplify2array(flattened.table)
        dimnames(flattened.table)[[3L]] <- statistic.names
        flattened.table <- CopyAttributes(flattened.table, x)
        flattened.table
    } else
        FlattenTableAndDropStatisticsIfNecessary(x)
}

removeRowsAndCols <- function(x, remove.rows, remove.columns, function.name)
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
    if (all(keep.rows) && all(keep.cols))
        return(x)
    # Subset the input using the appropriate indices
    removeElementsFromArray(x, keep.rows, keep.cols, function.name)
}

#' Helper function that removes elements from vectors or arrays. Used internally
#' by removeRowsAndCols
#' @param keep.rows A logical vector of size NROW(x) denoting which row elements to keep
#' @param keep.columns A logical vector of size NCOL(x) denoting which column elements to keep
#' @param function.name Name of the calling parent function that uses this function. Used in the
#' thrown error message when unexpected input occurs.
#' @importFrom flipU CopyAttributes
#' @noRd
removeElementsFromArray <- function(x, keep.rows, keep.columns, function.name)
{
    n.dim <- getDim(x)
    output <- if (n.dim == 1)
        x[keep.rows, drop = FALSE]
    else if (n.dim == 2)
        x[keep.rows, keep.columns, drop = FALSE]
    else
    {
        if (isQTable(x) && n.dim == 3L)
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

#' Determines which entries to keep
#' @param strings A vector of strings e.g. \code{LETTERS[1:3]}
#' @param entries.to.remove A vector of test strings e.g. "A"
#' @param dim.length Integer given the return length in the default case.
#' @return a logical vector of which strings are to remark. In the above the return
#' output would be \code{c(FALSE, TRUE, TRUE)} since "A" has been matched to be removed.
#' @noRd
entriesToKeep  <- function(strings, entries.to.remove, dim.length)
{
    if (is.null(strings))
        return(rep(TRUE, dim.length))
    !strings %in% entries.to.remove
}

# determines the possible names of the row elements in NROW by checking the appropriate
# names or rownames call respectively depending on the data structure
# i.e. a simple vector of length n is considered to have n rows.
rowNames <- function(x)
{
    if(getDim(x) == 1L)
        return(names(x))
    rownames(x)
}

# determines the possible names of the columns elements by checking the colnames if appropriate
# otherwise it returns NULL
colNames <- function(x)
{
    if (getDim(x) == 1L)
        return(NULL)
    colnames(x)
}

# Determines the dimension of the input object.
getDim <- function(x)
{
    x.dim <- dim(x)
    n.dim <- if(is.null(x.dim)) 1L else length(x.dim)
    n.dim
}

#' Generalized helper function to subset and weight the inputs if the subset and weight vectors
#' are appropriate
#' @param x list of inputs to process with subset and weights
#' @param subset logical vector to subset the input data
#' @param weights numeric vector to weight the data.
#' @param keep.total.weight Character string to determine whether the total weight should be returned
#'  with the output as an attributes. If \code{NULL}, no total weights are computed. If \code{Average},
#'  the total weight for all the inputs is returned. If \code{AverageColumns}, then the total weights are
#'  computed for each column of input data.
#' @param warn logical whether to warn if any incompatible input is used
#' @param function.name Name of the calling parent function that uses this function
#' @details The subset and weights are appropriate on data structures that contain variable data
#' e.g. variables, matrices and data.frames. It doesn't apply to QTables since they are summary
#' tables which may have different dimension that input variables
#' Q Tables are returned without modification if they are input to this function and a warning
#' thrown if appropriate (see \code{warn})
#' @noRd
subsetAndWeightInputsIfNecessary <- function(x, subset = NULL, weights = NULL,
                                             return.total.element.weights = "No",
                                             warn = FALSE, function.name)
{
    subset.required <- subsetRequired(subset)
    weighting.required <- return.total.element.weights %in% c("TotalWeight", "ByColumn") ||
        weightsRequired(weights)
    if (!subset.required && !weighting.required)
        return(x)
    qtables.used <- vapply(x, isQTable, logical(1))
    if (warn && any(qtables.used))
    {
        action.used <- paste0(c("a filter", "weights")[c(subset.required, weighting.required)], collapse = " or ")
        warn.msg <- paste0(function.name, " is unable to apply ", action.used, " to the input ",
                           ngettext(sum(qtables.used), msg1 = "Q Table ", msg2 = "Q Tables "),
                           "since the original variable data is unavailable.")
        warning(warn.msg)
    }
    if (all(qtables.used))
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
        x <- lapply(x, subsetInput, subset = subset)
        if (weighting.required)
        {
            weights <- weights[subset]
            n.rows[1] <- sum(subset)
        }
    }
    if (weighting.required)
    {
        weights <- checkWeights(weights, n.rows[1], warn = warn)
        x <- lapply(x, function(x) x * weights)
        if (return.total.element.weights != "No")
        {
            if (return.total.element.weights == "TotalWeight")
                attr(x[[1L]], "sum.weights") <- sum(computeTotalWeights(x[[1L]], weights), na.rm = TRUE)
            else if (return.total.element.weights == "ByColumn")
                x <- lapply(x, appendTotalWeightAttribute, weights = weights)
            else if (return.total.element.weights != "Yes")
                stop("Unexpected argument, ", dQuote(return.total.element.weights, q = FALSE),", for ",
                     sQuote("return.total.element.weights"), ". Allowable choices are ",
                     paste0(dQuote(c("No", "Yes", "TotalWeight", "ByColumns"), q = FALSE),
                            collapse = ", "))
        }
    }
    x
}

#' Helper function to check if the subset input is valid and not trivial
#' @noRd
subsetRequired <- function(subset)
{
    if (!is.null(subset) && !is.logical(subset))
        stop("The subset argument should be a logical vector")
    !is.null(subset) && !all(subset)
}

#' Helper function to check if the weights input is valid and not trivial
#' @noRd
weightsRequired <- function(weights)
{
    if (!is.null(weights) && !is.numeric(weights))
        stop("The weights argument should be a numeric vector")
    !is.null(weights) && !all(weights == 1)
}

#' Processes the input weights to see if they are valid against the input data
#' i.e. the correct size and also handles missing and invalid negative weights.
#' @noRd
checkWeights <- function(x, n.required, warn)
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

#' Processes the input subset to see if it is valid against the input data
#' i.e. the correct size and also checks if there are any missing subsetted values
#' @noRd
checkSubset <- function(x, n.required, warn = FALSE)
{
    if (length(x) != n.required)
        throwErrorSubsetOrWeightsWrongSize("subset", length(x), n.required)
    if (anyNA(x) && warn)
    {
        x[is.na(x)] <- FALSE
        warning("The subset argument contains missing values. ",
                "Data correspondong to these were filtered out.")
    }
    x
}

#' Helper function to subset the appropriate dimension, retaining attributes as necessary.
#' Also leaves Q Tables without modification and no subset applied.
#' @noRd
subsetInput <- function(x, subset)
{
    if (isQTable(x))
        return(x)
    n.dim = getDim(x)
    output <- if (n.dim == 1) x[subset, drop = FALSE] else x[subset, , drop = FALSE]
    if (is.data.frame(x))
    {
        output <- mapply(function(x, y) CopyAttributes(x, y), output, x, SIMPLIFY = FALSE)
        output <- as.data.frame(output,
                                col.names = names(x),
                                row.names = as.integer(row.names(x)[subset]))
    }
    CopyAttributes(output, x)
}

#' @param weights A numeric vector of weights assumed to have n elements
#' @param x An input to compute weights against. It code be either a numeric vector,
#' matrix or data.frame, assumed to also have n elements or n rows respectively.
#' @noRd
computeTotalWeights <- function(x, weights)
{
    if (is.data.frame(x))
        vapply(x, sumWeights, numeric(1L), weights = weights)
    else if (is.matrix(x))
        apply(x, 2L, sumWeights, weights = weights)
    else
        sumWeights(x, weights)
}

sumWeights <- function(x, weights)
{
    sum(weights[!is.na(x)], na.rm = TRUE)
}

appendTotalWeightAttribute <- function(x, weights)
{
    attr(x, "sum.weights") <- computeTotalWeights(x, weights)
    x
}

#' Helper function to throw an informative error message when an invalid subset or weight
#' vector is attempted.
#' @noRd
throwErrorSubsetOrWeightsWrongSize <- function(input.type, input.length, required.length)
{
    stop("The ", input.type, " vector has length ", input.length, ". However, it needs to ",
         "have length ", required.length, " to match the number of cases in the ",
         "supplied input data.")
}

# Needs to be called after the data has been processed to be numeric
# e.g. after conversion from Nominal to numeric
warnIfDataHasMissingValues <- function(x, remove.missing = TRUE)
{
    if (remove.missing == TRUE)
        for (i in seq_along(x))
        {
            if (anyNA(x[[i]]))
            {
                warning("Missing values have been ignored in calculation.")
                break
            }
        }
}

#' Helper function to give an informative message when an inappropriate data type is used
#' @noRd
throwErrorInvalidDataForNumericFunc <- function(invalid.type, function.name)
{
    stop(invalid.type, " data has been supplied but ",
         function.name,
         " requires numeric data.")
}

#' Takes the input element x and if a QTable will find its relevant statistics
#' via the use of lookupStatistics. If more than 1 is found a warning is thrown
#' that multiple statistics are found and the function being applied stored in the
#' function.name argument might not be appropriate.
#' @noRd
checkForMultipleStatistics <- function(x, function.name)
{
    different.statistics <- lookupStatistics(x)
    if (length(different.statistics) > 1)
        throwWarningAboutDifferentStatistics(different.statistics,
                                             function.name)
}

#' helper function to collate the desired error message and append a string
#' to contact support if they wish the behaviour of the software to change.
#' Mentions the parent calling function in the function.name argument to help
#' the user pinpoint which function was used in generating the error.
#' @importFrom flipU IsRServer
#' @noRd
throwErrorContactSupportForRequest <- function(desired.message, function.name)
{
    contact.details <- determineAppropriateContact()
    stop.msg <- paste0(function.name, " ",
                       desired.message,
                       contact.details)
    stop(stop.msg)

}

#' Helper function that creates a  string that either returns the generic support
#' email for Q/Displayr users or directs them to the open source channels if a general user.
#' @noRd
determineAppropriateContact <- function()
{
    contact <- if (IsRServer()) "support@displayr.com" else
        "opensource@displayr.com or raise an issue at https://github.com/Displayr/verbs"
    paste("Contact support at", contact, "if you wish this to be changed.")
}

#' Checks if the input is a character variable and throws an error since
#' function.name doesn't allow that data type.
#' @noRd
checkIfCharacter <- function(x, function.name)
{
    if (is.character(x))
        throwErrorInvalidDataForNumericFunc(invalid.type = "Text",
                                            function.name = function.name)
}

#' Checks if the input is a time or date variable and throws an error since
#' function.name doesn't allow that data type.
#' @noRd
checkIfDateTime <- function(x, function.name)
{
    if (isDateTime(x))
        throwErrorInvalidDataForNumericFunc(invalid.type = "Date/Time",
                                            function.name = function.name)
}

#' Checks if the input is partially named. That is, some elements have names while
#' other elements are missing names. If identified, an error is thrown since this
#' is not permissible in the name matching suite of functions.
#' function.name containing the string of the parent function calling this is
#' used in the message construction thrown to the user.
#' @noRd
checkPartiallyNamed <- function(names.to.check, function.name)
{
    missing.names <- vapply(names.to.check,
                            function(x) any(x == "" | is.na(x)),
                            logical(1))
    if (any(missing.names))
    {
        output.msg <- paste0("requires inputs with dimensions that are either fully named or ",
                             "unnamed to calculate output. One input element has a dimension with some ",
                             "named values while other values are not named. Please name all elements if you ",
                             "wish to compute ", function.name, " by matching elements. ")
        throwErrorContactSupportForRequest(output.msg, function.name)
    }
}

#' Attempts to match the elements by name using an exact character match of their names
#' @param x.names A list of two character vectors to have their names matched
#' @param hide.unmatched logical to specify if unmatched names should be hidden . That is,
#' removed from the addition. If \code{TRUE} the input elements have any unmatched dimension names
#' removed. If \code{FALSE}, any elements that dont have matching names are given an integer index
#' of \code{NA}.
#' @return A list with a index mapping the elements between the two inputs. Each element
#' is a named integer vector. The integers give the indices to map the array indices. Then
#' names give the element dimension names.
#' @noRd
exactMatchDimensionNames <- function(x.names, hide.unmatched, warn, function.name)
{
    set.function <- if (hide.unmatched) intersect else union
    all.x.names <- set.function(x.names[[1L]], x.names[[2L]])
    matched.indices <- lapply(x.names, function(x) {
        out <- match(all.x.names, x, nomatch = NA)
        names(out) <- all.x.names
        out
    })
    if (hide.unmatched && warn)
    {
        unlisted.names <- unlist(x.names)
        if (any(unmatched <- !unlisted.names %in% all.x.names))
            attr(matched.indices, "unmatched") <- unique(unlisted.names[unmatched])
    }
    matched.indices
}

#' Creates a list with the same number of elements as \code{x}. Typically a list with
#' two named elements to compare. The default value of each of the named elements
#' will be \code{NA} since they might not be matched. Matched elements should be given
#' in the second argument which retains the indices of the matches elements.
#' @noRd
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

#' Find the fuzzy matched names using the Levenshtein distance as a metric. It is
#' expected that the mapping list will be prepopulated with exact case sensitive matches already.
#' @param names.to.match list with two character vectors with the names that require matching
#'   from list element 2 to list element 1. e.g. list(c("Bananas", "hello", "apple"), c("Apple", "banana", "hello"))
#' @param mapping.list List with two named integer vectors containing the indices
#'  where elements match in the first named integer vector
#'  e.g. list(c(Bananas = NA, hello = 2, apple= NA), c(Apple = NA, banana = NA, hello = 2))
#' @return The mapping list with the elements updated with the fuzzy Levenshtein matched updates.
#' e.g. here that would be list()
#' @noRd
findLevenshteinMatches <- function(names.to.match, mapping.list)
{
    name.distances <- adist(names.to.match[[1L]],
                            names.to.match[[2L]],
                            ignore.case = TRUE)
    # only one match with a distance of 1 or less
    # The or operator is to handle the case when there are two matches,
    # one with dist 0 and another with dist 1
    dimnames(name.distances) <- names.to.match
    unique.match.to.first.names <- apply(name.distances, 2, function(x) sum(x <= 0) == 1 || sum(x <= 1) == 1)
    if (any(unique.match.to.first.names))
    {
        levenshtein.matches <- apply(name.distances[, unique.match.to.first.names, drop = FALSE], 2, which.min)
        # Check for multiple matching to same element in first and if so, don't match.
        if (any(duplicated.matches <- duplicated(levenshtein.matches)))
        {
            mapped.to.same <- levenshtein.matches[duplicated.matches]
            levenshtein.matches <- levenshtein.matches[levenshtein.matches != mapped.to.same]
        }
        mapping.list[[1L]][levenshtein.matches] <- unname(levenshtein.matches)
        mapping.list[[2L]][names(levenshtein.matches)] <- levenshtein.matches
    }
    mapping.list
}

#' Helper function to inspect the mapping list created by createMappingList
#' and processed in fuzzyMatchRowNames. Elements with missing values denote unmatched
#' elements. This function returns the names of all the unmatched elements.
#' @noRd
checkRemainingInMappingList <- function(mapping.list)
{
    lapply(mapping.list, function(x) names(x)[is.na(x)])
}

#' Helper function that converts the text to lower case and removes punctuation.
#' It returns a vector with the names corresponding to the original input elements
#' @noRd
simplifyTextForFuzzyMatching <- function(x)
{
    # Coerce to lower case
    x.names <- tolower(x)
    # Remove punctuation and whitespace
    x.names <- gsub("[[:punct:][:blank:]]+", "", x.names)
    names(x) <- x.names
    x
}

#' Function to fuzzy match phrases that correspond to variants of don't know responses.
#' @noRd
isDontKnow <- function(x)
{
    x <- gsub("[^A-Za-z0-9_\\s]", "", x)
    patt <- "dontknow|unsure|notsure|donotknow|noidea|notapplicable"
    grepl(pattern = patt, x = tolower(x))
}

#' Function to fuzzy match phrases that correspond to variants of none of these responses.
#' @noRd
isNoneOfThese <- function(x)
{
    grepl(pattern = "none|nothing", x = tolower(x));
}

#' Function to fuzzy match phrases that correspond to variants of other responses.
#' @noRd
isOther <- function(x)
{
    grepl(pattern = "\\sother|^other", x = tolower(x))
}

#' Function to fuzzy match phrases that correspond to variants of all of these responses.
#' @noRd
isAllOfThese <- function(x)
{
    x <- tolower(x)
    patt <- "all of these|any of these|all of them|any of them"
    grepl(pattern = patt, x = x) | x %in% c("any", "all")
}

#' Function to implement the fuzzy search matching of common questionnaire responses using
#' the internal functions of isOther, isAllOfThese, isNoneOfThese, isDontKnow
#' @param mapping.list A mapping list of not completely matched elements created in createMappingList
#' @param function.to.check A function to do the matching. Should be one of isOther, isAllOfThese, isNoneOfThese, isDontKnow
#' @param warn logical to determine if a user is warned if there are ambiguous fuzzy matches.
#' @details Ambiguous fuzzy matches will be ignored and a potential warning thrown.
#' @noRd
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

#' Updates the mapping vector (vector element of a constructed mapping list).
#' @param mapping.vector The vector to be updated.
#' @param index The indices in the vector to update
#' @param value the value to assign to the index.
#' @noRd
updateMappingVector <- function(mapping.vector, index, value)
{
    mapping.vector[index] <- value
    mapping.vector
}

#' Takes a mapping list and returns the indices that have the specified names
#' @param mapping.list The mapping list to check
#' @param mapping.names A list of names to search
#' @noRd
findMappingIndices <- function(mapping.list, mapping.names)
{
    mapply(function(x, nam) {
        which(names(x) == nam)
    }, mapping.list, mapping.names, SIMPLIFY = FALSE)
}

#' Takes a mapping list and updates to the map the appropriate indices to the identified
#' fuzzy matches.
#' @param mapping.list The mapping list to update.
#' @param mapping.names A list containing the mapping of names to the simplified name in
#'   fuzzy.matches.
#' @param fuzzy.matches A vector of names that were simplified in simplifyTextForFuzzyMatching
#' @noRd
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

#' Search the inputs for opposite infinites
#' @param x list of inputs to be searched
#' @param nan.output Output of Sum that had NaN elements
#' @param match.elements Argument from Sum that determines if/how rows are matched
#' @return A logical vector that specifies which of the NaN elements are the result of adding
#'  opposite infinities.
#' @noRd
determineIfOppositeInfinitiesWereAdded <- function(x, nan.output, match.elements)
{
    # Inspect the data to check for adding opposite infinities if possible.
    # If a single input, inspect all the inputs at once
    if (length(x) == 1)
        opposite.infinities <- checkForOppositeInfinites(unlist(x))
    else # If multiple inputs, check the element-wise elements separately, only if the structures are the same and
    { # no reshaping is done that could mess with the structure.
        identical.dims <- Reduce(identical, lapply(x, dim))
        identical.dimnames <- Reduce(identical, lapply(x, dimnames))
        no.matching <- all(match.elements == "No")
        if (identical.dims && (identical.dimnames || no.matching))
        {
            nan.elements <- which(nan.output)
            data.frames <- vapply(x, is.data.frame, logical(1L))
            inputs <- x
            if (any(data.frames))
                inputs[data.frames] <- lapply(x[data.frames], as.matrix)
            elements.calculating.to.nan <- lapply(1:length(nan.elements), function(i) {
                unlist(lapply(inputs, `[`, nan.elements[i]))
            })
            opposite.infinities <- logical(length(nan.output))
            opposite.infinities[nan.elements] <- vapply(elements.calculating.to.nan,
                                                        checkForOppositeInfinites,
                                                        logical(1L))
        } else
        { # Do a rough search, if no NaNs existed earlier, then it might have been the result of adding opposite Infs
            no.nans.before <- vapply(x, function(x) all(!isNaN(x)), logical(1L))
            opposite.infinities <- rep(FALSE, 2L)
            if (all(no.nans.before))
            {
                if (all(nan.output))
                    opposite.infinities <- !opposite.infinities
                else
                    opposite.infinities[1L] <- TRUE
            }
        }
    }
    opposite.infinities
}

#' Throws a warning explaining the possible reason for \code{NaN} in the output is due to
#' summing opposite infinities i.e. \code{Inf + -Inf}.
#' @param opposite.infinities logical vector that flags if the input element contains
#'   opposite infinities.
#' @param function.name character string of the parent calling function.
#' @return NULL used for its side effect of a possible warning if opposite infinities
#' found in the input object
#' @noRd
warnAboutOppositeInfinities <- function(opposite.infinities, function.name)
{
    if (any(opposite.infinities))
    {
        if (all(opposite.infinities))
            warning.msg <- " cannot be computed as the data contains both Inf and -Inf."
        else
            warning.msg <- " cannot compute some values as the data contains both Inf and -Inf."
        warning(function.name, warning.msg)
    }
}


sanitizeAttributes <- function(output,
                               attributes.to.keep = c("dim", "dimnames", "names"))
{
    if (is.data.frame(output)) attributes.to.keep <- c(attributes.to.keep, "class", "row.names")
    attributes.added <- setdiff(names(attributes(output)), attributes.to.keep)
    if (!is.null(attributes.added))
    {
        for (att in attributes.added)
            attr(output, att) <- NULL
    }
    output
}

standardizedDimensions <- function(x)
{
    x.dim <- dim(x)
    if (is.null(x.dim))
        return(length(x))
    x.dim
}

recycleIfNecessary <- function(x, warn = FALSE, function.name)
{
    # Check dims and if they match, return early
    standardized.dims <- lapply(x, standardizedDimensions)
    if (identical(standardized.dims[[1L]], standardized.dims[[2L]]))
        return(x)
    # Check any mismatched input with the simplest cases first.
    lengths <- lapply(x, length)
    scalars <- lengths == 1L
    # If there is a single scalar (two scalars would already have returned earlier)
    if (sum(scalars) == 1L)
    {
        scalar.ind <- which(scalars)
        scalar.val <- x[[scalar.ind]]
        dims.to.replicate <- standardized.dims[[which(!scalars)]]
        x[[scalar.ind]] <- array(scalar.val, dim = dims.to.replicate)
        return(x)
    }
    input.dims <- vapply(x, getDim, integer(1L))
    one.dim.inputs <- input.dims == 1L
    # If both inputs are single dimensional (vector or 1d array)
    if (all(one.dim.inputs) && lengths[[1L]] != lengths[[2L]])
        throwErrorAboutDimensionMismatch(standardized.dims, function.name)
    dims.to.match <- determineReshapingDimensions(standardized.dims)
    # If only one to be recycled and names are required
    to.recycle <- vapply(dims.to.match, function(x) !is.null(x), logical(1L))
    number.to.reshape <- sum(to.recycle)
    recycle.ind <- which(to.recycle)
    # If there is a single dimensional input that is not a scalar
    if (sum(one.dim.inputs) == 1L)
    {
        dims.required <- dims.to.match[[recycle.ind]][["dims.required"]]
        prod.dims <- vapply(standardized.dims, prod, numeric(1L))
        standardized.dims <- standardized.dims[[recycle.ind]]
        if (warn && prod.dims[1L] != prod.dims[2L])
            throwWarningAboutRecycling(standardized.dims, dims.required)
        return(recycleOneDimensionalInput(x, input.dims, function.name))
    }
    # One element is to be reshaped from a array/matrix with a unit dim and the other isn't
    if (is.null(unlist(dims.to.match)) &&
        !identical(standardized.dims[[1L]], standardized.dims[[2L]]))
        throwErrorAboutDimensionMismatch(standardized.dims, function.name)
    if (warn)
    {
        if (sum(to.recycle) == 1L)
        {
            standardized.dims <- standardized.dims[[recycle.ind]]
            dims.required <- dims.to.match[[recycle.ind]][["dims.required"]]
        } else
            dims.required <- dims.to.match
        throwWarningAboutRecycling(standardized.dims, dims.required)
    }
    mapply(recycleElement, x, dims.to.match, SIMPLIFY = FALSE)
}

recycleElement <- function(x, dim.list)
{
    if (is.null(dim.list))
        return(x)
    dim.to.rep <- dim.list[["dim.to.rep"]]
    dims.required <- dim.list[["dims.required"]]
    if (!is.null(dn <- dimnames(x)))
        dim.names <- dn
    else
        dim.names <- replicate(length(dims.required), NULL, simplify = FALSE)
    n.more.names <- length(dims.required) - length(dim.names)
    if (n.more.names)
        dim.names[length(dim.names) + n.more.names] <- replicate(n.more.names, NULL, simplify = FALSE)
    dim.name.lengths <- vapply(dim.names,
                               function(x) if (is.null(x)) 0L else length(x),
                               integer(1L))
    expand.names <- (dims.required != dim.name.lengths) & (dim.name.lengths == 1L)
    if (any(expand.names))
    {
        which.to.expand <- which(expand.names)
        dim.names[which.to.expand] <- mapply(function(x,  n) rep(x, n),
                                             dim.names[which.to.expand],
                                             dims.required[which.to.expand],
                                             SIMPLIFY = FALSE)
    }
    basic.array <- (length(dim.to.rep) == length(dims.required)) ||
        (length(dim.to.rep) == 1L && dim.to.rep == 1L) ||
        (length(dims.required) == 3L)
    if (is.null(unlist(dim.names)))
        dim.names <- NULL
    if (basic.array)
        out <- array(x, dim = dims.required, dimnames = dim.names)
    else
        out <- array(rep(x, each = dims.required[[1L]]),
                     dim = dims.required, dimnames = dim.names)
    out
}

recycleOneDimensionalInput <- function(x, input.dimensions, function.name)
{
    one.d.ind <- which(input.dimensions == 1)
    other.ind <- which(input.dimensions != 1)
    dims.required <- dim(x[[other.ind]])
    one.d.input  <- x[[one.d.ind]]
    if (!is.null(x.names <- names(x[[one.d.ind]])))
        names.required <- x.names
    else
        names.required <- NULL
    one.d.length <- length(one.d.input)
    n.dim.required <- length(dims.required)
    # If length of 1d array matches one of the other, recycle it
    if (dims.required[2L] == one.d.length)
    {
        names.required <- createDimNames(names.required, index = 2L, n.dim = n.dim.required)
        x[[one.d.ind]] <- array(rep(one.d.input, each = dims.required[1L]),
                                dim = dims.required,
                                dimnames = names.required)
    } else if (dims.required[1L] == one.d.length)
    {
        names.required <- createDimNames(names.required, index = 1L, n.dim = n.dim.required)
        x[[one.d.ind]] <- array(one.d.input, dim = dims.required,
                                dimnames = names.required)
    } else if (length(dims.required) == 3L)
    {
        names.required <- createDimNames(names.required, index = 3L, n.dim = n.dim.required)
        x[[one.d.ind]] <- array(rep(one.d.input, each = prod(dims.required[1:2])), dim = dims.required,
                                dimnames = names.required)
    }
    x
}

createDimNames <- function(names.required, index, n.dim)
{
    if (is.null(names.required))
        return(NULL)
    output <- replicate(n.dim, NULL, simplify = FALSE)
    output[[index]] <- names.required
    output
}

#' Produces a list specifying how to expand one of the inputs.
#' e.g. if input one is an n x p array and input 2 is an n x 1.
#' Then input 2 can be recycled into an n x p by repeating the row
#' dimension p times.
#' @param dims A list containing an integer vector of the dimensions of the two
#' inputs. E.g. the above example would have list(c(n, p), c(n, 1))
#' @return A list with two elements. \code{NULL} if the input isn't to be recycled.
#' Otherwise contains a sublist with two elements called, \itemize{
#' \item dims.required : The new dimensions of the recycled element
#' \item dim.to.rep : Which dimension to do the reshaping. Takes the value 1 if
#' the row dimension is to be repeated and 2 if the column element is to be repeated.
#' }
#' @noRd
determineReshapingDimensions <- function(dims)
{
    dim.lengths <- vapply(dims, length, integer(1L))
    which.min.dim <- which.min(dim.lengths)
    min.dim <- min(dim.lengths)
    truncated.dims <- lapply(dims, `[`, 1:min.dim)
    trunc.dims.that.agree <- truncated.dims[[1L]] == truncated.dims[[2L]]
    out <- list(NULL, NULL)
    if (all(trunc.dims.that.agree)) # one is a matrix, other is a 3d array with inner matrix same dim
    {
        out[[which.min.dim]] <- list(dims.required = dims[[which.max(dim.lengths)]],
                                     dim.to.rep = 1:min.dim)
        return(out)
    }
    unit.dims <- lapply(truncated.dims, function(x) x == 1L)
    # row vector and a column vector, both should be recycled.
    unit.vectors <- vapply(unit.dims, sum, integer(1L)) == 1L
    row.and.column <- all(unit.dims[[1L]] | unit.dims[[2L]])
    if (all(unit.vectors) && row.and.column)
    {
        dims.required <- mapply(function(dims, ind) dims[!ind], dims, unit.dims)
        if (which(unit.dims[[1L]]) == 1L)
            dims.required <- rev(dims.required)
        out <- lapply(unit.dims, function(x) list(dims.required = dims.required,
                                                  dim.to.rep = which(!x)))
        return(out)
    }
    agreements <- lapply(unit.dims, function(x) x | trunc.dims.that.agree)
    possible.reshaping <- vapply(agreements, all, logical(1))
    # Not possible to recycle, return the two slot list with NULL elements
    if (!any(possible.reshaping))
    { # Finally inspect to see if a row vector can be recycled by row
        one.is.a.vector <- dim.lengths == 1L
        one.has.columns <- dim.lengths > 1L
        if (sum(one.is.a.vector) == 1L && sum(one.has.columns) == 1L)
        {
            by.row.possible <- dims[[which(one.is.a.vector)]] == dims[[which(one.has.columns)]][2L]
            if (by.row.possible)
                possible.reshaping <- one.has.columns
        }
    }
    if (!any(possible.reshaping))
        return(out)
    # Specify the appropriate element that can be recycled.
    element.to.copy <- which(!possible.reshaping)
    out[[which(possible.reshaping)]] <- list(dims.required = dims[[element.to.copy]],
                                             dim.to.rep = which(agreements[[element.to.copy]]))
    out
}

throwErrorAboutDimensionMismatch <- function(standardized.dims, function.name)
{
    dim.sizes <- vapply(standardized.dims, length, integer(1L))
    input.type <- unique(vapply(dim.sizes, getOutputType, character(1L)))
    msg <- paste0(function.name, " requires the inputs to have the same ",
                  "dimension or partially agreeing dimensions. In this case, ")
    standardized.dims <- vapply(standardized.dims, numToDimname, character(1L))
    if (length(input.type) == 1L)
    {
        input.type <- if (input.type == "matrix") "matrices" else paste0(input.type, "s")
        msg <- paste0(msg, "the inputs are two ", input.type)
    } else
        msg <- paste0(msg, "the inputs are a ", paste0(input.type, collapse = " and "))
    msg <- paste0(msg, " with ", paste0(standardized.dims, collapse = " and "), " ",
                  "respectively. Please ensure the inputs have the same or partially agreeing ",
                  "dimensions before attempting to recompute ", function.name)
    stop(msg)
}

coerceToVectorTo1dArrayIfNecessary <- function(input)
{
    arrays <- vapply(input, is.array, logical(1L))
    if (!all(arrays))
        for (i in which(!arrays))
        {
            attr(input[[i]], "dim") <- length(input[[i]])
            if (!is.null(names(input[[i]])))
            {
                attr(input[[i]], "dimnames") <- list(attr(input[[i]], "names"))
                attr(input[[i]], "names") <- NULL
            }
        }
    input
}

matchDimensionElements <- function(input, match.rows, match.columns,
                                   warn, function.name)
{
    matching.args <- c(match.rows, match.columns)
    matching.type <- vapply(matching.args,
                            function(x) if (startsWith(x, "Yes")) "exact" else "fuzzy",
                            character(1L))
    hide.unmatched <- vapply(matching.args, endsWith, logical(1L), suffix = "hide unmatched")
    # Do rows first, then columns
    if (match.rows != "No")
        input <- matchElements(input,
                               matching.type = matching.type[1L],
                               hide.unmatched = hide.unmatched[1L],
                               dimension = 1L,
                               warn = warn,
                               function.name = function.name)
    if (match.columns != "No")
    {
        # Check if columns with column names exist
        n.colnames <- vapply(input, function(x) length(colNames(x)), integer(1L))
        if (all(vapply(input, NCOL, integer(1L)) == 1L) && sum(n.colnames) < 2L)
            return(input)
        dim.lengths <- vapply(input, function(x) length(dim(x)), integer(1L))
        if (any(dim.lengths < 2L))
        {
            standardized.dims <- lapply(input, standardizedDimensions)
            throwErrorAboutDimensionMismatch(standardized.dims, function.name)
        }
        input <- matchElements(input,
                               matching.type = matching.type[2L],
                               hide.unmatched = hide.unmatched[2L],
                               dimension = 2L,
                               warn = warn,
                               function.name = function.name)
    }
    input
}

matchElements <- function(input,
                          matching.type,
                          hide.unmatched,
                          dimension,
                          warn,
                          function.name)
{
    element.names <- lapply(input, switch(dimension, rowNames, colnames))
    # Catch case where no names exist in at least one input and dimension lengths don't match
    number.inputs.without.names <- length(Filter(is.null, element.names))
    dim.lengths <- vapply(input, switch(dimension, NROW, NCOL), integer(1L))
    if (number.inputs.without.names == 2L)
    {
        if (dim.lengths[[1L]] == dim.lengths[[2L]])
            return(input)
        else if (!any(dim.lengths == 1L)) # Not possible to recycle and be compatible
        {
            standardized.dims <- lapply(input, standardizedDimensions)
            throwErrorAboutDimensionMismatch(standardized.dims, function.name)
        }
    } else if (number.inputs.without.names != 0L)
        throwErrorAboutNamesRequiredForMatching(dimension, function.name)
    # Check all names are present with no missing names
    checkPartiallyNamed(element.names, function.name = function.name)
    if (matching.type == "exact")
    {
        mapping <- exactMatchDimensionNames(element.names, hide.unmatched, warn, function.name)
        matched.input <- mapply(permuteDimension,
                                input = input,
                                name.mapping = mapping,
                                MoreArgs = list(unmatched = NULL,
                                                dimension = dimension),
                                SIMPLIFY = FALSE)
        unmatched <- attr(mapping, "unmatched")
    } else
    {
        mapping <- fuzzyMatchDimensionNames(element.names, hide.unmatched)
        if (hide.unmatched || is.null(mapping[["unmatched"]]))
            unmatched.list <- list(NULL, NULL)
        else
            unmatched.list <- mapping[["unmatched"]]
        unmatched <- unlist(mapping[["unmatched"]])
        if (!is.null(mapping[["mapping.list"]]))
            mapping <- mapping[["mapping.list"]]
        matched.input <- mapply(permuteDimension,
                                input = input,
                                name.mapping = mapping,
                                unmatched = unmatched.list,
                                MoreArgs = list(dimension = dimension),
                                SIMPLIFY = FALSE)
    }
    existing.unmatched <- attr(input, "unmatched")
    if ((!is.null(existing.unmatched) || hide.unmatched) && warn)
        attr(matched.input, "unmatched") <- c(existing.unmatched, unmatched)
    matched.input
}

permuteDimension <- function(input, name.mapping, dimension, unmatched = NULL)
{
    name.function <- switch(dimension, rowNames, colnames)
    observed.dim.names <- name.function(input)
    if (identical(names(name.mapping), observed.dim.names))
        return(input)
    if (setequal(names(name.mapping), observed.dim.names) && !anyNA(name.mapping))
        input <- reorderDimension(input,
                                  order = name.mapping,
                                  dimension = dimension)
    else
        input <- reorderDimensionAndShowUnmatched(input,
                                                  name.mapping = name.mapping,
                                                  dimension = dimension,
                                                  unmatched = unmatched)
    input
}

getDimensionLength <- function(x)
{
    if (d <- length(dim(x))) d else 1L
}

reorderDimension <- function(input, order, dimension)
{
    dim.length <- getDimensionLength(input)
    if (dimension == 1L)
        switch(dim.length,
               input[order],
               input[order, ],
               input[order, , ])
    else
        switch(dim.length - 1L,
               input[, order],
               input[, order, ])
}

reorderDimensionAndShowUnmatched <- function(input,
                                             name.mapping,
                                             dimension,
                                             unmatched = NULL)
{
    dim <- dim(input)
    dim.names <- dimnames(input)
    dim[dimension] <- length(name.mapping)
    dim.names[[dimension]] <- names(name.mapping)
    existing.mapping <- name.mapping[!is.na(name.mapping)]
    output <-  array(NA,
                     dim = dim,
                     dim.names)
    if (is.data.frame(input))
        input <- as.matrix(input)
    dim.length <- getDimensionLength(input)
    if (dimension == 1L)
    {
        switch(dim.length,
               output[names(existing.mapping)] <- input[existing.mapping],
               output[names(existing.mapping), ] <- input[existing.mapping, ],
               output[names(existing.mapping), , ] <- input[existing.mapping, , ])
        if (!is.null(unmatched))
            switch(dim.length,
                   output[unmatched] <- input[unmatched],
                   output[unmatched, ] <- input[unmatched, ],
                   output[unmatched, , ] <- input[unmatched, , ])

    } else
    {
        ind <- dim.length - 1L
        switch(ind,
               output[, names(existing.mapping)] <- input[, existing.mapping],
               output[, names(existing.mapping), ] <- input[, existing.mapping, ])
        if (!is.null(unmatched))
            switch(ind,
                   output[, unmatched] <- input[, unmatched],
                   output[, unmatched, ] <- input[, unmatched, ])
    }
    output
}

throwErrorAboutNamesRequiredForMatching <- function(dimension, function.name)
{
    dimension.to.match <- switch(dimension, "row", "column")
    err.msg <- paste0("requires inputs that have named ", dimension.to.match, "s in order ",
                      "to match elements by name. Please provide names for all ",
                      dimension.to.match, "s in all input elements or change the matching options ",
                      "to not match ", dimension.to.match, " elements before attempting to recalculate. ")
    throwErrorContactSupportForRequest(err.msg, function.name)
}

valid.custom.matching.options <- c("Yes - hide unmatched", "Yes - show unmatched",
                                   "Fuzzy - hide unmatched", "Fuzzy - show unmatched",
                                   "No")
valid.matching.option <- c("No", "Yes - hide unmatched", "Yes - show unmatched")

throwErrorInvalidMatchingArgument <- function(function.name)
{
    stop("The provided argument to match.elements is invalid. ",
         "It needs to be a single string with one of the options ",
         paste0(sQuote(valid.matching.option, q = FALSE), collapse = ", "),
         " or a named character vector of length two with names 'match.rows' and 'match.columns' ",
         "where the elements are one of ",
         paste0(sQuote(valid.custom.matching.options, q = FALSE), collapse = ", "),
         ". If no names are provided, it is assumed the first element for rows and second for columns. ",
         "Please provide a valid argument before attempting to recalculate ", function.name)
}

checkMatchingArguments <- function(matching.args.provided, function.name)
{
    n.args <- length(matching.args.provided)
    if (!is.character(matching.args.provided))
        throwErrorInvalidMatchingArgument(function.name)
    if (!n.args %in% 1:2)
        throwErrorInvalidMatchingArgument(function.name)
    if (n.args == 1L)
        args.correct <- matching.args.provided %in% valid.matching.option
    else
    {
        names.provided <- names(matching.args.provided)
        if (!is.null(names.provided))
        {
            matches <- pmatch(names.provided, c("rows", "columns"))
            if (any(is.na(matches)))
                throwErrorInvalidMatchingArgument(function.name)
            matching.args.provided <- matching.args.provided[matches]
        }
        args.correct <- vapply(matching.args.provided,
                               function(x) x %in% valid.custom.matching.options,
                               logical(1L))
    }
    if (any(!args.correct))
        throwErrorInvalidMatchingArgument(function.name)
    matching.args.provided
}

#' Attempts to match the elements by name using an fuzzy character match of their names
#' @param x A list of two elements to have their names matched
#' @param ignore.unmatched logical to specify if unmatched names should be ignored and the binding
#' completed. If \code{TRUE} the binding is attempted to be completed even when some names dont match.
#' In that case, an extra element is binded to the output but has entry zero, so the computation can
#' continue. If \code{FALSE}, any elements that dont have matching names will trigger an error to
#' be generated.
#' @param warn logical to specify whether warnings should be thrown
#' @param function.name Name of the root parent function call.
#' @noRd
#' @importFrom utils adist
fuzzyMatchDimensionNames <- function(x.names, hide.unmatched, warn = TRUE)
{
    # Check if exact matching can be done.
    exact.matches <- intersect(x.names[[1L]], x.names[[2L]])
    none.match <- identical(exact.matches, character(0))
    if (!none.match)
    {
        exact.matched.indices <- lapply(x.names, function(x) {
            matches <- match(exact.matches, x, nomatch = NA)
            names(matches) <- exact.matches
            matches
        })
        unmatched.names <- lapply(x.names, function(x) setdiff(x, exact.matches))
    } else
    {
        exact.matched.indices <- replicate(2, NULL)
        unmatched.names <- x.names
    }
    all.matched <- identical(unlist(unmatched.names), character(0))
    if (all.matched)
        return(exact.matched.indices)
    # Create mapping list and update using Fuzzy checks
    mapping.list <- createMappingList(x.names, exact.matched.indices[[1L]])
    mapping.list <- findLevenshteinMatches(unmatched.names, mapping.list)
    unmatched.names <- checkRemainingInMappingList(mapping.list)
    all.matched <- identical(unlist(unmatched.names), character(0))
    if (all.matched)
        return(matchFuzzyMapping(mapping.list, hide.unmatched, unmatched = NULL))
    # Cycle through variants
    mapping.list <- checkVariants(mapping.list, isOther, warn = warn)
    mapping.list <- checkVariants(mapping.list, isDontKnow, warn = warn)
    mapping.list <- checkVariants(mapping.list, isNoneOfThese, warn = warn)
    mapping.list <- checkVariants(mapping.list, isAllOfThese, warn = warn)
    unmatched.names <- checkRemainingInMappingList(mapping.list)
    all.matched <- identical(unlist(unmatched.names), character(0))
    if (all.matched)
        return(matchFuzzyMapping(mapping.list, hide.unmatched, unmatched = NULL))
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
        return(matchFuzzyMapping(mapping.list, hide.unmatched, unmatched = NULL))
    matchFuzzyMapping(mapping.list, hide.unmatched, unmatched = unmatched.names)
}

#' Takes the mapping list generated by fuzzyMatchDimensionNames and standardizes it.
#' @param mapping.list The list of two named integer vectors of the mapping generated by
#' fuzzyMatchDimensionNames.
#' @param hide.unmatched logical element to determine whether unmatched values should be hidden.
#' @param unmatched Either a list with two character vectors giving the names of unmatched
#' elements or \code{NULL} if there are no unmatched.
#' @return The standardization involves creating a larger list with two named elements.
#' The first element, called \code{mapping.list}, is a list of two named integer vectors
#' that point to the matched slices. The mapping list includes the union of both matched
#' and unmatched elements. Any unmatched elements are given an integer value of \code{NA}.
#' The ordering of the mapping is the dimension names (both matched and unmatched) of
#' the first input element and is appended with any unmatched elements in the second element.
#' The second part of the return list is a list with the names of the unmatched elements.
#' @noRd
matchFuzzyMapping <- function(mapping.list, hide.unmatched, unmatched)
{
    if (hide.unmatched || is.null(unmatched))
    {
        first.mapping <- mapping.list[[1L]][!is.na(mapping.list[[1L]])]
        remapped.list <- lapply(mapping.list, function(x) {
            out <- match(first.mapping, x)
            names(out) <- names(x)[out]
            out
        })
        return(list(mapping.list = remapped.list,
                    unmatched = unmatched))
    }
    # Reconstruct the mapping
    # Default to the first mapping (first input and add any unmatched on the second input)
    if (!is.null(unmatched) && length(unmatched[[2L]]))
    {
        unmatched.second <- rep(NA, length(unmatched[[2L]]))
        names(unmatched.second) <- unmatched[[2L]]
    } else
        unmatched.second <- NULL
    first.mapping <- c(mapping.list[[1L]], unmatched.second)
    second.mapping <- match(mapping.list[[1L]], mapping.list[[2L]], incomparables = NA)
    if (any(unmatched.first <- is.na(mapping.list[[1L]])))
        names(second.mapping)[unmatched.first] <- names(mapping.list[[1L]])[unmatched.first]
    if (any(matched.first <- !unmatched.first))
        names(second.mapping)[matched.first] <- names(mapping.list[[2L]])[second.mapping[!is.na(second.mapping)]]
    second.mapping <- c(second.mapping, unmatched.second)
    mapping.list <- list(first.mapping, second.mapping)
    list(mapping.list = mapping.list, unmatched = unmatched)
}


#' Checks if the elements in the input list x have the same number of rows or
#' the same number of columns (either condition is sufficient to pass)
#' @noRd
checkDimensionsEqual <- function(x, function.name)
{
    dims <- lapply(x, standardizedDimensions)
    if (!identical(dims[[1L]], dims[[2L]]))
    {
        error.msg <- paste0("requires inputs to have the same number of rows or the same ",
                            "number of columns. ")
        throwErrorContactSupportForRequest(error.msg, function.name)
    }
}

#' Determine the Labels when matching
#' @noRd
assignLabelsIfPossible <- function(input, dimension)
{
    if (1L %in% dimension)
        input <- addDimensionLabels(input, 1L)
    input.dims <- vapply(input, getDim, integer(1L))
    if (2L %in% dimension && all(input.dims > 1L))
        input <- addDimensionLabels(input, 2L)
    input
}

addDimensionLabels <- function(input, dimension)
{
    name.function <- switch(dimension, rowNames, colnames)
    dimension.names <- lapply(input, name.function)
    dims.required <- standardizedDimensions(input)
    # When doing elementwise addition, the the dimension names of the left element are
    # retained and the right element names discarded. If there are names on the right
    # and not on the left, they should move to the left to be retained in the output.
    if (identical(vapply(dimension.names, is.null, logical(1L)), c(TRUE, FALSE)))
        dimnames(input[[1L]])[[dimension]] <- dimension.names[[2L]]
    else
    {
        dimension.names <- Filter(function(x) !is.null(x), dimension.names)
        if (length(dimension.names) > 1L && !identical(dimension.names[[1L]], dimension.names[[2L]]))
        {
            new.dim.names <- paste0(dimension.names[[1L]], " + ", dimension.names[[2L]])
            input <- lapply(input,
                            function(x) {
                                dimnames(x)[[dimension]] <- new.dim.names
                                x
                            })
        }
    }
    input
}

throwWarningAboutRecycling <- function(standardized.dims, dims.to.match)
{
    if (is.list(dims.to.match))
        dims.to.match <- dims.to.match[[1L]][["dims.required"]]
    output.type <- getOutputType(length(dims.to.match))
    if (is.list(standardized.dims))
    {
        standardized.dims <- vapply(standardized.dims, numToDimname, character(1L))
        prefix.msg <- paste0("Two elements with ",
                             paste0(standardized.dims, collapse = " and "),
                             " respectively were ")
    } else
    {
        if (all(standardized.dims == 1L))
            prefix.msg <- paste0("A scalar element was ")
        else
            prefix.msg <- paste0("An input element with ", numToDimname(standardized.dims),
                                 " was ")
    }
    output.dims <- numToDimname(dims.to.match)
    warn.msg <- paste0(prefix.msg, "recycled to a ", output.type, " with ", output.dims)
    warning(warn.msg)

}

getOutputType <- function(n.dims)
{
    switch(n.dims,
           "vector",
           "matrix",
           "Q Table")
}

numToDimname <- function(x)
{
    dimension.names <- switch(length(x),
                              "row",
                              c("row", "column"),
                              c("row", "column", "statistic"))
    endings <- ifelse(x > 1, "s", "")
    x <- paste(x, paste0(dimension.names, endings))
    if (length(x) == 3L)
        x <- c(paste0(c(x[1], x[2]), collapse = ", "), x[3])
    paste0(x, collapse = " and ")
}

throwWarningAboutUnmatched <- function(unmatched.names, function.name)
{
    n.unmatched <- length(unmatched.names)
    unmatched.names <- dQuote(unmatched.names)
    if (n.unmatched == 1L)
        prefix.msg <- paste0("There was a single unmatched category (", unmatched.names, ") ",
                             "that was removed in the calculation of ", function.name, ". ")
    else
        prefix.msg <- paste0("There were unmatched categories that were removed from the calculation of ",
                             function.name, ". ",
                             paste0("They had the category names: ", paste0(unmatched.names, collapse = ", "), ". "))
    warning(prefix.msg,
            "If you wish ", ngettext(n.unmatched, "this category ", "these categories "),
            "to be used in the calculation, consider ",
            "modifying the name matching options to show the unmatched categories or ",
            "inspecting and possibly modifying the names of the inputs to ensure ",
            "there is a valid match.")
}

characterStatistics <- c("Columns Compared", "Column Comparisons")

removeCharacterStatisticsFromQTables <- function(x)
{
    array.qtables <- vapply(x, function(x) isQTable(x) && getDim(x) == 3L, logical(1L))
    if (any(array.qtables))
        x[array.qtables] <- lapply(x[array.qtables], removeCharacterStatistics)
    x
}

removeCharacterStatistics <- function(x)
{
    table.stats <- possibleStatistics(x)
    if (any(character.stats <- table.stats %in% characterStatistics))
    {
        y <- x[, , which(!character.stats)]
        storage.mode(y) <- "numeric"
        x <- CopyAttributes(y, x)
    }
    x
}

qTableHasMultipleStatistics <- function(qtable)
{
    is.null(attr(qtable, "statistic")) &&
        !is.null(attr(qtable, "questiontypes")) &&
        getDim(qtable) > 1L
}

isNaN <- function(x)
{
    if (!is.data.frame(x))
        return(is.nan(x))
    vapply(x, is.nan, logical(nrow(x)))
}
