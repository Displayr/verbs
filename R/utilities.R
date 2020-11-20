#' Common processing functions for all inputs for Sum.
#'
#' This function will take the arbitrary number of inputs to the function and
#' inspect for appropriate use and then do common tasks. This includes checking
#' the data types are valid for numeric calculations, converting special inputs
#' to numeric, removing unwanted rows and columns, applying subsets and weights
#' Same input parameters as Sum with an additional one to track the function name call.
#' @param check.statistics Logical element to Look for mulitple statistics in the inputs.
#' @param function.name String used to communicate to the user which parent function
#'  called this function when throwing warnings or errors.
#' @noRd
processArguments <- function(...,
                             remove.missing = TRUE,
                             remove.rows = c("NET", "SUM", "Total"),
                             remove.columns = c("NET", "SUM", "Total"),
                             subset = NULL,
                             weights = NULL,
                             warn = FALSE,
                             check.statistics = TRUE,
                             function.name)
{
    x <- list(...)
    checkInputTypes(x, function.name = function.name)
    x <- lapply(x, extractChartDataIfNecessary)
    x <- convertToNumeric(x)
    x <- subsetAndWeightInputsIfNecessary(x,
                                          subset = subset,
                                          weights = weights,
                                          warn = warn,
                                          function.name = function.name)
    x <- lapply(x, removeRowsAndCols,
                remove.rows = remove.rows,
                remove.columns = remove.columns,
                warn = warn,
                function.name = function.name)
    if (warn)
    {
        if (check.statistics && any(qtables <- vapply(x, isQTable, logical(1))))
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

#' Check if the input is not a text or date/time data type. Also verify
#' that the input doesn't contain a mix of QTables and variable types
#' Error otherwise with an informative message.
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

#' For checking if a data type is a variable set and/or QTable
#' @noRd
hasQuestionAttribute <- function(x)
{
    all(c("question", "questiontype") %in% names(attributes(x)))
}

isQTable <- function(x)
{
    all(c("questions", "name") %in% names(attributes(x)))
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
    x <- lapply(x, AsNumeric, binary = FALSE)
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
#' @param warn Logical element to determine if metadata should be retained if warnings
#' are to be thrown later. i.e. if a warning is required, an attribute is added to
#' give a single warning later instead of multiple warnings every time this function is called
#' @param function.name Name of the calling parent function that used this function.
#' @noRd
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
        attr(x, "Removed Indices") <- list(rows = row.names[removed.rows],
                                           columns = col.names[removed.cols])
    x
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

#' Helper function about to throw a warning when some elements are removed in the
#' process in removeRowsAndCols
#' @noRd
throwWarningAboutRemovedIndices <- function(index.name, removed.categories)
{
    warning("These categories have been removed from the ", index.name,
            ": ",
            paste(removed.categories, collapse = ", "),
            ".")
}

#' Helper function that inspects the metadata generated in removeRowsAndColumns
#' to see if any rows and columns were removed on all inputs. If any are found,
#' they are collated and a single warning thrown each for rows and columns respectively.
#' @noRd
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
    if(getDim(x) == 1)
        return(names(x))
    rownames(x)
}

# determines the possible names of the columns elements by checking the colnames if appropriate
# otherwise it returns NULL
colNames <- function(x)
{
    if (getDim(x) == 1)
        return(NULL)
    colnames(x)
}

# Determines the dimension of the input object.
getDim <- function(x)
{
    x.dim <- dim(x)
    n.dim <- if(is.null(x.dim)) 1 else length(x.dim)
    n.dim
}

#' Generalized helper function to subset and weight the inputs if the subset and weight vectors
#' are appropriate
#' @param x list of inputs to process with subset and weights
#' @param subset logical vector to subset the input data
#' @param weights numeric vector to weight the data.
#' @param warn logical whether to warn if any incompatible input is used
#' @param function.name Name of the calling parent function that uses this function
#' @details The subset and weights are appropriate on data structures that contain variable data
#' e.g. variables, matrices and data.frames. It doesn't apply to QTables since they are summary
#' tables which may have different dimension that input variables
#' Q Tables are returned without modification if they are input to this function and a warning
#' thrown if appropriate (see \code{warn})
#' @noRd
subsetAndWeightInputsIfNecessary <- function(x, subset = NULL, weights = NULL, warn = FALSE, function.name)
{
    subset.required <- subsetRequired(subset)
    weighting.required <- weightsRequired(weights)
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
    else
        n.rows <- vapply(x[!qtables.used], NROW, integer(1))
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
        checkWeights(weights, n.rows[1], warn = warn)
        x <- lapply(x, function(x) x * weights)
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

#' Checks if the elements in the input list x have the same number of rows or
#' the same number of columns (either condition is sufficient to pass)
#' @noRd
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
    CopyAttributes(output, x)
}

#' Helper function to weight the appropriate dimension, values weighted columnwise.
#' Also leaves Q Tables without modification and no weights applied.
#' @noRd
weightInput <- function(x, weights)
{
    if (isQTable(x))
        return(x)
    x * weights
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
checkMissingData <- function(x, remove.missing = TRUE)
{
    if (remove.missing == TRUE && any(vapply(x, anyNA, logical(1))))
        warning("Missing values have been ignored in calculation.")
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

#' Used to sum out the appopriate dimension when a 2D table with multiple statistics is used
#' @noRd
sumWithin3Darray <- function(x, summing.function, remove.missing)
{
    apply(x, 3, summing.function, na.rm = remove.missing)
}

#' For functions that allow multiple inputs only if they are all vectors. This
#' helper function identifies elements that are not vectors and throws an informative
#' message to the user stating which inputs are not supported.
#' @noRd
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

#' Simple function that redirects the desired element name matching behaviour, along with
#' function parameters relevant for those functions
#' @noRd
matchRows <- function(x, match.elements, warn, function.name)
{
    switch(match.elements,
           `Yes - ignore if unmatched` = exactMatchRowNames(x, ignore.unmatched = TRUE,
                                                            warn = warn, function.name = function.name),
           `Yes - error if unmatched` = exactMatchRowNames(x, ignore.unmatched = FALSE,
                                                           warn = warn, function.name = function.name),
           `Fuzzy - ignore if unmatched` = fuzzyMatchRowNames(x, ignore.unmatched = TRUE,
                                                              warn = warn, function.name = function.name),
           `Fuzzy - error if unmatched` = fuzzyMatchRowNames(x, ignore.unmatched = FALSE,
                                                             warn = warn, function.name = function.name),
           `No` = cbindAfterCheckingDimensions(x, warn = warn, function.name = function.name))
}

#' helper function to see if the input
#' @param x A list with elements that should have the same row size suitable for binding by column
#' @warn logical to check whether a warning should appear if the data doesnt seem suitable to bind.
#' @details If the elements are of different size, then an error is thrown since cbind cannot occur.
#' Also a check is done to see if the row names are identical before binding and a warning thrown
#' if appropriate.
#' @noRd
cbindAfterCheckingDimensions <- function(x, warn, function.name)
{
    n.rows <- vapply(x, NROW, integer(1))
    if (diff(n.rows) != 0)
    {
        stop("Two inputs have a different number of rows and cannot be joined to ",
             "compute ", function.name)
    }
    if (warn && !Reduce(identical, lapply(x, rowNames)))
        warning("The argument for matching names was set to 'No' in ", function.name, ". ",
                "However, the inputs don't have identical row names ",
                "and the calculation in ", function.name, " might not be appropriate. ")
    do.call(cbind, x)
}

#' Attempts to match the elements by name using an exact character match of their names
#' @param x A list of two elements to have their names matched
#' @param ignore.unmatched logical to specify if unmatched names should be ignored and the binding
#' completed. If \code{TRUE} the binding is attempted to be completed even when some names dont match.
#' In that case, an extra element is binded to the output but has entry zero, so the computation can
#' continue. If \code{FALSE}, any elements that dont have matching names will trigger an error to
#' be generated.
#' @param warn logical to specify whether warnings should be thrown
#' @param function.name Name of the root parent function call.
#' @noRd
exactMatchRowNames <- function(x, ignore.unmatched, warn, function.name)
{
    n.rows <- vapply(x, length, integer(1))
    # Check there is the same number of elements before attempting to match
    # Error if appropriate and unmatched cannot be ignored
    checkLengthsSuitableForMatching(n.rows, ignore.unmatched, function.name)
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

#' Helper function used by exactMatchRowNames and fuzzyMatchRowNames that binds the
#' inputs if there are no names present.
#' @param x list of two elements to be binded.
#' @param ignore.unmatched logical inherited from their parent function exact or fuzzy matching
#'  to determine if possible warnings or errors should be thrown if the dimensions dont match.
#' @param function.name Name of the root parent function calling this used in the thrown warnings
#' or errors.
#' @noRd
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

checkLengthsSuitableForMatching <- function(n.lengths, ignore.unmatched, function.name)
{
    if (!ignore.unmatched && n.lengths[1] != n.lengths[2])
        stop(function.name, " cannot be computed since matching elements by name is required. ",
             "However, after possible removing rows, the input elements have different lengths (",
             paste0(n.lengths, collapse = ' and '), " respectively). Consider relaxing the ",
             "name matching options or modify the inputs to have the same number of elements ",
             "before proceeding with a name matched computation again.")
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
fuzzyMatchRowNames <- function(x, ignore.unmatched, warn = FALSE, function.name)
{
    row.names <- lapply(x, rowNames)
    n.rows <- vapply(x, length, integer(1))
    # Check there is the same number of elements before attempting to match
    # Error if appropriate and unmatched cannot be ignored
    checkLengthsSuitableForMatching(n.rows, ignore.unmatched, function.name)
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
    mapping.list <- findLevenshteinMatches(unmatched.row.names, mapping.list)
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
                             "that couldn't be matched without ambiguity. These had the names ",
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

#' Binds the two elements contained the element x using the mapping information
#' contained in mapping.list and information about which names didn't match in
#' unmatched names. This function is intended to be the final step of the fuzzy matching
#' function fuzzyMatchRowName
#' @param x A list of two named vectors to have their names matched
#' @param mapping.list List with two named vectors the same size as \code{x}. However its
#' elements contain the indices indicting where each element should be mapped when bound
#' together. Any elements that werent possible to match will have the element index of NA
#' and should have an element in the next argument called \code{unmatched.names}
#' @param unmatched.names List with two named vectors where elements denote the unmatched
#' names. The unmatched elements will be returned but their corresponding paired element
#' will be zero.
#' @noRd
#' @importFrom utils adist
bindUsingMappingAndAppendUnmatched <- function(x, mapping.list, unmatched.names)
{
    n.unmatched <- length(unlist(unmatched.names))
    n.first <- length(x[[1]])
    unmatched.second <- mapping.list[[2L]][is.na(mapping.list[[2L]])]
    template.out <- numeric(n.first + length(unmatched.second))
    names(template.out) <- c(names(x[[1L]]), names(unmatched.second))
    unmatched.values <- mapply(function(x, nam) x[nam], x, unmatched.names, SIMPLIFY = FALSE)
    mapping.list[[2L]] <- match(mapping.list[[1L]], mapping.list[[2L]], incomparables = NA)
    mapply(function(x, ind, unmatched) {
        template.out[which(!is.na(ind))] <- x[ind[!is.na(ind)]]
        template.out[names(unmatched)] <- unmatched
        template.out
    }, x, mapping.list, unmatched.values)
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

#' Helper function to take the indices and bind togther the elements of x by matching
#' them by index.
#' @noRd
bindUsingMapping <- function(x, indices)
{
    mapply(function(x, ind) x[unname(ind)], x, indices)
}

#' Helper function to take the indices and bind togther the elements of x by matching
#' them by index. In the fuzzy matching functions the indices are reconciled against the
#' first list dimension to get the matching correct.
#' @noRd
bindUsingFuzzyMapping <- function(x, mapping.list)
{
    mapping.list[[2]] <- match(mapping.list[[1]], mapping.list[[2]])
    return(bindUsingMapping(x, mapping.list))
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
    grepl(pattern = patt, x = x) || x %in% c("any", "all")
}

#' Function to implement the fuzzy search matching of common questionnaire responses using
#' the internal functions of isOther, isAllOfThese, isNoneOfThese, isDontKnow
#' @param mapping.list A mapping list of not completely matched elements created in createMappingList
#' @param function.to.check A function to do the matching. Should be one of isOther, isAllOfThese, isNoneOfThese, isDontKnow
#' @param warn logical to determine if a user is warned if there are ambiguous fuzzy matches.
#' @detail ambiguous fuzzy matches will be ignored and a potential warning thrown.
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
