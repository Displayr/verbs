#' @rdname CountOperators
#' @title Checking occurrences or counts of elements in inputs
#' @description \code{Count} gives the total count of elements that satisfy some pre-specified conditions
#'  in the case of a single input and the count is produced element-wise for multiple inputs.
#' @inheritParams Sum
##' @param ignore.missing Logical element controlling whether missing values should be
#'   be ignored during calculations? Defaults to \code{TRUE}. If set to \code{FALSE} and if one of
#'   input elements contains missing values, then the resulting computed count will also be
#'   \code{NA}. In the case of \code{\link{AnyOf}} or \code{\link{NoneOf}}, the result will only be \code{NA}
#'   if all other elements in the calculation are \code{FALSE} for their respective calculation.
#' @param elements.to.count Specifies the conditions to identify which elements are used in \code{Count},
#'   \code{AnyOf} or \code{NoneOf}. The format should either be a list, a character vector or a numeric vector.
#'   If a \itemize{
#'   \item numeric vector is provided: it is assumed to contain the individual values to be checked in
#'   the input for \code{AnyOf}, \code{Count} or \code{NoneOf} calculations.
#'   \item character vector is provided: it is assumed to contain the level labels to be checked in factor
#'   inputs provided.
#'   \item a list is provided: It is assumed to contain at least one named element. The two possible names
#'   are \code{'categorical'} and \code{'numeric'}. The \code{'categorical'} element should contain a
#'   character vector of levels as described above for any factor variables. The \code{'numeric'} element can
#'   either be a numeric vector or it can be a single character element. If the latter is provided,
#'    it will be parsed into a more general list of numeric conditions. The more general numeric condition
#'    list has the following possible elements.
#'       \itemize{
#'            \item \code{'values'}: A numeric vector of values to check in the input (NA permissible here to
#'               check for missing values)
#'            \item \code{'lt'}: A single numeric value to denote a less than condition.
#'            \item \code{'lte'}: A single numeric value to denote a less than or equal condition.'
#'            \item \code{'gt'}: A single numeric value to denote a greater than condition.'
#'            \item \code{'range'}: A list where each elements contains two numeric values
#'             to denote a closed interval e.g. c(1L, 2L) denotes the range 1 <= x <= 2.
#'        }
#'    }
#' @examples
#' test.array <- array(1:18, dim = c(3L, 6L))
#' test.array[7] <- NA
#' counting.condition <- list(numeric = list(values = c(NA, 1, 2, 3),
#'                            gt = 17, range = list(c(4, 5), c(10, 15))))
#' Count(test.array, elements.to.count = counting.condition)
#' counting.cond.as.str <- "NA,1,2,3,>17,4-5,10-15"
#' Count(test.array, elements.to.count = list(numeric = counting.cond.as.str))
#' test.factor <- factor(sample(c("Apples", "Oranges", "Grapes", NA), size = 100, replace = TRUE))
#' Count(test.factor, elements.to.count = c("Oranges", "Grapes", NA))
#' @export
Count <- function(...,
                  remove.rows = NULL, remove.columns = NULL,
                  match.elements = "Yes",
                  elements.to.count = list(categorical = NA_character_,
                                           numeric = NA_real_),
                  ignore.missing = TRUE,
                  subset = NULL,
                  warn = TRUE)
{
    if (is.null(unlist(list(...))))
        return(NA)
    fun.call <- match.call()
    fun.call[[1L]] <- countInputs
    fun.call[["operation"]] <- count
    fun.call[["function.name"]] <- sQuote("Count")
    eval.fun <- if (is.logical(warn)) eval else evalHandlingConditions
    eval.fun(fun.call, parent.frame())
}

#' @rdname CountOperators
#' @description \code{AnyOf} checks if any of the elements satisfy some pre-specified conditions
#'  in the case of a single input. In the case of multiple inputs the check is performed element-wise.
#' @export
AnyOf <- function(...,
                  remove.rows = NULL, remove.columns = NULL,
                  match.elements = "Yes",
                  elements.to.count = list(categorical = NA_character_,
                                           numeric = NA_real_),
                  ignore.missing = TRUE,
                  subset = NULL,
                  warn = TRUE)
{
    if (is.null(unlist(list(...))))
        return(NA)
    fun.call <- match.call()
    fun.call[[1L]] <- countInputs
    fun.call[["operation"]] <- anyOf
    fun.call[["function.name"]] <- sQuote("AnyOf")
    eval.fun <- if (is.logical(warn)) eval else evalHandlingConditions
    eval.fun(fun.call, parent.frame())
}

#' @rdname CountOperators
#' @description \code{NoneOf} checks if none of the elements satisfy some pre-specified conditions
#'  in the case of a single input. In the case of for multiple inputs the check is performed element-wise.
#'  It is the complement of \code{AnyOf}.
#'
#'  Similar to \code{\link{Sum}}, the inputs can be matched via their row and/or column names
#'  in the case of multiple inputs with the output element being filtered by row and/or column.
#' @export
NoneOf <- function(...,
                   remove.rows = NULL, remove.columns = NULL,
                   match.elements = "Yes",
                   elements.to.count = list(categorical = NA_character_,
                                            numeric = NA_real_),
                   ignore.missing =TRUE,
                   subset = NULL,
                   warn = TRUE)
{
    if (is.null(unlist(list(...))))
        return(NA)
    fun.call <- match.call()
    fun.call[[1L]] <- countInputs
    fun.call[["operation"]] <- noneOf
    fun.call[["function.name"]] <- sQuote("NoneOf")
    eval.fun <- if (is.logical(warn)) eval else evalHandlingConditions
    eval.fun(fun.call, parent.frame())
}

#' @rdname CountOperators
#' @param x A single input to be used when performing the calculation on each row/column
#'  dimension in \code{AnyOfEachRow}, \code{CountEachRow}, \code{NoneOfEachRow}, \code{AnyOfEachColumn}, \code{CountEachColumn}, \code{NoneOfEachColumn}.
#' @export
CountEachRow <- function(x,
                         remove.rows = NULL, remove.columns = c("NET", "SUM", "Total"),
                         elements.to.count = list(categorical = NA_integer_,
                                                  numeric = NA_integer_),
                         ignore.missing = TRUE,
                         warn = TRUE)
{
    if (is.null(x))
        return(NA)
    fun.call <- match.call()
    fun.call[[1L]] <- countEachDimension
    fun.call[["dimension"]] <- 1L
    fun.call[["operation"]] <- count
    fun.call[["function.name"]] <- sQuote("CountEachRow")
    eval.fun <- if (is.logical(warn)) eval else evalHandlingConditions
    eval.fun(fun.call, parent.frame())
}

#' @rdname CountOperators
#' @inheritParams CountEachRow
#' @export
CountEachColumn <- function(x,
                            remove.rows = NULL, remove.columns = c("NET", "SUM", "Total"),
                            elements.to.count = list(categorical = NA_integer_,
                                                     numeric = NA_integer_),
                            ignore.missing = TRUE,
                            warn = TRUE)
{
    if (is.null(x))
        return(NA)
    fun.call <- match.call()
    fun.call[[1L]] <- countEachDimension
    fun.call[["dimension"]] <- 2L
    fun.call[["operation"]] <- count
    fun.call[["function.name"]] <- sQuote("CountEachColumn")
    eval.fun <- if (is.logical(warn)) eval else evalHandlingConditions
    eval.fun(fun.call, parent.frame())
}

#' @rdname CountOperators
#' @inheritParams CountEachRow
#' @export
AnyOfEachRow <- function(x,
                         remove.rows = NULL, remove.columns = c("NET", "SUM", "Total"),
                         elements.to.count = list(categorical = NA, numeric = NA),
                         ignore.missing = TRUE,
                         warn = TRUE)
{
    if (is.null(x))
        return(NA)
    fun.call <- match.call()
    fun.call[[1L]] <- countEachDimension
    fun.call[["dimension"]] <- 1L
    fun.call[["operation"]] <- anyOf
    fun.call[["function.name"]] <- sQuote("AnyOfEachRow")
    eval.fun <- if (is.logical(warn)) eval else evalHandlingConditions
    eval.fun(fun.call, parent.frame())
}

#' @rdname CountOperators
#' @inheritParams CountEachRow
#' @export
AnyOfEachColumn <- function(x,
                            remove.rows = NULL, remove.columns = c("NET", "SUM", "Total"),
                            elements.to.count = list(categorical = NA, numeric = NA),
                            ignore.missing = TRUE,
                            warn = TRUE)
{
    if (is.null(x))
        return(NA)
    fun.call <- match.call()
    fun.call[[1L]] <- countEachDimension
    fun.call[["dimension"]] <- 2L
    fun.call[["operation"]] <- anyOf
    fun.call[["function.name"]] <- sQuote("AnyOfEachColumn")
    eval.fun <- if (is.logical(warn)) eval else evalHandlingConditions
    eval.fun(fun.call, parent.frame())
}

#' @rdname CountOperators
#' @inheritParams CountEachRow
#' @export
NoneOfEachRow <- function(x,
                          remove.rows = NULL, remove.columns = c("NET", "SUM", "Total"),
                          elements.to.count = list(categorical = NA, numeric = NA),
                          ignore.missing = TRUE,
                          warn = TRUE)
{
    if (is.null(x))
        return(NA)
    fun.call <- match.call()
    fun.call[[1L]] <- countEachDimension
    fun.call[["dimension"]] <- 1L
    fun.call[["operation"]] <- noneOf
    fun.call[["function.name"]] <- sQuote("NoneOfEachRow")
    eval.fun <- if (is.logical(warn)) eval else evalHandlingConditions
    eval.fun(fun.call, parent.frame())
}

#' @rdname CountOperators
#' @inheritParams CountEachRow
#' @export
NoneOfEachColumn <- function(x,
                             remove.rows = NULL, remove.columns = c("NET", "SUM", "Total"),
                             elements.to.count = list(categorical = NA, numeric = NA),
                             ignore.missing = TRUE,
                             warn = TRUE)
{
    if (is.null(x))
        return(NA)
    fun.call <- match.call()
    fun.call[[1L]] <- countEachDimension
    fun.call[["dimension"]] <- 2L
    fun.call[["operation"]] <- noneOf
    fun.call[["function.name"]] <- sQuote("NoneOfEachColumn")
    eval.fun <- if (is.logical(warn)) eval else evalHandlingConditions
    eval.fun(fun.call, parent.frame())
}

# Common function to produce the output for the count suite of operations.
countInputs <- function(...,
                        operation = count,
                        remove.rows = NULL, remove.columns = NULL,
                        match.elements = "Yes",
                        elements.to.count = list(categorical = NA_character_,
                                                 numeric = NA_real_),
                        ignore.missing = TRUE,
                        subset = NULL, weights = NULL,
                        warn = FALSE,
                        function.name)
{
    x <- list(...)
    if (is.null(unlist(x)))
        return(NA)
    n.inputs <- length(x)
    elements.to.count <- validateElementsToCount(elements.to.count, function.name)
    if (!ignore.missing && anyNA(unlist(elements.to.count)))
        ignore.missing <- TRUE
    x <- processArgumentsForCounting(x,
                                     remove.missing = FALSE,
                                     remove.rows = remove.rows, remove.columns = remove.columns,
                                     subset = subset, weights = weights,
                                     return.total.element.weights = "No",
                                     check.statistics = TRUE,
                                     warn = warn,
                                     function.name = function.name)
    counting.conditions <- elementsToCountAsConditions(elements.to.count)
    if (n.inputs == 1)
    {
        x.to.boolean <- inputToBoolean(x[[1L]], counting.conditions,
                                       ignore.missing = ignore.missing,
                                       function.name = function.name)
        count.output <- operation(x.to.boolean, ignore.missing = ignore.missing)
    } else
    {
        match.elements[tolower(match.elements) == "yes"] <- "Yes - hide unmatched"
        match.elements <- checkMatchingArguments(match.elements,
                                                 function.name = function.name)
        is.noneOf.operator <- identical(operation, noneOf)
        boolean.operation <- if (is.noneOf.operator) anyOf else operation
        .booleanFunction <- function(x, y)
        {
            matched.inputs <- calculateBinaryOperation(x, y,
                                                       operation = list,
                                                       match.elements = match.elements,
                                                       remove.missing = FALSE,
                                                       function.name = function.name,
                                                       warn = warn)
            matched.inputs[[2L]] <- inputToBoolean(matched.inputs[[2L]],
                                                   counting.conditions = counting.conditions,
                                                   ignore.missing = ignore.missing,
                                                   function.name = function.name)
            output <- boolean.operation(matched.inputs[[1L]], matched.inputs[[2L]], ignore.missing = ignore.missing)
            attr(output, "unmatched") <- attr(matched.inputs, "unmatched")
            output
        }
        x[[1L]] <- inputToBoolean(x[[1L]], counting.conditions = counting.conditions,
                                  ignore.missing = ignore.missing, function.name = function.name)
        if (length(x) == 1 && identical(operation, count))
            count.output <- x[[1L]] * 1L
        else
            count.output <- Reduce(.booleanFunction, x)
        if (is.noneOf.operator)
            count.output <- !count.output
        if (warn)
        {
            throwWarningIfTransposedInput(count.output, function.name)
            unmatched.elements <- attr(count.output, "unmatched")
            if (!is.null(unmatched.elements))
                throwWarningAboutUnmatched(unmatched.elements, function.name)
        }
        count.output <- sanitizeAttributes(count.output)
    }
    if (getDimensionLength(count.output) == 1L)
    {
        count.output <- setNames(as.vector(count.output), nm = names(count.output))
    }
    count.output
}

count <- function(x, y = NULL, ignore.missing = TRUE)
{
    if (is.null(y))
        return(sum(x, na.rm = ignore.missing))
    x + y
}

anyOf <- function(x, y = NULL, ignore.missing = TRUE)
{
    if (is.null(y))
        return(any(x, na.rm = ignore.missing))
    x | y
}

noneOf <- function(x, ignore.missing = TRUE)
{
    !anyOf(x, ignore.missing = ignore.missing)
}

countEachDimension <- function(x,
                               dimension,
                               operation = count,
                               remove.rows = NULL,
                               remove.columns = c("NET", "SUM", "Total"),
                               elements.to.count = list(categorical = NA_character_,
                                                        numeric = NA_real_),
                               ignore.missing = TRUE,
                               warn = FALSE,
                               function.name)
{
    if (missing(dimension))
        throwErrorAboutMissingDimensionArgument(substitute(operation), function.name)
    elements.to.count <- validateElementsToCount(elements.to.count, function.name)
    if (is.null(x))
        return(switch(as.character(substitute(operation)),
                      count = 0L,
                      anyOf = FALSE,
                      noneOf = TRUE))
    if (!ignore.missing && anyNA(unlist(elements.to.count)))
        ignore.missing <- TRUE
    x <- processArgumentsForCounting(list(x),
                                     remove.missing = FALSE, # This is only used to trigger a warning
                                     remove.rows = remove.rows, remove.columns = remove.columns,
                                     subset = NULL, weights = NULL,
                                     check.statistics = FALSE,
                                     return.total.element.weights = "No",
                                     warn = warn,
                                     function.name = function.name)
    counting.conditions <- elementsToCountAsConditions(elements.to.count)
    x.to.boolean <- inputToBoolean(x[[1L]], counting.conditions = counting.conditions,
                                   ignore.missing = ignore.missing, function.name = function.name)
    booleanOperationEachDimension(x.to.boolean,
                                  operation,
                                  dimension,
                                  ignore.missing = ignore.missing)
}

#' @importFrom flipU IsQTable
booleanOperationEachDimension <- function(x, operation, dimension = 1L, ignore.missing)
{
    by.row <- dimension == 1L
    x.names <- if (by.row) rowNames(x) else colNames(x)
    if (IsQTable(x) && getDimensionLength(x) > 2L)
    {
        dims <- if (by.row) c(1L, 3L) else 2:3
        y <- apply(x, dims, operation, ignore.missing = ignore.missing)
        if (NCOL(y) == 1L)
            y <- setNames(as.vector(y), x.names)
    } else if (NCOL(x) == 1L)
    {
        if (by.row)
        {
            if (identical(operation, count))
                y <- 1L * x
            else if (identical(operation, anyOf))
                y <- x
            else
                y <- !x
            y <- setNames(as.vector(y), x.names)
        } else
            y <- setNames(operation(x, ignore.missing = ignore.missing),
                          x.names)
    } else
        y <- apply(x, dimension, operation, ignore.missing = ignore.missing)
    y
}

# Inspects the elements to count argument and validates that it is the correct structure.
# It is required to either be a
# * A list with two elements named categorical and numeric to denote the conditions
#   for counting the elements in the inputs
# * character vector: In this case, it is assumed that the characters are the levels to count
#    for categorical variables (factors)
# * numeric vector: In this case, it is assumed that the values are the values to count
#    for each element in general numeric inputs.
validateElementsToCount <- function(elements.to.count, function.name)
{
    n.elements.to.count <- length(elements.to.count)
    is.a.vector <- is.vector(elements.to.count)
    is.a.list <- is.list(elements.to.count)
    is.a.character <- is.character(elements.to.count)
    is.a.numeric <- is.numeric(elements.to.count)
    valid.structure <- is.a.list || (is.a.vector && (is.a.character || is.a.numeric))
    if (!valid.structure)
        throwErrorAboutElementsToCountArgument(function.name)
    if (is.a.list)
    {
        inds <- pmatch(names(elements.to.count), c("categorical", "numeric"))
        if (length(inds) == 0L) throwErrorAboutElementsToCountArgument(function.name)
        if (length(inds) == 1L)
        {
            elements.to.count <- append(elements.to.count, list(NULL), after = inds %% 2)
            inds <- 1:2
        }
        categorical.part <- elements.to.count[[inds[1L]]]
        if (!is.character(categorical.part) && !all(is.na(categorical.part)) && !is.null(categorical.part))
            throwErrorAboutElementsToCountArgument(function.name)
        missing.string <- categorical.part == "Missing data used only by Q/Displayr"
        if (any(missing.string, na.rm = TRUE))
            elements.to.count[[inds[1L]]][missing.string] <- NA_character_
        numeric.part <- elements.to.count[[inds[2L]]]
        elements.to.count <- elements.to.count[inds]
        names(elements.to.count) <- c("categorical", "numeric")
        if (!is.null(numeric.part))
        {
            numeric.part <- validateNumericElementsToCount(numeric.part, function.name)
            elements.to.count[["numeric"]] <- numeric.part
        }
    }
    if (is.a.numeric)
        elements.to.count <- list(categorical = NULL,
                                  numeric = list(values = elements.to.count))
    if (is.a.character)
        elements.to.count <- list(categorical = elements.to.count, numeric = NULL)
    elements.to.count
}

validateNumericElementsToCount <- function(numeric.values, function.name)
{
    if (is.list(numeric.values))
    {
        valid.names <- c("values", "range", "gt", "gte", "lt", "lte")
        matched.names <- match(names(numeric.values), valid.names)
        if (anyNA(matched.names) || length(matched.names) == 0L)
            stop("The numeric part of the elements.to.count list be a named list ",
                 "with the possible names: 'values', 'range', 'gt', 'gte', 'lt', 'lte'")
        if (1L %in% matched.names)
        {
            values <- numeric.values[["values"]]
            values.valid <- is.numeric(values) || all(is.na(values))
        } else
            values.valid <- TRUE
        if (2L %in% matched.names)
        {
            ranges <- numeric.values[["range"]]
            range.is.list <- is.list(ranges)
            ranges.are.numeric <- all(vapply(ranges, is.numeric, logical(1L)))
            no.missing <- !any(vapply(ranges, anyNA, logical(1L)))
            length.two <- all(vapply(ranges, length, integer(1L)) == 2L)
            ranges.valid <- range.is.list && ranges.are.numeric && no.missing && length.two
        } else
            ranges.valid <- TRUE
        inequalities.to.check <- 3:6 %in% matched.names
        if (any(inequalities.to.check))
        {
            matched.inds <- matched.names %in% 3:6
            inequalities.valid <- vapply(numeric.values[matched.inds],
                                         function(x) length(x) == 1L && is.numeric(x),
                                         logical(1L))
            all.inequalities.valid <- all(inequalities.valid)
            inequalities.attempted <- valid.names[!inequalities.valid]
        } else
        {
            all.inequalities.valid <- TRUE
            inequalities.attempted <- NULL
        }
        checks <- structure(c(values = values.valid,
                              ranges = ranges.valid,
                              inequalities = all.inequalities.valid),
                            inequalities.attempted = inequalities.attempted)
        checkElementsToCountNumericList(checks)
    }
    if (is.character(numeric.values))
    {
        if (length(numeric.values) > 1L)
            throwErrorAboutElementsToCountArgument(function.name)
        numeric.values <- parseStringOfNumericConditions(numeric.values, function.name)
    }
    if (!is.list(numeric.values) && (is.numeric(numeric.values) || all(is.na(numeric.values))))
        numeric.values <- list(values = numeric.values)
    numeric.values
}

checkElementsToCountNumericList <- function(checks.valid)
{
    if (all(checks.valid))
        return(NULL)
    .stopMsg <- function(element.names, reason)
    {
        paste0("The ", paste0(sQuote(element.names), collapse = ", "), " ",
               ngettext(length(element.names), "element", "elements"), " of the numeric part of the elements.to.count ",
               "list ", reason)
    }
    if (!checks.valid["values"])
        stop(.stopMsg("values", "needs to contain numeric values"))
    if (!checks.valid["ranges"])
        stop(.stopMsg("range", "needs to be a list where all elements are two numeric values"))
    if (!checks.valid["inequalities"])
    {
        inequalities.attempted <- unique(attr(checks.valid, "inequalities.attempted"))
        if (length(inequalities.attempted) == 1L)
            stop(.stopMsg(inequalities.attempted,
                          "needs to be a single numeric value denoting the boundary of the inequality"))
        else
            stop(.stopMsg(inequalities.attempted,
                          "each need to be a single numeric value denoting the boundary of each inequality"))
    }

}

# Parses the string of numeric conditions into a list giving all the conditions.
# The input format is a simple string of the form
# "1,2,3,4,<-10,5-10,>=20" to denote single values, inequalities or ranges.
# The output format is a list with the numeric values of the form
# list(values = 1:4, lt = -10L, gte = 20L, range = list(c(5L, 10L)))
#  The range is a separate list since multiple ranges could be used
parseStringOfNumericConditions <- function(string.of.values, function.name)
{
    sanitized.string <- checkElementsToCountCharactersValid(string.of.values, function.name)
    split.strings <- strsplit(sanitized.string, ",")[[1L]]
    numeric.list <- list()
    inequalities <- c("<=" = "lte", ">=" = "gte", "<" = "lt", ">" = "gt")
    for (inequality.type in names(inequalities))
    {
        potential.inequalities <- grepl(inequality.type, split.strings)
        if (any(potential.inequalities))
        {
            inequality.name <- inequalities[inequality.type]
            numeric.list[[inequality.name]] <- parseInequalities(split.strings[potential.inequalities],
                                                                 inequality.type,
                                                                 function.name)
            split.strings <- split.strings[!potential.inequalities]
        }
    }
    numeric.list <- mergeInequalities(numeric.list)
    minus.char.exists <- grepl("-", split.strings, useBytes = TRUE)
    if (any(minus.char.exists))
    {
        split.strings.with.minus <- split.strings[minus.char.exists]
        true.ranges <- grepl(r"(^-?\d*\.?\d*--?\d*\.?\d*$)", split.strings.with.minus)
        negative.values <- grepl(r"(^-?(\d*\.?\d*|Inf)$)", split.strings.with.minus)
        if (any((!true.ranges & !negative.values)))
            throwErrorAboutInvalidCharsInElementsToCount(string.of.values, function.name)
        true.ranges <- true.ranges & !negative.values
        if (any(true.ranges))
        {
            potential.ranges <- parseRanges(split.strings.with.minus[true.ranges], function.name)
            split.strings <- split.strings[-which(minus.char.exists)[true.ranges]]
            if (length(numeric.list) > 0L)
            {
                numeric.list <- reconcileBoundedAndOneSidedIntervals(potential.ranges, numeric.list)
            } else
                numeric.list[["range"]] <- potential.ranges
        }
    }
    missing.values <- split.strings == "NA"
    if (any(missing.values))
    {
        numeric.list[["values"]] <- NA
        split.strings <- split.strings[!missing.values]
    } else
    {
        numeric.list[["values"]] <- NULL
    }
    numeric.values.exist <- length(split.strings) > 0L
    if (numeric.values.exist)
    {
        x <- unique(as.numeric(split.strings))
        # If there is more than one element in the list that means other ranges or inequalities are in the conditions
        # Check if the values are not already covered in those conditions already
        if (length(numeric.list) > 1L)
        {
            conditions.to.check <- elementsToCountAsConditions(list(numeric = numeric.list[-length(numeric.list)]))
            values.covered <- lapply(conditions.to.check[["numeric"]], eval.parent, n = 1L)
            values.covered <- if (length(values.covered) == 1L) values.covered[[1L]] else Reduce(`|`, values.covered)
            if (!all(values.covered))
                numeric.list[["values"]] <- append(numeric.list[["values"]], x[!values.covered])
        } else
            numeric.list[["values"]] <- append(numeric.list[["values"]], x)
    }
    numeric.list
}

# Take a string of one sided inequalities split by , e.g. '<3,>=2,<Inf'
# and parse them, removing any redundant inequalities if possible
parseInequalities <- function(strings, inequality.type, function.name)
{
    patt <- paste0("^(", inequality.type, "-?\\d+\\.?\\d*|", inequality.type, "-?Inf)$")
    valid.inequalities <- grepl(patt, strings)
    if (any(!valid.inequalities))
        throwErrorAboutInvalidCharsInElementsToCount(strings, function.name)
    values <- gsub(inequality.type, "", strings)
    values <- as.numeric(values)
    if (length(values) > 1L)
        values <- singleInequalityBoundaryValue(values, inequality.type)
    values
}

# Merges inequalities (gives the most general if both lt (<) and lte (<=) conditions are used)
# Also merges two inequalities into one if it covers the entire real line. E.g.
# >5 and <10 become a single <= Inf
mergeInequalities <- function(list.of.inequalities)
{
    # Check if there are two less than conditions (< and <=) and use only the most general one
    inequality.names <- names(list.of.inequalities)
    if (length(list.of.inequalities) > 1L && any(lt.or.lte <- startsWith(inequality.names, "lt")))
    {
        if (sum(lt.or.lte) == 2L)
        {
            vals <- list.of.inequalities[lt.or.lte]
            if (vals[[1L]] == vals[[2L]])
                list.of.inequalities["lt"] <- NULL
            else
                list.of.inequalities[names(which.min(unlist(vals)))] <- NULL

        }
    }
    # Check if there are two greater than conditions (> and >=) and use only the most general one
    inequality.names <- names(list.of.inequalities)
    if (length(list.of.inequalities) > 1L && any(gt.or.gte <- startsWith(inequality.names, "gt")))
    {
        if (sum(gt.or.gte) == 2L)
        {
            vals <- list.of.inequalities[gt.or.gte]
            if (vals[[1L]] == vals[[2L]])
                list.of.inequalities["gt"] <- NULL
            else
                list.of.inequalities[names(which.max(unlist(vals)))] <- NULL
        }
    }
    # If both a less than and greater than condition exist
    # check they dont overlap or cover all values
    if (length(list.of.inequalities) > 1L)
    {
        lower.bound.val <- list.of.inequalities$lt
        upper.bound.val <- list.of.inequalities$gt
        same.boundary <- lower.bound.val == upper.bound.val
        if (lower.bound.val > upper.bound.val)
        {
            list.of.inequalities <- list(lte = Inf)
        }
    }
    list.of.inequalities
}

# Determine the appropriate boundary value to use for multiple inequalities of the same type
# E.g. >10 and >5 becomes >5
singleInequalityBoundaryValue <- function(values, inequality.type)
{
    if (startsWith(inequality.type, ">"))
        return(min(values))
    max(values)
}

# Parses the range strings so that 1-5 and -10--3 become list(c(1L, 5L), c(-10L, -3L)
# Any overlapping ranges are merged into one range.
parseRanges <- function(strings, function.name)
{
    # regex pattern here is find the hyphen after the first digits with an optional starting - sign
    ## The \\K is to forget the previous matches and only match the last -
    if (any(grepl("NA|^Inf", strings)))
        throwErrorAboutInvalidCharsInElementsToCount(strings, function.name)
    strings <- gsub(r"(-\.)", "-0.", strings)
    strings <- gsub(r"(^\.)", "0.", strings)
    values <- strsplit(strings, r"(^-?\d\.?\d*\K(-))", perl = TRUE)
    values <- lapply(values, function(x) sub(r"(^\.$)", 0, x))
    values <- lapply(values, as.numeric)
    lengths <- vapply(values, length, integer(1L))
    if (any(lengths != 2L))
        throwErrorAboutInvalidCharsInElementsToCount(strings, function.name)
    values <- lapply(values, sort)
    if (length(values) > 1L)
        values <- mergeOverlappingRanges(values)
    values
}

# Function to check if the ranges can be merged.
canMergeRanges <- function(first.range, second.range)
{
    # This first check shouldn't be required if the ranges are sorted by increasing lower bounds
    if (first.range[1L] >= second.range[1L] && first.range[1L] <= second.range[2L])
        return(TRUE)
    if (first.range[2L] >= second.range[1L] && first.range[2L] <= second.range[2L])
        return(TRUE)
    if (first.range[1L] <= second.range[1L] && first.range[2L] >= second.range[2L])
        return(TRUE)
    FALSE
}

# Given a list of ranges, any overlapping ones are merged.
# E.g. list(c(-5L, 0L), c(10L, 15L), c(-2L, 5L)) becomes list(c(-5L, 5L), c(10L, 15L))
mergeOverlappingRanges <- function(values)
{
    # Sort ranges by lower limit increasing
    values <- values[order(vapply(values, "[[", numeric(1L), 1L))]
    merge.occured <- FALSE
    for (i in seq_len(length(values) - 1L))
    {
        can.merge <- canMergeRanges(values[[i]], values[[i + 1L]])
        if (can.merge)
        {
            all.values <- unlist(values[i:(i + 1L)])
            values[[i + 1L]] <- range(all.values)
            values[[i]] <- NA
            merge.occured <- TRUE
        }
    }
    if (merge.occured)
        values <- Filter(Negate(anyNA), values)
    values
}

# Removes any redundant conditions when there are one sided inequalities and bounded intervals.
# E.g. bounded.intervals = list(c(0L, 5L), c(10L, 15L)) and one.sided.intervals = list(lt = 2L, gte = 12L)
# becomes list(lte = 5L, gte = 10L) and the redundant intervals are dropped.
reconcileBoundedAndOneSidedIntervals <- function(bounded.intervals, one.sided.intervals)
{
    interval.types <- names(one.sided.intervals)
    if (length(one.sided.intervals) == 1L)
        output <- reconcileBoundedIntervalsAgainstOneSidedInterval(bounded.intervals, one.sided.intervals)
    else
        output <- reconcileBoundedIntervalsAgainstTwoOneSidedIntervals(bounded.intervals, one.sided.intervals)
    bounded.intervals <- output[["bounded"]]
    numeric.list <- append(list(), output[["one.sided"]])
    if (length(bounded.intervals) > 0L)
        numeric.list[["range"]] <- bounded.intervals
    numeric.list
}

reconcileBoundedIntervalsAgainstOneSidedInterval <- function(bounded.intervals, one.sided.interval, interval.type)
{
    if (missing(interval.type))
        interval.type <- names(one.sided.interval)
    lower.bounds <- vapply(bounded.intervals, `[[`, numeric(1L), 1L)
    upper.bounds <- vapply(bounded.intervals, `[[`, numeric(1L), 2L)
    one.sided.boundary <- one.sided.interval[[1L]]
    gt.condition <- startsWith(interval.type, "gt")
    if (gt.condition)
    {
        completely.covered <- lower.bounds > one.sided.boundary
        overlapping <- !completely.covered & upper.bounds >= one.sided.boundary
    } else
    {
        completely.covered <- upper.bounds < one.sided.boundary
        overlapping <- !completely.covered & lower.bounds <= one.sided.boundary
    }
    if (any(overlapping))
    {
        operation <- if (gt.condition) min else max
        values <- if (gt.condition) lower.bounds[overlapping] else upper.bounds[overlapping]
        one.sided.interval[[1L]] <- operation(values)
        if (!endsWith(interval.type, "e"))
            names(one.sided.interval) <- paste0(interval.type, "e")
    }
    absorbed <- completely.covered | overlapping
    if (any(absorbed))
        bounded.intervals[absorbed] <- NULL
    list(one.sided = one.sided.interval, bounded = bounded.intervals)
}

# Determine the non-redundant conditions when two one.sided conditions are given as well as bounded intervals
# e.g. <0, -3-5, 6-10, >8
# This is done with two checks using each one.sided condition against the bounded intervals.
# Then deducing the non-redundant conditions from both results.
reconcileBoundedIntervalsAgainstTwoOneSidedIntervals <- function(bounded.intervals, one.sided.intervals)
{
    one.sided.results <- mapply(reconcileBoundedIntervalsAgainstOneSidedInterval,
                                one.sided.intervals, names(one.sided.intervals),
                                MoreArgs = list(bounded.intervals = bounded.intervals),
                                SIMPLIFY = FALSE)
    bounded.results <- lapply(one.sided.results, `[[`, "bounded")
    output <- list()
    potential.bounded <- intersect(bounded.results[[1L]], bounded.results[[2L]])
    potential.onesided <- lapply(one.sided.results, `[[`, "one.sided")
    new.onesided.names <- unlist(lapply(potential.onesided, names))
    if (!is.null(new.onesided.names))
    {
        potential.onesided <- lapply(potential.onesided, unname)
        names(potential.onesided)[names(potential.onesided) %in% names(new.onesided.names)] <- new.onesided.names
    }

    output[["one.sided"]] <- mergeInequalities(potential.onesided)
    output[["bounded"]] <- if (length(output[["one.sided"]]) > 0L) potential.bounded else NULL
    output
}

# Converts the list of conditions specified simply with their values as the boundary points into
# a list of quoted conditions that can be evaluated later.
# e.g. elements.to.count = list(categorical = c("foo", "bar"), numeric = list(values = c(NA, 1,2,3))
# output = list(categorical = x %in% c("foo", "bar"), numeric = x %in% c(NA, 1, 2, 3))
elementsToCountAsConditions <- function(elements.to.count)
{
    if (!is.null(categorical.conditions <- elements.to.count[["categorical"]]))
        elements.to.count[["categorical"]] <- quoteCondition(categorical.conditions, "values")
    if (!is.null(numeric.conditions <- elements.to.count[["numeric"]]))
    {
        numeric.conditions <- quoteConditions(numeric.conditions)
        multiple.numeric.conditions <- vapply(numeric.conditions, is.list, logical(1L))
        if (any(multiple.numeric.conditions))
            elements.to.count[["numeric"]] <- flattenToSingleList(numeric.conditions)
        else
            elements.to.count[["numeric"]] <- numeric.conditions
    }
    elements.to.count
}

# Converts an input into a logical structure using the conditions provided
# e.g. x = array(1:12, dim = 3:4, dimnames = list(letters[1:3], LETTERS[1:4]))
# and the condition is x > 5 will give a logical array output with the same dim
# and dimnames as x but with logical elements instead of numeric. The counting conditions
# are ones provided by elementsToCountAsConditions
inputToBoolean <- function(x, counting.conditions = NULL, ignore.missing = TRUE, function.name)
{
    if (is.character(x) && is.matrix(x))
        x <- as.data.frame(lapply(split(x, col(x)), factor))
    if (is.data.frame(x))
        output <- vapply(x, inputToBoolean, logical(nrow(x)),
                         counting.conditions = counting.conditions,
                         ignore.missing = ignore.missing,
                         function.name = function.name)
    else if (is.factor(x))
    {
        check.condition <- counting.conditions[["categorical"]]
        if (is.null(check.condition))
            throwErrorAboutMissingCondition(x, function.name)
        output <- eval(check.condition)
        # Replace any NAs in the original data since they are masked when using %in%
        if (!ignore.missing && any(missing.values <- is.na(x)))
            is.na(output) <- missing.values
    } else
    {
        numeric.conditions <- counting.conditions[["numeric"]]
        if (is.null(numeric.conditions))
            throwErrorAboutMissingCondition(x, function.name)
        boolean.outputs <- lapply(numeric.conditions, eval.parent, n = 1L)
        boolean.outputs <- lapply(boolean.outputs,
                                  function(output)
                                  {
                                      mostattributes(output) <- attributes(x)
                                      output
                                  })
        output <- if (length(boolean.outputs) == 1L) boolean.outputs[[1L]] else Reduce(`|`, boolean.outputs)
        mostattributes(output) <- attributes(x)
        # NAs are masked when using the %in% operator for all the specified values.
        # They should be restored if NAs are not to be ignored.
        if (!ignore.missing && "values" %in% names(numeric.conditions) &&
            any(missing.values <- is.na(x)))
            is.na(output) <- missing.values
    }
    if (ignore.missing && any(missing.vals <- is.na(output)))
        output[missing.vals] <- FALSE
    output
}

# Convert all conditions to be suitable for eval
quoteConditions <- function(conditions)
{
    mapply(quoteCondition, conditions, names(conditions))
}

# Convert values into conditions suitable for eval
quoteCondition <- function(values, comparison)
{
    if (comparison == "range")
        return(mapply(quoteCondition, values,
                      MoreArgs = list(comparison = "interval")))
    switch(comparison,
           "gte"    = bquote(x >= .(values)),
           "gt"     = bquote(x >  .(values)),
           "lte"    = bquote(x <= .(values)),
           "lt"     = bquote(x <  .(values)),
           "values" = bquote(x %in% .(values)),
           "interval"  = bquote(x >= .(values[1L]) & x <= .(values[2L])))
}

# Check elements to count string has valid characters
checkElementsToCountCharactersValid <- function(string, function.name)
{
    sanitized <- tolower(string)
    sanitized <- gsub("inf", "Inf", sanitized)
    sanitized <- gsub("na", "NA", sanitized)
    sanitized <- gsub("\\s*", "", sanitized, useBytes = TRUE)
    remaining.chars <- gsub("[0-9-,<>=\\.]|Inf|NA", "", sanitized, perl = TRUE)
    if (length(remaining.chars) > 1L || nzchar(remaining.chars))
        throwErrorAboutInvalidCharsInElementsToCount(string, function.name)
    # Return the string with white space removed
    sanitized
}

throwErrorAboutInvalidCharsInElementsToCount <- function(string, function.name)
{
    strings.provided <- paste0(dQuote(gsub("\\s", "", string)), collapse = ", ")
    stop("The provided input to ", function.name, " specifying the elements to count is ",
         strings.provided, " and is invalid. ",
         "The format should be a range like 1-10, an open interval like: >5, >=5, <10, <=10 or ",
         "specific values like Inf or NA to denote infinity or a missing values")
}

throwErrorAboutElementsToCountArgument <- function(function.name)
{
    stop(sQuote("elements.to.count"), " needs to either be a list with two elements named ",
         "categorical and numeric or a character vector or numeric vector")
}

#' @importFrom flipU IsQTable
throwErrorAboutMissingCondition <- function(x, function.name)
{
    if (isVariable(x))
        x.type <-  "Categorical variable"
    else if (is.factor(x))
        x.type <-"factor"
    else if (IsQTable(x))
        x.type <- "Table"
    else
        x.type <- class(x)[[1L]]
    input.type <- if (x.type %in% c("Categorical variable", "factor")) "categorical" else "numeric"
    stop("A ", x.type, " was used as an input to ", function.name, ". ",
         "However, no ", input.type, " values were specified as elements to count in the calculation. ",
         "Specify the ", input.type, " elements to count before using ", function.name, " again.")
}

throwErrorAboutMissingDimensionArgument <- function(operation, function.name)
{
    stop(sQuote('dimension'), " argument is required to specify which dimension ",
         "to calculate the ", operation, " operation.")
}
