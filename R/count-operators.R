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
        if (length(elements.to.count) != 2L) throwErrorAboutElementsToCountArgument(function.name)
        inds <- pmatch(names(elements.to.count), c("categorical", "numeric"))
        categorical.part <- elements.to.count[[inds[1L]]]
        if (!is.character(categorical.part) && !all(is.na(categorical.part)) && !is.null(categorical.part))
            throwErrorAboutElementsToCountArgument(function.name)
        numeric.part <- elements.to.count[[inds[[2L]]]]
        elements.to.count <- elements.to.count[inds]
        names(elements.to.count) <- c("categorical", "numeric")
        numeric.part <- validateNumericElementsToCount(numeric.part, function.name)
        elements.to.count[["numeric"]] <- numeric.part
    }
    if (is.a.numeric)
        elements.to.count <- list(categorical = NULL, numeric = list(values = elements.to.count))
    if (is.a.character)
        elements.to.count <- list(categorical = elements.to.count, numeric = NULL)
    elements.to.count
}

validateNumericElementsToCount <- function(numeric.values, function.name)
{
    if (is.character(numeric.values))
    {
        if (length(numeric.values) > 1L)
            throwErrorAboutElementsToCountArgument(function.name)
        numeric.values <- parseStringOfNumericConditions(numeric.values, function.name)
    }

    if (is.numeric(numeric.values))
        numeric.values <- list(values = numeric.values)
    numeric.values
}

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
    minus.char.exists <- grepl("-", split.strings, useBytes = TRUE)
    if (any(minus.char.exists))
    {
        split.strings.with.minus <- split.strings[minus.char.exists]
        true.ranges <- grepl("^-?\\d+--?\\d+$", split.strings.with.minus)
        negative.values <- grepl("^-?(\\d+|Inf)$", split.strings.with.minus)
        if (any((!true.ranges & !negative.values)))
            throwErrorAboutInvalidCharsInElementsToCount(string.of.values, function.name)
        if (any(true.ranges))
        {
            numeric.list[["range"]] <- parseRanges(split.strings.with.minus[true.ranges],
                                                   function.name)
            split.strings <- split.strings[-which(minus.char.exists)[true.ranges]]
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
        numeric.list[["values"]] <- append(numeric.list[["values"]],
                                           unique(as.numeric(split.strings)))
    }
    numeric.list
}

parseInequalities <- function(strings, inequality.type, function.name)
{
    patt <- paste0("^(", inequality.type, "-?\\d+$|", inequality.type, "-?Inf$)")
    valid.inequalities <- grepl(patt, strings)
    if (any(!valid.inequalities))
        throwErrorAboutInvalidCharsInElementsToCount(strings, function.name)
    values <- gsub(inequality.type, "", strings)
    values <- as.numeric(values)
    if (length(values) > 1L)
        values <- singleInequalityBoundaryValue(values, inequality.type)
    values
}

singleInequalityBoundaryValue <- function(values, inequality.type)
{
    if (startsWith(inequality.type, ">"))
        return(min(values))
    max(values)
}

parseRanges <- function(strings, function.name)
{
    # regex pattern here is find the hyphen after the first digits with an optional starting - sign
    ## The \\K is to forget the previous matches and only match the last -
    if (any(grepl("NA|^Inf", strings)))
        throwErrorAboutInvalidCharsInElementsToCount(strings, function.name)
    values <- strsplit(strings, "^-?\\d+\\K(-)", perl = TRUE)
    values <- lapply(values, as.numeric)
    lengths <- vapply(values, length, integer(1L))
    if (any(lengths != 2L))
        throwErrorAboutInvalidCharsInElementsToCount(strings, function.name)
    values <- lapply(values, sort)
    if (length(values) > 1L)
        values <- mergeOverlappingRanges(values)
    values
}

canMerge <- function(first.range, second.range)
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

mergeOverlappingRanges <- function(values)
{
    # Sort ranges by lower limit increasing
    values <- values[order(vapply(values, "[[", numeric(1L), 1L))]
    merge.occured <- FALSE
    for (i in seq_len(length(values) - 1L))
    {
        can.merge <- canMerge(values[[i]], values[[i + 1L]])
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

inputToBoolean <- function(x, counting.conditions)
{
    if (is.logical(x))
        return(x)
    if (is.data.frame(x))
        return(vapply(x, inputToBoolean, logical(nrow(x)),
                      counting.conditions = counting.conditions))
    if (is.factor(x))
        return(eval(counting.conditions[["categorical"]]))
    numeric.conditions <- counting.conditions[["numeric"]]
    boolean.outputs <- lapply(numeric.conditions, eval.parent, n = 1L)
    if (length(boolean.outputs) == 1L)
        return(boolean.outputs[[1L]])
    Reduce(`|`, boolean.outputs)
}

quoteConditions <- function(conditions)
{
    mapply(quoteCondition, conditions, names(conditions))
}

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

checkElementsToCountCharactersValid <- function(string, function.name)
{
    sanitized <- tolower(string)
    sanitized <- gsub("inf", "Inf", sanitized)
    sanitized <- gsub("na", "NA", sanitized)
    sanitized <- gsub("\\s*", "", sanitized, useBytes = TRUE)
    remaining.chars <- gsub("[0-9-,<>=]|Inf|NA", "", sanitized, perl = TRUE)
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
