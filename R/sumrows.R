#' @rdname SumOperations
#' @export
SumRows <- function(...,
                    remove.missing = TRUE,
                    remove.rows = c("NET", "SUM", "Total"),
                    remove.columns = c("NET", "SUM", "Total"),
                    subset = NULL,
                    weights = NULL,
                    match.elements = "Yes - ignore if unmatched",
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
                warn = warn,
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
