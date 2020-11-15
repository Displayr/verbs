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
    function.name <- match.call()[[1]]
    x <- processArguments(...,
                          remove.missing = remove.missing,
                          function.name = function.name,
                          remove.rows = remove.rows,
                          remove.columns = remove.columns,
                          subset = subset,
                          weights = weights,
                          warn = warn)
    if (match.elements != "No")
        x <- matchElements(x,
                           match.elements = match.elements,
                           by.row = TRUE,
                           warn = warn)
    else
        checkDimensions(x, by.row = TRUE, function.name = function.name, warn = warn)
    sum.function <- if (remove.missing) sumRows else sumRowsIncludingNAs
    sum.output <- sumElements(x, sum.function)
    if (warn && is.nan(sum.output))
        checkForOppositeInfinites(x, function.name = function.name)
    sum.output
}

sumRowsIncludingNAs <- function(x, y)
{
    sumRows(x, y, remove.missing = FALSE)
}

sumRows<- function(x, y, remove.missing = TRUE)
{
    x <- sumRowsSingleInput(x, remove.missing = remove.missing)
    if (missing(y))
        return(x)
    y <- sumRowsSingleInput(y, remove.missing = remove.missing)
    if (remove.missing)
    {
        if (any(missing.vals <- is.na(x)))
            x[missing.vals] <- 0
        if (any(missing.vals <- is.na(y)))
            y[missing.vals] <- 0
    }
    # Attributes stripped, otherwise the return element will have the attributes of x
    x + as.vector(y)
}

sumRowsSingleInput <- function(x, remove.missing)
{
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
        x
    else
        rowSums(x, na.rm = remove.missing)
}

setAppropriateNames <- function(output, x)
{
    if (length(x) > 1)
    {
        appropriate.rownames <- Reduce(intersect, lapply(x, rowNames))
        if (!is.null(appropriate.rownames))
            output <- setRowNames(output, appropriate.rownames)
    }
    output
}

setRowNames <- function(x, names.to.use)
{
    if (getDim(x) == 1)
        names(x) <- names.to.use
    else
        rownames(x) <- names.to.use
    x
}
