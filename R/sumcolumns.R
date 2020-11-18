#' @export
SumColumns <- function(...,
                       remove.missing = TRUE,
                       remove.rows = c("NET", "SUM", "Total"),
                       remove.columns = c("NET", "SUM", "Total"),
                       subset = NULL,
                       weights = NULL,
                       match.elements = "Yes - ignore if unmatched",
                       warn = FALSE)
{
    function.name <- sQuote(match.call()[[1]], q = FALSE)
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
                           by.row = FALSE,
                           warn = warn)
    else
        checkColumnDimensionsEqual(x, function.name = function.name)
    sum.function <- if (remove.missing) sumCols else sumColsIncludingNAs
    sum.output <- sumElements(x, sum.function)
    if (warn && is.nan(sum.output))
        checkForOppositeInfinites(x, function.name = function.name)
    # Keep rownames if they are consistent, otherwise discard
    sum.output
}

sumColsIncludingNAs <- function(x, y)
{
    sumCols(x, y, remove.missing = FALSE)
}

sumCols <- function(x, y, remove.missing = TRUE)
{
    x <- sumColsSingleInput(x, remove.missing = remove.missing)
    if (missing(y))
        return(x)
    y <- sumColsSingleInput(y, remove.missing = remove.missing)
    if (remove.missing)
    {
        if (any(missing.vals <- is.na(x)))
            x[missing.vals] <- 0
        if (any(missing.vals <- is.na(y)))
            y[missing.vals] <- 0
    }
    # Attributes stripped, otherwise the return element will have the attributes of x
    as.vector(x) + as.vector(y)
}

sumColsSingleInput <- function(x, remove.missing = TRUE)
{
    # 2D Table with Multiple statistics is stored as a 3d array
    # and handled as a special case here.
    if (isQTable(x) && length(dim(x)) > 2)
        sumWithin3Darray(x, summing.function = colSums, remove.missing = remove.missing)
    else if (NROW(x) == 1)
        x
    else
        colSums(x, na.rm = remove.missing)
}
