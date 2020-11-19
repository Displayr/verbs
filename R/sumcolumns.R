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
    x <- list(...)
    if (length(x) > 1)
        lapply(x, checkIfSuitableVectorType, function.name = function.name)
    x <- processArguments(...,
                          remove.missing = remove.missing,
                          function.name = function.name,
                          remove.rows = remove.rows,
                          remove.columns = remove.columns,
                          subset = subset,
                          weights = weights,
                          warn = warn)
    output.names <- lapply(x, getColumnNames)
    sum.output <- lapply(x, sumCols, remove.missing = remove.missing)
    sum.output <- joinOutputs(sum.output, output.names)
    if (warn && any(nan.outputs <- is.nan(sum.output)))
    {
        opposite.infinities <- vapply(x[nan.outputs])
    }

    sum.output
}

sumCols <- function(x, remove.missing = TRUE)
{
    # 2D Table with Multiple statistics is stored as a 3d array
    # and handled as a special case here.
    if (isQTable(x) && length(dim(x)) > 2)
        sumWithin3Darray(x, summing.function = colSums, remove.missing = remove.missing)
    else if (NCOL(x) == 1)
    {
        y <- sum(x, na.rm = remove.missing)
        names(y) <- getColumnNames(x)
        y
    } else if (NROW(x) == 1)
        x
    else
        colSums(x, na.rm = remove.missing)
}

getColumnNames <- function(x)
{
    if (getDim(x) == 1)
        attr(x, "label")
    else
        colNames(x)
}

listInputIncludesMatrices <- function(x)
{
    result <- FALSE
    for(i in seq_along(x))
        if (is.matrix(x[[i]]) || is.array(x[[i]]))
        {
            result <- TRUE
            break;
        }
    result
}

joinOutputs <- function(x, output.names)
{
    elements.are.matrices <- listInputIncludesMatrices(x)
    if (!elements.are.matrices)
    {
        x <- unlist(x)
        names(x) <- unlist(output.names)
    } else
        x <- do.call(cbind, x)
    x
}
