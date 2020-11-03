checkType <- function(x, function.name)
{
    if(is.character(x))
        stop("'", function.name, "' requires numeric data.")
    # This function is just a rough place holder for something more rigorous
    if(is.numeric(x))
        return(NULL)

    # Include support contact method or something (e.g., email address) in message, using
    #        an open source one by default and our customer support one if we know they are
    #        using it from within Displayr.
    if (length(dim(x)) > 2)
        stop("'", function.name, "' only works for 1 and 2 dimensional tables. Contact support if
             you want this to be changed")
}

checkTypes <- function(..., function.name)
{
    lst <- list(...)
    for (i in length(lst))
        checkType(lst[[i]], function.name)
}


checkDifferentStatistics <- function(x, function.name, warn)
{
    # This test should only be run on a table, to check that it
    # doesnot, say, contain both a mean and a median, and is attempting
    # to sum these.
    if (!warn)
        return(NULL)
    possible.stats <- possibleStatistics(x)
    if (is.null(possible.stats) || length(possible.stats) <= 1)
        return(NULL)
    known.statistics <- tolower(possible.stats) %in% statisticNames()
    if (any(known.statistics))
        warning("The input data may contains statistics of different types ",
        "(i.e., ",
        paste(possible.stats[known.statistics], collapse = ", "),
        "), it may not be appropriate to compute their '",
        function.name, "'.")
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
    if (NCOL(x) == 1)
        return(NULL)
    # This call to tail should be replaced by Last
    tail(dimnames(x), 1)[[1]]
}


checkForOppositeInfinites <- function(x, warn)
{
    # Note that this function really needs to be applying across the entirety of everything being summed.
    # As this is pretty rate, I think that the optimal way to do it is likely to be to only do it at
    # the end of the function if the answer is NA.
    if(warn && all(c(Inf, -Inf) %in% x))
        warning("Sum cannot be computed as the data contains both Inf and -Inf.")
}

removeRows <- function(x, remove.rows, warn)
{
    # This is a bad check to see if it's a table. There should be something
    # much smarter/better.
    if (is.numeric(x) & !is.matrix(x) & is.null(names(x)))
        return(x)
    keep <- rowsToKeep(x, remove.rows, warn)
    if (is1D(x))
        return(x[keep])
    x[keep, ]
}


rowsToKeep <- function(x, remove.rows, warn)
{
    row.names <- rowNames(x)
    remove <- entriesToRemove(row.names, remove.rows)
    removeWarning("rows", row.names[remove], warn)
    !remove
}

removeWarning <- function(index.name, removed.categories, warn)
{
    if (warn)
        warning("These categories have been removed from the ", index.name,
                ": ",
                paste(removed.categories, collapse = ", "),
                ".")
}

entriesToRemove  <- function(categories, remove.categories)
{
    categories %in% remove.categories
}

rowNames <- function(x)
{
    if(is1D(x))
        return(names(x))
    rownames(x)
}


is1D <- function(x)
{
    # Place holder for something a bit smarter
    if (is.numeric(x) & !is.matrix(x))
        return(TRUE)
    if(!is.null(dim(x)) && length(dim(x)) == 1)
        return(TRUE)
    is.vector(x) & !is.list(x)
}
