#' @title Percentage growth of a series
#' @description Computes the percentage growth from pairs of consecutive
#'   elements in a numeric vector or in columns of a numeric matrix,
#' @param x A numeric \code{vector}, \code{matrix}, \code{array}, \code{table}
#'   or \code{data frame}.
#' @return ?
Growth <- function(x)
{

    # vector
    if (is.atomic(x) && !is.array(x))
    {
        growthVector(x)
    }
    else # matrix, array, table, higher dimensions?
    {

    }
}

growthVector <- function(x)
{

}

