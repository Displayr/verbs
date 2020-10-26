#' General-Purpose Element-wise Summation
#'
#' A replacement for \code{\link{sum}} supporting element-wise
#' summation of many objects in addition to vectors.
#' @param ... Objects to be summed; e.g. Q Tables, arrays
#' @param ignore.missing Should missing values removed? Defaults to \code{TRUE},
#' differing from \code{na.rm} in \code{\link{sum}}.
#' @return The sum.
#' @export
#' @examples
#' Sum(1:3, 2:4)
Sum <- function(..., ignore.missing = TRUE)
    sum(..., na.rm = ignore.missing)
