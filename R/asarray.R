#' Convert objects to arrays
#'
#' @param x Any object
#' @return The same object with all attributes removed except for dim and dimnames
#' @export
AsArray <- function(x)
{
    n.dims <- length(dim(x))
    if (n.dims < 5 && n.dims > 0) return(useBaseSubscript(x))
    all.attrs <- attributes(x)
    attrs.to.keep <- c("dim", "dimnames")
    attrs.to.remove <- setdiff(names(all.attrs), attrs.to.keep)

    attributes(x)[attrs.to.remove] <- NULL
    x
}

useBaseSubscript <- function(x) {
    y <- unclass(x)
    n.dims <- length(dim(x))
    switch(n.dims,
        y[TRUE],
        y[, ],
        y[, , ],
        y[, , , ],
        y[, , , , ]
    )
}