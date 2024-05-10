#' @export
AsArray <- function(x)
{
    all_attrs <- attributes(x)
    attrs_to_keep <- c("dim", "dimnames")
    attrs_to_remove <- setdiff(names(all_attrs), attrs_to_keep)

    x_cleaned <- x
    for (attr_name in attrs_to_remove) {
        attributes(x_cleaned)[attr_name] <- NULL
    }
    x_cleaned
}

