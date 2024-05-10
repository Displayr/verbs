#' @export
t.QTable <- function(x)
{
    if (checkIsQTable(x)) {
        all_attrs <- attributes(x)
        attrs_to_keep <- c("dim", "dimnames")
        attrs_to_remove <- setdiff(names(all_attrs), attrs_to_keep)

        x_cleaned <- x
        for (attr_name in attrs_to_remove) {
            attributes(x_cleaned)[attr_name] <- NULL
        }
        x_cleaned
    } else {
        stop('Input must be a QTable object')
    }
}

checkIsQTable <- function(x) {
    # Check its classes
    class_x <- class(x)
    is_qtable <- "QTable" %in% class_x
    valid_classes_only <- length(setdiff(class_x, c("QTable", "matrix", "array"))) == 0

    is_qtable && valid_classes_only
}

