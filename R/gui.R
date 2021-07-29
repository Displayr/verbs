parseCategoriesToRemove <- function(exclude.categories, all.dimnames)
{
    # Handle edge case where user wants to remove one label and it contains a comma
    trimmed.input <- trimws(exclude.categories)
    if (trimmed.input %in% all.dimnames)
        return(trimmed.input)
    # Otherwise split the string using either ; or , depending on the input
    sep <- ifelse(grepl(";", exclude.categories), ";", ",")
    trimws(strsplit(exclude.categories, sep)[[1L]])
}

#' @title Parse Categories to Remove in GUI textboxes
#' @param input.str A list containing two characters with the desired categories
#'   in each dimension to be removed. The first element for the rows, the second for
#'   the columns. The expected format is either a comma separated string or ;
#'   separated string for each dimension.
#' @param inputs A list containing all the inputs that have rows and columns that
#'   are potentially removed.
#' @return A list with two character vectors that contain the category names of
#'   all row/column slices that are to be removed.
#' @export
ParseCategoriesToRemove <- function(input.str, inputs)
{
    stopifnot("List of length two required" =  length(input.str) == 2 && is.list(input.str),
              "Strings expected in each list element" = all(vapply(input.str, is.character, logical(1L))))
    inputs <- checkInputsAtMost2DOrQTable(inputs)
    all.row.names <- unique(unlist(lapply(inputs, rowNames)))
    all.col.names <- unique(unlist(lapply(inputs, colNames)))
    all.dimnames <- list(all.row.names, all.col.names)
    categories.to.remove <- mapply(parseCategoriesToRemove,
                                   input.str, all.dimnames,
                                   SIMPLIFY = FALSE)
    has.row.spans <- any(vapply(inputs, checkSpans, logical(1L), dimension = 1L))
    has.col.spans <- any(vapply(inputs, checkSpans, logical(1L), dimension = 2L))
    spans.exist <- list(has.row.spans, has.col.spans)
    WarnIfUserSelectionHasNoMatch(categories.to.remove, all.dimnames, spans.exist)
}

checkSpans <- function(x, dimension)
{
    isQTable(x) && attr(x, if (dimension == 1L) "has.row.spans" else "has.col.spans")
}

warnIfUserSelectionHasNoMatch <- function(parsed.string, dimnames, has.spans, dimension.name)
{
    defaults <- c("NET", "SUM")
    unmatched.elements <- !parsed.string %in% dimnames & !parsed.string %in% defaults
    defaults.chosen <- setequal(parsed.string, defaults)
    ## Don't warn if user hasn't changed from defaults, unless table has been flattened
    if (any(unmatched.elements) && ((has.spans && !defaults.chosen) || !defaults.chosen))
    {
        bad.labels <- paste0("'", paste(parsed.string[unmatched.elements], collapse = "', '"), "'")
        msg <- ngettext(sum(unmatched.elements),
                        paste0("The following ", dimension.name, " label requested to be excluded was ",
                               "not found in the input data: ", bad.labels, "."),
                        paste0("The following ", dimension.name, " labels specified to be excluded were ",
                               "not found in the input data: ", bad.labels, "."))
        good.labels <- dimnames[1:min(3, length(dimnames))]
        msg <- paste0(msg, " Please supply labels such as '", paste(good.labels, collapse = "', '"), "'.")
        warning(msg)
    }
}

WarnIfUserSelectionHasNoMatch <- function(parsed.strings, all.dimnames, has.spans)
{
    mapply(warnIfUserSelectionHasNoMatch,
           parsed.strings,
           all.dimnames,
           has.spans,
           list("row", "column"))
}
