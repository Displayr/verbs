parseCategoriesToRemove <- function(exclude.categories, all.dimnames)
{
    if (!nzchar(exclude.categories))
        return(NULL)
    # Handle edge case where user wants to remove one label and it contains a comma
    trimmed.input <- trimws(exclude.categories)
    if (length(trimmed.input) == 1L && trimmed.input %in% all.dimnames)
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
    categories.to.remove
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

warnUnmatchedCategoricalLabels <- function(unmatched, delim)
{
    max.displayed <- 3L
    unmatched <- Filter(length, unmatched)
    quoted <- lapply(unmatched,
                     function(x) {
                         y <- trimws(x[1:min(max.displayed, length(x))])
                         y <- paste0(y, collapse = delim)
                         if (length(x) > max.displayed)
                             y <- paste0(y, delim, "...")
                         sQuote(y)
                     })
    n.user.incorrect <- length(unmatched[["user"]])
    msg <- paste0(ngettext(n.user.incorrect, "The label", "The labels"),
                  " entered in the CATEGORIES TO COUNT section ",
                  ngettext(n.user.incorrect, "was", "were"),
                  " not found in the input data: ", quoted[["user"]], ".")
    prefix <- NULL
    if (length(unmatched[["levels"]]))
    {
        msg <- paste0(msg, " Possible labels include ", quoted[["levels"]], ".")
        delims <- c(";", ",")
        delims.found <- lapply(delims, grepl, x = unmatched[["levels"]])
        delims.observed <- vapply(delims.found, any, logical(1L))
        if (any(delims.observed))
        {
            delims <- sQuote(delims[delims.observed])
            symbol  <- ngettext(length(symbols), "symbol ", "symbols ")
            prefix <- paste0("It is not possible to unambiguously determine which CATEGORIES TO COUNT while ",
                             "the labels contain the ", symbol, delims, ". ",
                             "Remove the symbol from the labels in your data and update the entry in the ",
                             "CATEGORIES TO COUNT section. ")
        }
    }
    warning(prefix, msg)
}

#' @title Parsing Categorical Labels to Count
#' @description Checks a delimited string contains the levels of factors contained in a data.frame. If there
#'    are unmatched elements in the string, then a warning is thrown advising the user of the inability to match
#' @param concatted.labels A single chracter element containing the delimited cagetories to count
#'    e.g. "foo, bar, baz" or "foo; bar; baz"
#' @param input Either a \code{data.frame} containing factors or a factor or numeric object.
#'    If any other input is given, then NULL is returned.
#'    \code{concatted.labels}.
#' @return A delimited string containing only the categorical labels that exist in the supplied \code{data.frame}
#' @export
ParseCategoricalLabels <- function(concatted.labels, input)
{
    all.labels <- if (is.data.frame(input)) Reduce(union, lapply(input, levels)) else levels(input)
    if (is.null(all.labels))
        return(NULL)
    delims <- c(",", ";")
    split.labels <- lapply(delims, function(x) trimws(strsplit(concatted.labels, split = x)[[1L]]))
    split.labels <- lapply(split.labels, function(x) Filter(nzchar, x))
    matches <- lapply(split.labels, function(x) x %in% all.labels)
    all.match <- vapply(matches, all, logical(1L))
    if (any(all.match))
        return(split.labels[[which.max(all.match)]])
    # Pick the strategy that has the best matching
    n.matches <- vapply(matches, sum, integer(1L))
    best.strat <- which.max(n.matches)
    split.labels <- split.labels[[best.strat]]
    parsed.labels <- split.labels[matches[[best.strat]]]
    levels.unmatched <- all.labels[!all.labels %in% parsed.labels]
    user.unmatched <- split.labels[!matches[[best.strat]]]
    warnUnmatchedCategoricalLabels(list(levels = levels.unmatched, user = user.unmatched),
                                   delim = paste0(delims[best.strat], " "))
    return(parsed.labels)
}
