#' @export
t.QTable <- function(x)
{
    # Defer to base method to throw errors for unsupported types
    if (!is.matrix(x) && getDimensionLength(x) > 2L) NextMethod(t, x)
    x.attrs <- attributes(x)
    attr.to.reverse <- c("questiontypes", "questions", "dimnets", "dimduplicates")
    x.attrs[c("dim", "dimnames")] <- NULL
    for (attr in names(x.attrs))
    {
        if (attr %in% attr.to.reverse) {
            x.attrs[[attr]] <- rev(x.attrs[[attr]])
            next
        }
        if (attr == "QStatisticsTestingInfo") {
            x.attrs[[attr]] <- transposeQStatInfo(x)
            next
        }
        if (attr == "span") {
            x.attrs[[attr]] <- transposeSpan(x.attrs[[attr]])
            next
        }
        if (attr == "mapped.dimnames") {
            x.attrs[[attr]] <- transposeMappedDimnames(x.attrs[[attr]])
            if (is.null(x.attrs[[attr]])) {
                q.stat <- x.attrs[["QStatisticsTestingInfo"]]
                q.stat <- removeMappedDimnames(x.attrs[["QStatisticsTestingInfo"]])
                x.attrs[["QStatisticsTestingInfo"]] <- q.stat
            }
            next
        }
        if (attr == "celltext") {
            x.attrs[[attr]] <- transposeCellText(x.attrs[[attr]])
        }
    }
    output <- NextMethod(t, x)
    x.attrs[["dim"]] <- dim(output)
    x.attrs[["dimnames"]] <- dimnames(output)
    mostattributes(output) <- x.attrs
    is.transposed <- attr(output, "is.transposed")
    attr(output, "is.transposed") <- incrementTransposeAttr(is.transposed)
    # Update name so the timing of the transpose is clear
    attr(output, "name") <- paste0("t(", attr(output, "name"), ")")
    output
}

transposeQStatInfo <- function(x) {
    x.dim <- dim(x)
    q.stat <- attr(x, "QStatisticsTestingInfo")
    if (length(x.dim) == 2L && !any(x.dim == 1L) && !isMultiStatTable(x)) {
        new.ind <- col(x) + (row(x) - 1L) * ncol(x)
        q.stat <- q.stat[as.vector(new.ind), , drop = FALSE]
    }
    reconcileDimnamesInQStatInfo(q.stat)
}

reconcileDimnamesInQStatInfo <- function(q.stat) {
    row.and.col.ind <- match(c("Row", "Column"), names(q.stat), nomatch = 0L)
    dims.found <- row.and.col.ind > 0L
    names(dims.found) <- c("Row", "Column")
    if (all(dims.found))
    {
        row.and.col.df <- q.stat[row.and.col.ind]
        row.and.col.df <- setNames(rev(row.and.col.df), c("Row", "Column"))
        q.stat <- cbind.data.frame(row.and.col.df, q.stat[-row.and.col.ind])
        return(q.stat)
    }
    if (any(dims.found)) return(q.stat[-row.and.col.ind])
    q.stat
}

removeMappedDimnames <- function(q.stat) {
    q.stat[names(q.stat) %in% c("Row", "Column")] <- NULL
    q.stat
}

transposeSpan <- function(span) {
    if (length(span) == 2L) {
        span <- rev(span)
        names(span) <- c("rows", "columns")
        return(span)
    }
    list(rows = data.frame(), columns = span[[1L]])
}

transposeMappedDimnames <- function(mapped.dimnames) {
    if (length(mapped.dimnames) == 1L) return(NULL)
    original.dimnames <- names(mapped.dimnames)
    mapped.dimnames <- rev(mapped.dimnames)
    names(mapped.dimnames) <- c("Row", "Column")
    if (any(stat.dim <- original.dimnames == "Statistic")) {
        names(mapped.dimnames)[(2:1)[stat.dim]] <- "Statistic"
    }
    mapped.dimnames
}

transposeCellText <- function(cell.text) {
    if (!is.array(cell.text)) {
        return(NULL)
    }

    d <- dim(cell.text)
    n.dim <- length(d)
    if (n.dim == 2 && d[1] == 1) {
        # t(cell.text) would result in a 2D array, but we want a 1D array
        cell.text <- array(cell.text)
    } else if (n.dim < 3) {
        cell.text <- t(cell.text)
    } else {
        cell.text <- aperm(cell.text, c(2, 1, 3:n.dim))
    }
    cell.text
}

incrementTransposeAttr <- function(x) {
    if (is.null(x)) return(1L)
    x + 1L
}
