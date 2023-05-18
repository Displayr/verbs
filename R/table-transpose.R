#' @export
t.qTable <- function(x)
{
    if (!is.matrix(x)) NextMethod(t, x)
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
    }
    output <- NextMethod(t, x)
    x.attrs[["dim"]] <- dim(output)
    x.attrs[["dimnames"]] <- dimnames(output)
    mostattributes(output) <- x.attrs
    is.transposed <- attr(output, "is.transposed")
    attr(output, "is.transposed") <- incrementTransposeAttr(is.transposed)
    output
}

transposeQStatInfo <- function(x) {
    x.dim <- dim(x)
    q.stat <- attr(x, "QStatisticsTestingInfo")
    if (length(x.dim) == 2L && isMultiStatTable(x))
        return(reconcileDimnamesInQStatInfo(q.stat))
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
    mapped.dimnames <- rev(mapped.dimnames)
    names(mapped.dimnames) <- c("Row", "Column")
    mapped.dimnames
}

incrementTransposeAttr <- function(x) {
    if (is.null(x)) return(1L)
    x + 1L
}
