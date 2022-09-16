#' @export
t.qTable <- function(object, ...)
{
    if (!is.matrix(object)) NextMethod(t, object)
    object.attrs <- attributes(object)
    attr.to.reverse <- c("questiontypes", "questions", "dimnets", "dimduplicates")
    object.attrs[c("dim", "dimnames")] <- NULL
    for (attr in names(object.attrs))
    {
        if (attr %in% attr.to.reverse) {
            object.attrs[[attr]] <- rev(object.attrs[[attr]])
            next
        }
        if (attr == "QStatisticsTestingInfo") {
            object.attrs[[attr]] <- transposeQStatInfo(object)
            next
        }
        if (attr == "span") {
            object.attrs[[attr]] <- transposeSpan(object.attrs[[attr]])
            next
        }
        if (attr == "mapped.dimnames") {
            object.attrs[[attr]] <- transposeMappedDimnames(object.attrs[[attr]])
            if (is.null(object.attrs[[attr]])) {
                q.stat <- object.attrs[["QStatisticsTestingInfo"]]
                q.stat <- removeMappedDimnames(object.attrs[["QStatisticsTestingInfo"]])
                object.attrs[["QStatisticsTestingInfo"]] <- q.stat
            }
            next
        }
    }
    output <- NextMethod(t, object)
    object.attrs[["dim"]] <- dim(output)
    object.attrs[["dimnames"]] <- dimnames(output)
    mostattributes(output) <- object.attrs
    is.transposed <- attr(output, "is.transposed")
    attr(output, "is.transposed") <- incrementTransposeAttr(is.transposed)
    output
}

transposeQStatInfo <- function(object) {
    x.dim <- dim(object)
    q.stat <- attr(object, "QStatisticsTestingInfo")
    if (length(x.dim) == 2L && !any(x.dim == 1L)) {
        new.ind <- col(object) + (row(object) - 1L) * ncol(object)
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
