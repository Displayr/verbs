context("Transposing Q Tables")

tbls <- readRDS("qTablesWithZStatInCells.rds")
tbls.by.dim <- split(tbls, vapply(tbls, getDimensionLength, numeric(1L)))
higher.dim.tables <- tbls.by.dim[-(1:2)]
one.of.each <- lapply(higher.dim.tables, "[[", 1L)
matrices <- tbls.by.dim[[2]]
vectors <- tbls.by.dim[[1]]
makeFakeMultiStat1DTable <- function(tbl) {
    output <- cbind(`z-Statistic` = tbl,
                    Count = sample(20:50, size = NROW(tbl), replace = TRUE))
    output <- flipU::CopyAttributes(output, tbl)
    attr(output, "statistic") <- NULL
    class(output) <- c("qTable", class(output))
    attr(output, "span")[["columns"]] <- data.frame(c("z-Statistic", "Count"), check.names = FALSE)
    output
}

multi.stat.1d <- lapply(vectors, makeFakeMultiStat1DTable)

hasRowNETs <- function(x) any(rownames(x) %in% c("SUM", "NET"))
removeRowNETs <- function(x) {
    n.dim <- getDimensionLength(x)
    non.nets <- which(!rownames(x) %in% c("SUM", "NET"))
    args <- c(list(x), rep(alist(, )[1L], n.dim))
    args[[2L]] <- non.nets
    do.call("[", args)
}
subscripted.matrices <- lapply(Filter(hasRowNETs, matrices), removeRowNETs)
subscripted.vectors <- lapply(Filter(hasRowNETs, vectors), removeRowNETs)
subscripted.multi.stat.1d <- lapply(Filter(hasRowNETs, multi.stat.1d), removeRowNETs)

# Utility function for clobbering QTable attributes and keeping only a basic
# 1d or 2d matrix
removeTableAttr <- function(x) {
    x.attr <- attributes(x)
    x.attr <- x.attr[c("dim", "dimnames")]
    attributes(x) <- x.attr
    x
}

test_that("Transposing not possible on higher dim tables", {
    for (high.dim.tbl in one.of.each)
        expect_error(t(high.dim.tbl), "argument is not a matrix")
})

test_that("Transposing possible on matrices", {
    for (tbl in matrices)
        expect_error(t(tbl), NA)
})

test_that("Transposing matrices has correct values and structure", {
    convertToRowMatrix <- function(table) {
        x.dim <- dim(table)
        x.dimnames <- dimnames(table)
        array(as.vector(table), dim = x.dim,
              dimnames = x.dimnames)
    }
    checkMatrixTranspose <- function(qtable) {
        mat <- convertToRowMatrix(qtable)
        t.qtable <- t(qtable)
        expect_equal(attr(t.qtable, "is.transposed"), 1L)
        t.mat <- t(mat)
        t.elements <- removeTableAttr(t.qtable)
        expect_equal(t.elements, t.mat)
        # Check QStat Testing Info
        # The original column major ordering should  match the elements in the
        # transposed matrix elements
        q.stat <- attr(qtable, "QStatisticsTestingInfo")
        stats <- q.stat[["zstatistic"]]
        if (anyNA(stats)) # QStat info stores as NaN but matrix as NA
            stats[is.na(stats)] <- NA
        qtable.as.vector <- as.vector(t.mat)
        ind <- if (isMultiStatTable(qtable))
            seq(from = 1, to = length(qtable.as.vector), by = ncol(qtable))
        else
            seq_len(length(qtable))
        expected.out <- qtable.as.vector[ind]
        expect_equal(as.vector(stats), expected.out)
        # Check spans
        original.span <- attr(qtable, "span")
        t.span <- attr(t.qtable, "span")
        expect_equal(unname(t.span), rev(unname(original.span)))
        expect_equal(names(t.span), names(original.span))
        # Check attributes that should reverse
        attrs <- c("questions", "questiontypes", "dimnets", "dimduplicates")
        for (att in attrs)
            expect_equal(attr(t.qtable, att), rev(attr(qtable, att)))
        # Check name attributes
        expect_equal(attr(t.qtable, "name"), paste0("t(", attr(qtable, "name"), ")"))
        # Check mapped dimnames attr
        mapped.dimnames <- attr(qtable, "mapped.dimnames")
        if (!is.null(mapped.dimnames)) {
            t.mapped.dimnames <- attr(t.qtable, "mapped.dimnames")
            expected.mapped.dimnames <- setNames(rev(mapped.dimnames), names(mapped.dimnames))
            if ("Statistic" %in% names(mapped.dimnames)) {
                stat.in.cols <- names(mapped.dimnames)[2] == "Statistic"
                names(expected.mapped.dimnames) <- if (stat.in.cols) c("Statistic", "Column") else c("Row", "Statistic")
            }
            expect_equal(t.mapped.dimnames, expected.mapped.dimnames)
        }
        if ("Statistic" %in% names(mapped.dimnames)[[2]]) {
            q.stat <- attr(qtable, "QStatisticsTestingInfo")
            q.stat[names(q.stat) == "Row"] <- NULL
            attr(qtable, "QStatisticsTestingInfo") <- q.stat
        }
        attr(qtable, "is.transposed") <- 2L
        attr(qtable, "name") <- paste0("t(t(", attr(qtable, "name"), "))")
        expect_equal(t(t.qtable), qtable)
    }
    for (tbl in matrices)
        checkMatrixTranspose(tbl)
    for (tbl in subscripted.matrices)
        checkMatrixTranspose(tbl)
    for (tbl in multi.stat.1d)
        checkMatrixTranspose(tbl)
    for (tbl in subscripted.multi.stat.1d)
        checkMatrixTranspose(tbl)
})

test_that("Transposing vectors has correct values and structure", {
    convertTo1DRowMatrix <- function(x) {
        n <- length(x)
        x.names <- if (!is.null(names(x))) names(x) else dimnames(x)[[1L]]
        x.dimnames <- list(NULL, dimnames(x)[[1]])
        array(as.vector(x), dim = c(1L, n),
              dimnames = x.dimnames)
    }
    convertTo1DColMatrix <- function(table) {
        r.names <- dimnames(table)[[1L]]
        attr(table, "dim") <- c(length(table), 1L)
        attr(table, "dimnames") <- list(r.names, NULL)
        # Used to check transpose of transpose
        attr(table, "is.transposed") <- 2L
        span <- attr(table, "span")
        span[["columns"]] <- data.frame()
        attr(table, "span") <- span
        # Mapped dims should disappear
        attr(table, "mapped.dimnames") <- NULL
        q.stat <- attr(table, "QStatisticsTestingInfo")
        row.and.col.ind <- match(c("Row", "Column"), names(q.stat), nomatch = 0L)
        if (any(row.and.col.ind > 0L)) {
            row.and.col.ind <- row.and.col.ind[row.and.col.ind > 0L]
            q.stat[row.and.col.ind] <- NULL
        }
        attr(table, "name") <- paste0("t(t(", attr(table, "name"), "))")
        attr(table, "QStatisticsTestingInfo") <- q.stat
        table
    }
    checkVectorToRowMatrix <- function(qtable) {
        q.stat <- attr(qtable, "QStatisticsTestingInfo")
        mat <- convertTo1DRowMatrix(qtable)
        t.qtable <- t(qtable)
        t.qstat <- attr(t.qtable, "QStatisticsTestingInfo")
        # Used to check first transpose
        expect_equal(attr(t.qtable, "is.transposed"), 1L)
        t.elements <- removeTableAttr(t.qtable)
        expect_equal(t.elements, mat)
        # Span should swap
        original.span <- attr(qtable, "span")
        span <- attr(t.qtable, "span")
        expect_equal(original.span[[1]], span[[2]])
        expect_equal(span[[1]], data.frame())
        expect_equal(names(span), c("rows", "columns"))
        # mapped.dimnames should be removed as structure changed
        # Also check QStatisticsTestingInfo doesn't have the mapped dimnames
        if (!is.null(attr(qtable, "mapped.dimnames"))) {
            expect_null(attr(t.qtable, "mapped.dimnames"))
            expect_true("Row" %in% names(q.stat))
            expect_true(all(!c("Row", "Column") %in% names(t.qstat)))
            q.stat <- q.stat[!names(q.stat) %in% c("Row", "Column")]
        }
        # QStatisticsTestingInfo should remain unchanged
        expect_equal(t.qstat, q.stat)
        # Expect transpose operator almost invertible
        expect_equal(t(t.qtable), convertTo1DColMatrix(qtable))
    }
    for (tbl in vectors)
        checkVectorToRowMatrix(tbl)
    for (tbl in subscripted.vectors)
        checkVectorToRowMatrix(tbl)
})
