context("Subscripting QTables")

arrayAsTable <- function(dims, dimnames = NULL) {
    if (missing(dims))
        stop("dims argument required")
    output <- array(sample(1:100, size = prod(dims), replace = TRUE), dim = dims, dimnames = dimnames)
    class(output) <- c("QTable", class(output))
    attr(output, "statistic") <- "Average"
    attr(output, "name") <- paste0("table.", paste0(dims, collapse = "."))
    output
}

makeMultistat <- function(tbl) {
    tbl.ms <- array(0, dim = c(dim(tbl)))
    library(abind)
    tbl.ms <- abind(tbl, tbl.ms, along = length(dim(tbl)) + 1)
    tbl.ms <- CopyAttributes(tbl.ms, tbl, attr.to.not.copy = c("dimnames", "dim", "statistic"))
    dimnames(tbl.ms) <- c(dimnames(tbl), list(c("z-Statistic", "Average")))
    attr(tbl.ms, "statistic") <- NULL
    tbl.ms
}

set.seed(12321)
# Very simple arrays
x.1 <- arrayAsTable(1)
x.1.named <- arrayAsTable(1, list("Foo"))

## All names
randomName <- function(n = 5L) {
    paste0(sample(c(LETTERS, letters), size = 5L, replace = TRUE), collapse = "")
}

random.names <- lapply(6:2, function(x) replicate(x, randomName()))
indexed.random.names <- setNames(random.names, 6:2)
if (anyDuplicated(unlist(random.names))) stop("array names should be unique")
array.names <- random.names
# Typical 1D Array
x.6 <- arrayAsTable(6)
x.6.named <- arrayAsTable(6, array.names[1L])
# Typical 2D array
x.6.5 <- arrayAsTable(6:5)
x.6.5.named <- arrayAsTable(6:5, array.names[1:2])
x.6.5.4 <- arrayAsTable(6:4)
x.6.5.4.named <- arrayAsTable(6:4, array.names[1:3])
x.6.5.4.3 <- arrayAsTable(6:3)
x.6.5.4.3.named <- arrayAsTable(6:3, array.names[1:4])
x.6.5.4.3.2 <- arrayAsTable(6:2)
x.6.5.4.3.2.named <- arrayAsTable(6:2, array.names[1:5])

tables.wont.drop <- list(x.1 = x.1, x.1.named = x.1.named,
                         x.6 = x.6, x.6.named = x.6.named,
                         x.6.5 = x.6.5, x.6.5.named = x.6.5.named,
                         x.6.5.4 = x.6.5.4, x.6.5.4.named = x.6.5.4.named,
                         x.6.5.4.3 = x.6.5.4.3, x.6.5.4.3.named = x.6.5.4.3.named,
                         x.6.5.4.3.2 = x.6.5.4.3.2, x.6.5.4.3.2.named = x.6.5.4.3.2.named)

subsettable.tables <- tables.wont.drop[!startsWith(names(tables.wont.drop), "x.1")]

test_that("Empty indices handled appropriately", {
    for (input in tables.wont.drop) {
        # Valid for [ but throw an error for [[
        expect_equal(input[], input)
        expected.double.err <-
           capture_error(throwErrorEmptyDoubleIndex(attr(input, "name"), dim(input)))[["message"]]
        expect_error(input[[]], expected.double.err, fixed = TRUE)
        for (drop.arg in c(TRUE, FALSE))
            expect_equal(input[drop = drop.arg], input)
    }
})

singleSubscriptTable <- function(tab, ind, drop = NULL) {
    args <- c(list(tab), ind)
    if (!is.null(drop)) args <- c(args, drop = drop)
    y <- do.call(`[`, args)
    attr(y, "mapped.dimnames") <- NULL
    y
}
expectedSingleTable <- function(tab, ind, drop = NULL) {
    y <- singleSubscriptTable(unclass(tab), ind, drop)
    class(y) <- c("QTable", class(y))
    attr(y, "statistic") <- "Average"
    attr(y, "name") <- paste0("table.", paste0(dim(tab), collapse = "."))
    if (!is.array(y) && length(y) > 1L) {
        y <- as.array(y)
    }
    attr(y, "is.subscripted") <- !identical(dim(y), dim(tab)) ||
        !identical(dimnames(y), dimnames(tab)) ||
        !identical(as.vector(y), as.vector(tab))
    y
}
doubleSubscriptTable <- function(tab, ind, exact = NULL) {
    args <- c(list(tab), ind)
    if (!is.null(exact)) args <- c(args, exact = exact)
    y <- do.call(`[[`, args)
    attr(y, "mapped.dimnames") <- NULL
    y
}
expectedDoubleTable <- function(tab, ind, exact = NULL) {
    y <- doubleSubscriptTable(unclass(tab), ind, exact)
    if (!inherits(y, "QTable"))
        class(y) <- c("QTable", class(y))
    attr(y, "name") <- paste0("table.", paste0(dim(tab), collapse = "."))
    attr(y, "statistic") <- "Average"
    attr(y, "is.subscripted") <- !identical(dim(y), dim(tab)) ||
        !identical(dimnames(y), dimnames(tab)) ||
        !identical(as.vector(y), as.vector(tab))
    y
}

index.template <- rep(alist(, )[1L], 5L)

test_that("Check indices subscripted correctly", {
    # Brute force function to evaluate the arguments with the correct indices and use ... for drop
    arg.template <- replicate(5L, NULL, simplify = FALSE)
    n.possible <- 6:2
    n.selected <- n.possible %/% 2
    randomIndex <- function(n.possible, size) sample.int(n.possible, size = size)
    randomLetters <- function(n.possible, size) {
        indexed.random.names[[as.character(n.possible)]][randomIndex(n.possible, size)]
    }

    for (tab in subsettable.tables) {
        n.dim <- length(dim(tab))
        n <- n.possible[1:n.dim]
        selected <- n.selected[1:n.dim]
        args <- mapply(randomIndex, n, selected, SIMPLIFY = FALSE)
        s.args <- args
        # Single [ tests, these are always valid
        for (drop in list(TRUE, FALSE, NULL)) {
            drop.null <- is.null(drop)
            if (drop.null) {
                test.table <- singleSubscriptTable(tab, s.args)
                expected <- expectedSingleTable(tab, s.args)
            } else {
                test.table <- singleSubscriptTable(tab, s.args, drop = drop)
                expected <- expectedSingleTable(tab, s.args, drop = drop)
            }
            expect_equal(test.table, expected)
            if (!is.null(dimnames(tab))) {
                s.named.args <- mapply(randomLetters, n, selected, SIMPLIFY = FALSE)
                if (drop.null) {
                    test.table <- singleSubscriptTable(tab, s.named.args)
                    expected <- expectedSingleTable(tab, s.named.args)
                } else {
                    test.table <- singleSubscriptTable(tab, s.named.args, drop = drop)
                    expected <- expectedSingleTable(tab, s.named.args, drop = drop)
                }
                expect_equal(test.table, expected)
            }
        }
        # Double [[ tests, these are only valid if single ind provided on each dim
        d.args <- lapply(args, sample, size = 1L)
        valid.d.args <- lapply(d.args, "[[", 1L)
        test.table <- doubleSubscriptTable(tab, valid.d.args)
        expected <- expectedDoubleTable(tab, valid.d.args)
        expect_equal(test.table, expected)
        # Expect error if [[ used with more than one value in a dimension
        expected.double.err <-
            capture_error(throwErrorTableDoubleIndex(attr(tab, "name"), dim(tab)))[["message"]]
        expect_error(doubleSubscriptTable(tab, s.args), expected.double.err, fixed = TRUE)
        if (!is.null(dimnames(tab))) {
            named.args <- mapply(randomLetters, n, selected, SIMPLIFY = FALSE)
            valid.named <- lapply(named.args, "[[", 1L)
            shorter.named <- lapply(valid.named, substr, 1, 4)
            expected <- expectedDoubleTable(tab,
                                            shorter.named,
                                            exact = FALSE)
            attr(expected, "name") <- sub(paste0(shorter.named, collapse = ","),
                                          paste0(valid.named, collapse = ","),
                                          attr(expected, "name"), fixed = TRUE)
            expect_equal(doubleSubscriptTable(tab, valid.named),
                         expected)
            expect_equal(doubleSubscriptTable(tab, valid.named),
                         expectedDoubleTable(tab, valid.named))
            expect_error(doubleSubscriptTable(tab, shorter.named, exact = TRUE),
                         "subscript out of bounds")
        }
    }
})

test_that("Check entire dimension works when index is empty", {
    randomIndex <- function(n.possible, size) sample.int(n.possible, size = size)
    randomPartialLetters <- function(n.possible, size) {
        ind.size <- (6:2)[n.possible]
        indexed.random.names[[as.character(ind.size)]][randomIndex(ind.size, length(size))]
    }
    for (tab in subsettable.tables) {
        dims <- dim(tab)
        n.dim <- length(dims)
        if (n.dim == 1) # Single dim array not relevant here
            next
        args <- NULL
        index.args <- index.template[1:n.dim]
        ind.selected <- sort(sample.int(n.dim, size = length(index.args) %/% 2))
        inds.chosen <- lapply(ind.selected, function(x) sample.int(dims[x], size = dims[x] %/% 2))
        index.args[ind.selected] <- inds.chosen
        args <- index.args
        test.table <- singleSubscriptTable(tab, args)
        expected <- expectedSingleTable(tab, args)
        expect_equal(test.table, expected)
        if (!is.null(dimnames(tab))) {
            named.args <- index.args
            named.args[ind.selected] <- mapply(randomPartialLetters, ind.selected, inds.chosen, SIMPLIFY = FALSE)
            test.table <- singleSubscriptTable(tab, named.args)
            expected <- expectedSingleTable(tab, named.args)
            expect_equal(test.table, expected)
        }
    }
})

test_that("Informative message when user provides incorrect arguments", {
    dim.names <- makeNumericDimNames(dim(x.6.5.4))
    dimnames(x.6.5.4) <- dim.names
    expect_error(x.6.5.4[1, 2],
                 capture_error(throwErrorTableIndexInvalid(attr(x.6.5.4, "name"),
                                                           6:4, dim.names))[["message"]],
                 fixed = TRUE)
    expect_error(x.6.5.4[1, 2, 3, 1],
                 capture_error(throwErrorTableIndexInvalid(attr(x.6.5.4, "name"),
                                                           6:4, dim.names))[["message"]],
                 fixed = TRUE)
    expect_error(x.6.5.4[1, 2, 3], NA)
    expect_error(x.6.5.4[[1, 2]],
                 capture_error(throwErrorTableDoubleIndex(attr(x.6.5.4, "name"), 6:4))[["message"]],
                 fixed = TRUE)
    expect_error(x.6.5.4[["A", "B"]],
                 capture_error(throwErrorTableDoubleIndex(attr(x.6.5.4, "name"), 6:4))[["message"]],
                 fixed = TRUE)
    # DS-3850 Additions
    expect_error(x.6[[1, 2]],
                 capture_error(throwErrorTableDoubleIndex(attr(x.6, "name"), 6))[["message"]],
                 fixed = TRUE)
    expect_error(x.6[[c(1, 2)]],
                 capture_error(throwErrorTableDoubleIndex(attr(x.6, "name"), 6))[["message"]],
                 fixed = TRUE)
    nms <- names(x.6.named)
    expect_error(x.6.named[[nms[1], nms[2]]], capture_error(x.6[[1, 2]])[["message"]], fixed = TRUE)
    expect_error(x.6.named[[c(nms[1], nms[2])]], capture_error(x.6[[1:2]])[["message"]], fixed = TRUE)

})

test_that("drop and exact recognised and used appropriately", {
    # Redundant arrays (can drop)
    x.2.1 <- arrayAsTable(2:1)
    x.2.1 <- arrayAsTable(2:1, dimnames = list(LETTERS[1:2], "a"))

    expected.error <- capture_error(throwErrorOnlyNamed("drop", "["))[["message"]]
    expect_error(x.1[Drop = FALSE], expected.error, fixed = TRUE)
    expect_error(x.1[drop = "TRUE"], "drop argument should be TRUE or FALSE")
    for (arg in c(TRUE, FALSE))
        expect_equal(x.1[drop = arg], x.1)
    expect_equal(x.2.1[drop = FALSE], x.2.1)
    expect_equal(x.2.1[drop = TRUE], x.2.1)

    expected <- unclass(x.2.1)[, 1, drop = FALSE]
    attr(expected, "statistic") <- "Average"
    class(expected) <- c("QTable", class(expected))
    attr(expected, "name") <- "table.2.1"
    dimnames(expected) <- unname(dimnames(expected))
    attr(expected, "mapped.dimnames") <- list(Row = LETTERS[1:2], Column = "a")
    attr(expected, "is.subscripted") <- FALSE
    expect_equal(x.2.1[, 1, drop = FALSE], expected)

    # Dropped output has the right class
    attr(x.2.1, "statistic") <- "Average"
    x.2.1.dropped <- structure(
        as.vector(x.2.1),
        class = c("QTable", "integer"),
        statistic = "Average",
        name = "table.2.1",
        names = LETTERS[1:2],
        statistic = "Average",
        is.subscripted = TRUE
    )

    expect_equal(x.2.1[, 1, drop = TRUE], x.2.1.dropped)

    expected.error <- capture_error(throwErrorOnlyNamed("exact", "[["))[["message"]]
    expect_error(x.6.5.named[[2, 3, Exact = TRUE]], expected.error, fixed = TRUE)
    expect_error(x.6.5.named[[2, 3, exact = "TRUE"]], "exact argument should be TRUE or FALSE")
    ## DS-3850 additions
    nms <- dimnames(x.6.5.named)
    short.nms <- lapply(nms, substr, start = 0, stop = 2)
    integer.subscript <- x.6.5.named[[2, 3]]
    character.subscript <- x.6.5.named[[nms[[1]][2], nms[[2]][3]]]
    attr(integer.subscript, "name") <- attr(character.subscript, "name") <- NULL
    expect_error(x.6.5.named[[short.nms[[1]][2], short.nms[[2]][3], exact = TRUE]])
    expect_equal(integer.subscript, character.subscript)
})

test_that("Array structure is retained", {
    y <- x.6.5[1][1]
    expect_true(is.numeric(y) && !is.array(y))
})

tbls <- readRDS("qTablesWithZStatInCells.rds")

checkAttributesMatch <- function(x, y, attr.to.ignore = c("name", "names", "dimnames", "mapped.dimnames"))
{
    for (attr in attr.to.ignore)
        attr(x, attr) <- attr(y, attr) <- NULL
    testthat::expect_equal(attributes(x), attributes(y))
}

test_that("DS-3810, DS-3809: Subset QStatisticsTestingInfo for single index",
{
    set.seed(345)

    for (tbl in tbls)
    {
        ## pick a unique z-stat value from middle of table to test
        tbl.vec <- as.vector(tbl)
        unique.vals <- tbl.vec[!duplicated(tbl.vec) & !duplicated(rev(tbl.vec))]
        chosen.val <- unique.vals[length(unique.vals)]
        arr.idx <- drop(which(tbl == chosen.val, arr.ind = TRUE, useNames = FALSE))
        if (!is.null(dim(arr.idx)) || NROW(arr.idx) == 0)  # every value in tbl appears more than once
        { #  pick randomly from center
            dims <- dim(tbl)
            arr.idx <- sapply(dims, function(dim.len)
                {
                    if (dim.len == 1)
                        return(1)
                    sample(2:dim.len, 1)
                })
        }
        out <- do.call(`[`, c(list(tbl), as.list(arr.idx)))
        attr.zstat <- attr(out, "QStatisticsTestingInfo")[, "zstatistic"]
        expect_equal(as.numeric(out), attr.zstat, check.attributes = FALSE)
        out.d <- do.call(`[[`, c(list(tbl), as.list(arr.idx)))
        expect_equal(as.numeric(out.d), attr.zstat, check.attributes = FALSE)
        checkAttributesMatch(out, out.d)
        label.idx <- mapply(`[`, dimnames(tbl), arr.idx)
        out <- do.call(`[`, c(list(tbl), as.list(label.idx)))
        expect_equal(as.numeric(out), attr.zstat, check.attributes = FALSE)
        out.d <- do.call(`[[`, c(list(tbl), as.list(label.idx)))
        expect_equal(as.numeric(out.d), attr.zstat, check.attributes = FALSE)
        checkAttributesMatch(out, out.d)
    }
})

## TODO: 1D Tables (char., int, logical idx),
## 4D tables (multi x multi, grid x multi, multi x grid, grid x grid;
## 3D output, 2D output, 1D output),
## 5D Tables, 2D multistat tables,
## int vector subset, D-column matrix subset, logical array subset,

#####################################################################
## 1D tables
set.seed(23)
dim.lens.avail <- vapply(lapply(tbls, dim), length, 1L)
test.cases <- names(tbls)[dim.lens.avail == 1L]
for (test.case in test.cases)
{
    tbl <- tbls[[test.case]]
    start.idx <- sample(length(tbl), 1)
    end.idx <- sample(seq_along(tbl)[-start.idx], 1)
    test.name <- paste0("DS-3809: subvectors of 1D qTables: ",
                        test.case, "[", start.idx, ":", end.idx, "]")
    test_that(test.name,
              {
                  idx <- start.idx:end.idx
                  q.stat.info.out <- attr(tbl[idx], "QStatisticsTestingInfo")
                  z.stat.out <- q.stat.info.out[, "zstatistic"]
                  expected <- unclass(tbl)[idx]
                  expect_equal(z.stat.out, expected, check.attributes = FALSE)
                  char.idx <- names(tbl)[idx]
                  z.stat.out <- attr(tbl[char.idx],
                                     "QStatisticsTestingInfo")[, "zstatistic"]
                  expected <- unclass(tbl)[char.idx]
                  expect_equal(z.stat.out, expected, check.attributes = FALSE)

                  logical.idx <- seq_along(tbl) %in% idx
                  z.stat.out <- attr(tbl[logical.idx],
                                     "QStatisticsTestingInfo")[, "zstatistic"]
                  expected <- unclass(tbl)[logical.idx]
                  expect_equal(z.stat.out, expected, check.attributes = FALSE)

              })
}

set.seed(98)
test.cases <- names(tbls)[dim.lens.avail == 2L]
for (test.case in test.cases)
{
   tbl <- tbls[[test.case]]
   row.idx <- sample(nrow(tbl), 1)
   test.name <- paste0("DS-3809: Row slices of simple tables: ",
                       test.case, "[", row.idx, ",]")
   test_that(test.name,
   {
       q.stat.info.out <- attr(tbl[row.idx, ], "QStatisticsTestingInfo")
       z.stat.out <- q.stat.info.out[, "zstatistic"]
       expected <- unclass(tbl)[row.idx, ]
       expect_equal(z.stat.out, expected, check.attributes = FALSE)
   })

   col.idx <- sample(ncol(tbl), 1)
   test.name <- paste0("DS-3809: Column slices of simple tables: ",
                       test.case, "[,", col.idx, "]")
   test_that(test.name,
   {
       z.stat.out <- attr(tbl[, col.idx], "QStatisticsTestingInfo")[, "zstatistic"]
       expected <- unclass(tbl)[, col.idx]
       expect_equal(z.stat.out, expected, check.attributes = FALSE)
   })
}

test_that("DS-3809: Two-col. matrix indices to 2D Table",
{
   tbl <- tbls[["PickOne.by.Date"]]
   idx <- rbind(c(2, ncol(tbl)),
                c(nrow(tbl), 2))
   expected <- unclass(unname(tbl))[idx]
   z.out <- attr(tbl[idx], "QStatisticsTestingInfo")[, "zstatistic"]
   expect_equal(expected, z.out, check.attributes = FALSE)
})

test_that("DS-3809: Logical vector indices to 2D Table",
{
    tbl <- tbls[["PickAny.by.PickOne"]]
    set.seed(323)
    n <- length(tbl)
    idx <- logical(n)
    idx[sample(n, floor(n / 2))] <- TRUE
    expected <- unclass(unname(tbl))[idx]
    z.out <- attr(tbl[idx], "QStatisticsTestingInfo")[, "zstatistic"]
    expect_equal(expected, z.out, check.attributes = FALSE)
})

test_that("DS-3809: Integer vector indices to 2D Table",
{
    tbl <- tbls[["PickAny.by.PickOne"]]
    set.seed(3230)
    n <- length(tbl)
    idx <- sample(n, floor(n / 2))
    expected <- unclass(unname(tbl))[idx]
    z.out <- attr(tbl[idx], "QStatisticsTestingInfo")[, "zstatistic"]
    expect_equal(expected, z.out, check.attributes = FALSE)
})

test_that("DS-3810: Logical array indices to 3D table",
{
    tbl <- tbls[["NumberMulti.by.NumberGrid"]]
    idx <- array(FALSE, dim(tbl))
    idx[2, 1, 1] <- idx[2, 2, 3] <- TRUE
    out <- tbl[idx]
    expected <- c(unclass(tbl)[2, 1, 1], unclass(tbl)[2, 2, 3])
    expect_equal(unclass(out), expected, check.attributes = FALSE)
    expect_equal(attr(out, "QStatisticsTestingInfo")[, "zstatistic"], expected,
                 check.attributes = FALSE)

})

set.seed(986)
grid.types <- c("PickAnyGrid", "PickOneMulti", "NumberGrid")
test.cases <- names(tbls)[dim.lens.avail == 3L]
for (test.case in test.cases)
{
    tbl <- tbls[[test.case]]
    slice.indices <- vapply(dim(tbl), function(len) sample.int(len, 1), 1L)

    test.name <- paste0("DS-3810: Submatrices of 3D qTables: ",
                        test.case, "[", slice.indices[1L], ",,]")
    test_that(test.name,
              {
                  z.stat.out <- attr(tbl[slice.indices[1L], , ], "QStatisticsTestingInfo")[, "zstatistic"]
                  z.stat.out <- as.numeric(z.stat.out)
                  expected <- unclass(tbl)[slice.indices[1L], , ]
                  expected <- as.vector(t(expected))
                  expect_equal(z.stat.out, expected, check.attributes = FALSE)
              })
    test.name <- paste0("DS-3810: Submatrices of 3D qTables: ",
                        test.case, "[, ", slice.indices[2L], ",]")
    test_that(test.name,
              {
                  z.stat.out <- attr(tbl[, slice.indices[2L], ], "QStatisticsTestingInfo")[, "zstatistic"]
                  z.stat.out <- as.numeric(z.stat.out)
                  expected <- unclass(tbl)[, slice.indices[2L], ]
                  expected <- as.vector(t(expected))  # t() for row-major order in attr.
                  expect_equal(z.stat.out, expected, check.attributes = FALSE)
              })

    test.name <- paste0("DS-3810: Submatricess of 3D qTables: ",
                    test.case, "[,,", slice.indices[3L], "]")
    test_that(test.name,
              {
                  z.stat.out <- attr(tbl[, , slice.indices[3L]], "QStatisticsTestingInfo")[, "zstatistic"]
                  z.stat.out <- as.numeric(z.stat.out)
                  expected <- unclass(tbl)[, , slice.indices[3L]]
                  expected <- as.vector(t(expected))  # t() for row-major order in attr.
                  expect_equal(z.stat.out, expected, check.attributes = FALSE)
              })

    idx <- vapply(dim(tbl)[-1], sample, 1L, size = 1)
    test.name <- paste0("DS-3810: 1D slices from 3D qTables: ",
                        test.case, "[,", paste0(idx, collapse = ","), "]")
    test_that(test.name,
              {
                  z.stat.out <- attr(tbl[, idx[1L], idx[2L]], "QStatisticsTestingInfo")[, "zstatistic"]
                  z.stat.out <- as.numeric(z.stat.out)
                  expected <- unclass(tbl)[, idx[1L], idx[2L]]
                  expected <- as.vector(expected)
                  expect_equal(is.na(expected), is.na(z.stat.out))
                  expect_equal(z.stat.out, expected, check.attributes = FALSE)
              })
    idx <- vapply(dim(tbl)[-2], sample, 1L, size = 1)
    test.name <- paste0("DS-3810: 1D slices from 3D qTables: ",
                        test.case, "[", paste0(idx, collapse = ",,"), "]")
    test_that(test.name,
              {
                  z.stat.out <- attr(tbl[idx[1L], , idx[2L]], "QStatisticsTestingInfo")[, "zstatistic"]
                  z.stat.out <- as.numeric(z.stat.out)
                  expected <- unclass(tbl)[idx[1L], , idx[2L]]
                  expected <- as.vector(expected)
                  expect_equal(is.na(expected), is.na(z.stat.out))
                  expect_equal(z.stat.out, expected, check.attributes = FALSE)
              })
    idx <- vapply(dim(tbl)[-3], sample, 1L, size = 1)
    test.name <- paste0("DS-3810: 1D slices from 3D qTables: ",
                        test.case, "[", paste0(idx, collapse = ","), ",]")
    test_that(test.name,
              {
                  z.stat.out <- attr(tbl[idx[1L], idx[2L], ], "QStatisticsTestingInfo")[, "zstatistic"]
                  z.stat.out <- as.numeric(z.stat.out)
                  expected <- unclass(tbl)[idx[1L], idx[2L], ]
                  expected <- as.vector(expected)
                  expect_equal(is.na(expected), is.na(z.stat.out))
                  expect_equal(z.stat.out, expected, check.attributes = FALSE)
              })
}

set.seed(7676)
grid.types <- c("PickAnyGrid", "PickOneMulti", "NumberGrid")
test.cases <- names(tbls)[dim.lens.avail == 4L]
for (test.case in test.cases)
{
    tbl <- tbls[[test.case]]
    slice.indices <- vapply(dim(tbl), function(len) sample(2L:len, 1), 1L)
    slice.indices2 <- vapply(dim(tbl), function(len) sample(2L:len, 1), 1L)
    idx <- vapply(dim(tbl)[c(1, 2)], function(len) sample(2L:len, 1), 1L)
    test.name <- paste0("DS-3810: Submatrices of 4D qTables: ",
                        test.case, "[", idx[1L], ",", idx[2L], ",,]")
    test_that(test.name,
              {
                  z.stat.out <- attr(tbl[idx[1L], idx[2L], , ],
                                     "QStatisticsTestingInfo")[, "zstatistic"]
                  z.stat.out <- as.numeric(z.stat.out)
                  expected <- unclass(unname(tbl))[idx[1L], idx[2L], , ]
                  # if (attr(tbl, "questiontypes")[1L] %in% grid.types)
                  #     expected <- t(expected)  # t() for row-major order in attr.
                  expected <- as.vector(t(expected))
                  expect_equal(is.na(expected), is.na(z.stat.out))
                  expect_equal(z.stat.out, expected, check.attributes = FALSE)
              })
    idx <- vapply(dim(tbl)[c(1, 3)], function(len) sample(2L:len, 1), 1L)
    test.name <- paste0("DS-3810: Submatrices of 4D qTables: ",
                        test.case, "[", idx[1L], ",,", idx[2L], ",]")
    test_that(test.name,
              {
                  z.stat.out <- attr(tbl[idx[1L], , idx[2L], ],
                                     "QStatisticsTestingInfo")[, "zstatistic"]
                  z.stat.out <- as.numeric(z.stat.out)
                  expected <- unclass(unname(tbl))[idx[1L], , idx[2L], ]
                  expected <- as.vector(t(expected))
                  expect_equal(is.na(expected), is.na(z.stat.out))
                  expect_equal(z.stat.out, expected, check.attributes = FALSE)
              })
    idx <- vapply(dim(tbl)[c(1, 4)], function(len) sample(2L:len, 1), 1L)
    test.name <- paste0("DS-3810: Submatrices of 4D qTables: ",
                        test.case, "[", idx[1L], ",,,", idx[2L], "]")
    test_that(test.name,
              {
                  z.stat.out <- attr(tbl[idx[1L], , , idx[2L]],
                                     "QStatisticsTestingInfo")[, "zstatistic"]
                  z.stat.out <- as.numeric(z.stat.out)
                  expected <- unclass(unname(tbl))[idx[1L], , , idx[2L]]
                  expected <- as.vector(t(expected))
                  expect_equal(is.na(expected), is.na(z.stat.out))
                  expect_equal(z.stat.out, expected, check.attributes = FALSE)
              })
    idx <- vapply(dim(tbl)[c(2, 3)], function(len) sample(2L:len, 1), 1L)
    test.name <- paste0("DS-3810: Submatrices of 4D qTables: ",
                        test.case, "[,", idx[1L], ",", idx[2L], ",]")
    test_that(test.name,
              {
                  z.stat.out <- attr(tbl[, idx[1L], idx[2L], ],
                                     "QStatisticsTestingInfo")[, "zstatistic"]
                  z.stat.out <- as.numeric(z.stat.out)
                  expected <- unclass(unname(tbl))[, idx[1L], idx[2L], ]
                  expected <- as.vector(t(expected))
                  expect_equal(is.na(expected), is.na(z.stat.out))
                  expect_equal(z.stat.out, expected, check.attributes = FALSE)
              })
    idx <- vapply(dim(tbl)[c(2, 4)], function(len) sample(2L:len, 1), 1L)
    test.name <- paste0("DS-3810: Submatrices of 4D qTables: ",
                        test.case, "[,", idx[1L], ",,", idx[2L], "]")
    test_that(test.name,
              {
                  z.stat.out <- attr(tbl[, idx[1L], , idx[2L]],
                                     "QStatisticsTestingInfo")[, "zstatistic"]
                  z.stat.out <- as.numeric(z.stat.out)
                  expected <- unclass(unname(tbl))[, idx[1L], , idx[2L]]
                  expected <- as.vector(t(expected))
                  expect_equal(is.na(expected), is.na(z.stat.out))
                  expect_equal(z.stat.out, expected, check.attributes = FALSE)
              })
    idx <- vapply(dim(tbl)[c(3, 4)], function(len) sample(2L:len, 1), 1L)
    test.name <- paste0("DS-3810: Submatrices of 4D qTables: ",
                        test.case, "[,,", idx[1L], ",", idx[2L], "]")
    test_that(test.name,
              {
                  z.stat.out <- attr(tbl[, , idx[1L], idx[2L]],
                                     "QStatisticsTestingInfo")[, "zstatistic"]
                  z.stat.out <- as.numeric(z.stat.out)
                  expected <- unclass(unname(tbl))[, , idx[1L], idx[2L]]
                  expected <- as.vector(t(expected))
                  expect_equal(is.na(expected), is.na(z.stat.out))
                  expect_equal(z.stat.out, expected, check.attributes = FALSE)
              })

    idx <- vapply(dim(tbl)[-1], function(len) sample(2:len, 1), 1L)
    test.name <- paste0("DS-3810: 1D slices of 4D qTables: ",
                        test.case, "[,", idx[1L], ",", idx[2L], ",", idx[3L], "]")
    test_that(test.name,
              {
                  z.stat.out <- attr(tbl[, idx[1L], idx[2L], idx[3L]],
                                     "QStatisticsTestingInfo")[, "zstatistic"]
                  z.stat.out <- as.numeric(z.stat.out)
                  expected <- unclass(unname(tbl))[, idx[1L], idx[2L], idx[3L]]
                  expect_equal(is.na(expected), is.na(z.stat.out))
                  expect_equal(z.stat.out, expected, check.attributes = FALSE)
              })

    idx <- vapply(dim(tbl)[-2], function(len) sample(2:len, 1), 1L)
    test.name <- paste0("DS-3810: 1D slices of 4D qTables: ",
                        test.case, "[", idx[1L], ",,", idx[2L], ",", idx[3L], "]")
    test_that(test.name,
              {
                  z.stat.out <- attr(tbl[idx[1L], , idx[2L], idx[3L]],
                                     "QStatisticsTestingInfo")[, "zstatistic"]
                  z.stat.out <- as.numeric(z.stat.out)
                  expected <- unclass(unname(tbl))[idx[1L], , idx[2L], idx[3L]]
                  expect_equal(is.na(expected), is.na(z.stat.out))
                  expect_equal(z.stat.out, expected, check.attributes = FALSE)
              })

    idx <- vapply(dim(tbl)[-3], function(len) sample(2:len, 1), 1L)
    test.name <- paste0("DS-3810: 1D slices of 4D qTables: ",
                        test.case, "[", idx[1L], ",", idx[2L], ",,", idx[3L], "]")
    test_that(test.name,
              {
                  z.stat.out <- attr(tbl[idx[1L], idx[2L], , idx[3L]],
                                     "QStatisticsTestingInfo")[, "zstatistic"]
                  z.stat.out <- as.numeric(z.stat.out)
                  expected <- unclass(unname(tbl))[idx[1L], idx[2L], , idx[3L]]
                  expect_equal(is.na(expected), is.na(z.stat.out))
                  expect_equal(z.stat.out, expected, check.attributes = FALSE)
              })

    idx <- vapply(dim(tbl)[-4], function(len) sample(2:len, 1), 1L)
    test.name <- paste0("DS-3810: 1D slices of 4D qTables: ",
                        test.case, "[", idx[1L], ",", idx[2L], ",", idx[3L], ",]")
    test_that(test.name,
              {
                  z.stat.out <- attr(tbl[idx[1L], idx[2L], idx[3L], ],
                                     "QStatisticsTestingInfo")[, "zstatistic"]
                  z.stat.out <- as.numeric(z.stat.out)
                  expected <- unclass(unname(tbl))[idx[1L], idx[2L], idx[3L], ]
                  # if (attr(tbl, "questiontypes")[1L] %in% grid.types)
                  #     expected <- t(expected)  # t() for row-major order in attr.
                  expect_equal(is.na(expected), is.na(z.stat.out))
                  expect_equal(z.stat.out, expected, check.attributes = FALSE)
              })
}

checkAttribute <- function(x, attr.name, desired.attr) {
    if (is.null(desired.attr)) {
        testthat::expect_null(attr(x, attr.name))
        return(invisible())
    }
    x.attributes <- attributes(x)
    testthat::expect_true(attr.name %in% names(x.attributes))
    x.attr <- attr(x, attr.name)
    testthat::expect_equal(x.attr, desired.attr)
}

test_that("Span attributes retained properly", {
    checkSpanAttribute <- function(input, expected.attr) checkAttribute(input, "span", expected.attr)
    ############
    # No spans #
    ############
    ## 1d case #
    ############
    values <- c(9.91, 17.39, 11, 14.63, 16.01, 17.06, 13.99, 100)
    value.names <- c("15-18", "19 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49", "NET")
    age.table <- array(values, dim = length(values), dimnames = list(value.names))
    class(age.table) <- c("QTable", class(age.table))
    attr(age.table, "statistic") <- "%"
    checkSpanAttribute(age.table[1:2], NULL)
    checkSpanAttribute(age.table[c("19 to 24", "45 to 49")], NULL)
    checkSpanAttribute(age.table[as.logical(rep(c(1L, 0L), c(2L, 4L)))], NULL)
    empty.span <- list(rows = data.frame(value.names, fix.empty.names = FALSE))
    attr(age.table, "span") <- empty.span
    expected.empty <- empty.span
    expected.empty[[1]] <- empty.span[[1]][1:2, , drop = FALSE]
    checkSpanAttribute(age.table[1:2], expected.empty)
    logical.age.table <- array(FALSE, dim = dim(age.table))
    age.table.logical <- logical.age.table
    age.table.logical[1:2] <- TRUE
    checkSpanAttribute(age.table[age.table.logical], expected.empty)
    int.mat.age.table <- which(age.table.logical, arr.ind = TRUE)
    checkSpanAttribute(age.table[int.mat.age.table], expected.empty)
    expected.empty[[1]] <- empty.span[[1]][c(2, 7), , drop = FALSE]
    checkSpanAttribute(age.table[c("19 to 24", "45 to 49")], expected.empty)
    ############
    ## 2d case #
    ############
    dims <- 6:7
    values <- round(runif(prod(dims)), 2)
    dimnames.2d <- list(c("Coca Cola", "Diet Coke", "Coke Zero", "Pepsi", "Pepsi Light", "Pepsi Max"),
                        c("Don't know", "Hate", "Dislike", "Neither", "Like", "Love", "NET"))
    table.2d <- array(values, dim = 6:7, dimnames = dimnames.2d)
    attr(table.2d, "statistic") <- "Average"
    span.2d <- list(rows = data.frame(c("Cokes", "Cokes", "Cokes", "Standard Pepsi", NA, "Standard Pepsi"),
                                      dimnames.2d[[1L]],
                                      fix.empty.names = FALSE),
                    columns = data.frame(c(NA, "Don't like", "Don't like", NA, "Like", "Like", NA),
                                         dimnames.2d[[2L]],
                                         fix.empty.names = FALSE))
    empty.span <- list(rows = data.frame(rownames(table.2d), fix.empty.names = FALSE),
                       columns = data.frame(colnames(table.2d), fix.empty.names = FALSE))
    logical.2d.mat <- array(FALSE, dim = dim(table.2d))
    class(table.2d) <- c("QTable", class(table.2d))
    checkSpanAttribute(table.2d[1:2, 3:4], NULL)
    attr(table.2d, "span") <- empty.span
    expected.span <- empty.span
    expected.span[[1L]] <- empty.span[[1L]][1:2, , drop = FALSE]
    expected.span[[2L]] <- empty.span[[2L]][3:4, , drop = FALSE]
    checkSpanAttribute(table.2d[1:2, 3:4], expected.span)
    table.2d.logical <- logical.2d.mat
    table.2d.logical[1:2, 3:4] <- TRUE
    checkSpanAttribute(table.2d[table.2d.logical], NULL)
    int.mat.2d <- which(table.2d.logical, arr.ind = TRUE)
    checkSpanAttribute(table.2d[int.mat.2d], NULL)
    ############
    ## 3d case #
    ############
    table.3d <- array(rep(as.vector(table.2d), 2L), dim = c(dim(table.2d), 2L),
                      dimnames = c(dimnames(table.2d), list(c("Row %", "Expected %"))))
    class(table.3d) <- c("QTable", class(table.3d))
    checkSpanAttribute(table.3d[2:3, 3:4, ], NULL)
    empty.span <- list(rows = data.frame(rownames(table.3d), fix.empty.names = FALSE),
                       columns = data.frame(colnames(table.3d), fix.empty.names = FALSE))
    attr(table.3d, "span") <- empty.span
    expected.span <- empty.span
    expected.span[[1L]] <- empty.span[[1L]][2:3, , drop = FALSE]
    expected.span[[2L]] <- empty.span[[2L]][3:4, , drop = FALSE]
    checkSpanAttribute(table.3d[2:3, 3:4, ], expected.span)
    table.3d.logical <- array(FALSE, dim = dim(table.3d))
    table.3d.mat <- table.3d.logical
    table.3d.mat[2:3, 3:4, ] <- TRUE
    checkSpanAttribute(table.3d[table.3d.mat], NULL)
    int.mat.3d <- which(table.3d.mat, arr.ind = TRUE)
    checkSpanAttribute(table.3d[int.mat.3d], NULL)
    ###################
    # General 1d case #
    ###################
    age.span <- data.frame(c("&lt;25", "&lt;25", "25-39", "25-39", "25-39", NA, NA, NA),
                           value.names,
                           fix.empty.names = FALSE)
    attr(age.table, "span") <- list(rows = age.span)
    expected.span <- list(rows = age.span[1:2, ])
    checkSpanAttribute(age.table[1:2], expected.span)
    checkSpanAttribute(age.table[c("15-18", "19 to 24")], expected.span)
    age.table.logical <- logical.age.table
    age.table.logical[1:2] <- TRUE
    checkSpanAttribute(age.table[age.table.logical], expected.span)
    checkSpanAttribute(age.table[which(age.table.logical, arr.ind = TRUE)], expected.span)
    expected.span <- list(rows = age.span[2:3, ])
    checkSpanAttribute(age.table[2:3], expected.span)
    checkSpanAttribute(age.table[c("19 to 24", "25 to 29")], expected.span)
    age.table.logical <- logical.age.table
    age.table.logical[2:3] <- TRUE
    checkSpanAttribute(age.table[age.table.logical], expected.span)
    checkSpanAttribute(age.table[which(age.table.logical, arr.ind = TRUE)], expected.span)
    expected.span <- list(rows = age.span[c(1, 3), ])
    checkSpanAttribute(age.table[c(1, 3)], expected.span)
    checkSpanAttribute(age.table[c("15-18", "25 to 29")], expected.span)
    age.table.logical <- logical.age.table
    age.table.logical[c(1, 3)] <- TRUE
    checkSpanAttribute(age.table[age.table.logical], expected.span)
    checkSpanAttribute(age.table[which(age.table.logical, arr.ind = TRUE)], expected.span)
    expected.span <-  list(rows = age.span[c(1, 7), ])
    checkSpanAttribute(age.table[c(1, 7)], expected.span)
    checkSpanAttribute(age.table[c("15-18", "45 to 49")], expected.span)
    age.table.logical <- logical.age.table
    age.table.logical[c(1, 7)] <- TRUE
    checkSpanAttribute(age.table[age.table.logical], expected.span)
    checkSpanAttribute(age.table[which(age.table.logical, arr.ind = TRUE)], expected.span)
    # Drop the span if it is not there (all NA)
    expected.span <- list(rows = age.span[2L][6:7, , drop = FALSE])
    checkSpanAttribute(age.table[6:7], expected.span)
    checkSpanAttribute(age.table[c("40 to 44", "45 to 49")], expected.span)
    ###########
    # 2d case #
    ###########
    dims <- 6:7
    values <- round(runif(prod(dims)), 2)
    dimnames.2d <- list(c("Coca Cola", "Diet Coke", "Coke Zero", "Pepsi", "Pepsi Light", "Pepsi Max"),
                        c("Don't know", "Hate", "Dislike", "Neither", "Like", "Love", "NET"))
    table.2d <- array(values, dim = 6:7, dimnames = dimnames.2d)
    span.2d <- list(rows = data.frame(c("Cokes", "Cokes", "Cokes", "Standard Pepsi", NA, "Standard Pepsi"),
                                      dimnames.2d[[1L]],
                                      fix.empty.names = FALSE),
                    columns = data.frame(c(NA, "Don't like", "Don't like", NA, "Like", "Like", NA),
                                         dimnames.2d[[2L]],
                                         fix.empty.names = FALSE))
    attr(table.2d, "span") <- span.2d
    attr(table.2d, "statistic") <- "Row %"
    class(table.2d) <- c("QTable", class(table.2d))

    # Check subscripting with dimensions dropped
    sub.1row <- table.2d[2,]
    expect_equal(attr(sub.1row, "span")$column, NULL)
    expect_equal(attr(sub.1row, "span")$row, attr(table.2d, "span")$column)
    sub.1col <- table.2d[,2]
    expect_equal(attr(sub.1col, "span")$column, NULL)
    expect_equal(attr(sub.1col, "span")$row, attr(table.2d, "span")$row)

    ## Cell reference checks
    ### All in first column
    checkSpanAttribute(table.2d[1:2], NULL)
    table.2d.logical <- logical.2d.mat
    table.2d.logical[1:2] <- TRUE
    checkSpanAttribute(table.2d[table.2d.logical], NULL)
    checkSpanAttribute(table.2d[which(table.2d.logical, arr.ind = TRUE)], NULL)
    ### All in 3rd column
    checkSpanAttribute(table.2d[6 * 2 + 1:6], NULL)
    table.2d.logical <- logical.2d.mat
    table.2d.logical[6 * 2 + 1:6] <- TRUE
    checkSpanAttribute(table.2d[table.2d.logical], NULL)
    checkSpanAttribute(table.2d[which(table.2d.logical, arr.ind = TRUE)], NULL)
    ### Don't return span attribute if cell references are not a slice of the array
    checkSpanAttribute(table.2d[c(1, 3, 5, 7, 12)], NULL)
    table.2d.logical <- logical.2d.mat
    table.2d.logical[c(1, 3, 5, 7, 12)] <- TRUE
    checkSpanAttribute(table.2d[table.2d.logical], NULL)
    checkSpanAttribute(table.2d[which(table.2d.logical, arr.ind = TRUE)], NULL)
    ## Row checks
    ### Subscripting only rows with a span, all columns included
    expected.span <- span.2d
    expected.span[[1L]] <- expected.span[[1L]][1:2, ]
    checkSpanAttribute(table.2d[1:2, ], expected.span)
    checkSpanAttribute(table.2d[c("Coca Cola", "Diet Coke"), ], expected.span)
    table.2d.logical <- logical.2d.mat
    table.2d.logical[1:2, ] <- TRUE
    checkSpanAttribute(table.2d[table.2d.logical], NULL)
    checkSpanAttribute(table.2d[which(table.2d.logical, arr.ind = TRUE)], NULL)
    expected.span <- span.2d
    expected.span[[1L]] <- expected.span[[1L]][1:4, ]
    checkSpanAttribute(table.2d[1:4, ], expected.span)
    checkSpanAttribute(table.2d[c("Coca Cola", "Diet Coke", "Coke Zero", "Pepsi"), ], expected.span)
    expected.span <- span.2d
    expected.span[[1L]] <- expected.span[[1L]][3:5, ]
    checkSpanAttribute(table.2d[3:5, ], expected.span)
    checkSpanAttribute(table.2d[c("Coke Zero", "Pepsi", "Pepsi Light"), ], expected.span)
    ### Span dropped if there is none
    expected.span <- span.2d
    expected.span[[1]] <- expected.span[[1]][2][5, , drop = FALSE]
    expect_equal(attr(table.2d[5, ], "span")$rows, expected.span$columns)
    expect_equal(attr(table.2d["Pepsi Light", ], "span")$rows, expected.span$columns)
    ## Column checks
    ### Column span dropped but rows remain
    expected.span <- span.2d
    expected.span[[2]] <- expected.span[[2]][2][c(1, 4), , drop = FALSE]
    checkSpanAttribute(table.2d[, c(1, 4)], expected.span)
    checkSpanAttribute(table.2d[, c("Don't know", "Neither")], expected.span)
    ### Column spans found appropriately
    expected.span <- span.2d
    expected.span[[2]] <- span.2d[[2]][c(1, 3, 5), ]
    checkSpanAttribute(table.2d[, c(1, 3, 5)], expected.span)
    checkSpanAttribute(table.2d[, c("Don't know", "Dislike", "Like")], expected.span)
    ## Row and Column checks
    expected.span <- span.2d
    expected.span[[1L]] <- span.2d[[1]][1:3, , drop = FALSE]
    expected.span[[2L]] <- span.2d[[2]][1:3, , drop = FALSE]
    checkSpanAttribute(table.2d[1:3, 1:3], expected.span)
    checkSpanAttribute(table.2d[c("Coca Cola", "Diet Coke", "Coke Zero"),
                                c("Don't know", "Hate", "Dislike")], expected.span)
    ### No span remains if all NA
    expected.span <- span.2d
    expected.span[[1L]] <- expected.span[[1L]][2][5, , drop = FALSE]
    expected.span[[2L]] <- expected.span[[2L]][2][c(1, 4), , drop = FALSE]
    checkSpanAttribute(table.2d[5, c(1, 4), drop = FALSE], expected.span)
    expect_equal(attr(table.2d["Pepsi Light", c("Don't know", "Neither")], "span")$rows,
        expected.span$columns)
    # 2d table with multiple statistics (3d array)
    table.3d <- array(rep(as.vector(table.2d), 2L), dim = c(dim(table.2d), 2L),
                      dimnames = c(dimnames(table.2d), list(c("Row %", "Expected %"))))
    attr(table.3d, "span") <- span.2d
    class(table.3d) <- c("QTable", class(table.3d))
    ### Both rows and columns ok
    expected.span <- span.2d
    expected.span[[1]] <- span.2d[[1]][1:2, , drop = FALSE]
    expected.span[[2]] <- span.2d[[2]][1:2, , drop = FALSE]
    checkSpanAttribute(table.3d[1:2, 1:2, ], expected.span)
    checkSpanAttribute(table.3d[c("Coca Cola", "Diet Coke"), c("Don't know", "Hate"), ], expected.span)
    ### Span dropped if not required
    expected.span <- span.2d
    expected.span[[1]] <- expected.span[[1]][2][5, , drop = FALSE]
    expected.span[[2]] <- expected.span[[2]][c(2, 4, 6), , drop = FALSE]
    checkSpanAttribute(table.3d[5, c(2, 4, 6), ], expected.span)
    checkSpanAttribute(table.3d["Pepsi Light", c("Hate", "Neither", "Love"), ], expected.span)
    ### Isn't affected by stat selection
    checkSpanAttribute(table.3d[,, 1], span.2d)
    expect_equal(attr(table.3d[5, c(2, 4, 6), 1], "span")$rows, expected.span$columns)
    expect_true(is.null(attr(table.3d[5, c(2, 4, 6), 1], "span")$columns))
    checkSpanAttribute(table.3d["Pepsi Light", c("Hate", "Neither", "Love"), "Row %", drop = FALSE], expected.span)
    checkSpanAttribute(table.3d[5, c(2, 4, 6), 2, drop = FALSE], expected.span)
    expect_equal(attr(table.3d["Pepsi Light", c("Hate", "Neither", "Love"), "Expected %"], "span")$rows, expected.span$columns)
    checkSpanAttribute(table.3d[5, c(2, 4, 6), ], expected.span)
    checkSpanAttribute(table.3d["Pepsi Light", c("Hate", "Neither", "Love"), ], expected.span)
})

env <- new.env()
source(system.file("tests", "QTables.R", package = "verbs"), local = env)

test_that("DS-3797: Attributes renamed appropriately after subsetting", {
    tbl <- env$qTable.2D
    attr(tbl, "customAttr") <- "FooBar"
    out <- tbl[1:2, 1:2]
    attr.names.out <- names(attributes(out))
    expected.renamed <- paste0("original.",
                               c("dimnets", "dimduplicates", "span",
                                 "basedescriptiontext", "basedescription",
                                 "questiontypes", "footerhtml"))
    expected.basic <- c("dim", "dimnames", "class", "statistic", "questions")
    expected.modified <- c("QStatisticsTestingInfo", "span", "name", "subscripted.footerhtml",
                           "questiontypes", "mapped.dimnames", "is.subscripted")
    expected.custom <- "customAttr"
    attr.names.expected <- c(expected.renamed, expected.basic,
                             expected.modified, expected.custom)
    expect_setequal(attr.names.out, attr.names.expected)
})

test_that("DS-3837: Can subset QTables missing dimnames",
{
    expect_error(capture.output(str(tbls[[3]])), NA)
    tbl <- tbls[[3]]
    tbl.unnamed <- unname(tbl)
    q.stat.info <- attr(tbl[1:2, 2:1], "QStatisticsTestingInfo")
    q.stat.info.unnamed <- attr(tbl.unnamed[1:2, 2:1], "QStatisticsTestingInfo")
    expected <- unclass(tbl.unnamed)[1:2, 2:1]
    expected.z <- as.vector(t(expected))
    expect_equal(unclass(q.stat.info.unnamed[, "zstatistic"]), expected.z,
                 check.attributes = FALSE)
    df.idx <- 2
    row.idx <- q.stat.info.unnamed[df.idx, "Row"]
    col.idx <- q.stat.info.unnamed[df.idx, "Column"]
    expect_equal(q.stat.info.unnamed[df.idx, "zstatistic"], expected[row.idx, col.idx])
    expect_equal(q.stat.info[df.idx, "zstatistic"], expected[row.idx, col.idx])
    expect_equal(q.stat.info[, -2:-1], q.stat.info.unnamed[, -2:-1])

    tbl.unnamed <- unname(tbls[["PickAnyGrid"]])
    out <- tbl.unnamed[3:4, 3:2]
    q.stat.info.unnamed <- attr(out, "QStatisticsTestingInfo")
    expected <- unclass(tbl.unnamed)[3:4, 3:2]
    expected.z <- as.vector(t(expected))
    expect_equal(unclass(q.stat.info.unnamed[, "zstatistic"]), expected.z,
                 check.attributes = FALSE)
    df.idx <- 3
    row.idx <- q.stat.info.unnamed[df.idx, "Row"]
    col.idx <- q.stat.info.unnamed[df.idx, "Column"]
    expect_equal(q.stat.info.unnamed[df.idx, "zstatistic"], expected[row.idx, col.idx])

    tbl.ms <- makeMultistat(tbls[["PickAnyGrid.by.Date"]])
    expect_error(y <- tbl.ms[, , 1, 1:2], NA)
    expect_equal(as.vector(y[c(1, 10, 12, 20, 22)][c(1, 3)][1]), as.vector(y[1]))
    expect_equal(as.vector(quantile(y)), as.vector(quantile(unclass(y))))
    expect_equal(summary(y), summary(unclass(y)))
})

test_that("DS-3829: Add lookup/array indices to QStatisticsTestingInfo",
{
    tbl <- tbls[["PickAny.by.PickOne"]]
    row.major.idx <- 2:1
    dimnames.tbl <- dimnames(tbl)
    names(dimnames.tbl) <- c("Row", "Column")
    expected.index <- expand.grid(dimnames.tbl[row.major.idx])[, row.major.idx]
    q.test.info <- attr(tbl, "QStatisticsTestingInfo")
    expected <- cbind(expected.index, q.test.info)
    idx <- 1:3
    out <- tbl[idx, idx]
    q.test.info.out <- attr(out, "QStatisticsTestingInfo")
    expect_equal(q.test.info.out, expected)

    ## Indices not added twice on 2nd subsetting
    q.test.info.copy <- attr(out[idx, idx], "QStatisticsTestingInfo")
    expect_equal(q.test.info.copy, q.test.info.out)
})

test_that("DS-3829: TestInfo lookup indices correct after dropping dimensions",
{
    ## single row from a 2D crosstab
    tbl <- tbls[["PickAny.by.PickOne"]]
    row.major.idx <- 2:1
    dimnames.tbl <- dimnames(tbl)
    names(dimnames.tbl) <- c("Row", "Column")
    expected.index <- expand.grid(dimnames.tbl[row.major.idx])
    expected.index <- expected.index[, "Column", drop = FALSE]
    colnames(expected.index) <- "Row"
    q.test.info <- attr(tbl, "QStatisticsTestingInfo")
    expected <- cbind(expected.index, q.test.info)
    keep.idx <- 1:3
    expected <- expected[keep.idx, ]
    out <- tbl[1, ]
    q.test.info.out <- attr(out, "QStatisticsTestingInfo")

    ## Single column from a 2D crosstab, drop = FALSE
    expected.index <- expand.grid(dimnames.tbl[row.major.idx])[, row.major.idx]
    q.test.info <- attr(tbl, "QStatisticsTestingInfo")
    expected <- cbind(expected.index, q.test.info)
    keep.idx <- nrow(tbl) * (0:(ncol(tbl) - 1)) + 1
    expected <- expected[keep.idx, ]
    expected[, 1] <- droplevels(expected[, 1])
    expected[, 2] <- droplevels(expected[, 2])
    out <- tbl[, 1, drop = FALSE]
    q.test.info.out <- attr(out, "QStatisticsTestingInfo")
    expect_equal(q.test.info.out, expected)

    ## Single column from a 2D crosstab, drop = TRUE
    expected <- expected[, !colnames(expected) %in% "Column", drop = FALSE]
    out <- tbl[, 1]
    q.test.info.out <- attr(out, "QStatisticsTestingInfo")
    expect_equal(q.test.info.out, expected[-1])

    ## 3D table to 2D
    tbl <- tbls[["PickOneMulti.by.Date"]]
    out <- tbl[, 2, ]
    q.test.info.out <- attr(out, "QStatisticsTestingInfo")
    expect_equal(colnames(q.test.info.out)[1:2], c("Row", "Column"))
})

test_that("DS-3843 questiontypes attribute is modified correctly",
{
    checkQuestionTypesAttr <- function(x, desired.attr) checkAttribute(x, "questiontypes", desired.attr)
    # Basic tables
    ## PickOne
    tbl <- tbls[["PickOne"]]
    checkQuestionTypesAttr(tbl[1:3], "PickOne")
    checkQuestionTypesAttr(tbl[1], "Number")
    checkQuestionTypesAttr(tbl["Under 40"], "Number")
    checkQuestionTypesAttr(tbl[], "PickOne")
    checkQuestionTypesAttr(tbl[1:3, drop = FALSE], "PickOne")

    ## Number
    number.tbl <- structure(c(Age = 42), statistic = "Average", dim = 1L,
                            class = c("array", "QTable"), questiontypes = "Number")
    tbl <- number.tbl
    checkQuestionTypesAttr(tbl[1], "Number")
    checkQuestionTypesAttr(tbl[1, drop = FALSE], "Number")
    checkQuestionTypesAttr(tbl[], "Number")

    ## Number Multi
    number.multi.tbl <- structure(runif(3L), dimnames = list(c("Young", "Medium", "Old")),
                                  dim = 3L, statistic = "Average", questiontypes = "NumberMulti",
                                  class = c("array", "QTable"))
    tbl <- number.multi.tbl
    checkQuestionTypesAttr(tbl[1:3], "NumberMulti")
    checkQuestionTypesAttr(tbl[1:2], "NumberMulti")
    checkQuestionTypesAttr(tbl[1], "Number")

    # Crosstab
    tbl <- tbls[["PickOne.by.PickOne"]]
    checkQuestionTypesAttr(tbl[, 1, drop = FALSE], c("PickOne", "PickOne"))
    checkQuestionTypesAttr(tbl[1, , drop = FALSE], c("PickOne", "PickOne"))
    checkQuestionTypesAttr(tbl[1:2, 2:3], c("PickOne", "PickOne"))
    checkQuestionTypesAttr(tbl[, 1], "PickOne")
    checkQuestionTypesAttr(tbl[1, ], "PickOne")
    checkQuestionTypesAttr(tbl[, 1], "PickOne")
    checkQuestionTypesAttr(tbl[1:3], "PickOne")
    logical.arr <- array(rep(c(TRUE, FALSE), c(2, 1)), dim = c(3, 3))
    checkQuestionTypesAttr(tbl[logical.arr], c("PickOne", "PickOne"))
    logical.arr[, 2:3] <- FALSE
    checkQuestionTypesAttr(tbl[logical.arr], "PickOne")

    # Nominal x Multi
    tbl <- structure(array(runif(8L, min = 16, max = 20), dim = c(8, 1),
                           dimnames = list(c("15-18", "19 to 24", "25 to 29", "30 to 34",
                                             "35 to 39", "40 to 44", "45 to 49", "NET"), "Total Spend")),
                     statistic = "Average", class = c("array", "QTable"), questiontypes = c("PickOne", "Number"))
    checkQuestionTypesAttr(tbl[1:3], c("PickOne", "Number"))
    checkQuestionTypesAttr(tbl[2], c("PickOne", "Number"))

    # Grid
    tbl <- tbls[["PickOneMulti"]]
    checkQuestionTypesAttr(tbl[, 1], "PickAny")
    tbl <- tbls[["PickAnyGrid"]]
    checkQuestionTypesAttr(tbl[, 1], "PickAny")


    # Grid Crosstabs
    tbl <- tbls[["PickOne.by.PickAnyGrid"]]
    checkQuestionTypesAttr(tbl[, , 1], c("PickOne", "PickAny"))
    checkQuestionTypesAttr(tbl[1, , ], "PickAnyGrid")
    checkQuestionTypesAttr(tbl[, 1, ], c("PickOne", "PickAny"))
    checkQuestionTypesAttr(tbl[1, 1, ], "PickAny")

    tbl <- tbls[["NumberGrid.by.Date"]]
    checkQuestionTypesAttr(tbl[, , 1], c("NumberGrid"))
    checkQuestionTypesAttr(tbl[1, , ], c("NumberMulti", "Date"))
    checkQuestionTypesAttr(tbl[, 1, ], c("NumberMulti", "Date"))
    checkQuestionTypesAttr(tbl[1, 1, ], "Date")

    # Grid by Grid
    tbl <- tbls[["PickAnyGrid.by.NumberGrid"]]
    checkQuestionTypesAttr(tbl[1, , , ], c("PickAny", "NumberGrid"))
    checkQuestionTypesAttr(tbl[, 1, , ], c("PickAny", "NumberGrid"))
    checkQuestionTypesAttr(tbl[, , 1, ], c("PickAnyGrid", "NumberMulti"))
    checkQuestionTypesAttr(tbl[, , , 1], c("PickAnyGrid", "NumberMulti"))
    checkQuestionTypesAttr(tbl[1, 1, , ], c("NumberGrid"))
    checkQuestionTypesAttr(tbl[, , 1, 1], c("PickAnyGrid"))
    checkQuestionTypesAttr(tbl[1, , 1, ], c("PickAny", "NumberMulti"))
    checkQuestionTypesAttr(tbl[, 1, , 1], c("PickAny", "NumberMulti"))
    checkQuestionTypesAttr(tbl[1, 1, 1, ], c("NumberMulti"))
    checkQuestionTypesAttr(tbl[, 1, 1, 1], c("PickAny"))

    # Text Edge cases
    tbl <- structure(array(c("Foo", "Bar", "Baz"), dim = 3),
                     statistic = "Text", questiontypes = character(0L),
                     class = c("QTable", "array"))
    checkQuestionTypesAttr(tbl[1:2], character(0L))
    tbl <- structure(array(c("Foo", "Bar", "Baz", "MFoo", "MBar", "MBaz"), dim = c(3, 2)),
                     statistic = "Text", questiontypes = c("Text", "PickOne"),
                     class = c("QTable", "array"))
    checkQuestionTypesAttr(tbl[1:2], c("Text", "PickOne"))

    # Multistat versions

    ## Basic Number table
    number.multi.stat.tbl <- structure(c(42, 5), dim = 1:2,
                                       dimnames = list("Age", c("Average", "Standard Deviation")),
                                       class = c("array", "QTable"), questiontypes = "Number")
    tbl <- number.multi.stat.tbl
    checkQuestionTypesAttr(tbl[1], "Number")
    checkQuestionTypesAttr(tbl[2], "Number")
    checkQuestionTypesAttr(tbl[1, drop = FALSE], "Number")
    checkQuestionTypesAttr(tbl[], "Number")

    ## Number x Number - multi stat
    tbl <- structure(array(c(0.745, 0.02), dim = c(1, 1, 2),
                           dimnames = list("Total Spend", "", c("Correlation", "Standard Error"))),
                     class = c("array", "QTable"), questiontypes = c("Number", "Number"))
    checkQuestionTypesAttr(tbl[1], rep("Number", 2L))
    checkQuestionTypesAttr(tbl[1:2], rep("Number", 2L))
    checkQuestionTypesAttr(tbl[, , 2], rep("Number", 2L))

    # Pick one
    tbl <- makeMultistat(tbls[["PickOne"]])
    checkQuestionTypesAttr(tbl[, 1], "PickOne")
    checkQuestionTypesAttr(tbl[1:3, ], "PickOne")

    # Crosstab
    tbl <- makeMultistat(tbls[["PickOne.by.PickOne"]])
    checkQuestionTypesAttr(tbl[, 1, ], "PickOne")
    checkQuestionTypesAttr(tbl[, 1, 1], "PickOne")
    checkQuestionTypesAttr(tbl[, , 1], c("PickOne", "PickOne"))

    # Grid
    tbl <- makeMultistat(tbls[["PickOneMulti"]])
    checkQuestionTypesAttr(tbl[, 1, ], "PickAny")

    tbl <- makeMultistat(tbls[["PickAnyGrid"]])
    checkQuestionTypesAttr(tbl[, 1, ], "PickAny")

    # Grid Crosstabs
    tbl <- makeMultistat(tbls[["PickOne.by.PickAnyGrid"]])
    checkQuestionTypesAttr(tbl[, , 1, ], c("PickOne", "PickAny"))
    checkQuestionTypesAttr(tbl[, , 1, 1], c("PickOne", "PickAny"))
    checkQuestionTypesAttr(tbl[1, , , ], "PickAnyGrid")
    checkQuestionTypesAttr(tbl[, 1, , ], c("PickOne", "PickAny"))
    checkQuestionTypesAttr(tbl[1, 1, , ], "PickAny")
    checkQuestionTypesAttr(tbl[, , , 1], c("PickOne", "PickAnyGrid"))

    tbl <- makeMultistat(tbls[["NumberGrid.by.Date"]])
    checkQuestionTypesAttr(tbl[, , 1, ], "NumberGrid")
    checkQuestionTypesAttr(tbl[1, , , ], c("NumberMulti", "Date"))
    checkQuestionTypesAttr(tbl[, 1, , ], c("NumberMulti", "Date"))
    checkQuestionTypesAttr(tbl[1, 1, , ], "Date")

    # Grid by Grid
    tbl <- makeMultistat(tbls[["PickAnyGrid.by.NumberGrid"]])
    checkQuestionTypesAttr(tbl[1, , , , ], c("PickAny", "NumberGrid"))
    checkQuestionTypesAttr(tbl[1, , , , 1], c("PickAny", "NumberGrid"))
    checkQuestionTypesAttr(tbl[, 1, , , ], c("PickAny", "NumberGrid"))
    checkQuestionTypesAttr(tbl[, , 1, , ], c("PickAnyGrid", "NumberMulti"))
    checkQuestionTypesAttr(tbl[, , , 1, ], c("PickAnyGrid", "NumberMulti"))
    checkQuestionTypesAttr(tbl[1, 1, , , ], c("NumberGrid"))
    checkQuestionTypesAttr(tbl[, , 1, 1, ], "PickAnyGrid")
    checkQuestionTypesAttr(tbl[1, , 1, , ], c("PickAny", "NumberMulti"))
    checkQuestionTypesAttr(tbl[, 1, , 1, ], c("PickAny", "NumberMulti"))
    checkQuestionTypesAttr(tbl[1, 1, 1, , ], c("NumberMulti"))
    checkQuestionTypesAttr(tbl[, 1, 1, 1, ], "PickAny")
    checkQuestionTypesAttr(tbl[, , , , 1], c("PickAnyGrid", "NumberGrid"))

    # Multi-stat with negative subscripts
    tbl <- makeMultistat(tbls[["PickAny.by.PickOne"]])
    checkQuestionTypesAttr(tbl[-2, -3, "z-Statistic"], c("PickAny", "PickOne"))
})

test_that("DS-3824 Statistic Attribute checks", {
    # Single stat retained
    for (tbl in tbls) {
        subs.tbl <- tbl[sample.int(length(tbl), size = 2)]
        subs.stat.attr <- attr(subs.tbl, "statistic")
        stat.attr <- attr(tbl, "statistic")
        stat.attr.exists <- !is.null(stat.attr)
        if (stat.attr.exists) {
            expect_equal(subs.stat.attr, stat.attr)
        } else {
            expect_null(subs.stat.attr)
        }
    }
    # Test statistic inherited if multi stat dropped to single stat
    checkStatisticAttribute <- function(x, desired.attr) checkAttribute(x, "statistic", desired.attr)
    # Simple 2d table
    dims <- 6:7
    values <- round(runif(prod(dims)), 2)
    dimnames.2d <- list(c("Coca Cola", "Diet Coke", "Coke Zero", "Pepsi", "Pepsi Light", "Pepsi Max"),
                        c("Don't know", "Hate", "Dislike", "Neither", "Like", "Love", "NET"))
    table.2d <- array(values, dim = 6:7, dimnames = dimnames.2d)
    # 2d table with multiple statistics (3d array)
    table.3d <- array(c(as.vector(table.2d), as.vector(table.2d) * rnorm(prod(dims), mean = 1, sd = 0.05)),
                      dim = c(dim(table.2d), 2L),
                      dimnames = c(dimnames(table.2d), list(c("Row %", "Expected %"))))
    class(table.3d) <- c("QTable", class(table.3d))

    output <- table.3d[1:2, 2:3, ]
    checkStatisticAttribute(output, NULL)
    statistic.dim <- rev(dim(table.3d))[1]
    statistic.names <- rev(dimnames(table.3d))[[1L]]
    for (i in statistic.dim) {
        relevant.stat.name <- statistic.names[i]
        checkStatisticAttribute(table.3d[1:2, 2:3, i], relevant.stat.name)
        checkStatisticAttribute(table.3d[, , i], relevant.stat.name)
        not.dropped.table.3d <- table.3d[, , i, drop = FALSE]
        checkStatisticAttribute(not.dropped.table.3d, NULL)
        not.dropped.table.3d <- table.3d[1:2, 2:3, i, drop = FALSE]
        checkStatisticAttribute(not.dropped.table.3d, NULL)
        dropped.table.3d <- table.3d[, , i, drop = TRUE]
        checkStatisticAttribute(dropped.table.3d, relevant.stat.name)
    }
    # Single Array referencing checks
    ############
    ## 1d case #
    ############
    values <- c(9.91, 17.39, 11, 14.63, 16.01, 17.06, 13.99, 100)
    value.names <- c("15-18", "19 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49", "NET")
    age.table <- array(values, dim = length(values), dimnames = list(value.names))
    class(age.table) <- c("QTable", class(age.table))
    attr(age.table, "statistic") <- "%"
    logical.age.table <- array(FALSE, dim = dim(age.table))
    age.table.logical <- logical.age.table
    age.table.logical[1:2] <- TRUE
    int.mat.age.table <- which(age.table.logical, arr.ind = TRUE)
    checkStatisticAttribute(age.table[age.table.logical], "%")
    checkStatisticAttribute(age.table[int.mat.age.table], "%")
    ############
    ## 2d case #
    ############
    dims <- 6:7
    values <- round(runif(prod(dims)), 2)
    dimnames.2d <- list(c("Coca Cola", "Diet Coke", "Coke Zero", "Pepsi", "Pepsi Light", "Pepsi Max"),
                        c("Don't know", "Hate", "Dislike", "Neither", "Like", "Love", "NET"))
    table.2d <- array(values, dim = 6:7, dimnames = dimnames.2d)
    attr(table.2d, "statistic") <- "Average"
    logical.2d.mat <- array(FALSE, dim = dim(table.2d))
    class(table.2d) <- c("QTable", class(table.2d))
    table.2d.logical <- logical.2d.mat
    table.2d.logical[1:2, 3:4] <- TRUE
    int.mat.2d <- which(table.2d.logical, arr.ind = TRUE)
    checkStatisticAttribute(table.2d[table.2d.logical], "Average")
    checkStatisticAttribute(table.2d[int.mat.2d], "Average")
    ############
    ## 3d case #
    ############
    table.3d <- array(rep(as.vector(table.2d), 2L), dim = c(dim(table.2d), 2L),
                      dimnames = c(dimnames(table.2d), list(c("Row %", "Expected %"))))
    class(table.3d) <- c("QTable", class(table.3d))
    table.3d.logical <- array(FALSE, dim = dim(table.3d))
    table.3d.mat <- table.3d.logical
    table.3d.mat[2:3, 3:4, ] <- TRUE
    int.mat.3d <- which(table.3d.mat, arr.ind = TRUE)
    checkStatisticAttribute(table.3d[table.3d.mat], NULL) #Both stats used
    checkStatisticAttribute(table.3d[table.3d.mat[, , 1]], NULL) #Recycling
    table.3d.mat[, , 2] <- FALSE
    checkStatisticAttribute(table.3d[table.3d.mat], "Row %")
    checkStatisticAttribute(table.3d[, , c(TRUE, TRUE)], NULL)
    checkStatisticAttribute(table.3d[, , c(TRUE, FALSE)], "Row %")
    checkStatisticAttribute(table.3d[, , c(FALSE, TRUE)], "Expected %")
    # integer referencing
    checkStatisticAttribute(table.3d[int.mat.3d], NULL) #Both stats used
    checkStatisticAttribute(table.3d[int.mat.3d[1:4, ]], "Row %") #Only first
    checkStatisticAttribute(table.3d[int.mat.3d[5:8, ]], "Expected %") #Only second
    # Can retain statistic attribute if repeated use
    checkStatisticAttribute(table.3d[, , 1, drop = FALSE], NULL)
    checkStatisticAttribute(table.3d[, , 1, drop = FALSE][1], "Row %")
    checkStatisticAttribute(table.3d[, , 1, drop = FALSE][1:2, 2:3, 1], "Row %")
    #############
    # Num x Num #
    #############
    num.by.num <- array(runif(2), dim = c(1, 1), dimnames = list("Var1", "Var2"))
    class(num.by.num) <- c("QTable", class(num.by.num))
    attr(num.by.num, "statistic") <- "Average"
    checkStatisticAttribute(num.by.num[1], "Average")
    # Multiple stats
    num.by.num.multi <- array(num.by.num, dim = c(1, 1, 2),
                              dimnames = list("Var1", "Var2", c("Stat1", "Stat2")))
    class(num.by.num.multi) <- c("QTable", class(num.by.num.multi))
    checkStatisticAttribute(num.by.num.multi[1], "Stat1")
    checkStatisticAttribute(num.by.num.multi[2], "Stat2")
    checkStatisticAttribute(num.by.num.multi[1:2], NULL)
    checkStatisticAttribute(num.by.num.multi[, , "Stat1"], "Stat1")
    checkStatisticAttribute(num.by.num.multi[, , "Stat2"], "Stat2")
})

test_that("DS-3838: Permuting complex QTables preserves correct stat testing attr order",
{
    ## Permute each dimension of the table then subset to a single value
    set.seed(745)
    tbl <- tbls[["PickOne.by.PickAnyGrid"]]  # 1D x 2D
    idx <- dimnames(tbl)
    idx <- lapply(idx, function(vec) sample(vec, length(vec)))
    out <- tbl[idx[[1]], idx[[2]], idx[[3]]]

    idx1 <- sample(length(out), 1)
    out1 <- out[idx1]
    z.out1 <- attr(out1, "QStatisticsTestingInfo")[, "zstatistic"]
    z.expected <- unclass(tbl)[idx[[1]], idx[[2]], idx[[3]]]
    z.expected1 <- z.expected[idx1]
    expect_equal(z.out1, z.expected1)

    tbl <- tbls[["PickOne.by.PickAnyGrid"]]  # 1D x 2D
    idx <- dimnames(tbl)
    idx <- lapply(idx, function(vec) sample(vec, length(vec)))
    out <- tbl[idx[[1]], idx[[2]], idx[[3]]]

    idx1 <- sample(length(out), 1)
    out1 <- out[idx1]
    z.out1 <- attr(out1, "QStatisticsTestingInfo")[, "zstatistic"]
    z.expected <- unclass(tbl)[idx[[1]], idx[[2]], idx[[3]]]
    z.expected1 <- z.expected[idx1]
    expect_equal(z.out1, z.expected1)


    tbl <- tbls[["NumberGrid.by.Date"]]  # 2D x 1D
    idx <- dimnames(tbl)
    idx <- lapply(idx, function(vec) sample(vec, length(vec)))
    out <- tbl[idx[[1]], idx[[2]], idx[[3]]]

    idx1 <- sample(length(out), 1)
    out1 <- out[idx1]
    z.out1 <- attr(out1, "QStatisticsTestingInfo")[, "zstatistic"]
    z.expected <- unclass(tbl)[idx[[1]], idx[[2]], idx[[3]]]
    z.expected1 <- z.expected[idx1]
    expect_equal(z.out1, z.expected1)

    tbl <- tbls[["PickOneMulti.by.PickAnyGrid"]]  # 2D x 2D
    idx <- dimnames(tbl)
    idx <- lapply(idx, function(vec) sample(vec, length(vec)))
    out <- tbl[idx[[1]], idx[[2]], idx[[3]], idx[[4]]]

    idx1 <- sample(length(out), 1)
    out1 <- out[idx1]
    z.out1 <- attr(out1, "QStatisticsTestingInfo")[, "zstatistic"]
    z.expected <- unclass(tbl)[idx[[1]], idx[[2]], idx[[3]], idx[[4]]]
    z.expected1 <- z.expected[idx1]
    expect_equal(z.out1, z.expected1)
})

multi.stat.test.cases <- c("PickOne", "Date.by.PickAny", "NumberMulti.by.NumberGrid",
                           "PickAnyGrid", "PickOneMulti.by.PickAny",
                           "PickOneMulti.by.PickAnyGrid")
tbls.multi.stat <- lapply(tbls[multi.stat.test.cases], makeMultistat)
test_that("DS-3838: Updating QStatisticsTestingInfo for 2D multi-stat table",
{
    tbl <- tbls.multi.stat[["PickOne"]]
    out <- tbl[1, ]
    expect_equal(names(out), c("z-Statistic", "Average"))
    z.stat.out <- attr(out, "QStatisticsTestingInfo")[, "zstatistic"]
    expect_equal(z.stat.out, unclass(tbl)[1, "z-Statistic"])

    out <- tbl[, 2]
    expect_equal(names(out), rownames(tbl))
    z.stat.out <- attr(out, "QStatisticsTestingInfo")[, "zstatistic"]
    expect_equal(z.stat.out, unclass(tbl)[, "z-Statistic"],
                 check.attributes = FALSE)
    expect_equal(attr(out, "statistic"), "Average")

    row.idx <- 2:3
    out <- tbl[row.idx, 2:1]
    expect_equal(dimnames(out), list(rownames(tbl)[row.idx],
                                     rev(colnames(tbl))))
    z.stat.out <- attr(out, "QStatisticsTestingInfo")[, "zstatistic"]
    expect_equal(z.stat.out, unclass(tbl)[row.idx, "z-Statistic"],
                 check.attributes = FALSE)

    n <- length(tbl)
    idx <- logical(n)
    set.seed(487)
    idx[sample(n, 4)] <- TRUE
    out <- tbl[idx]

    expected <- unclass(tbl)[idx]
    expect_equal(unclass(out), expected, check.attributes = FALSE)
    idx.z <- idx
    idx.z[(floor(n / 2) + 1):n] <- FALSE
    expected.z <- unclass(tbl)[idx.z]
    q.stat.out <- attr(out, "QStatisticsTestingInfo")
    expect_equal(ncol(q.stat.out), 6)
    z.stat <- q.stat.out[, "zstatistic"]
    expect_equal(z.stat, expected.z, check.attributes = FALSE)
})

test_that("DS-3838: Subset QStatisticsTestingInfo for grid V.S. multi-stat summary table",
{
    tbl <- tbls.multi.stat[["PickAnyGrid"]]
    out <- tbl[2, 3, ]
    expect_equal(names(out), c("z-Statistic", "Average"))
    z.stat.out <- attr(out, "QStatisticsTestingInfo")[, "zstatistic"]
    expect_equal(z.stat.out, unclass(tbl)[2, 3, "z-Statistic"])

    out <- tbl[, 2, ]
   expect_equal(dimnames(out), dimnames(tbl)[c(1, 3)])
    z.stat.out <- attr(out, "QStatisticsTestingInfo")[, "zstatistic"]
    expect_equal(z.stat.out, unclass(tbl)[, 2, "z-Statistic"],
               check.attributes = FALSE)
    expect_equal(attr(out, "statistic"), NULL)

    out <- tbl[3:4, 3:2, 1]
    expect_equal(dimnames(out), list(dimnames(tbl)[[1L]][3:4],
                                     dimnames(tbl)[[2L]][3:2]))
    z.stat.out <- attr(out, "QStatisticsTestingInfo")[, "zstatistic"]
    expected.z <- as.vector(t(unclass(tbl)[3:4, 3:2, "z-Statistic"]))
    expect_equal(z.stat.out, expected.z, check.attributes = FALSE)
})

test_that("DS-3838: Subset QTestInfo for multi-stat xtab of 1D questions",
{
    tbl <- tbls.multi.stat[["Date.by.PickAny"]]
    out <- tbl[6, 1, ]
    expect_equal(names(out), c("z-Statistic", "Average"))
    z.stat.out <- attr(out, "QStatisticsTestingInfo")[, "zstatistic"]
    expect_equal(z.stat.out, unclass(tbl)[6, 1, "z-Statistic"])

    out <- tbl[4, 3, 2]
    expect_equal(dimnames(out), NULL)
    z.stat.out <- attr(out, "QStatisticsTestingInfo")[, "zstatistic"]
    expect_equal(z.stat.out, unclass(tbl)[4, 3, "z-Statistic"])

    out <- tbl[4:5, , ]
    expected.dim <- dimnames(tbl)
    expected.dim[[1]] <- expected.dim[[1]][4:5]
    expect_equal(dimnames(out), expected.dim)
    q.test.info.out <- attr(out, "QStatisticsTestingInfo")
    rownames(q.test.info.out) <- NULL
    z.stat.out <- q.test.info.out[, "zstatistic"]
    expect_equal(unname(q.test.info.out[, 1:2]), unname(expand.grid(expected.dim[2:1])[, 2:1]))
    expect_equal(z.stat.out, as.vector(t(unclass(tbl)[4:5, , "z-Statistic"])),
               check.attributes = FALSE)
    expect_equal(attr(out, "statistic"), NULL)

    out <- tbl[c(2, 5), 1:2, 2]
    expect_equal(dimnames(out), list(dimnames(tbl)[[1L]][c(2, 5)],
                                     dimnames(tbl)[[2L]][1:2]))
    z.stat.out <- attr(out, "QStatisticsTestingInfo")[, "zstatistic"]
    expected.z <- as.vector(t(unclass(tbl)[c(2, 5), 1:2, "z-Statistic"]))
    expect_equal(z.stat.out, expected.z, check.attributes = FALSE)
})

test_that("DS-3838: Subset QTestInfo for 4D multi-stat tables",
{
    tbl <- tbls.multi.stat[["NumberMulti.by.NumberGrid"]]
    out <- tbl[2:1, 1:2, 3, 1]
    expected <- unclass(tbl)[2:1, 1:2, 3, 1]
    q.test.info.out <- attr(out, "QStatisticsTestingInfo")
    col.names.orig <- colnames(attr(tbl, "QStatisticsTestingInfo"))
    col.expected <- c("Row", "Column", col.names.orig)
    expect_equal(colnames(q.test.info.out), col.expected)
    expect_equal(levels(q.test.info.out[, "Row"]),
                 dimnames(tbl)[[1L]][1:2])
    expected <- as.vector(t(expected))
    expect_equal(q.test.info.out[, "zstatistic"], expected,
                 check.attributes = FALSE)
    q.info.row.idx <- 3
    expected <- unclass(tbl)[q.test.info.out[[1L]][q.info.row.idx],
                           q.test.info.out[[1L]][q.info.row.idx], 3, 1]
    expect_equal(q.test.info.out[q.info.row.idx, "zstatistic"], expected)

    out <- tbl[2:1, 1:2, 3, 1, drop = FALSE]
    q.test.info.out <- attr(out, "QStatisticsTestingInfo")
    expected.dim.names <- c("Inner Row", "Outer Row", "Column", "Statistic")
    expect_equal(dim(out), c(2, 2, 1, 1))
    col.expected <- c(expected.dim.names[-length(expected.dim.names)],
                      col.names.orig)
    expect_equal(colnames(q.test.info.out), col.expected)
    expect_equal(nrow(q.test.info.out), prod(dim(out)))

    tbl <- tbls.multi.stat[["NumberMulti.by.NumberGrid"]]
    idx <- array(FALSE, dim(tbl))
    idx[4, 3, 1, 2] <- idx[2, 2, 3, 1] <- idx[4, 1, 2, 1] <- TRUE
    out <- tbl[idx]
    expect_equal(unclass(out), unclass(tbl)[idx], check.attributes = FALSE)
    idx[4, 3, 1, 2] <- FALSE
    expected <- unclass(tbl)[idx]
    expect_equal(attr(out, "QStatisticsTestingInfo")[, "zstatistic"], expected,
                 check.attributes = FALSE)

    out <- tbl[4, 3:2, 1:2, 1:2]
    expect_equal(dim(out), c(2, 2, 2))
    expected <- unclass(tbl)[4, 3:2, 1:2, 1]
    q.stat.info.out <- attr(out, "QStatisticsTestingInfo")
    expect_equal(q.stat.info.out[, "zstatistic"], as.vector(t(expected)),
                 check.attributes = FALSE)

    out <- tbl[2, 1, , 2]
    expect_equal(length(out), dim(tbl)[3])
    expected <- unclass(tbl)[2, 1, , 1]
    q.stat.info.out <- attr(out, "QStatisticsTestingInfo")
    expect_equal(q.stat.info.out[, "zstatistic"], as.vector(t(expected)),
                 check.attributes = FALSE)
})

test_that("DS-3838: Subset QTestInfo for 5D table (multi-stat xtab of two grid V.Sets)",
{
    tbl <- tbls.multi.stat[["PickOneMulti.by.PickAnyGrid"]]
    idx <- dimnames(tbl)
    idx <- lapply(idx, function(vec) sample(vec, length(vec)))
    out <- tbl[idx[[1]], idx[[2]], idx[[3]], idx[[4]], ]
    idx1 <- sample(floor(length(out) / 2), 1)
    out1 <- out[idx1]
    z.out1 <- attr(out1, "QStatisticsTestingInfo")[, "zstatistic"]
    z.expected <- unclass(tbl)[idx[[1]], idx[[2]], idx[[3]], idx[[4]], ]
    z.expected1 <- z.expected[idx1]
    expect_equal(unclass(out1), z.expected1, check.attributes = FALSE)
    expect_equal(z.out1, z.expected1)

    out <- tbl[, 2, 3, , 1]
    z.expected <- unclass(tbl)[, 2, 3, , 1]
    z.expected <- as.vector(t(z.expected))
    q.stat.out <- attr(out, "QStatisticsTestingInfo")
    z.out <- q.stat.out[, "zstatistic"]
    expect_equal(z.out, z.expected, check.attributes = FALSE)

    out <- tbl[1, , , 2, 2:1]
    z.expected <- unclass(tbl)[1, , , 2, 1]
    z.expected <- as.vector(t(z.expected))
    q.stat.out <- attr(out, "QStatisticsTestingInfo")
    z.out <- q.stat.out[, "zstatistic"]
    expect_equal(z.out, z.expected, check.attributes = FALSE)
})

test_that("DS-3810: Can subset QTestInfo for RAW DATA tables",
{
    tbl <- env$qTable.rawdata
    q.test.info.expected <- attr(tbl, "QStatisticalTestingInfo")[5:6, ]
    expect_error(out <- tbl[5:6], NA)
    q.test.info.out <- attr(out, "QStatisticalTestingInfo")
    expect_equal(q.test.info.out, q.test.info.expected)
})

test_that("DS-3846: Ensure higher order dim tables can be flattened", {
    checkSpanAttribute <- function(input, expected.attr) checkAttribute(input, "span", expected.attr)
    createDF <- function(var, ind) data.frame(var[ind], fix.empty.names = FALSE)
    createSpanAttr <- function(row.span, row.ind, col.span, col.ind) {
        new.spans <- mapply(createDF, list(row.span, col.span), list(row.ind, col.ind), SIMPLIFY = FALSE)
        names(new.spans) <- if (length(new.spans) == 1L) "rows" else c("rows", "columns")
        new.spans
    }

    subscriptCompleteTable <- function(tbl, expected.span.dims) {
        indices <- lapply(dim(tbl), function(x) sample.int(x, size = x - (x > 2L)))
        args <- c(list(tbl), indices)
        new.tbl <- do.call("[", args)
        mapped.dimnames <- attr(new.tbl, "mapped.dimnames")
        names(indices) <- names(mapped.dimnames)
        relevant.inds <- indices[which(names(indices) %in% expected.span.dims)]
        names(dimnames(tbl)) <- names(mapped.dimnames)
        relevant.dim.names <- dimnames(tbl)[expected.span.dims]

        expected.span <- createSpanAttr(relevant.dim.names[[1L]], relevant.inds[[1L]],
                                        relevant.dim.names[[2L]], relevant.inds[[2L]])
        checkSpanAttribute(new.tbl, expected.span)
    }
    determineQDims <- function(x) questionDimension(attr(x, "questiontypes"))
    # 2D x 1D or 1D x 2D tests
    possible.tbls <-  Filter(function(x) setequal(1:2, x), lapply(tbls, determineQDims))
    ## All 2D x 1D tables
    two.by.one.tbls <- tbls[names(Filter(function(x) all.equal(x, 2:1), possible.tbls))]
    expected.span.dims <- c("Row", "Outer Column")
    for (tbl in two.by.one.tbls) subscriptCompleteTable(tbl, expected.span.dims)
    ## All 1D x 2D tables
    one.by.two.tbls <- tbls[names(Filter(function(x) all.equal(x, 1:2), possible.tbls))]
    expected.span.dims <- c("Inner Row", "Outer Row")
    for (tbl in one.by.two.tbls) subscriptCompleteTable(tbl, expected.span.dims)
    ## All 2D x 2D tables
    two.by.two.tbls <-  Filter(function(x) identical(determineQDims(x), c(2L, 2L)), tbls)
    expected.span.dims <- c("Inner Row", "Outer Column")
    for (tbl in two.by.two.tbls) subscriptCompleteTable(tbl, expected.span.dims)
})

test_that("DS-3869: QStatTestInfo correct when reordering rows of multi-stat tbl of 1D q.",
{
    tbl <- tbls[["PickOne"]]
    tbl.ms <- makeMultistat(tbl)
    idx <- c(2, 4, 1, 3)
    out <- tbl.ms[idx, ]
    q.test.info.out <- attr(out, "QStatisticsTestingInfo")
    expect_equal(q.test.info.out[, "zstatistic"], unclass(tbl.ms)[idx, 1],
                 check.attributes = FALSE)
    expect_equal(as.character(q.test.info.out[, "Row"]), rownames(out))
})


test_that("DS-3873: Can subset 1D multi-stat table",
{
    tbl <- tbls[["PickOneMulti"]]
    tbl.ms <- makeMultistat(tbl)
    out <- tbl.ms[2, 3, ]
    expect_equal(attr(out, "questiontype"), "Number")
    expect_error(q.test.info.out <- attr(out[1], "QStatisticsTestingInfo"), NA)
    expect_equal(q.test.info.out[, "zstatistic"], unclass(tbl.ms)[2, 3, 1],
               check.attributes = FALSE)

    tbl <- tbls[["PickAnyGrid.by.NumberGrid"]]
    tbl.ms <- makeMultistat(tbl)
    dimnames(tbl.ms)[[5L]] <- c("Column %", "Row %")
    out <- tbl.ms[3, 1, 2, 2, ]
    expect_equal(attr(out, "questiontype"), "PickAny")
    expect_error(q.test.info.out <- attr(out[1], "QStatisticsTestingInfo"), NA)
    expect_equal(q.test.info.out[, "zstatistic"], unclass(tbl.ms)[3, 1, 2, 2, 1],
                 check.attributes = FALSE)

})

test_that("DS-3838: Can subset xtab with Number question",
{   ## xtab with a length-1 dimension should not error when drop = TRUE
    tbl <- tbls[["PickOne"]]
    tbl.xtab <- as.matrix(tbl)
    tbl.xtab <- flipU::CopyAttributes(tbl.xtab, tbl,
                                      attr.to.not.copy = c("dim", "dimnames"))
    attr(tbl.xtab, "questiontypes") <- c("PickOne", "Number")
    attr(tbl.xtab, "questions") <- c("Q3. Age", "Q10. NPS")
    attr(tbl.xtab, "span") <- list(rows = attr(tbl, "span")$rows,
                                   columns = unname(data.frame("NPS")))
    colnames(tbl.xtab) <- "nps"

    row.idx <- 1:3
    out <- tbl.xtab[row.idx, 1, drop = FALSE]
    test.info.out <- attr(out, "QStatisticsTestingInfo")
    expect_equal(test.info.out[, "zstatistic"], unclass(tbl)[row.idx],
                 check.attributes = FALSE)
    q.test.info.expected <- cbind(Row = as.factor(rownames(tbl)[row.idx]),
                                  Column = as.factor(rep("nps", 3)),
                                  attr(tbl, "QStatisticsTestingInfo")[row.idx, ])
    expect_equal(test.info.out, q.test.info.expected, check.attributes = FALSE)

    expect_error(out <- tbl.xtab[row.idx, 1, drop = TRUE], NA)
    expect_equal(length(out), length(row.idx))
    test.info.out <- attr(out, "QStatisticsTestingInfo")
    q.test.info.expected <- q.test.info.expected[, -(1:2)]
    expect_equal(test.info.out, q.test.info.expected, check.attributes = FALSE)
    expect_equal(attr(out, "questiontypes"), c("PickOne", "Number"))

    tbl.ms <- makeMultistat(tbl.xtab)
    out <- tbl.ms[1:2, 1, 2]
    test.info.out <- attr(out, "QStatisticsTestingInfo")
    q.test.info.expected <- q.test.info.expected[1:2, ]
    expect_equal(test.info.out, q.test.info.expected, check.attributes = FALSE)

    tbl <- tbls[["Ranking"]]
    tbl.xtab <- t(as.matrix(tbl))
    tbl.xtab <- flipU::CopyAttributes(tbl.xtab, tbl,
                                      attr.to.not.copy = c("dim", "dimnames"))
    attr(tbl.xtab, "questiontypes") <- c("Number", "Ranking")
    attr(tbl.xtab, "questions") <- c("nps", "Most frequently consumed cola")
    rownames(tbl.xtab) <- "nps"
    attr(tbl.xtab, "span") <- list(rows = unname(data.frame("NPS")),
                                   columns = attr(tbl, "span")$rows)

    col.idx <- c("Pepsi", "Pepsi Light", "Pepsi Max")
    out.no.drop <- tbl.xtab[1, col.idx, drop = FALSE]
    out <- tbl.xtab[1, col.idx]
    expected.span <- attr(tbl.xtab, "span")
    df.idx <- colnames(tbl.xtab) %in% col.idx
    expected.span$columns <- expected.span$columns[df.idx, , drop = FALSE]
    expect_equal(attr(out.no.drop, "span"), expected.span)
    expect_true(is.null(attr(out, "span")$columns))
    test.info.expected <- attr(tbl.xtab, "QStatisticsTestingInfo")
    test.info.expected <- test.info.expected[df.idx, ]
    expect_equal(attr(out, "QStatisticsTestingInfo"), test.info.expected)

    tbl.ms <- makeMultistat(tbl.xtab)
    col.idx <- 3:4
    out <- tbl.ms[1, col.idx, 2:1]
    expect_equal(dim(out), c(2, 2))
    expect_equal(attr(out, "questiontypes"), c("Number", "Ranking"))
    test.info.out <- attr(out, "QStatisticsTestingInfo")
    test.info.expected <- attr(tbl.xtab, "QStatisticsTestingInfo")
    test.info.expected <- test.info.expected[col.idx, ]
    test.info.expected <- cbind(Row = as.factor(colnames(tbl.xtab)[col.idx]),
                                test.info.expected)
    expect_equal(test.info.out, test.info.expected, check.attributes = FALSE)
})

test_that("DS-4814 drop argument working when subscripting to scalar", {
    tbl <- tbls[["PickOne"]]
    x.scalar <- tbl[1]
    x.scalar.with.drop <- tbl[1, drop = TRUE]
    x.not.scalar <- tbl[1, drop = FALSE]
    expect_equal(x.scalar, x.scalar.with.drop)
    expect_equal(as.vector(x.scalar), as.vector(x.not.scalar))
    expect_is(x.scalar, "numeric")
    expect_is(x.scalar.with.drop, "numeric")
    expect_is(x.not.scalar, "QTable")
    expect_is(x.not.scalar, "array")
    expect_equal(dim(x.not.scalar), 1)
    expect_null(dim(x.scalar))
})

test_that("DS-4987 UpdateQStatisticsTestInfo can be subscripted with all FALSE logical", {
    test <- structure(
        array(1:6, dim = 3:2, dimnames = list(letters[1:3], LETTERS[1:2])),
        questiontypes = c("PickOne", "PickOne"),
        questions = c("Q1", "Q2"),
        statistic = "Count",
        span = list(rows = data.frame("a", "b"), columns = data.frame("A", "B")),
        QStatisticsTestingInfo = data.frame(
            zstatistic = -2:3,
            significancearrowratio = numeric(6L),
            signifacancedirection = rep("None", 6L),
            significancesizemultiplier = rep(1L, 6L),
            significanceissignificance = logical(6L),
            significancergbcolor = numeric(6L),
            zstatistic = rep(NaN, 6L),
            pcorrected = rep(NaN, 6L)
        ),
        class = c("matrix", "array", "QTable")
    )
    expect_error(simple.logical <- test[FALSE, FALSE], NA)
    expect_error(all.logical <- test[logical(nrow(test)), logical(ncol(test))], NA)
    expect_equal(dim(simple.logical), rep(0L, 2L))
    expect_null(attributes(simple.logical)[["QStatisticsTestingInfo"]])
    expect_equal(dim(all.logical), rep(0L, 2L))
    expect_null(attributes(all.logical)[["QStatisticsTestingInfo"]])
})

test_that("DS-5024 Tables Flattened by rules will be subscriptable", {
    qtable.with.no.rule <- structure(
        array(
            c(-2.2, -0.6, -4.6, 0.7, 2.2, 0.6, 4.6, -0.7, -0.8, 2.3, -0.9, 1.8, 0.8, -2.3, 0.9, -1.8,
              0.4, 0.5, -1.8, 2.8, -0.4, -0.5, 1.8, -2.8, -1.3, -1.1, -0.2, 2.9, 1.3, 1.1, 0.2, -2.9),
            dim = c(4L, 8L),
            dimnames = list(
                c("Coke", "Pepsi", "Coke Zero", "Pepsi Max"),
                c("Male", "Female", "Male", "Female", "Male", "Female", "Male", "Female")
            )
        ),
        statistic = "z-Statistic",
        class = c("matrix", "array", "QTable"),
        span = list(
            rows = data.frame(c("Coke", "Pepsi", "Coke Zero", "Pepsi Max"), fix.empty.names = FALSE),
            columns = data.frame(
                rep(c("Feminine", "Health-conscious", "Innocent", "Older"), each = 2L),
                rep(c("Male", "Female"), 4L),
                fix.empty.names = FALSE
            )
        ),
        QStatisticsTestingInfo = data.frame(
            significancearrowratio = c(0.2, 0.2, 0, 0, 0, 0, 0, 0, 0, 0, 0.2, 0.2, 0, 0, 0, 0,
                                       0.8, 0.8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.4, 0.4, 0.4, 0.4),
            significancedirection = c("Down", "Up", "None")[c(1, 2, 3, 3, 3, 3, 3, 3, 3, 3, 2, 1, 3, 3, 3, 3,
                                                              1, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 2, 1, 2, 1)],
            significancefontsizemultiplier = c(0.5, 1.9, 1, 1, 1, 1, 1, 1, 1, 1, 1.9, 0.5, 1, 1, 1, 1,
                                               0.2, 4.4, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2.8, 0.3, 2.8, 0.3),
            significanceissignificant = as.logical(c(1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0,
                                                     1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1)),
            zstatistic = c(-2.2, 2.2, -0.8, 0.8, 0.4, -0.4, -1.3, 1.3, -0.6, 0.6, 2.3, -2.3, 0.5, -0.5, -1.1, 1.1,
                           -4.6, 4.6, -0.9, 0.9, -1.8, 1.8, -0.2, 0.2, 0.7, -0.7, 1.8, -1.8, 2.8, -2.8, 2.9, -2.9),
            pcorrected = c(0.0, 0.0, 0.3, 0.3, 0.6, 0.6, 0.1, 0.1, 0.4, 0.4, 0.0, 0.0, 0.5, 0.5, 0.2, 0.2,
                           0, 0, 0.3, 0.3, 0.0, 0.0, 0.8, 0.8, 0.4, 0.4, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)
        ),
        questiontypes = c("PickAny", "PickOne"),
        name = "no.rule.tbl",
        questions = c("q5", "Gender")
    )
    qtable.with.rule <- structure(
        array(
            c(-2.2, -0.6, -4.6, 0.7, 2.2, 0.6, 4.6, -0.7, -0.8, 2.3, -0.9, 1.8, 0.8, -2.3, 0.9, -1.8,
              0.4, 0.5, -1.8, 2.8, -0.4, -0.5, 1.8, -2.8, -1.3, -1.1, -0.2, 2.9, 1.3, 1.1, 0.2, -2.9),
            dim = c(4L, 8L),
            dimnames = list(
                c("Coke", "Pepsi", "Coke Zero", "Pepsi Max"),
                c("Male", "Female", "Male", "Female", "Male", "Female", "Male", "Female")
            )
        ),
        statistic = "z-Statistic",
        class = c("matrix", "array", "QTable"),
        span = list(
            rows = data.frame(c("Coke", "Pepsi", "Coke Zero", "Pepsi Max"), fix.empty.names = FALSE),
            columns = data.frame(
                rep(c("Feminine", "Health-conscious", "Innocent", "Older"), each = 2L),
                rep(c("Male", "Female"), 4L),
                fix.empty.names = FALSE
            )
        ),
        QStatisticsTestingInfo = data.frame(
            significancearrowratio = c(0.2, 0.2, 0, 0, 0, 0, 0, 0, 0, 0, 0.2, 0.2, 0, 0, 0, 0,
                                       0.8, 0.8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.4, 0.4, 0.4, 0.4),
            significancedirection = c("Down", "Up", "None")[c(1, 2, 3, 3, 3, 3, 3, 3, 3, 3, 2, 1, 3, 3, 3, 3,
                                                              1, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 2, 1, 2, 1)],
            significancefontsizemultiplier = c(0.5, 1.9, 1, 1, 1, 1, 1, 1, 1, 1, 1.9, 0.5, 1, 1, 1, 1,
                                               0.2, 4.4, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2.8, 0.3, 2.8, 0.3),
            significanceissignificant = as.logical(c(1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0,
                                                     1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1)),
            zstatistic = c(-2.2, 2.2, -0.8, 0.8, 0.4, -0.4, -1.3, 1.3, -0.6, 0.6, 2.3, -2.3, 0.5, -0.5, -1.1, 1.1,
                           -4.6, 4.6, -0.9, 0.9, -1.8, 1.8, -0.2, 0.2, 0.7, -0.7, 1.8, -1.8, 2.8, -2.8, 2.9, -2.9),
            pcorrected = c(0.0, 0.0, 0.3, 0.3, 0.6, 0.6, 0.1, 0.1, 0.4, 0.4, 0.0, 0.0, 0.5, 0.5, 0.2, 0.2,
                           0, 0, 0.3, 0.3, 0.0, 0.0, 0.8, 0.8, 0.4, 0.4, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)
        ),
        questiontypes = c("PickAnyGrid", "PickOne"),
        name = "rule.tbl",
        questions = c("q5", "Gender")
    )
    qtable.with.rule.grid.in.cols <- structure(
        array(
            c(4.7, -4.7, 2.2, -2.2, 0.7, -0.7, 0.7, -0.7, 0.9, -0.9, 0.9, -0.9, -2.4, 2.4, 1.8, -1.8,
              1.8, -1.8, -0.4, 0.4, -0.6, 0.6, 2.8, -2.8, 0.2, -0.2, 1.4, -1.4, 1.2, -1.2, 3, -3),
            dim = c(8L, 4L),
            dimnames = list(
                c("Female", "Male", "Female", "Male", "Female", "Male", "Female", "Male"),
                c("Feminine", "Health-conscious", "Innocent", "Older")
            )
        ),
        statistic = "z-Statistic",
        class = c("matrix", "array", "QTable"),
        span = list(
            columns = data.frame(c("Feminine", "Health-conscious", "Innocent", "Older"), fix.empty.names = FALSE),
            rows = data.frame(
                rep(c("Coke", "Pepsi", "Coke Zero", "Pepsi Max"), each = 2L),
                rep(c("Male", "Female"), 4L),
                fix.empty.names = FALSE
            )
        ),
        QStatisticsTestingInfo = data.frame(
            significancearrowratio = c(0.2, 0.2, 0, 0, 0, 0, 0, 0, 0, 0, 0.2, 0.2, 0, 0, 0, 0,
                                       0.8, 0.8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.4, 0.4, 0.4, 0.4),
            significancedirection = c("Down", "Up", "None")[c(1, 2, 3, 3, 3, 3, 3, 3, 3, 3, 2, 1, 3, 3, 3, 3,
                                                              1, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 2, 1, 2, 1)],
            significancefontsizemultiplier = c(0.5, 1.9, 1, 1, 1, 1, 1, 1, 1, 1, 1.9, 0.5, 1, 1, 1, 1,
                                               0.2, 4.4, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2.8, 0.3, 2.8, 0.3),
            significanceissignificant = as.logical(c(1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0,
                                                     1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1)),
            zstatistic = c(4.7, 0.9, 1.8, 0.2, -4.7, -0.9, -1.8, -0.2, 2.2, 0.9, -0.4, 1.4, -2.2, -0.9, 0.4, -1.4,
                           0.7, -2.4, -0.6, 1.2, -0.7, 2.4, 0.6, -1.2, 0.7, 1.8, 2.8, 3, -0.7, -1.8, -2.8, -3),
            pcorrected = c(0.0, 0.0, 0.3, 0.3, 0.6, 0.6, 0.1, 0.1, 0.4, 0.4, 0.0, 0.0, 0.5, 0.5, 0.2, 0.2,
                           0, 0, 0.3, 0.3, 0.0, 0.0, 0.8, 0.8, 0.4, 0.4, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)
        ),
        questiontypes = c("PickOne", "PickAnyGrid"),
        name = "t.rule.tbl",
        questions = c("Gender", "q5")
    )

    qtable.with.rule.multi.stat <- array(
        c(qtable.with.rule, rep(1, length(qtable.with.rule))),
        dim = c(dim(qtable.with.rule), 2L),
        dimnames = c(dimnames(qtable.with.rule), list(c("z-Statistic", "Not Duplicate")))
    )
    mostattributes(qtable.with.rule.multi.stat) <- attributes(qtable.with.rule)
    class(qtable.with.rule.multi.stat) <- c("array", "QTable")
    dim(qtable.with.rule.multi.stat) <- c(dim(qtable.with.rule), 2L)
    dimnames(qtable.with.rule.multi.stat) <- c(
        dimnames(qtable.with.rule),
        list(c("z-Statistic", "Not Duplicate"))
    )
    attr(qtable.with.rule.multi.stat, "statistic") <- NULL

    qtable.with.rule.grid.in.cols.multi.stat <- array(
        c(qtable.with.rule.grid.in.cols, rep(1, length(qtable.with.rule.grid.in.cols))),
        dim = c(dim(qtable.with.rule.grid.in.cols), 2L),
        dimnames = c(dimnames(qtable.with.rule.grid.in.cols), list(c("z-Statistic", "Not Duplicate")))
    )
    mostattributes(qtable.with.rule.grid.in.cols.multi.stat) <- attributes(qtable.with.rule.grid.in.cols)
    class(qtable.with.rule.grid.in.cols.multi.stat) <- c("array", "QTable")
    dim(qtable.with.rule.grid.in.cols.multi.stat) <- c(dim(qtable.with.rule.grid.in.cols), 2L)
    dimnames(qtable.with.rule.grid.in.cols.multi.stat) <- c(
        dimnames(qtable.with.rule.grid.in.cols),
        list(c("z-Statistic", "Not Duplicate"))
    )
    attr(qtable.with.rule.grid.in.cols.multi.stat, "statistic") <- NULL

    expect_warning(
        subscripted.rule.table <- qtable.with.rule[, c("Male", "Female")],
        "Duplicate labels present in the input table: Male, Female.",
        fixed = TRUE
    )
    original.q.stat.info <- attr(qtable.with.rule, "QStatisticsTestingInfo")[["zstatistic"]]
    relevant.ind <- rep(rep(c(TRUE, FALSE), c(2L, 6L)), 4L)
    expect_equal(
        t(subscripted.rule.table) |> as.vector(),
        original.q.stat.info[relevant.ind]
    )
    expect_equal(
        attr(subscripted.rule.table, "QStatisticsTestingInfo")[["zstatistic"]],
        original.q.stat.info[relevant.ind]
    )
    expect_silent(
        output <- qtable.with.rule.multi.stat[, 1:2, 1]
    )
    expect_equal(
        attr(output, "QStatisticsTestingInfo")[["zstatistic"]],
        original.q.stat.info[relevant.ind]
    )
    expect_warning(
        qtable.with.rule.grid.in.cols[c("Male", "Female"), ],
        "Duplicate labels present in the input table: Female, Male.",
        fixed = TRUE
    )
    subscripted.table.grid.in.cols.with.rule <- qtable.with.rule.grid.in.cols[, 1:2]
    original.q.stat.info <- attr(qtable.with.rule.grid.in.cols, "QStatisticsTestingInfo")[["zstatistic"]]
    relevant.ind <- rep(rep(c(TRUE, FALSE), each = 2L), 8L)
    expect_equal(
        t(subscripted.table.grid.in.cols.with.rule) |> as.vector(),
        original.q.stat.info[relevant.ind]
    )
    expect_silent(
        s.table.grid.in.cols.rule.multi.stat <- qtable.with.rule.grid.in.cols.multi.stat[, 1:2, 1]
    )
    expect_equal(
        attr(s.table.grid.in.cols.rule.multi.stat, "QStatisticsTestingInfo")[["zstatistic"]],
        original.q.stat.info[relevant.ind]
    )

    expect_silent(
        subscripted.normal.table <- qtable.with.no.rule[, 1:2]
    )
    expect_equal(
        attr(subscripted.rule.table, "QStatisticsTestingInfo"),
        attr(subscripted.normal.table, "QStatisticsTestingInfo")
    )
})



test_that("DS-5046 Mathematical operators don't play nicely with subscripted QTables", {
    # Testing that mathematical operations applied to subscripted QTables
    # produce identical values to the equivalent non-QTable versions.


    # Test case 1 includes a bunch of NAs and Infs so good for
    # checking NA handling.
    qtbl <- tbls[["PickOne.by.PickOne"]]
    non.qtbl <- unclass(qtbl)
    first.idx <- 1:2
    second.idx <- 2L
    # Division test
    expected.division <- non.qtbl[, first.idx] / non.qtbl[, second.idx]
    qtbl.division <- qtbl[, first.idx] / qtbl[, second.idx]
    attributes(expected.division) <- attributes(unclass(qtbl[, first.idx]))
    class(expected.division) <- c("QTable", "matrix", "array")
    expect_equal(qtbl.division, expected.division)
    qtbl.1d.by.scalar <- qtbl[, 2] / 5
    expected.division <- non.qtbl[, 2] / 5
    attributes(expected.division) <- attributes(unclass(qtbl[, 2]))
    class(expected.division) <- c("QTable", "numeric")
    expect_equal(qtbl.1d.by.scalar, expected.division)
    # Multiplication test
    qtbl.multiplication <- qtbl[, first.idx] * qtbl[, second.idx]
    expected.multiplication <- non.qtbl[, first.idx] * non.qtbl[, second.idx]
    attributes(expected.multiplication) <- attributes(unclass(qtbl[, first.idx]))
    class(expected.multiplication) <- c("QTable", "matrix", "array")
    expect_equal(qtbl.multiplication, expected.multiplication)
    # Addition test
    qtbl.addition <- qtbl[, first.idx] + qtbl[, second.idx]
    expected.addition <- non.qtbl[, first.idx] + non.qtbl[, second.idx]
    attributes(expected.addition) <- attributes(unclass(qtbl[, first.idx]))
    class(expected.addition) <- c("QTable", "matrix", "array")
    expect_equal(qtbl.addition, expected.addition)
    # Subtraction test
    expected.subtraction <- non.qtbl[, first.idx] - non.qtbl[, second.idx]
    qtbl.subtraction <- qtbl[, first.idx] - qtbl[, second.idx]
    attributes(expected.subtraction) <- attributes(unclass(qtbl[, first.idx]))
    class(expected.subtraction) <- c("QTable", "matrix", "array")
    expect_equal(qtbl.subtraction, expected.subtraction)

    qtbl <- tbls[["PickAnyGrid"]]
    non.qtbl <- unclass(qtbl)
    first.idx <- c(1, 2)
    second.idx <- 3
    # Division test
    expected.division <- non.qtbl[, first.idx] / non.qtbl[, second.idx]
    qtbl.division <- qtbl[, first.idx] / qtbl[, second.idx]
    attributes(expected.division) <- attributes(unclass(qtbl[, first.idx]))
    class(expected.division) <- c("QTable", class(expected.division))
    expect_equal(qtbl.division, expected.division)
    # Multiplication test
    qtbl.multiplication <- qtbl[, first.idx] * qtbl[, second.idx]
    expected.multiplication <- non.qtbl[, first.idx] * non.qtbl[, second.idx]
    attributes(expected.multiplication) <- attributes(unclass(qtbl[, first.idx]))
    class(expected.multiplication) <- c("QTable", class(expected.multiplication))
    expect_equal(qtbl.multiplication, expected.multiplication)
    # Addition test
    qtbl.addition <- qtbl[, first.idx] + qtbl[, second.idx]
    expected.addition <- non.qtbl[, first.idx] + non.qtbl[, second.idx]
    attributes(expected.addition) <- attributes(unclass(qtbl[, first.idx]))
    class(expected.addition) <- c("QTable", class(expected.addition))
    expect_equal(qtbl.addition, expected.addition)
    # Subtraction test
    expected.subtraction <- non.qtbl[, first.idx] - non.qtbl[, second.idx]
    qtbl.subtraction <- qtbl[, first.idx] - qtbl[, second.idx]
    attributes(expected.subtraction) <- attributes(unclass(qtbl[, first.idx]))
    class(expected.subtraction) <- c("QTable", class(expected.subtraction))
    expect_equal(qtbl.subtraction, expected.subtraction)

    # Only second member of each pair is subscripted.

    expected.division <- non.qtbl / non.qtbl[, second.idx]
    class(expected.division) <- c("QTable", class(expected.division))
    expect_equal(
        qtbl / qtbl[, second.idx],
        expected.division
    )
    expected.multiplication <- non.qtbl * non.qtbl[, second.idx]
    class(expected.multiplication) <- c("QTable", class(expected.multiplication))
    expect_equal(
        qtbl * qtbl[, second.idx],
        expected.multiplication
    )
    expected.addition <- non.qtbl + non.qtbl[, second.idx]
    class(expected.addition) <- c("QTable", class(expected.addition))
    expect_equal(
        qtbl + qtbl[, second.idx],
        expected.addition
    )
    expected.subtraction <- non.qtbl - non.qtbl[, second.idx]
    class(expected.subtraction) <- c("QTable", class(expected.subtraction))
    expect_equal(
        qtbl - qtbl[, second.idx],
        expected.subtraction
    )

    # Higher Dimensional Table
    qtbl <- tbls[["PickAnyGrid.by.PickOne"]]
    non.qtbl <- unclass(qtbl)
    first.idx <- c(1, 2)
    second.idx <- 3
    expect_equal(
        qtbl[, first.idx, ] / qtbl[, second.idx, ],
        non.qtbl[, first.idx, ] / non.qtbl[, second.idx, ]
    )
    expect_equal(
        qtbl[, first.idx, ] * qtbl[, second.idx, ],
        non.qtbl[, first.idx, ] * non.qtbl[, second.idx, ]
    )
    expect_equal(
        qtbl[, first.idx, ] + qtbl[, second.idx, ],
        non.qtbl[, first.idx, ] + non.qtbl[, second.idx, ]
    )
    expect_equal(
        qtbl[, first.idx, ] - qtbl[, second.idx, ],
        non.qtbl[, first.idx, ] - non.qtbl[, second.idx, ]
    )

    # Multiple statistics
    load("numeric.grid.nominal.with.multiple.stats.qtable.rda")
    qtbl <- numeric.grid.nominal.with.multiple.stats.qtable
    non.qtbl <- unclass(qtbl)
    first.idx <- 1:2
    second.idx <- 1

    # Columns
    expect_equal(
        qtbl[, first.idx, 1, 1] / qtbl[, second.idx, 1, 1],
        non.qtbl[, first.idx, 1, 1] / non.qtbl[, second.idx, 1, 1]
    )
    expect_equal(
        qtbl[, first.idx, 1, 1] * qtbl[, second.idx, 1, 1],
        non.qtbl[, first.idx, 1, 1] * non.qtbl[, second.idx, 1, 1]
    )
    expect_equal(
        qtbl[, first.idx, 1, 1] + qtbl[, second.idx, 1, 1],
        non.qtbl[, first.idx, 1, 1] + non.qtbl[, second.idx, 1, 1]
    )
    expect_equal(
        qtbl[, first.idx, 1, 1] - qtbl[, second.idx, 1, 1],
        non.qtbl[, first.idx, 1, 1] - non.qtbl[, second.idx, 1, 1]
    )

    # Rows

    expect_equal(
        qtbl[first.idx, 1, 1, 1] / qtbl[second.idx, 1, 1, 1],
        non.qtbl[first.idx, 1, 1, 1] / non.qtbl[second.idx, 1, 1, 1]
    )
    expect_equal(
        qtbl[first.idx, 1, 1, 1] * qtbl[second.idx, 1, 1, 1],
        non.qtbl[first.idx, 1, 1, 1] * non.qtbl[second.idx, 1, 1, 1]
    )
    expect_equal(
        qtbl[first.idx, 1, 1, 1] + qtbl[second.idx, 1, 1, 1],
        non.qtbl[first.idx, 1, 1, 1] + non.qtbl[second.idx, 1, 1, 1]
    )
    expect_equal(
        qtbl[first.idx, 1, 1, 1] - qtbl[second.idx, 1, 1, 1],
        non.qtbl[first.idx, 1, 1, 1] - non.qtbl[second.idx, 1, 1, 1]
    )

    # 3rd Dim
    expect_equal(
        qtbl[1, 1, first.idx, 1] / qtbl[1, 1, second.idx, 1],
        non.qtbl[1, 1, first.idx, 1] / non.qtbl[1, 1, second.idx, 1]
    )
    expect_equal(
        qtbl[1, 1, first.idx, 1] * qtbl[1, 1, second.idx, 1],
        non.qtbl[1, 1, first.idx, 1] * non.qtbl[1, 1, second.idx, 1]
    )
    expect_equal(
        qtbl[1, 1, first.idx, 1] + qtbl[1, 1, second.idx, 1],
        non.qtbl[1, 1, first.idx, 1] + non.qtbl[1, 1, second.idx, 1]
    )
    expect_equal(
        qtbl[1, 1, first.idx, 1] - qtbl[1, 1, second.idx, 1],
        non.qtbl[1, 1, first.idx, 1] - non.qtbl[1, 1, second.idx, 1]
    )

    # One stat by another
    expect_equal(
        qtbl[, , , 1] / qtbl[, , , 2],
        non.qtbl[, , , 1] / non.qtbl[, , , 2]
    )
    expect_equal(
        qtbl[, , , 1] * qtbl[, , , 2],
        non.qtbl[, , , 1] * non.qtbl[, , , 2]
    )
    expect_equal(
        qtbl[, , , 1] + qtbl[, , , 2],
        non.qtbl[, , , 1] + non.qtbl[, , , 2]
    )
    expect_equal(
        qtbl[, , , 1] - qtbl[, , , 2],
        non.qtbl[, , , 1] - non.qtbl[, , , 2]
    )

})

test_that("DS-5072 Ensure subscripted table dimensions/str matches base R", {
    tbls <- readRDS("qTablesWithZStatInCells.rds")
    scalar <- structure(
        array(0.67, dim = 1L, dimnames = list("Age")),
        statistic = "Average",
        class = "QTable",
        questiontypes = "Number",
        questions = c("Age", "SUMMARY"),
        name = "Age - Average",
        QStatisticsTestingInfo = data.frame(
            significancesignificant = FALSE,
            zstatistic = -.1,
            pcorrected = 0.29
        ),
        footerhtml = "Age - Average <br/>N = 1000"
    )
    single.dim <- Filter(function(x) getDimensionLength(x) == 1L, tbls)[[1L]]
    two.dim <- Filter(function(x) getDimensionLength(x) == 2L && !isMultiStatTable(x), tbls)[[1L]]
    single.dim.multi.stat <- array(rep(single.dim, 2L))
    mostattributes(single.dim.multi.stat) <- attributes(single.dim)
    attr(single.dim.multi.stat, "statistic") <- NULL
    dim(single.dim.multi.stat) <- c(length(single.dim), 2L)
    dimnames(single.dim.multi.stat) <- list(dimnames(single.dim)[[1L]], c("z-Statistic", "some interesting stat"))
    attr(single.dim.multi.stat, "span")[["columns"]] <- data.frame(
        dimnames(single.dim.multi.stat)[[2L]],
        fix.empty.names = FALSE
    )
    two.dim.multi.stat <- rep(two.dim, 2L)
    mostattributes(two.dim.multi.stat) <- attributes(two.dim)
    attr(two.dim.multi.stat, "statistic") <- NULL
    dim(two.dim.multi.stat) <- c(dim(two.dim), 2L)
    dimnames(two.dim.multi.stat) <- c(
        dimnames(two.dim),
        list(c("z-Statistic", "some interesting stat"))
    )
    # Create unclassed versions of the tables so they are subscripted using the base operator
    base.scalar <- unclass(scalar)
    base.single.dim <- unclass(single.dim)
    base.single.dim.multi.stat <- unclass(single.dim.multi.stat)
    base.two.dim <- unclass(two.dim)
    base.two.dim.multi.stat <- unclass(two.dim.multi.stat)
    # scalar
    subscripted.scalar <- scalar[1L]
    base.subscripted.scalar <- base.scalar[1L]
    expect_is(subscripted.scalar, "numeric")
    expect_false(is.array(subscripted.scalar))
    expect_identical(
        setdiff(class(subscripted.scalar), "QTable"),
        class(base.subscripted.scalar)
    )
    attr(subscripted.scalar, "subscripted.footerhtml") |> expect_equal(attr(scalar, "footerhtml"))
    # Can be subscripted again
    subscripted.subscripted.scalar <- subscripted.scalar[1L]
    expect_true(attr(subscripted.subscripted.scalar, "original.is.subscripted"))
    attr(subscripted.subscripted.scalar, "original.is.subscripted") <- NULL
    attr(subscripted.subscripted.scalar, "name") <- "some.table"
    waldo::compare(subscripted.subscripted.scalar, subscripted.scalar)
    # 1d
    subscripted.single.dim <- single.dim[1:3]
    base.subscripted.single.dim <- base.single.dim[1:3]
    expect_is(subscripted.single.dim, "QTable")
    expect_is(subscripted.single.dim, "array")
    expect_setequal(
        setdiff(class(subscripted.single.dim), "QTable"),
        class(base.subscripted.single.dim)
    )
    expect_true(is.array(subscripted.single.dim))
    expect_true(is.array(base.subscripted.single.dim))
    expect_equal(dim(subscripted.single.dim), dim(base.subscripted.single.dim))
    # Can be subscripted again
    subscripted.subscripted.single.dim <- subscripted.single.dim[1:2]
    expect_equal(
        as.vector(subscripted.subscripted.single.dim),
        as.vector(unclass(subscripted.single.dim[1:2]))
    )
    # 1d to scalar
    subscripted.single.dim <- single.dim[1]
    base.subscripted.single.dim <- base.single.dim[1]
    expect_is(subscripted.single.dim, "QTable")
    expect_is(subscripted.single.dim, "numeric")
    expect_setequal(
        setdiff(class(subscripted.single.dim), "QTable"),
        class(base.subscripted.single.dim)
    )
    expect_true(!is.array(subscripted.single.dim) && length(subscripted.single.dim) == 1L)
    expect_true(is.vector(base.subscripted.single.dim) && length(base.subscripted.single.dim) == 1L)
    # 1d - multi stat
    subscripted.single.dim.multi.stat <- single.dim.multi.stat[1:3, 1]
    base.subscripted.single.dim.multi.stat <- base.single.dim.multi.stat[1:3, 1]
    expect_is(subscripted.single.dim.multi.stat, "QTable")
    expect_is(subscripted.single.dim.multi.stat, "numeric")
    expect_setequal(
        setdiff(class(subscripted.single.dim.multi.stat), "QTable"),
        class(base.subscripted.single.dim.multi.stat)
    )
    expect_false(is.array(subscripted.single.dim.multi.stat))
    expect_false(is.array(base.subscripted.single.dim.multi.stat))
    expect_null(dim(subscripted.single.dim.multi.stat))
    expect_null(dim(base.subscripted.single.dim.multi.stat))
    # 2d stays as matrix
    subscripted.two.dim <- two.dim[1:3, 2:3]
    base.subscripted.two.dim <- base.two.dim[1:3, 2:3]
    expect_is(subscripted.two.dim, "QTable")
    expect_is(subscripted.two.dim, "array")
    expect_setequal(
        setdiff(class(subscripted.two.dim), "QTable"),
        class(base.subscripted.two.dim)
    )
    expect_true(is.array(subscripted.two.dim))
    expect_true(is.array(base.subscripted.two.dim))
    expect_gt(length(dim(subscripted.two.dim)), 1L)
    expect_equal(dim(subscripted.two.dim), dim(base.subscripted.two.dim))
    # 2d when dropped becomes vector
    subscripted.two.dim <- two.dim[1:3, 2]
    base.subscripted.two.dim <- base.two.dim[1:3, 2]
    expect_is(subscripted.two.dim, "QTable")
    expect_false(is.array(subscripted.two.dim))
    expect_setequal(
        setdiff(class(subscripted.two.dim), "QTable"),
        class(base.subscripted.two.dim)
    )
    expect_false(is.array(base.subscripted.two.dim))
    expect_true(is.vector(base.subscripted.two.dim))
    expect_null(dim(subscripted.two.dim))
    expect_equal(dim(subscripted.two.dim), dim(base.subscripted.two.dim))
    # 2d when not dropped stays as matrix
    subscripted.two.dim <- two.dim[1:3, 2, drop = FALSE]
    base.subscripted.two.dim <- base.two.dim[1:3, 2, drop = FALSE]
    expect_is(subscripted.two.dim, "QTable")
    expect_setequal(
        setdiff(class(subscripted.two.dim), "QTable"),
        class(base.subscripted.two.dim)
    )
    expect_true(is.matrix(subscripted.two.dim))
    expect_true(is.matrix(base.subscripted.two.dim))
    expect_gt(length(dim(subscripted.two.dim)), 1L)
    expect_equal(dim(subscripted.two.dim), dim(base.subscripted.two.dim))
    # 2d multi-stat to vector
    subscripted.two.dim.multi.stat <- two.dim.multi.stat[1:3, 1, 2]
    base.subscripted.two.dim.multi.stat <- base.two.dim.multi.stat[1:3, 1, 2]
    expect_is(subscripted.two.dim.multi.stat, "QTable")
    expect_is(subscripted.two.dim.multi.stat, "numeric")
    expect_setequal(
        setdiff(class(subscripted.two.dim.multi.stat), "QTable"),
        class(base.subscripted.two.dim.multi.stat)
    )
    expect_false(is.array(subscripted.two.dim.multi.stat))
    expect_false(is.array(base.subscripted.two.dim.multi.stat))
    expect_null(dim(subscripted.two.dim.multi.stat))
    expect_null(dim(base.subscripted.two.dim.multi.stat))
})

duplicate.labels.tests <- readRDS("DS-5079_SubscriptingWithDuplicateLabels.rds")
for (test in duplicate.labels.tests)
    with(test, test_that(test.name, {
        suppressWarnings(eval(test.code))
    }))

test_that("SAQ-2077: Duplicate labels are not present in footer", {
    # Occurrences of duplicates in dimnames don't appear in footerhtml
    duplicate.1d.table <- duplicate.labels.tests[[1]][["input"]]
    attr(duplicate.1d.table, "name") <- "Exercise frequency"
    subscripted.table <- duplicate.1d.table[2:4, 1]
    names(subscripted.table) |> expect_equal(c("Less often", "Never", "Never_@_1"))
    attr(subscripted.table, "subscripted.footerhtml") |>
        startsWith(prefix = "Exercise frequency(Less often, Never), (%) SUMMARY<br />") |>
        expect_true()
    attr(subscripted.table, "footerhtml") |> expect_null()
    attr(duplicate.1d.table, "footerhtml") |>
        startsWith(prefix = "Exercise frequency SUMMARY<br />") |>
        expect_true()
    # Flattened Table disambiguation
    duplicate.2d.table.with.spans <- duplicate.labels.tests[[3]][["input"]]
    attr(duplicate.2d.table.with.spans, "name") <- "Preferred cola small by BANNER2"
    subscripted.banner.table <- duplicate.2d.table.with.spans[c("Coke", "Pepsi"), c(1:2, 4:5), 1]
    colnames(subscripted.banner.table) |>
        expect_equal(rep(c("Low", "High"), 2L))
    attr(subscripted.banner.table, "subscripted.footerhtml") |>
        startsWith(
            prefix = paste0(
                "Preferred cola small by BANNER2(Coke, Pepsi), ",
                "(Male - Low, Male - High, Female - Low, Female - High), ",
                "(Column %)<br />"
            )
        ) |>
        expect_true()
    attr(duplicate.2d.table.with.spans, "footerhtml") |>
        startsWith(prefix = "Preferred cola small by BANNER2<br />") |>
        expect_true()
})

test_that("DS-5090, DS-5135: Warning thrown if duplicate labels present in input",
{
    input <- duplicate.labels.tests[[4]]$input
    expect_silent(input["Coke", , ])
    expect_warning(input[, c("Low", "High"), ],
                   "Duplicate labels present in the input table: Low, High.")
})

test_that("DS-5120 Turn off subcripting in Q", {
    if (!is.null(get0("productName", envir = .GlobalEnv)))
        rm("productName", envir = .GlobalEnv)
    if (!is.null(get0("allowQTableSubscripting", envir = .GlobalEnv)))
        rm("allowQTableSubscripting", envir = .GlobalEnv)
    expect_true(qTableSubscriptingPermitted())
    assign("productName", "Q", envir = .GlobalEnv)
    expect_false(qTableSubscriptingPermitted())
    expect_equal(tbls[[1]][1], unclass(tbls[[1]][1]))
    rm("productName", envir = .GlobalEnv)
    assign("allowQTableSubscripting", function() { FALSE }, envir = .GlobalEnv)
    expect_false(qTableSubscriptingPermitted())
    assign("productName", "Q", envir = .GlobalEnv)
    expect_false(qTableSubscriptingPermitted())
    assign("allowQTableSubscripting", function() { TRUE }, envir = .GlobalEnv)
    expect_true(qTableSubscriptingPermitted())
    rm("productName", envir = .GlobalEnv)
    rm("allowQTableSubscripting", envir = .GlobalEnv)
})

test_that("DS-5129: Subscripting twice with duplicate labels",
{
    input <- duplicate.labels.tests[[2]]$input
    output <- suppressWarnings(input[1:4, , ][, 1:2, 2])
    expected <- as.numeric(t(output))
    z.stat.output <- attr(output, "QStatisticsTestingInfo")[, "zstatistic"]
    dim(z.stat.output) <- NULL
    expect_equal(expected, z.stat.output, ignore_attr = TRUE)
})

test_that("DS-5149 - Permute order of 1d table", {
    tbls.1d <- Filter(function(x) getDimensionLength(x) == 1L, tbls)
    directions <- c(TRUE, FALSE)
    for (tbl in tbls.1d)
        for (direction in directions) {
            expected.values <- sort(as.vector(tbl), decreasing = direction)
            expect_silent(output <- tbl[order(tbl, decreasing = direction)])
            expect_equal(as.vector(output), expected.values)
            expect_equal(
                as.vector(attr(output, "QStatisticsTestingInfo")[["zstatistic"]]),
                expected.values
            )
        }
    # 1d test with multi-stat
    tbls.1d.with.multi.stat <- lapply(tbls.1d, function(x) {
        y <- cbind(x, -x)
        attr(x, "statistic") <- NULL
        mostattributes(y) <- attributes(x)
        dim(y) <- c(length(x), 2L)
        dimnames(y) <- list(dimnames(x)[[1]], c("z-Statistic", "other stat"))
        y
    })
    clobberTable <- function(x) {
        y <- as.vector(x)
        attributes(y) <- attributes(x)[c("dim", "dimnames")]
        y
    }
    # 2d tests and 1d with multi-stat
    tbls.2d <- Filter(function(x) getDimensionLength(x) == 2L && !anyNA(x), tbls)
    for (tbl in c(tbls.2d, tbls.1d.with.multi.stat)) {
        indices <- lapply(dim(tbl), sample.int)
        basic.table <- unclass(tbl)
        expected.values <- basic.table[indices[[1]], indices[[2]]]
        expect_error(output <- tbl[indices[[1]], indices[[2]]], NA)
        expect_equal(clobberTable(output), expected.values)
        is.multi.stat <- is.null(attr(tbl, "statistic"))
        if (is.multi.stat) {
            expected.q.stat <- as.vector(basic.table[indices[[1]], 1])
        } else
            expected.q.stat <- as.vector(t(expected.values))
        expect_equal(
            as.vector(attr(output, "QStatisticsTestingInfo")[["zstatistic"]]),
            expected.q.stat
        )
    }
})

test_that("Multiple stats and dropping", {
    # PickOne with multiple stats
    x <- structure(
        array(1:6, dim = c(3L, 2L), dimnames = list(LETTERS[1:3], c("%", "Count"))),
        questiontypes = "PickOne",
        name = "some.table",
        QStatisticsTestingInfo = data.frame(
            zstatistic = c(-1.71, 1.71, 24.98)
        ),
        class = c("QTable", "matrix", "array")
    )
    # Down to a scalar without dropping
    expected.output <- unclass(x)["A", "Count", drop = FALSE]
    attributes(expected.output) <- list(
        dim = c(1L, 1L),
        dimnames = list("A", "Count"),
        QStatisticsTestingInfo = data.frame(
            zstatistic = -1.71
        ),
        questiontypes = "PickOne",
        original.questiontypes = "PickOne",
        class = c("QTable", "matrix", "array"),
        name = "some.table",
        is.subscripted = TRUE,
        mapped.dimnames = list(
            Row = "A",
            Statistic = "Count"
        )
    )
    expect_equal(
        x["A", "Count", drop = FALSE],
        expected.output
    )
    # Down to two values without dropping
    expected.output <- unclass(x)[c("A", "C"), "Count", drop = FALSE]
    attributes(expected.output) <- list(
        dim = c(2, 1L),
        dimnames = list(c("A", "C"), "Count"),
        QStatisticsTestingInfo = structure(
            list(
                Row = factor(c("A", "C")),
                zstatistic = c(-1.71, 24.98)
            ),
            row.names = c(1L, 3L),
            class = "data.frame"
        ),
        questiontypes = "PickOne",
        original.questiontypes = "PickOne",
        class = c("QTable", "matrix", "array"),
        name = "some.table",
        is.subscripted = TRUE,
        mapped.dimnames = list(
            Row = c("A", "C"),
            Statistic = "Count"
        )
    )
    expect_equal(
        x[c("A", "C"), "Count", drop = FALSE],
        expected.output
    )
})

test_that("i and j arguments allowed", {
    n <- 6L
    values <- array(
        runif(n),
        dim = c(n, 2L),
        dimnames = list(LETTERS[1:n], c("Male", "Female"))
    )
    qstat.info <- data.frame(zstatistic = runif(n * 2L, min = -3, max = 3))
    x <- structure(
        array(runif(n), dim = c(n, 2L), dimnames = list(LETTERS[1:n], c("Male", "Female"))),
        QStatisticsTestingInfo = qstat.info,
        class = c("matrix", "array", "QTable")
    )
    # [ subscripting ok
    expect_equal(
        x[i = LETTERS[1:3], j = c("Male", "Female")],
        x[LETTERS[1:3], c("Male", "Female")]
    )
    expect_equal(
        x[i = LETTERS[1:3], c("Male", "Female")],
        x[LETTERS[1:3], c("Male", "Female")]
    )
    expect_equal(
        x[LETTERS[1:3], j = c("Male", "Female")],
        x[LETTERS[1:3], c("Male", "Female")]
    )
    ## Able to use do.call(`[`, args), the name is messed up though
    expected.output <- x[LETTERS[1:3], c("Male", "Female")]
    expect_equal(
        do.call(`[`, list(x = x, i = LETTERS[1:3], j = c("Male", "Female"))),
        expected.output
    )
    expect_equal(
        do.call(`[`, list(x = x, i = LETTERS[1:3], c("Male", "Female"))),
        expected.output
    )
    expect_equal(
        do.call(`[`, list(x = x, LETTERS[1:3], j = c("Male", "Female"))),
        expected.output
    )
    # [[ also works
    expect_equal(
        x[[i = LETTERS[2], j = "Male"]],
        x[[LETTERS[2], "Male"]]
    )
    expect_equal(
        x[[i = LETTERS[2], "Male"]],
        x[[LETTERS[2], "Male"]]
    )
    expect_equal(
        x[[LETTERS[2], j = "Male"]],
        x[[LETTERS[2], "Male"]]
    )
    ## Able to use do.call(`[[`, args), the name is messed up though
    expected.output <- x[[LETTERS[2], "Male"]]
    expect_equal(
        do.call(`[[`, list(x = x, i = LETTERS[2], j = "Male")),
        expected.output
    )
    expect_equal(
        do.call(`[[`, list(x = x, i = LETTERS[2], "Male")),
        expected.output
    )
    expect_equal(
        do.call(`[[`, list(x = x, LETTERS[2], j = "Male")),
        expected.output
    )
})

test_that("DS-5314: Extracting more than one stat works", {
    q.stat.info <- data.frame(zstatistic = rnorm(12))
    table.with.three.stats <- structure(
        array(1:36, dim = c(3L, 4L, 3L), dimnames = list(letters[1:3], 1:4, c("A", "B", "C"))),
        name = "some.table",
        class = c("QTable", "array"),
        questiontypes = "PickOneMulti",
        QStatisticsTestingInfo = q.stat.info
    )
    expected.table <- unclass(table.with.three.stats)[c("a", "c"), , c("A", "C")]
    expected.q.stat.info <- data.frame(
        Row = factor(rep(c("a", "c"), each = 4L)),
        Column = factor(rep(1:4, 2L)),
        zstatistic = q.stat.info[c(1:4, 9:12), ]
    )
    rownames(expected.q.stat.info) <- c(1:4, 9:12)
    attributes(expected.table) <- list(
        dim = c(2L, 4L, 2L),
        dimnames = list(c("a", "c"), 1:4, c("A", "C")),
        class = c("QTable", "array"),
        questiontypes = "PickOneMulti",
        original.questiontypes = "PickOneMulti",
        name = "some.table",
        QStatisticsTestingInfo = expected.q.stat.info,
        is.subscripted = TRUE,
        mapped.dimnames = list(
            Row = c("a", "c"),
            Column = as.character(1:4),
            Statistic = c("A", "C")
        )
    )
    expect_equal(
        table.with.three.stats[c("a", "c"), , c("A", "C")],
        expected.table
    )
})

test_that("celltext attribute is correctly subscripted in tables", {
    tbls <- readRDS("tablesWithCellText.rds")

    t <- tbls[["PickOne"]]
    expect_equal(attr(t[2:3], "celltext"), structure(c("b", "c"), dim = 2L))
    expect_equal(attr(t[c("40 to 64", "65 or more")], "celltext"), structure(c("b", "c"), dim = 2L))

    t <- tbls[["PickOneWithMultiStat"]]
    expect_equal(attr(t[2:3, 2], "celltext"), structure(c("f", "g"), dim = c(2L)))
    expect_equal(attr(t[c("40 to 64", "65 or more"), "Average"], "celltext"), structure(c("f", "g"), dim = c(2L)))

    t <- tbls[["PickOneMulti"]]
    expect_equal(attr(t[, 3:4], "celltext"), structure(c("e", "f", "g", "h"), dim = c(2L, 2L)))

    t <- tbls[["PickOneMultiWithMultiStat"]]
    expect_equal(attr(t[, 3, ], "celltext"), structure(c("e", "f", "m", "n"), dim = c(2L, 2L)))
    expect_equal(attr(t[3:10], "celltext"), structure(c("c", "d", "e", "f", "g", "h", "i", "j"), dim = c(8L)))
})

test_that("Can insert subscripting information in footer in correct place", {
    typical.footer <- "Table Foo Sample size = 10"
    table.name <- "Table Foo"
    findInsertionPointInFooter(typical.footer, name = table.name) |> expect_equal(nchar(table.name))
    # Check reversed
    name.at.end.footer <- "Sample size = 10 Table Foo"
    findInsertionPointInFooter(name.at.end.footer, name = table.name) |> expect_equal(nchar(name.at.end.footer))
    # Check what happens if table name appears multiple times
    multiple.matches.footer <- "Table Foo Sample size = 10 Table Foo"
    findInsertionPointInFooter(multiple.matches.footer, name = table.name) |> expect_equal(nchar(table.name))
    # If table name not found
    no.match.footer <- "Sample size = 10"
    findInsertionPointInFooter(no.match.footer, name = table.name) |> expect_equal(-1L)
})
