context("Subscripting QTables")

arrayAsTable <- function(dims, dimnames = NULL) {
    if (missing(dims))
        stop("dims argument required")
    output <- array(sample(1:100, size = prod(dims), replace = TRUE), dim = dims, dimnames = dimnames)
    class(output) <- c("qTable", class(output))
    attr(output, "statistic") <- "Average"
    if (!is.null(dimnames))
        output <- verbs:::nameDimensionAttributes(output)
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
    do.call(`[`, args)
}
expectedSingleTable <- function(tab, ind, drop = NULL) {
    y <- singleSubscriptTable(unclass(tab), ind, drop)
    class(y) <- c("qTable", class(y))
    orig.name <- paste0("table.", paste0(dim(tab), collapse = "."))
    attr(y, "original.name") <- orig.name
    attr(y, "name") <- paste0(orig.name, "[", paste0(ind, collapse = ","), "]")
    attr(y, "statistic") <- "Average"
    if (!is.array(y))
        y <- as.array(y)
    if (!is.null(dimnames(y)))
        y <- verbs:::nameDimensionAttributes(y)
    # attr(y, "questiontype") <- verbs:::getUpdatedQuestionTypes(y, tab)
    y
}
doubleSubscriptTable <- function(tab, ind, exact = NULL) {
    args <- c(list(tab), ind)
    if (!is.null(exact)) args <- c(args, exact = exact)
    do.call(`[[`, args)
}
expectedDoubleTable <- function(tab, ind, exact = NULL) {
    y <- doubleSubscriptTable(unclass(tab), ind, exact)
    class(y) <- c("qTable", class(y))
    orig.name <- paste0("table.", paste0(dim(tab), collapse = "."))
    attr(y, "original.name") <- orig.name
    attr(y, "name") <- paste0(orig.name, "[", paste0(ind, collapse = ","), "]")
    attr(y, "statistic") <- "Average"
    if (!is.array(y))
        y <- as.array(y)
    y
}

index.template <- rep(alist(, )[1L], 5L)

test_that("Check indices subscriptted correctly", {
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
        s.args <- d.args <- args
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
        valid.d.args <- lapply(d.args, "[[", 1L)
        test.table <- doubleSubscriptTable(tab, valid.d.args)
        expected <- expectedDoubleTable(tab, valid.d.args)
        expect_equal(test.table, expected)
        # Expect error if [[ used with more than one value in a dimension
        expected.double.err <-
            capture_error(throwErrorTableDoubleIndex(attr(tab, "name"), dim(tab)))[["message"]]
        expect_error(doubleSubscriptTable(tab, d.args), expected.double.err, fixed = TRUE)
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
    expect_error(x.6.5.4[1, 2],
                 capture_error(throwErrorTableIndexInvalid(attr(x.6.5.4, "name"), 6:4))[["message"]],
                 fixed = TRUE)
    expect_error(x.6.5.4[1, 2, 3, 1],
                 capture_error(throwErrorTableIndexInvalid(attr(x.6.5.4, "name"), 6:4))[["message"]],
                 fixed = TRUE)
    expect_error(x.6.5.4[1, 2, 3], NA)
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
    class(expected) <- c("qTable", class(expected))
    attr(expected, "original.name") <- "table.2.1"
    attr(expected, "name") <- "table.2.1[,1]"
    expect_equal(x.2.1[, 1, drop = FALSE], expected)

    # Dropped output has the right class
    attr(x.2.1, "statistic") <- "Average"
    x.2.1.dropped <- structure(as.vector(x.2.1), dim = 2L,
                               class = c("qTable", "integer"),
                               statistic = "Average",
                               original.name = "table.2.1",
                               dimnames = list(Row = c("A", "B")),
                               name = "table.2.1[,1]",
                               statistic = "Average")

    expect_equal(x.2.1[, 1, drop = TRUE], x.2.1.dropped)

    expected.error <- capture_error(throwErrorOnlyNamed("exact", "[["))[["message"]]
    expect_error(x.6.5.named[[2, 3, Exact = TRUE]], expected.error, fixed = TRUE)
    expect_error(x.6.5.named[[2, 3, exact = "TRUE"]], "exact argument should be TRUE or FALSE")
})

test_that("Array structure is retained", {
    expect_true(is.array(x.6.5[1][1]))
})

tbls <- readRDS("qTablesWithZStatInCells.rds")

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
        label.idx <- mapply(`[`, dimnames(tbl), arr.idx)
        out <- do.call(`[`, c(list(tbl), as.list(label.idx)))
        expect_equal(as.numeric(out), attr.zstat, check.attributes = FALSE)
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
                  if (test.case == test.cases[[1L]])
                  {
                      expected.cols <- c("Row", colnames(attr(tbl,
                                                              "QStatisticsTestingInfo")))
                      expect_equal(colnames(q.stat.info.out), expected.cols)
                  }
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
       if (test.case == test.cases[[1L]])
       {
           expected.cols <- c("Row", colnames(attr(tbl, "QStatisticsTestingInfo")))
           expect_equal(colnames(q.stat.info.out), expected.cols)
       }
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
    class(age.table) <- c("qTable", class(age.table))
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
    class(table.2d) <- c("qTable", class(table.2d))
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
    class(table.3d) <- c("qTable", class(table.3d))
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
    checkSpanAttribute(age.table[6:7], NULL)
    checkSpanAttribute(age.table[c("40 to 44", "45 to 49")], NULL)
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
    class(table.2d) <- c("qTable", class(table.2d))
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
    expected.span <- span.2d[2]
    checkSpanAttribute(table.2d[5, ], expected.span)
    checkSpanAttribute(table.2d["Pepsi Light", ], expected.span)
    ## Column checks
    ### Column span dropped but rows remain
    expected.span <- span.2d[1]
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
    checkSpanAttribute(table.2d[5, c(1, 4)], NULL)
    checkSpanAttribute(table.2d["Pepsi Light", c("Don't know", "Neither")], NULL)
    # 2d table with multiple statistics (3d array)
    table.3d <- array(rep(as.vector(table.2d), 2L), dim = c(dim(table.2d), 2L),
                      dimnames = c(dimnames(table.2d), list(c("Row %", "Expected %"))))
    attr(table.3d, "span") <- span.2d
    class(table.3d) <- c("qTable", class(table.3d))
    ### Both rows and columns ok
    expected.span <- span.2d
    expected.span[[1]] <- span.2d[[1]][1:2, , drop = FALSE]
    expected.span[[2]] <- span.2d[[2]][1:2, , drop = FALSE]
    checkSpanAttribute(table.3d[1:2, 1:2, ], expected.span)
    checkSpanAttribute(table.3d[c("Coca Cola", "Diet Coke"), c("Don't know", "Hate"), ], expected.span)
    ### Span dropped if not required
    expected.span <- span.2d[2]
    expected.span[[1]] <- span.2d[[2]][c(2, 4, 6), , drop = FALSE]
    checkSpanAttribute(table.3d[5, c(2, 4, 6), ], expected.span)
    checkSpanAttribute(table.3d["Pepsi Light", c("Hate", "Neither", "Love"), ], expected.span)
    ### Isn't affected by stat selection
    checkSpanAttribute(table.3d[5, c(2, 4, 6), 1], expected.span)
    checkSpanAttribute(table.3d["Pepsi Light", c("Hate", "Neither", "Love"), "Row %"], expected.span)
    checkSpanAttribute(table.3d[5, c(2, 4, 6), 2], expected.span)
    checkSpanAttribute(table.3d["Pepsi Light", c("Hate", "Neither", "Love"), "Expected %"], expected.span)
    checkSpanAttribute(table.3d[5, c(2, 4, 6), ], expected.span)
    checkSpanAttribute(table.3d["Pepsi Light", c("Hate", "Neither", "Love"), ], expected.span)
})

env <- new.env()
source(system.file("tests", "QTables.R", package = "verbs"), local = env)

test_that("DS-3797: Attributes renamed appropriately after subsetting",
{
    tbl <- env$qTable.2D
    attr(tbl, "customAttr") <- "FooBar"
    out <- tbl[1:2, 1:2]
    attr.names.out <- names(attributes(out))
    expected.renamed <- paste0("original.",
                             c("dimnets", "dimduplicates", "span",
                               "basedescriptiontext", "basedescription",
                               "questiontypes",
                               "footerhtml", "name", "questions"))
    expected.basic <- c("dim", "dimnames", "class", "statistic")
    expected.modified <- c("QStatisticsTestingInfo", "span", "name", "questiontypes")
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
    expect_is(q.test.info.out[, 1], "factor")
    expect_equal(q.test.info.out, expected)

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
    expect_equal(q.test.info.out, expected)

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
    checkQuestionTypesAttr(tbl[1], "PickOne")
    checkQuestionTypesAttr(tbl["Under 40"], "PickOne")
    checkQuestionTypesAttr(tbl[], "PickOne")
    checkQuestionTypesAttr(tbl[1:3, drop = FALSE], "PickOne")

    ## Number
    number.tbl <- structure(c(Age = 42), statistic = "Average", dim = 1L,
                            class = c("array", "qTable"), questiontypes = "Number")
    tbl <- number.tbl
    checkQuestionTypesAttr(tbl[1], "Number")
    checkQuestionTypesAttr(tbl[1, drop = FALSE], "Number")
    checkQuestionTypesAttr(tbl[], "Number")

    ## Number Multi
    number.multi.tbl <- structure(runif(3L), dimnames = list(c("Young", "Medium", "Old")),
                                  dim = 3L, statistic = "Average", questiontypes = "NumberMulti",
                                  class = c("array", "qTable"))
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
    checkQuestionTypesAttr(tbl[1:3], "PickOne")
    logical.arr <- array(rep(c(TRUE, FALSE), c(2, 1)), dim = c(3, 3))
    checkQuestionTypesAttr(tbl[logical.arr], c("PickOne", "PickOne"))
    logical.arr[, 2:3] <- FALSE
    checkQuestionTypesAttr(tbl[logical.arr], "PickOne")

    # Nominal x Multi
    tbl <- structure(array(runif(8L, min = 16, max = 20), dim = c(8, 1),
                           dimnames = list(c("15-18", "19 to 24", "25 to 29", "30 to 34",
                                             "35 to 39", "40 to 44", "45 to 49", "NET"), "Total Spend")),
                     statistic = "Average", class = c("array", "qTable"), questiontypes = c("PickOne", "Number"))
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
                     class = c("qTable", "array"))
    checkQuestionTypesAttr(tbl[1:2], character(0L))
    tbl <- structure(array(c("Foo", "Bar", "Baz", "MFoo", "MBar", "MBaz"), dim = c(3, 2)),
                     statistic = "Text", questiontypes = c("Text", "PickOne"),
                     class = c("qTable", "array"))
    checkQuestionTypesAttr(tbl[1:2], c("Text", "PickOne"))

    # Multistat versions

    ## Basic Number table
    number.multi.stat.tbl <- structure(c(42, 5), dim = 1:2,
                                       dimnames = list("Age", c("Average", "Standard Deviation")),
                                       class = c("array", "qTable"), questiontypes = "Number")
    tbl <- number.multi.stat.tbl
    checkQuestionTypesAttr(tbl[1], "Number")
    checkQuestionTypesAttr(tbl[2], "Number")
    checkQuestionTypesAttr(tbl[1, drop = FALSE], "Number")
    checkQuestionTypesAttr(tbl[], "Number")

    ## Number x Number - multi stat
    tbl <- structure(array(c(0.745, 0.02), dim = c(1, 1, 2),
                           dimnames = list("Total Spend", "", c("Correlation", "Standard Error"))),
                     class = c("array", "qTable"), questiontypes = c("Number", "Number"))
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
    class(table.3d) <- c("qTable", class(table.3d))

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
    class(age.table) <- c("qTable", class(age.table))
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
    class(table.2d) <- c("qTable", class(table.2d))
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
    class(table.3d) <- c("qTable", class(table.3d))
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
    class(num.by.num) <- c("qTable", class(num.by.num))
    attr(num.by.num, "statistic") <- "Average"
    checkStatisticAttribute(num.by.num[1], "Average")
    # Multiple stats
    num.by.num.multi <- array(num.by.num, dim = c(1, 1, 2),
                              dimnames = list("Var1", "Var2", c("Stat1", "Stat2")))
    class(num.by.num.multi) <- c("qTable", class(num.by.num.multi))
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
    expect_equal(dimnames(out), list(Statistic = c("z-Statistic", "Average")))
    z.stat.out <- attr(out, "QStatisticsTestingInfo")[, "zstatistic"]
    expect_equal(z.stat.out, unclass(tbl)[1, "z-Statistic"])

    out <- tbl[, 2]
    expect_equal(dimnames(out), list(Row = rownames(tbl)))
    z.stat.out <- attr(out, "QStatisticsTestingInfo")[, "zstatistic"]
    expect_equal(z.stat.out, unclass(tbl)[, "z-Statistic"],
                 check.attributes = FALSE)
    expect_equal(attr(out, "statistic"), "Average")

    row.idx <- 2:3
    out <- tbl[row.idx, 2:1]
    expect_equal(dimnames(out), list(Row = rownames(tbl)[row.idx],
                                     Statistic = rev(colnames(tbl))))
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
    expect_equal(dimnames(out), list(Statistic = c("z-Statistic", "Average")))
    z.stat.out <- attr(out, "QStatisticsTestingInfo")[, "zstatistic"]
    expect_equal(z.stat.out, unclass(tbl)[2, 3, "z-Statistic"])

    out <- tbl[, 2, ]
    expect_equal(dimnames(out), setNames(dimnames(tbl)[c(1, 3)], c("Row", "Statistic")))
    z.stat.out <- attr(out, "QStatisticsTestingInfo")[, "zstatistic"]
    expect_equal(z.stat.out, unclass(tbl)[, 2, "z-Statistic"],
               check.attributes = FALSE)
    expect_equal(attr(out, "statistic"), NULL)

    out <- tbl[3:4, 3:2, 1]
    expect_equal(dimnames(out), list(Row = dimnames(tbl)[[1L]][3:4],
                                     Column = dimnames(tbl)[[2L]][3:2]))
    z.stat.out <- attr(out, "QStatisticsTestingInfo")[, "zstatistic"]
    expected.z <- as.vector(t(unclass(tbl)[3:4, 3:2, "z-Statistic"]))
    expect_equal(z.stat.out, expected.z, check.attributes = FALSE)
})

test_that("DS-3838: Subset QTestInfo for multi-stat xtab of 1D questions",
{
    tbl <- tbls.multi.stat[["Date.by.PickAny"]]
    out <- tbl[6, 1, ]
    expect_equal(dimnames(out), list(Statistic = c("z-Statistic", "Average")))
    z.stat.out <- attr(out, "QStatisticsTestingInfo")[, "zstatistic"]
    expect_equal(z.stat.out, unclass(tbl)[6, 1, "z-Statistic"])

    out <- tbl[4, 3, 2]
    expect_equal(dimnames(out), NULL)
    z.stat.out <- attr(out, "QStatisticsTestingInfo")[, "zstatistic"]
    expect_equal(z.stat.out, unclass(tbl)[4, 3, "z-Statistic"])

    out <- tbl[4:5, , ]
    expected.dim <- dimnames(tbl)
    expected.dim[[1]] <- expected.dim[[1]][4:5]
    expected.dim <- setNames(expected.dim, c("Row", "Column", "Statistic"))
    expect_equal(dimnames(out), expected.dim)
    q.test.info.out <- attr(out, "QStatisticsTestingInfo")
    rownames(q.test.info.out) <- NULL
    z.stat.out <- q.test.info.out[, "zstatistic"]
    expect_equal(q.test.info.out[, 1:2], expand.grid(expected.dim[2:1])[, 2:1])
    expect_equal(z.stat.out, as.vector(t(unclass(tbl)[4:5, , "z-Statistic"])),
               check.attributes = FALSE)
    expect_equal(attr(out, "statistic"), NULL)

    out <- tbl[c(2, 5), 1:2, 2]
    expect_equal(dimnames(out), list(Row = dimnames(tbl)[[1L]][c(2, 5)],
                                   Column = dimnames(tbl)[[2L]][1:2]))
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
    expect_equal(names(dimnames(out)), expected.dim.names)
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
    expected.dim.names <- c("Row", "Column", "Statistic")
    expect_equal(names(dimnames(out)), expected.dim.names)
    expected <- unclass(tbl)[4, 3:2, 1:2, 1]
    q.stat.info.out <- attr(out, "QStatisticsTestingInfo")
    expect_equal(q.stat.info.out[, "zstatistic"], as.vector(t(expected)),
                 check.attributes = FALSE)

    out <- tbl[2, 1, , 2]
    expect_equal(dim(out), dim(tbl)[3])
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
