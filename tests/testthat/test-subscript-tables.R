context("Subscripting QTables")

arrayAsTable <- function(dims, dimnames = NULL) {
    if (missing(dims))
        stop("dims argument required")
    output <- array(sample(1:100, size = prod(dims), replace = TRUE), dim = dims, dimnames = dimnames)
    class(output) <- c("qTable", class(output))
    attr(output, "name") <- paste0("table.", paste0(dims, collapse = "."))
    output
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
    if (!is.array(y))
        y <- as.array(y)
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
    class(expected) <- c("qTable", class(expected))
    attr(expected, "original.name") <- "table.2.1"
    attr(expected, "name") <- "table.2.1[,1]"
    expect_equal(x.2.1[, 1, drop = FALSE], expected)

    # Dropped output has the right class
    x.2.1.dropped <- structure(as.vector(x.2.1), dim = 2L,
                               class = c("qTable", "integer"),
                               original.name = "table.2.1",
                               dimnames = list(c("A", "B")),
                               name = "table.2.1[,1]")

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
        if (is.nan(attr.zstat))
            attr.zstat <- NA_real_
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
                        test.case, "[", start.idx, ":", end.idx,"]")
    test_that(test.name,
              {
                  idx <- start.idx:end.idx
                  z.stat.out <- attr(tbl[idx],
                                     "QStatisticsTestingInfo")[, "zstatistic"]
                  expected <- unclass(tbl)[idx]
                  expected[is.nan(expected)] <- NA
                  expect_equal(z.stat.out, expected, check.attributes = FALSE)

                  char.idx <- names(tbl)[idx]
                  z.stat.out <- attr(tbl[char.idx],
                                     "QStatisticsTestingInfo")[, "zstatistic"]
                  expected <- unclass(tbl)[char.idx]
                  expected[is.nan(expected)] <- NA
                  expect_equal(z.stat.out, expected, check.attributes = FALSE)

                  logical.idx <- seq_along(tbl) %in% idx
                  z.stat.out <- attr(tbl[logical.idx],
                                     "QStatisticsTestingInfo")[, "zstatistic"]
                  expected <- unclass(tbl)[logical.idx]
                  expected[is.nan(expected)] <- NA
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
                       test.case, "[", row.idx,",]")
   test_that(test.name,
   {
       z.stat.out <- attr(tbl[row.idx, ], "QStatisticsTestingInfo")[, "zstatistic"]
       expected <- unclass(tbl)[row.idx, ]
       expected[is.nan(expected)] <- NA
       expect_equal(z.stat.out, expected, check.attributes = FALSE)
   })

   col.idx <- sample(ncol(tbl), 1)
   test.name <- paste0("DS-3809: Column slices of simple tables: ",
                       test.case, "[,", col.idx,"]")
   test_that(test.name,
   {
       z.stat.out <- attr(tbl[, col.idx], "QStatisticsTestingInfo")[, "zstatistic"]
       expected <- unclass(tbl)[, col.idx]
       expected[is.nan(expected)] <- NA
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
    idx[sample(n, floor(n/2))] <- TRUE
    expected <- unclass(unname(tbl))[idx]
    z.out <- attr(tbl[idx], "QStatisticsTestingInfo")[, "zstatistic"]
    expect_equal(expected, z.out, check.attributes = FALSE)
})

test_that("DS-3809: Logical vector indices to 2D Table",
{
    tbl <- tbls[["PickAny.by.PickOne"]]
    set.seed(3230)
    n <- length(tbl)
    idx <- sample(n, floor(n/2))
    expected <- unclass(unname(tbl))[idx]
    z.out <- attr(tbl[idx], "QStatisticsTestingInfo")[, "zstatistic"]
    expect_equal(expected, z.out, check.attributes = FALSE)
})

set.seed(986)
grid.types <- c("PickAnyGrid", "PickOneMulti", "NumberGrid")
test.cases <- names(tbls)[dim.lens.avail == 3L]
for (test.case in test.cases)
{
    tbl <- tbls[[test.case]]
    slice.indices <- vapply(dim(tbl), function(len) sample.int(len, 1), 1L)

    test.name <- paste0("DS-3810: Single slices of 3D qTables: ",
                        test.case, "[", slice.indices[1L],",,]")
    test_that(test.name,
              {
                  z.stat.out <- attr(tbl[slice.indices[1L], , ], "QStatisticsTestingInfo")[, "zstatistic"]
                  z.stat.out <- as.numeric(z.stat.out)
                  expected <- unclass(tbl)[slice.indices[1L], , ]
                  if (attr(tbl, "questiontypes")[1L] %in% grid.types)
                      expected <- t(expected)  # t() for row-major order in attr.
                  expected <- as.vector(expected)
                  expect_equal(is.na(expected), is.na(z.stat.out))
                  expect_equal(z.stat.out, expected, check.attributes = FALSE)
              })
    test.name <- paste0("DS-3810: Single slices of 3D qTables: ",
                        test.case, "[, ", slice.indices[2L],",]")
    test_that(test.name,
              {
                  z.stat.out <- attr(tbl[, slice.indices[2L], ], "QStatisticsTestingInfo")[, "zstatistic"]
                  z.stat.out <- as.numeric(z.stat.out)
                  expected <- unclass(tbl)[, slice.indices[2L], ]
                  expected <- as.vector(t(expected))  # t() for row-major order in attr.
                  expected[is.nan(expected)] <- NA
                  expect_equal(z.stat.out, expected, check.attributes = FALSE)
              })
    test.name <- paste0("DS-3810: Single slices of 3D qTables: ",
                    test.case, "[,,", slice.indices[3L],"]")
    test_that(test.name,
              {
                  ## skip_if(!attr(tbl, "questiontypes")[2] %in% grid.types)
                  z.stat.out <- attr(tbl[, , slice.indices[3L]], "QStatisticsTestingInfo")[, "zstatistic"]
                  z.stat.out <- as.numeric(z.stat.out)
                  expected <- unclass(tbl)[, , slice.indices[3L]]
                  expected <- as.vector(t(expected))  # t() for row-major order in attr.
                  expected[is.nan(expected)] <- NA
                  expect_equal(z.stat.out, expected, check.attributes = FALSE)
              })
}

checkAttribute <- function(x, attr.name, desired.attr) {
    if (is.null(desired.attr)) {
        expect_null(attr(x, attr.name))
        return(invisible())
    }
    x.attributes <- attributes(x)
    expect_true(attr.name %in% names(x.attributes))
    x.attr <- attr(x, attr.name)
    expect_equal(x.attr, desired.attr)
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
    checkSpanAttribute(table.3d[2:3, 3:4 , ], NULL)
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
