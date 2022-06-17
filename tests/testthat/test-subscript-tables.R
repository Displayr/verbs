context("Subscripting QTables")

arrayAsTable <- function(dims, dimnames = NULL) {
    if (missing(dims))
        stop("dims argument required")
    output <- array(sample(1:100, size = prod(dims), replace = TRUE), dim = dims, dimnames = dimnames)
    class(output) <- c("QTable", class(output))
    output
}

set.seed(12321)
table.env <- new.env()
# Very simple arrays
x.1 <- arrayAsTable(1)
x.1.named <- arrayAsTable(1, list("Foo"))

## All names
array.names <- list(LETTERS[1:6], LETTERS[1:5], LETTERS[1:4], LETTERS[1:3], LETTERS[1:2])
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

# Redundant arrays (can drop)
x.2.1 <- arrayAsTable(2:1)
x.2.1 <- arrayAsTable(2:1, dimnames = list(LETTERS[1:2], "a"))
x.2.1.dropped <- as.array(x.2.1)[, 1]
class(x.2.1.dropped) <- class(x.2.1)

test_that("Empty indices passed ok", {
    for (input in tables.wont.drop) {
        expect_equal(input[], input)
        for (drop.arg in c(TRUE, FALSE))
            expect_equal(input[drop = drop.arg], input)
    }
})

test_that("Check indices subscriptted correctly", {
    subsetTable <- function(x, ind, ...) {
        n.ind <- length(ind)
        if (n.ind == 1L)
            return(x[ind[[1L]], ...])
        if (n.ind == 2L)
            return(x[ind[[1L]], ind[[2L]], ...])
        if (n.ind == 3L)
            return(x[ind[[1L]], ind[[2L]], ind[[3L]], ...])
        if (n.ind == 4L)
            return(x[ind[[1L]], ind[[2L]], ind[[3L]], ind[[4L]], ...])
        x[ind[[1L]], ind[[2L]], ind[[3L]], ind[[5L]], ind[[5L]], ...]
    }
    expectedTable <- function(x, ind, ...) {
        y <- subsetTable(unclass(x), ind, ...)
        class(y) <- class(x)
        y
    }
    arg.template <- replicate(5L, NULL, simplify = FALSE)
    arg.template <- setNames(arg.template, c("i", "j", "k", "l", "m"))
    n.possible <- 6:2
    n.selected <- n.possible %/% 2
    randomIndex <- function(n.possible, size) sample.int(n.possible, size = size)
    randomLetters <- function(n.possible, size) LETTERS[randomIndex(n.possible, size)]

    for (drop in list(TRUE, FALSE, NULL))
        for (tab in subsettable.tables) {
            n.dim <- length(dim(tab))
            args <- arg.template[1:n.dim]
            n <- n.possible[1:n.dim]
            selected <- n.selected[1:n.dim]
            inds <- mapply(randomIndex, n, selected, SIMPLIFY = FALSE)
            test.table <- if (is.null(drop)) subsetTable(tab, inds) else subsetTable(tab, inds, drop = drop)
            expected <- if (is.null(drop)) expectedTable(tab, inds) else expectedTable(tab, inds, drop = drop)
            expect_equal(test.table, expected)
            if (!is.null(dimnames(tab))) {
                n.ind <- mapply(randomLetters, n, selected, SIMPLIFY = FALSE)
                test.table <- if (is.null(drop)) subsetTable(tab, n.ind) else subsetTable(tab, n.ind, drop = drop)
                expected <- if (is.null(drop)) expectedTable(tab, n.ind) else expectedTable(tab, n.ind, drop = drop)
                expect_equal(test.table, expected)
            }
        }
})

test_that("Informative message when user provides incorrect arguments", {
    expect_error(x.6.5.4[1, 2],
                 capture_error(throwErrorTableIndexInvalid(quote(x.6.5.4), 3, 2))[["message"]],
                 fixed = TRUE)
})

test_that("drop recognised and used appropriately", {
    expected.error <- capture_error(throwErrorDropOnlyNamed())[["message"]]
    expect_error(x.1[Drop = FALSE], expected.error, fixed = TRUE)
    for (arg in c(TRUE, FALSE))
        expect_equal(x.1[drop = arg], x.1)
    expect_equal(x.2.1[drop = FALSE], x.2.1)
    expect_equal(x.2.1[drop = TRUE], x.2.1)
    expect_equal(x.2.1[, 1, drop = FALSE], x.2.1)
    expect_equal(x.2.1[, 1, drop = TRUE], x.2.1.dropped)
})
