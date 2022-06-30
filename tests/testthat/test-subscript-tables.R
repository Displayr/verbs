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

silentDput <- function(x) {
    if (is.symbol(x)) return(x)
    sink(tempfile())
    y <- dput(x)
    sink()
    y
}

singleSubscriptTable <- function(tab, ind, drop = NULL) {
    n.ind <- length(ind)
    args <- c(list(tab), lapply(ind, silentDput))
    if (!is.null(drop)) args <- c(args, drop = drop)
    do.call(`[`, args)
}
expectedSingleTable <- function(tab, ind, drop = NULL) {
    y <- singleSubscriptTable(unclass(tab), ind, drop)
    class(y) <- c("qTable", class(y))
    orig.name <- paste0("table.", paste0(dim(tab), collapse = "."))
    attr(y, "original.name") <- orig.name
    attr(y, "name") <- paste0(orig.name, "[", paste0(ind, collapse = ","), "]")
    y
}
doubleSubscriptTable <- function(tab, ind, exact = NULL) {
    n.ind <- length(ind)
    args <- c(list(tab), lapply(ind, silentDput))
    if (!is.null(exact)) args <- c(args, exact = exact)
    do.call(`[[`, args)
}
expectedDoubleTable <- function(tab, ind, exact = NULL) {
    y <- doubleSubscriptTable(unclass(tab), ind, exact)
    class(y) <- c("qTable", class(y))
    orig.name <- paste0("table.", paste0(dim(tab), collapse = "."))
    attr(y, "original.name") <- orig.name
    attr(y, "name") <- paste0(orig.name, "[", paste0(ind, collapse = ","), "]")
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
    x.2.1.dropped <- unclass(x.2.1)[, 1]
    class(x.2.1.dropped) <- c("qTable", class(x.2.1.dropped))
    attr(x.2.1.dropped, "original.name") <- "table.2.1"
    attr(x.2.1.dropped, "name") <- "table.2.1[,1]"

    expect_equal(x.2.1[, 1, drop = TRUE], x.2.1.dropped)

    expected.error <- capture_error(throwErrorOnlyNamed("exact", "[["))[["message"]]
    expect_error(x.6.5.named[[2, 3, Exact = TRUE]], expected.error, fixed = TRUE)
    expect_error(x.6.5.named[[2, 3, exact = "TRUE"]], "exact argument should be TRUE or FALSE")
})

checkAttribute <- function(x, attr.name, desired.attr) {
    x.attributes <- attributes(x)
    expect_true(attr.name %in% names(x.attributes))
    x.attr <- attr(x, attr.name)
    expect_equal(x.attr, desired.attr)
}

test_that("Span attributes retained properly", {
    agespans <- structure(c(`15-18` = 9.91139501339378, `19 to 24` = 17.3913043478261,
`25 to 29` = 11.0035029878426, `30 to 34` = 14.6301256954461,
`35 to 39` = 16.0107150216361, `40 to 44` = 17.0616113744076,
`45 to 49` = 13.9913455594478, NET = 100), dim = 8L, dimnames = list(
    c("15-18", "19 to 24", "25 to 29", "30 to 34", "35 to 39",
    "40 to 44", "45 to 49", "NET")), statistic = "%", span = list(
    rows = structure(list(c("&lt;25", "&lt;25", "25-39", "25-39",
    "25-39", NA, NA, NA), c("15-18", "19 to 24", "25 to 29",
    "30 to 34", "35 to 39", "40 to 44", "45 to 49", "NET")), class = "data.frame", names = c("",
    ""), row.names = c(NA, 8L))), basedescriptiontext = "sample size = 4853", basedescription = list(
    Minimum = 4853L, Maximum = 4853L, Range = FALSE, Total = 4853L,
    Missing = 0L, EffectiveSampleSize = 4853L, EffectiveSampleSizeProportion = 100,
    FilteredProportion = 0), QStatisticsTestingInfo = structure(list(
    significancearrowratio = structure(c(1, 1, 1, 0, 0.588688946015424,
    1, 0, 1), dim = 8L), significancedirection = structure(c("Down",
    "Up", "Down", "None", "Up", "Up", "None", "Up"), dim = 8L),
    significancefontsizemultiplier = structure(c(0.204498977505112,
    4.89, 0.204498977505112, 1, 3.29, 4.89, 1, 4.89), dim = 8L),
    significanceissignificant = structure(c(TRUE, TRUE, TRUE,
    FALSE, TRUE, TRUE, FALSE, TRUE), dim = 8L), zstatistic = structure(c(-8.70839337178774,
    6.1826076764711, -6.53422517465904, 0.685654121466462, 3.43413089896878,
    5.52625501318697, -0.586029163646552, 170.639971870602), dim = 8L),
    pcorrected = structure(c(3.08212076761074e-18, 6.30512753119206e-10,
    6.39396864465964e-11, 0.492931244011513, 0.000594457051110497,
    3.27138529598869e-08, 0.557855916890687, 0), dim = 8L)), class = "data.frame", row.names = c(NA,
8L)), questiontypes = "PickOne", footer.html = "&lt;div data-editable=\"true\" style=\"font-family:'Open
 Sans', sans-serif;font-size:8pt;font-weight:normal;font-style:normal;text-decoration:none;color:#505050
;text-align:center;\"&gt;S1 Age 2 SUMMARY&lt;br /&gt;sample size = 4853; 95% confidence level&lt;/div&gt
;", name = "table.S1.Age.2", questions = c("S1 Age 2",
"SUMMARY"), class = c("qTable", "array"))
    checkAttribute(agespans[1:2], "span",
                   list(rows = data.frame(rep("&lt;25", 2), c("15-18", "19 to 24"), fix.empty.names = FALSE)))
})
