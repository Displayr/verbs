context("Variance")

load("variable.Text.rda")
load("variable.Binary.rda")
load("variable.Nominal.rda")
load("variable.Numeric.rda")
load("variable.Time.rda")
load("variable.Date.rda")

if (flipU::IsRServer())
{
    contact.msg <- "support@displayr.com if you wish this to be changed."
} else
    contact.msg <- paste0("opensource@displayr.com or raise an issue ",
                          "at https://github.com/Displayr/verbs if you wish this to be changed.")

quoted.function <- sQuote("Variance")


load("table2D.PercentageAndCount.rda")
load("table1D.Average.rda")
load("table.1D.MultipleStatistics.rda")
load("table2D.PercentageNaN.rda")

test_that("Basic Single input tests", {
    x <- rnorm(20)
    expect_equal(Variance(x), var(x))
    x <- array(rnorm(12), dim = 3:4)
    expect_equal(Variance(x), var(as.vector(x)))
    expect_equal(StandardDeviation(x), sqrt(Variance(x)))
    x <- array(rnorm(12), dim = 3:4, dimnames = list(letters[1:3], LETTERS[1:4]))
    expect_equal(Variance(x, remove.rows = "a"), var(as.vector(x[-1, ])))
    expect_equal(Variance(x, remove.columns = "D"), var(as.vector(x[, -4])))
    expect_equal(Variance(x, remove.rows = "a", remove.columns = "D"), var(as.vector(x[-1, -4])))
    expect_equal(Variance(table1D.Average, remove.rows = "SUM", warn = TRUE),
                 var(table1D.Average[names(table1D.Average) != "SUM"]))
    expected.warning <- capture_warnings(throwWarningAboutDifferentStatistics(colnames(table.1D.MultipleStatistics),
                                                                              quoted.function))
    expect_warning(var.calc <- Variance(table.1D.MultipleStatistics, remove.columns = "SUM", warn = TRUE),
                   expected.warning, fixed = TRUE)
    expect_equal(var.calc, var(as.vector(table.1D.MultipleStatistics[rownames(table.1D.MultipleStatistics) != "SUM", ])))
    expected.warning <- capture_warnings(throwWarningAboutDifferentStatistics(dimnames(table2D.PercentageAndCount)[[3L]],
                                                                              quoted.function))
    expect_warning(var.calc <- Variance(table2D.PercentageAndCount, remove.columns = "NET", warn = TRUE),
                   expected.warning, fixed = TRUE)
    expect_equal(var.calc,
                 var(table2D.PercentageAndCount[, dimnames(table2D.PercentageAndCount)[[2]] != "NET", ]))
    expect_true(is.na(Variance(table2D.PercentageNaN, remove.missing = FALSE)))
    expect_false(is.na(Variance(table2D.PercentageNaN, remove.missing = TRUE)))
    # NULL input
    expect_true(is.na(Variance(NULL)))
    # Data frame input
    n <- 10L
    df <- setNames(data.frame(replicate(3, runif(n), simplify = FALSE)), letters[1:3])
    expect_equal(Variance(df), var(as.vector(as.matrix(df))))
    df.with.missing <- df
    df.with.missing <- as.data.frame(lapply(df.with.missing, function(x) {
        is.na(x) <- sample.int(length(x), size = 1L)
        x
    }))
    wgts <- runif(n)
    wgtd.mean <- Average(df.with.missing, weights = wgts)
    mat.with.missing <- as.matrix(df.with.missing)
    total.weight <- computeTotalWeights(mat.with.missing, weights = wgts)
    wgt.mat <- array(wgts, dim = dim(mat.with.missing))
    wgt.mat[is.na(mat.with.missing)] <- 0L
    expected.variance <- sum(((mat.with.missing - wgtd.mean)^2 * wgt.mat)/
                                 ((3*n - 1L)/(3 *n) * sum(total.weight)),
                             na.rm = TRUE)
    expect_equal(Variance(df.with.missing, weights = wgts),
                 expected.variance)
    expect_equal(Variance(as.matrix(df.with.missing), weights = wgts),
                 expected.variance)
})

test_that("Weighted single inputs", {
    weighted.variance <- function(x, weights, remove.missing = TRUE)
    {
        n <- length(x)
        if (remove.missing)
        {
            weights[is.na(weights)] <- 0
            if (getDimensionLength(x) > 1)
            {
                n.rep <- ncol(x)
                weights <- sapply(1:n.rep, function(i) {
                    y <- weights
                    y[is.na(x[, i])] <- 0
                    y
                    })
            } else
                weights[is.na(x)] <- 0
        }
        sw <- sum(weights)
        wm <- sum(weights * x, na.rm = remove.missing)/sw
        (sum(weights * x^2, na.rm = remove.missing) - sw * wm^2)/((n - 1L)/n * sw)
    }
    n <- 100L
    x <- rnorm(n)
    wgts <- runif(n)
    expect_equal(Variance(x, weights = wgts), weighted.variance(x, weights = wgts))
    x.with.na <- x
    is.na(x.with.na) <- sample(1:100, size = 5L)
    expect_equal(Variance(x.with.na, weights = wgts), weighted.variance(x.with.na, weights = wgts, remove.missing = TRUE))
    x <- array(rnorm(n^2), dim = rep(n, 2))
    expect_equal(Variance(x, weights = wgts), weighted.variance(x, weights = wgts))
    x.with.na <- x
    is.na(x.with.na) <- sample(1:n^2, size = n)
    expect_equal(Variance(x.with.na, weights = wgts), weighted.variance(x.with.na, weights = wgts))
})

addMissing <- function(x.list)
    lapply(x.list, function(x) {y <- x; is.na(y) <- sample.int(length(x), 1L); y})

test_that("Multiple inputs variance", {
    # Vector tests
    checkVariance <- function(x.list, remove.missing = FALSE)
    {
        x.matrix <- do.call(cbind, x.list)
        apply(x.matrix, 1L, var, na.rm = remove.missing)
    }
    X <- replicate(2, runif(10), simplify = FALSE)
    expect_equal(do.call(Variance, X), checkVariance(X))
    X <- replicate(10, runif(10), simplify = FALSE)
    expect_equal(do.call(Variance, X), checkVariance(X))
    # Check handling of missing values
    expected.warning <- capture_warnings(throwWarningAboutMinimumTwoValuesForVariance(quoted.function))
    expect_warning(var.calc <- Variance(c(NA, 1:2), c(2:3, NA), warn = TRUE),
                   expected.warning)
    expect_equal(var.calc,  c(NA, 2, NA))
    X.with.na <- addMissing(X)
    args <- X.with.na
    args[[length(args) + 1L]] <- FALSE
    names(args)[length(args)] <- "remove.missing"
    expect_equal(do.call(Variance, args), checkVariance(X.with.na, remove.missing = FALSE))
    args[[length(args)]] <- TRUE
    expect_equal(do.call(Variance, args), checkVariance(X.with.na, remove.missing = TRUE))
    # Matching vector tests
    set.of.names <- replicate(26L, paste0(sample(c(letters, LETTERS, 1:9), size = 5), collapse = "."))
    while(anyDuplicated(set.of.names))
        set.of.names <- replicate(26L, paste0(sample(c(letters, LETTERS, 1:9), size = 5), collapse = "."))
    checkMatchedVariances <- function(x.list, remove.missing = FALSE)
    {
        matched.x <- lapply(x.list[-1], function(x) matchDimensionElements(list(x.list[[1]], x),
                                                                           match.rows = "Yes - hide unmatched",
                                                                           match.columns = "Yes - hide unmatched",
                                                                           warn = FALSE,
                                                                           function.name = "tests")[[2L]])
        matched.first <- matchDimensionElements(list(x.list[[1]], matched.x[[1]]),
                                                match.rows = "Yes - hide unmatched",
                                                match.columns = "Yes - hide unmatched",
                                                warn = FALSE,
                                                function.name = "tests")[1L]
        matched.x <- c(matched.first, matched.x)
        all.variances <- checkVariance(matched.x, remove.missing = remove.missing)
        all.variances
    }
    extractMatchedElements <- function(X)
    {
        common.names <- Reduce(intersect, lapply(X, names))
        lapply(X, function(x) x[match(common.names, names(x), nomatch = 0L)])
    }
    # Same size tests, no unmatched
    X <- replicate(2, setNames(runif(10), nm = sample(set.of.names[1:10])), simplify = FALSE)
    args <- X
    expect_equal(do.call(Variance, args), checkMatchedVariances(X))
    X.with.na <- addMissing(X)
    args <- X.with.na
    args[[length(args) + 1L]] <- FALSE
    names(args)[length(args)] <- "remove.missing"
    expect_equal(do.call(Variance, args), checkMatchedVariances(X.with.na, remove.missing = FALSE))
    X <- replicate(10, setNames(runif(10), nm = sample(set.of.names[1:10])), simplify = FALSE)
    args <- X
    expect_equal(do.call(Variance, args), checkMatchedVariances(X))
    X.with.na <- addMissing(X)
    args <- X.with.na
    args[[length(args) + 1L]] <- FALSE
    names(args)[length(args)] <- "remove.missing"
    expect_equal(do.call(Variance, args), checkMatchedVariances(X.with.na, remove.missing = FALSE))
    # Some unmmatched
    X <- replicate(2, setNames(runif(10), nm = sample(set.of.names[1:12], size = 10)), simplify = FALSE)
    matched.X <- extractMatchedElements(X)
    args <- X
    expect_equal(do.call(Variance, args), checkVariance(matched.X))
    X.with.na <- addMissing(X)
    args <- X.with.na
    args[[length(args) + 1L]] <- FALSE
    names(args)[length(args)] <- "remove.missing"
    matched.X <- extractMatchedElements(X.with.na)
    expect_equal(do.call(Variance, args), checkVariance(matched.X, remove.missing = FALSE))
    X <- replicate(5, setNames(runif(10), nm = sample(set.of.names[1:12], size = 10)), simplify = FALSE)
    args <- X
    matched.X <- extractMatchedElements(X)
    expect_equal(do.call(Variance, args), checkVariance(matched.X))
    X.with.na <- addMissing(X)
    args <- X.with.na
    args[[length(args) + 1L]] <- FALSE
    names(args)[length(args)] <- "remove.missing"
    matched.X <- extractMatchedElements(X.with.na)
    expect_equal(do.call(Variance, args), checkVariance(matched.X, remove.missing = FALSE))
    # Handling missing values ok
    X <- list(c(NA, 1:4), c(NA, 11:14), c(0, 21:24), c(1, 31:34))
    expect_equal(do.call(Variance, c(X, remove.missing = FALSE)), checkVariance(X))
    expect_equal(do.call(Variance, X), checkVariance(X, remove.missing = TRUE))
    X <- list(c(NA, 1:4), c(NA, 11:14), c(0, 21:24), c(1, 31:34), c(2, 41:44))
    expect_equal(do.call(Variance, c(X, remove.missing = FALSE)), checkVariance(X))
    expect_equal(do.call(Variance, X), checkVariance(X, remove.missing = TRUE))
})

test_that("Multiple 2d inputs", {
    checkVariance <- function(x.list, remove.missing = TRUE)
    {
        dims <- dim(x.list[[1L]])
        x <- array(unlist(x.list), dim = c(dims, length(x.list)), dimnames = dimnames(x.list[[1L]]))
        apply(x, 1:2, var, na.rm = remove.missing)
    }
    # 2 inputs
    X <- replicate(2, array(runif(12), dim = 3:4, dimnames = list(letters[1:3], LETTERS[1:4])), simplify = FALSE)
    expect_equal(do.call(Variance, X), checkVariance(X))
    X.with.na <- addMissing(X)
    expect_equal(do.call(Variance, X.with.na), checkVariance(X.with.na))
    expect_equal(do.call(Variance, c(X.with.na, remove.missing = FALSE)),
                 checkVariance(X.with.na, remove.missing = FALSE))
    X <- replicate(2, array(runif(12), dim = 3:4, dimnames = list(letters[1:3], LETTERS[1:4])), simplify = FALSE)
    expect_equal(do.call(Variance, X), checkVariance(X))
    X.with.na <- addMissing(X)
    expect_equal(do.call(Variance, X.with.na), checkVariance(X.with.na))
    expect_equal(do.call(Variance, c(X.with.na, remove.missing = FALSE)),
                 checkVariance(X.with.na, remove.missing = FALSE))
    # 10 inputs
    set.of.names <- replicate(24L, paste0(sample(c(letters, 1:9), size = 10), collapse = ""))
    while(anyDuplicated(set.of.names))
        set.of.names <- replicate(24L, paste0(sample(c(letters, 1:9), size = 10), collapse = ""))
    X <- replicate(10, array(runif(100L), dim = rep(10L, 2L),
                             dimnames = list(sample(set.of.names[ 1:12], size = 10L),
                                             sample(set.of.names[13:24], size = 10L))),
                             simplify = FALSE)
    while(length(common.rownames <- Reduce(intersect, lapply(X, function(x) rownames(x)))) < 2L ||
          length(common.colnames <- Reduce(intersect, lapply(X, function(x) colnames(x)))) < 2L)
        X <- replicate(10, array(runif(100L), dim = rep(10L, 2L),
                                 dimnames = list(sample(set.of.names[ 1:12], size = 10L),
                                                 sample(set.of.names[13:24], size = 10L))),
                       simplify = FALSE)
    extractMatchedElements <- function(X)
    {
        common.names <- Reduce(intersect, lapply(X, dimnames))
        lapply(X, function(x) x[match(common.rownames, rownames(x), nomatch = 0L),
                                match(common.colnames, colnames(x), nomatch = 0L)])
    }
    matched.X <- extractMatchedElements(X)
    expect_equal(do.call(Variance, X), checkVariance(matched.X))
    X.with.na <- addMissing(X)
    matched.X.with.na <- extractMatchedElements(X.with.na)
    expect_equal(do.call(Variance, X.with.na), checkVariance(matched.X.with.na))
    expect_equal(do.call(Variance, c(X.with.na, remove.missing = FALSE)),
                 checkVariance(matched.X.with.na, remove.missing = FALSE))
})

test_that("Edge cases", {
    x <- setNames(1:10, letters[1:10])
    expected.warning <- capture_warnings(throwWarningAboutMinimumTwoValuesForVariance(quoted.function))
    expect_warning(output <- Variance(x, NULL, warn = TRUE),
                   capture_warnings(throwWarningAboutMinimumTwoValuesForVariance(quoted.function)))
    expect_equal(output, setNames(rep(NA, length(x)), names(x)))
    expect_warning(do.call(Variance, c(replicate(3, c(NA, runif(4)), simplify = FALSE),
                                       warn = TRUE, remove.missing = TRUE)),
                   expected.warning)
    expect_warning(do.call(Variance, c(replicate(3, c(NA, runif(4)), simplify = FALSE),
                                       warn = TRUE, remove.missing = FALSE)),
                   NA)
})
