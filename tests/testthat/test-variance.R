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

weighted.variance <- function(x, weights, sample = TRUE, remove.missing = TRUE)
{
    invalid <- is.na(x) | is.na(weights) | weights < 0
    n <- sum(!invalid)
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
    adjust <- if (sample) (n - 1)/n else 1L
    (sum(weights * x^2, na.rm = remove.missing) - sw * wm^2)/(adjust * sw)
}

checkSingleInput <- function(input, variance = TRUE, ...)
{
    fun <- if (variance) Variance else StandardDeviation
    x <- if (is.list(input)) unlist(input) else as.vector(input)
    if (is.null(input) || all(is.na(input)))
        expect_true(all(is.na(fun(x, variance = TRUE, remove.missing = FALSE))) &&
                    all(is.na(fun(x, variance = TRUE, remove.missing = TRUE))) &&
                    all(is.na(fun(x, variance = FALSE, remove.missing = TRUE))) &&
                    all(is.na(fun(x, variance = FALSE, remove.missing = FALSE))))
    else {
        possible.args <- expand.grid(sample = c(TRUE, FALSE), remove.missing = c(TRUE, FALSE))
        # Non-Weighted calculations
        weights <- match.call()[["missing"]]
        if (is.null(weights))
        {
            expected.fun <- if (variance) var else sd
            n <- sum(!is.na(x))
            adjustment <- c(1, (n - 1)/n)
            expected.calc <- vapply(c(FALSE, TRUE),
                                    function(na.arg) expected.fun(x, na.rm = na.arg),
                                    numeric(1L))
            if (!variance)
                adjustment <- sqrt(adjustment)
        } else {
            expected.calc <- vapply(c(FALSE, TRUE),
                                    function(na.arg) weighted.variance(x, weights = weights,
                                                                       remove.missing = na.arg),
                                    numeric(1L))

        }
        expected.calc <- outer(setNames(expected.calc,
                                        paste0("Remove ", c(FALSE, TRUE))),
                               setNames(adjustment, c("Sample", "Population")))
        expect_equal(fun(input, remove.missing = FALSE, sample = FALSE),
                     expected.calc["Remove FALSE", "Population"])
        expect_equal(fun(input, remove.missing = FALSE, sample = TRUE),
                     expected.calc["Remove FALSE", "Sample"])
        expect_equal(fun(input, remove.missing = TRUE, sample = FALSE),
                     expected.calc["Remove TRUE", "Population"])
        expect_equal(fun(input, remove.missing = TRUE, sample = TRUE),
                     expected.calc["Remove TRUE", "Sample"])
    }
}
doAllSingleTests <- function(x)
{
    checkSingleInput(x, variance = TRUE)
    checkSingleInput(x, variance = FALSE)
    n <- NROW(x)
    if (is.data.frame(x))
        data.frame(lapply(x, function(y) {
            is.na(y) <- sample.int(n, size = n/2)
            y
        }))
    else
        is.na(x) <- sample.int(n, size = n/2)
    checkSingleInput(x, variance = TRUE)
    checkSingleInput(x, variance = FALSE)
    all.na <- array(NA, dim = DIM(x))
    checkSingleInput(all.na, variance = TRUE)
    checkSingleInput(all.na, variance = FALSE)
}

test_that("Basic Single input tests", {
    # Edge cases
    expect_true(is.na(Variance(runif(1L), sample = TRUE)))
    expect_equal(Variance(runif(1L), sample = FALSE), 0)
    # Missing values handled properly first
    checkSingleInput(NA, variance = TRUE)
    checkSingleInput(NA, variance = FALSE)
    # All missing also handled
    n <- 20L
    all.na <- rep(NA, n)
    checkSingleInput(all.na, variance = TRUE)
    checkSingleInput(all.na, variance = FALSE)
    # Check NULLs handled
    checkSingleInput(NULL, variance = TRUE)
    checkSingleInput(NULL, variance = FALSE)
    # Check vectors, no missing handled
    x <- rnorm(n)
    doAllSingleTests(x)
    # Make half of them missing and check
    is.na(x) <- sample.int(n, size = n/2)
    doAllSingleTests(x)
    # Do it for a matrix
    m <- 5L
    x <- array(rnorm(n * m), dim = c(n, m))
    doAllSingleTests(x)
    is.na(x) <- sample.int(n * m, size = (n * m)/2)
    doAllSingleTests(x)
    x[TRUE] <- NA
    # data.frame checks
    n <- 10L
    df <- setNames(data.frame(replicate(3, runif(n), simplify = FALSE)), letters[1:3])
    doAllSingleTests(df)
    # Filtering option checks
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
    # weights in a single df
    wgts <- runif(n)
    df.with.missing <- setNames(data.frame(replicate(3, runif(n), simplify = FALSE)), letters[1:3])
    df.with.missing <- as.data.frame(lapply(df.with.missing, function(x) {
        is.na(x) <- sample.int(length(x), size = 1L)
        x
    }))
    wgtd.mean <- Average(df.with.missing, weights = wgts)
    mat.with.missing <- as.matrix(df.with.missing)
    total.weight <- computeTotalWeights(mat.with.missing, weights = wgts)
    wgt.mat <- array(wgts, dim = dim(mat.with.missing))
    wgt.mat[is.na(mat.with.missing)] <- 0L
    n <- sum(!is.na(mat.with.missing))
    expected.variance <- sum(((mat.with.missing - wgtd.mean)^2 * wgt.mat)/
                                 ((n - 1L)/n * sum(total.weight)),
                             na.rm = TRUE)
    expect_equal(Variance(df.with.missing, weights = wgts),
                 expected.variance)
    expect_equal(Variance(as.matrix(df.with.missing), weights = wgts),
                 expected.variance)
})

test_that("Weighted single inputs", {
    n <- 100L
    x <- rnorm(n)
    wgts <- runif(n)
    # Edge cases
    expect_true(is.nan(Variance(1L, weights = wgts[1])))
    expect_equal(Variance(1L, weights = wgts[1], sample = FALSE), 0)
    # Regular cases
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
    # Edge cases handled (one input is NULL)
    array.in <- array(1:12, dim = 3:4)
    expect_true(all(is.na(Variance(array.in, NULL, sample = TRUE))))
    expect_true(all(Variance(array.in, NULL, sample = FALSE) == 0))
    n <- prod(dim(array.in))
    is.na(array.in) <- sample.int(n, size = n/2)
    expect_equal(Variance(array.in, NULL, sample = FALSE), array.in * 0)
    # Vector tests
    checkVariance <- function(x.list, remove.missing = FALSE, sample = TRUE)
    {
        fun <- if (sample) var else pvar
        x.matrix <- do.call(cbind, x.list)
        apply(x.matrix, 1L, fun, na.rm = remove.missing)
    }
    X <- replicate(2, runif(10), simplify = FALSE)
    expect_equal(do.call(Variance, X), checkVariance(X))
    expect_equal(do.call(Variance, c(X, sample = FALSE)),
                         checkVariance(X, sample = FALSE))
    X <- replicate(10, runif(10), simplify = FALSE)
    expect_equal(do.call(Variance, X), checkVariance(X))
    expect_equal(do.call(Variance, c(X, sample = FALSE)),
                         checkVariance(X, sample = FALSE))
    # Check handling of missing values
    general.missing.warn <- capture_warnings(warnAboutMissingValuesIgnored())
    variance.call <- quote(Variance(c(NA, 1:2), warn = TRUE))
    observed.warn <- capture_warnings(observed.var <- eval(variance.call))
    expect_equal(observed.warn, general.missing.warn)
    expected.warning <- capture_warnings(throwWarningAboutMinimumCasesForVariance(quoted.function, sample = TRUE))
    observed.warnings <- capture_warnings(var.calc <- Variance(c(NA, 1:2), c(2:3, NA),
                                                               warn = TRUE,
                                                               sample = TRUE))
    expect_equal(Variance(c(NA, 1:2), c(2:3, NA), warn = "Foo"), c(NA, 2, NA))
    expect_setequal(observed.warnings, c(general.missing.warn, expected.warning))

    min.numb.warn <- capture_warnings(throwWarningAboutMinimumCasesForVariance(quoted.function, sample = FALSE))
    obs.warnings <- capture_warnings(Variance(c(rep(NA, 2L), 2L), c(2, rep(NA, 2)),
                                              warn = TRUE, sample = FALSE))
    expect_setequal(obs.warnings, c(general.missing.warn, min.numb.warn))
    expect_equal(Variance(c(rep(NA, 2L), 2L), c(2, rep(NA, 2)), warn = "Foo", sample = FALSE),
                 c(0, NA, 0))
    X.with.na <- addMissing(X)
    args <- X.with.na
    args[[length(args) + 1L]] <- FALSE
    names(args)[length(args)] <- "remove.missing"
    expect_equal(do.call(Variance, args),
                 checkVariance(X.with.na, remove.missing = FALSE))
    expect_equal(do.call(Variance, c(args, sample = FALSE)),
                 checkVariance(X.with.na, remove.missing = FALSE, sample = FALSE))

    args[[length(args)]] <- TRUE
    expect_equal(do.call(Variance, args),
                 checkVariance(X.with.na, remove.missing = TRUE))
    expect_equal(do.call(Variance, c(args, sample = FALSE)),
                 checkVariance(X.with.na, remove.missing = TRUE, sample = FALSE))
    # Matching vector tests
    set.of.names <- replicate(26L, paste0(sample(c(letters, LETTERS, 1:9), size = 5), collapse = "."))
    while(anyDuplicated(set.of.names))
        set.of.names <- replicate(26L, paste0(sample(c(letters, LETTERS, 1:9), size = 5), collapse = "."))
    checkMatchedVariances <- function(x.list, remove.missing = FALSE, sample = TRUE)
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
        all.variances <- checkVariance(matched.x,
                                       remove.missing = remove.missing,
                                       sample = sample)
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
    expect_equal(do.call(Variance, c(args, sample = FALSE)),
                 checkMatchedVariances(X, sample = FALSE))
    X.with.na <- addMissing(X)
    args <- X.with.na
    args[[length(args) + 1L]] <- FALSE
    names(args)[length(args)] <- "remove.missing"
    expect_equal(do.call(Variance, args), checkMatchedVariances(X.with.na, remove.missing = FALSE))
    expect_equal(do.call(Variance, c(args, sample = FALSE)),
                 checkMatchedVariances(X.with.na, remove.missing = FALSE, sample = FALSE))
    X <- replicate(10, setNames(runif(10), nm = sample(set.of.names[1:10])), simplify = FALSE)
    args <- X
    expect_equal(do.call(Variance, args), checkMatchedVariances(X))
    expect_equal(do.call(Variance, c(args, sample = FALSE)),
                 checkMatchedVariances(X, sample = FALSE))
    X.with.na <- addMissing(X)
    args <- X.with.na
    args[[length(args) + 1L]] <- FALSE
    names(args)[length(args)] <- "remove.missing"
    expect_equal(do.call(Variance, args), checkMatchedVariances(X.with.na, remove.missing = FALSE))
    expect_equal(do.call(Variance, c(args, sample = FALSE)),
                 checkMatchedVariances(X.with.na, remove.missing = FALSE, sample = FALSE))
    # Some unmmatched
    X <- replicate(2, setNames(runif(10), nm = sample(set.of.names[1:12], size = 10)), simplify = FALSE)
    matched.X <- extractMatchedElements(X)
    args <- X
    expect_equal(do.call(Variance, args), checkVariance(matched.X))
    expect_equal(do.call(Variance, c(args, sample = FALSE)),
                 checkVariance(matched.X, sample = FALSE))
    X.with.na <- addMissing(X)
    args <- X.with.na
    args[[length(args) + 1L]] <- FALSE
    names(args)[length(args)] <- "remove.missing"
    matched.X <- extractMatchedElements(X.with.na)
    expect_equal(do.call(Variance, args), checkVariance(matched.X, remove.missing = FALSE))
    expect_equal(do.call(Variance, c(args, sample = FALSE)),
                 checkVariance(matched.X, remove.missing = FALSE, sample = FALSE))
    X <- replicate(5, setNames(runif(10), nm = sample(set.of.names[1:12], size = 10)), simplify = FALSE)
    args <- X
    matched.X <- extractMatchedElements(X)
    expect_equal(do.call(Variance, args), checkVariance(matched.X))
    expect_equal(do.call(Variance, c(args, sample = FALSE)),
                 checkVariance(matched.X, sample = FALSE))
    X.with.na <- addMissing(X)
    args <- X.with.na
    args[[length(args) + 1L]] <- FALSE
    names(args)[length(args)] <- "remove.missing"
    matched.X <- extractMatchedElements(X.with.na)
    expect_equal(do.call(Variance, args), checkVariance(matched.X, remove.missing = FALSE))
    expect_equal(do.call(Variance, c(args, sample = FALSE)),
                 checkVariance(matched.X, remove.missing = FALSE, sample = FALSE))
    # Handling missing values ok
    X <- list(c(NA, 1:4), c(NA, 11:14), c(0, 21:24), c(1, 31:34))
    expect_equal(do.call(Variance, c(X, remove.missing = FALSE)), checkVariance(X))
    expect_equal(do.call(Variance, c(X, remove.missing = FALSE, sample = FALSE)),
                 checkVariance(X, sample = FALSE))
    expect_equal(do.call(Variance, X), checkVariance(X, remove.missing = TRUE))
    expect_equal(do.call(Variance, c(X, sample = FALSE)),
                 checkVariance(X, remove.missing = TRUE, sample = FALSE))
    X <- list(c(NA, 1:4), c(NA, 11:14), c(0, 21:24), c(1, 31:34), c(2, 41:44))
    expect_equal(do.call(Variance, c(X, remove.missing = FALSE)), checkVariance(X))
    expect_equal(do.call(Variance, c(X, remove.missing = FALSE, sample = FALSE)),
                 checkVariance(X, sample = FALSE))
    expect_equal(do.call(Variance, X), checkVariance(X, remove.missing = TRUE))
    expect_equal(do.call(Variance, c(X, sample = FALSE)),
                 checkVariance(X, remove.missing = TRUE, sample = FALSE))
})

test_that("Multiple 2d inputs", {
    checkVariance <- function(x.list, remove.missing = TRUE, sample = TRUE)
    {
        fun <- if (sample) var else pvar
        dims <- dim(x.list[[1L]])
        x <- array(unlist(x.list), dim = c(dims, length(x.list)), dimnames = dimnames(x.list[[1L]]))
        apply(x, 1:2, fun, na.rm = remove.missing)
    }
    # 2 inputs
    X <- replicate(2, array(runif(12), dim = 3:4, dimnames = list(letters[1:3], LETTERS[1:4])), simplify = FALSE)
    expect_equal(do.call(Variance, X), checkVariance(X))
    expect_equal(do.call(Variance, c(X, sample = FALSE)),
                 checkVariance(X, sample = FALSE))
    X.with.na <- addMissing(X)
    expect_equal(do.call(Variance, c(X.with.na, sample = FALSE)),
                 checkVariance(X.with.na, sample = FALSE))
   expect_equal(do.call(Variance, c(X.with.na, remove.missing = FALSE)),
                 checkVariance(X.with.na, remove.missing = FALSE))
   expect_equal(do.call(Variance, c(X.with.na, remove.missing = FALSE, sample = FALSE)),
                 checkVariance(X.with.na, remove.missing = FALSE, sample = FALSE))
    X <- replicate(2, array(runif(12), dim = 3:4, dimnames = list(letters[1:3], LETTERS[1:4])), simplify = FALSE)
    expect_equal(do.call(Variance, X), checkVariance(X))
    expect_equal(do.call(Variance, c(X, sample = FALSE)), checkVariance(X, sample = FALSE))
    X.with.na <- addMissing(X)
    expect_equal(do.call(Variance, X.with.na), checkVariance(X.with.na))
    expect_equal(do.call(Variance, c(X.with.na, sample = FALSE)),
                 checkVariance(X.with.na, sample = FALSE))
    expect_equal(do.call(Variance, c(X.with.na, remove.missing = FALSE)),
                 checkVariance(X.with.na, remove.missing = FALSE))
    expect_equal(do.call(Variance, c(X.with.na, remove.missing = FALSE, sample = FALSE)),
                 checkVariance(X.with.na, remove.missing = FALSE, sample = FALSE))
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
    expect_equal(do.call(Variance, c(X, sample = FALSE)),
                 checkVariance(matched.X, sample = FALSE))
    X.with.na <- addMissing(X)
    matched.X.with.na <- extractMatchedElements(X.with.na)
    expect_equal(do.call(Variance, X.with.na), checkVariance(matched.X.with.na))
    expect_equal(do.call(Variance, c(X.with.na, sample = FALSE)),
                         checkVariance(matched.X.with.na, sample = FALSE))
    expect_equal(do.call(Variance, c(X.with.na, remove.missing = FALSE)),
                 checkVariance(matched.X.with.na, remove.missing = FALSE))
    expect_equal(do.call(Variance, c(X.with.na, remove.missing = FALSE, sample = FALSE)),
                 checkVariance(matched.X.with.na, remove.missing = FALSE, sample = FALSE))
})

test_that("Edge cases", {
    x <- setNames(1:10, letters[1:10])
    expected.warning <- capture_warnings(throwWarningAboutMinimumCasesForVariance(quoted.function, sample = TRUE))
    expect_warning(output <- Variance(x, NULL, warn = TRUE),
                   expected.warning)
    expect_warning(Variance(x, NULL, warn = "Foo"), NA)
    expect_equal(output, setNames(rep(NA, length(x)), names(x)))
    expect_warning(do.call(Variance, c(replicate(3, c(NA, runif(4)), simplify = FALSE),
                                       warn = TRUE, remove.missing = TRUE)),
                   expected.warning)
    expect_warning(do.call(Variance, c(replicate(3, c(NA, runif(4)), simplify = FALSE),
                                       warn = "Foo", remove.missing = FALSE)),
                   NA)
})

test_that("Warnings muffled", {
    # Show the missing value warning usually
    input.array <- setNames(c(NA, 1:2), LETTERS[1:3])
    expected.cond <- capture_condition(warnAboutMissingValuesIgnored())
    observed.cond <- capture_condition(Sum(input.array, warn = TRUE))
    expect_equal(observed.cond, expected.cond)
    # Not show the missing value warning when not logical input given
    expect_equal(Variance(input.array, warn = "Foo"), var(input.array, na.rm = TRUE))
    expect_equal(Variance(input.array, sample = FALSE, warn = "Foo"), var(input.array, na.rm = TRUE)/2)
})
