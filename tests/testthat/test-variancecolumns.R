context("VarianceEachColumn")

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

quoted.function <- sQuote("VarianceColumns")

weighted.variance <- function(x, wgts, remove.missing, sample = TRUE)
{
    missing <- is.na(x) | is.na(wgts)
    wgts[missing | wgts < 0] <- 0
    sum.w <- sumWeights(x, wgts)
    valid <- !missing & wgts > 0
    n.x <- sum(valid)
    denom <- sum.w
    denom <- sum.w * (if (sample)  (n.x - 1L)/n.x else 1)
    weighted.mean <- sum(x * wgts, na.rm = remove.missing)/sum.w
    numer <- (sum(x^2 * wgts, na.rm = remove.missing) - sum.w * weighted.mean^2)
    numer/denom
}

test_that("Variables and Variable Sets", {

    n <- length(variable.Binary)
    weights <- runif(n)
    weights[sample.int(n, size = n/10)] <- NA
    weights[sample.int(n, size = n/10)] <- 0
    vars <- list(variable.Binary, variable.Numeric, variable.Nominal)
    subsets <- list(TRUE, sample(c(TRUE, FALSE), size = n, replace = TRUE))
    for (v in vars)
    {
        name <- attr(v, "label")
        for (s in subsets)
        {
            for (fun in c(var, pvar))
            {
                sample <- identical(fun, var)
                v <- if (all(s)) v else CopyAttributes(v[s], v)
                wgts <- if (all(s)) weights else weights[s]
                transformed.v <- flipTransformations::AsNumeric(v, binary = FALSE)
                expect_equal(VarianceColumns(v, sample = sample),
                             setNames(fun(transformed.v, na.rm = TRUE), name))
                expect_equal(VarianceColumns(v, remove.missing = FALSE, sample = sample),
                             setNames(fun(transformed.v, na.rm = FALSE), name))
                expect_equal(VarianceColumns(v, weights = wgts, sample = sample),
                             setNames(weighted.variance(transformed.v, wgts,
                                                        remove.missing = TRUE, sample = sample),
                                      name))
                expect_equal(VarianceColumns(v, weights = wgts, remove.missing = FALSE,
                                             sample = sample),
                             setNames(weighted.variance(transformed.v, wgts,
                                                        remove.missing = FALSE, sample = sample),
                                      name))
            }
        }
    }

    df <- data.frame(`Coca-Cola` = variable.Binary, Age = variable.Numeric,
                     Age = variable.Nominal, check.names = FALSE)
    for (fun in c(var, pvar))
    {
        sample <- identical(fun, var)
        expect_equal(VarianceColumns(df, sample = sample),
                     c("Coca-Cola" = fun(variable.Binary, na.rm = TRUE),
                       Age = fun(variable.Numeric, na.rm = TRUE),
                       Age = fun(variable.Numeric, na.rm = TRUE)))
        expect_equal(StandardDeviationColumns(df, sample = sample),
                     c("Coca-Cola" = sqrt(fun(variable.Binary, na.rm = TRUE)),
                       Age = sqrt(fun(variable.Numeric, na.rm = TRUE)),
                       Age = sqrt(fun(variable.Numeric, na.rm = TRUE))))

        expected.var <- weighted.variance(variable.Binary,
                                          weights,
                                          remove.missing = TRUE,
                                          sample = sample)
        expect_equal(VarianceColumns(variable.Binary, weights = weights, sample = sample),
                     c(`Coca-Cola` = expected.var))
        expect_true(is.na(VarianceColumns(variable.Binary,
                                          weights = weights,
                                          remove.missing = FALSE,
                                          sample = sample)))
        expected.var <- vapply(lapply(df, AsNumeric, binary = FALSE),
                               weighted.variance, numeric(1L),
                               wgts = weights, remove.missing = TRUE, sample = sample)
        expect_equal(VarianceColumns(df, weights = weights, sample = sample), expected.var)
    }
})

load("table2D.Percentage.rda")
load("table1D.Average.rda")
load("table2D.PercentageNaN.rda")

test_that("2d array input", {
    tables <- list(table2D.Percentage, table2D.PercentageNaN)
    for (tab in tables)
    {
        for (fun in c(var, pvar))
        {
            for (fun in c(var, pvar))
            {
                sample <- identical(fun, var)
                expect_equal(VarianceColumns(tab, remove.rows = NULL, remove.columns = NULL,
                                             remove.missing = FALSE, sample = sample),
                             apply(tab, 2L, fun, na.rm = FALSE))
                expect_equal(VarianceColumns(tab, remove.columns = "NET",
                                             remove.missing = FALSE, sample = sample),
                             apply(tab[rownames(tab) != "NET", colnames(tab) != "NET"],
                                   2L, fun, na.rm = FALSE))
                expect_equal(VarianceColumns(tab, remove.rows = "Pepsi",
                                             remove.missing = FALSE, sample = sample),
                             apply(tab[rownames(tab) != "Pepsi", ], 2L, fun, na.rm = FALSE))
                expect_equal(VarianceColumns(tab, remove.rows = NULL, remove.columns = NULL,
                                             remove.missing = TRUE, sample = sample),
                             apply(tab, 2L, fun, na.rm = TRUE))
                expect_equal(VarianceColumns(tab, remove.columns = "NET",
                                             remove.missing = TRUE, sample = sample),
                             apply(tab[rownames(tab) != "NET", colnames(tab) != "NET"],
                                   2L, fun, na.rm = TRUE))
                expect_equal(VarianceColumns(tab, remove.rows = "Pepsi",
                                             remove.missing = TRUE, sample = sample),
                             apply(tab[rownames(tab) != "Pepsi", ], 2L, fun, na.rm = TRUE))
                blank.input <- sanitizeAttributes(table2D.Percentage)
                dimnames(blank.input) <- NULL
                wgts <- runif(nrow(blank.input))
                all.weights <- apply(blank.input, 2L, sumWeights, weights = wgts)
                expect_equal(VarianceColumns(blank.input, sample = sample),
                             apply(blank.input, 2L, fun))
                split.x <- split(blank.input, rep(1:NCOL(blank.input), each = NROW(blank.input)))
                unnamed.split.x <- unname(split.x)
                expect_equal(VarianceColumns(blank.input, weights = wgts, sample = sample),
                             mapply(weighted.variance, unnamed.split.x,
                                    MoreArgs = list(remove.missing = TRUE, wgts = wgts, sample = sample)))
                input.with.colnames <- blank.input
                colnames(input.with.colnames) <- names(split.x) <- LETTERS[1:NCOL(blank.input)]
                expect_equal(VarianceColumns(input.with.colnames, weights = wgts, sample = sample),
                             mapply(weighted.variance, split.x,
                                    MoreArgs = list(remove.missing = TRUE, wgts = wgts, sample = sample)))
            }
        }
    }
})

test_that("3d array input", {
    load("table2D.PercentageAndCount.rda")
    tab <- table2D.PercentageAndCount
    for (fun in c(var, pvar))
    {
        sample <- identical(fun, var)
        expect_equal(VarianceColumns(tab, remove.rows = NULL, remove.columns = NULL,
                                     remove.missing = FALSE, sample = sample),
                     apply(tab, 2:3, fun, na.rm = FALSE))
        expect_equal(VarianceColumns(tab, remove.columns = "NET",
                                     remove.missing = FALSE, sample = sample),
                     apply(tab[rownames(tab) != "NET", colnames(tab) != "NET", ],
                           2:3, fun, na.rm = FALSE))
        expect_equal(VarianceColumns(tab, remove.rows = "Pepsi",
                                     remove.missing = FALSE, sample = sample),
                     apply(tab[rownames(tab) != "Pepsi", , ], 2:3, fun, na.rm = FALSE))
        expect_equal(VarianceColumns(tab, remove.rows = NULL, remove.columns = NULL,
                                     remove.missing = TRUE, sample = sample),
                     apply(tab, 2:3, fun, na.rm = TRUE))
        expect_equal(VarianceColumns(tab, remove.columns = "NET",
                                     remove.missing = TRUE, sample = sample),
                     apply(tab[rownames(tab) != "NET", colnames(tab) != "NET", ],
                           2:3, fun, na.rm = TRUE))
        expect_equal(VarianceColumns(tab, remove.rows = "Pepsi",
                                     remove.missing = TRUE, sample = sample),
                     apply(tab[rownames(tab) != "Pepsi", , ], 2:3, fun, na.rm = TRUE))
        blank.input <- sanitizeAttributes(table2D.PercentageAndCount)
        expected.error <- capture_error(throwErrorAboutHigherDimArray(3L, quoted.function))[["message"]]
        expect_error(VarianceColumns(blank.input), expected.error, fixed = TRUE)
    }
})

test_that("Warnings", {
    two.vals.warning <- capture_warnings(warnSampleVarCalcWithSingleVal(table1D.Average, 2L, quoted.function))
    missing.val.warning <- capture_warnings(warnAboutMissingValuesIgnored())
    opp.inf.warning <- capture_warnings(warnAboutOppositeInfinities(TRUE, quoted.function))
    expect_warning(VarianceColumns(c(Inf, 1, -Inf), warn = TRUE), opp.inf.warning)
    opp.inf.warning <- capture_warnings(warnAboutOppositeInfinities(c(TRUE, FALSE), quoted.function))
    expect_warning(VarianceColumns(cbind(c(Inf, 1, -Inf), 1:3), warn = TRUE),
                   opp.inf.warning)
    # Sample warning
    not.enough.warn <- capture_warnings(throwWarningAboutDimWithTooManyMissing(2L, sample = TRUE,
                                                                               function.name = quoted.function))
    variance.calc <- quote(VarianceColumns(c(NA, NA, 2), warn = TRUE))
    observed.warn <- capture_warnings(eval(variance.calc))
    expect_setequal(observed.warn, c(missing.val.warning, not.enough.warn))
    variance.calc[["warn"]] <- "Foo"
    observed.warn <- expect_warning(obs.var <- eval(variance.calc), NA)
    expect_true(is.na(obs.var))
    # Population warnings
    all.missing.warn <- capture_warnings(throwWarningAboutDimWithTooManyMissing(2L, sample = FALSE,
                                                                                function.name = quoted.function))
    variance.calc <- quote(VarianceColumns(c(NA, NA, 2), sample = FALSE, warn = TRUE))
    observed.warn <- capture_warnings(eval(variance.calc))
    expect_equal(observed.warn, missing.val.warning)
    variance.calc[["warn"]] <- "Foo"
    expect_equal(eval(variance.calc), 0)
    expect_warning(VarianceColumns(c(NA, NA, NA), sample = FALSE, warn = "Foo"), NA)
    # Other warnings
    observed.warnings <- capture_warnings(expect_equal(VarianceColumns(cbind(c(NA, NA, 2),
                                                                             1:3),
                                                                       warn = TRUE),
                                                       c(NA, 1)))
    expect_setequal(observed.warnings, c(missing.val.warning, not.enough.warn))
    expect_warning(VarianceColumns(cbind(c(NA, NA, 2), 1:3), warn = "Foo"), NA)
    expect_warning(VarianceColumns(array(c(rep(NA, 2), 1, 1:3), dim = c(3L, 2L)),
                                   warn = TRUE, remove.missing = FALSE),
                   NA)
})

test_that("EachColumn aliases working", {
    expect_equal(VarianceEachColumn, VarianceColumns)
    expect_equal(VarianceColumns(table2D.Percentage),
                 VarianceEachColumn(table2D.Percentage))
    expect_equal(StandardDeviationEachColumn, StandardDeviationColumns)
    expect_equal(StandardDeviationColumns(table2D.Percentage),
                 StandardDeviationEachColumn(table2D.Percentage))
})

test_that("Warnings muffled", {
    # Not show the missing value warning
    input.array <- array(1:12, dim = 3:4, dimnames = list(LETTERS[1:3], NULL))
    is.na(input.array) <- seq(from = 1, to = 12, by = 3)
    expect_equal(VarianceColumns(input.array, warn = "Foo"), apply(input.array, 2L, var, na.rm = TRUE))
})
