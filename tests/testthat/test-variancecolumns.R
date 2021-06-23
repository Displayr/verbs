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

weighted.variance <- function(x, wgts, remove.missing)
{
    invalid <- is.na(x) | is.na(wgts) | wgts < 0
    wgts[invalid] <- 0
    sum.w <- sumWeights(x, wgts)
    n.x <- length(x)
    denom <- (n.x - 1L)/n.x * sum.w
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
            v <- if (all(s)) v else CopyAttributes(v[s], v)
            wgts <- if (all(s)) weights else weights[s]
            transformed.v <- flipTransformations::AsNumeric(v, binary = FALSE)
            expect_equal(VarianceColumns(v),
                         setNames(var(transformed.v, na.rm = TRUE), name))
            expect_equal(VarianceColumns(v, remove.missing = FALSE),
                         setNames(var(transformed.v, na.rm = FALSE), name))
            expect_equal(VarianceColumns(v, weights = wgts),
                         setNames(weighted.variance(transformed.v, wgts, remove.missing = TRUE),
                                  name))
            expect_equal(VarianceColumns(v, weights = wgts, remove.missing = FALSE),
                         setNames(weighted.variance(transformed.v, wgts, remove.missing = FALSE),
                                  name))
        }
    }

    df <- data.frame(`Coca-Cola` = variable.Binary, Age = variable.Numeric,
                     Age = variable.Nominal, check.names = FALSE)
    expect_equal(VarianceColumns(df),
                 c("Coca-Cola" = var(variable.Binary, na.rm = TRUE),
                   Age = var(variable.Numeric, na.rm = TRUE),
                   Age = var(variable.Numeric, na.rm = TRUE)))
    expect_equal(StandardDeviationColumns(df),
                 c("Coca-Cola" = sd(variable.Binary, na.rm = TRUE),
                   Age = sd(variable.Numeric, na.rm = TRUE),
                   Age = sd(variable.Numeric, na.rm = TRUE)))

    expected.var <- weighted.variance(variable.Binary,
                                      weights,
                                      remove.missing = TRUE)
    expect_equal(VarianceColumns(variable.Binary, weights = weights),
                 c(`Coca-Cola` = expected.var))
    expect_true(is.na(VarianceColumns(variable.Binary,
                                      weights = weights,
                                      remove.missing = FALSE)))
    expected.var <- vapply(lapply(df, AsNumeric, binary = FALSE),
                           weighted.variance, numeric(1L),
                           wgts = weights, remove.missing = TRUE)
    expect_equal(VarianceColumns(df, weights = weights), expected.var)
})

load("table2D.Percentage.rda")
load("table1D.Average.rda")
load("table2D.PercentageNaN.rda")

test_that("2d array input", {
    tables <- list(table2D.Percentage, table2D.PercentageNaN)
    for (tab in tables)
    {
        expect_equal(VarianceColumns(tab, remove.rows = NULL, remove.columns = NULL,
                                     remove.missing = FALSE),
                     apply(tab, 2L, var, na.rm = FALSE))
        expect_equal(VarianceColumns(tab, remove.columns = "NET", remove.missing = FALSE),
                     apply(tab[rownames(tab) != "NET", colnames(tab) != "NET"],
                           2L, var, na.rm = FALSE))
        expect_equal(VarianceColumns(tab, remove.rows = "Pepsi", remove.missing = FALSE),
                     apply(tab[rownames(tab) != "Pepsi", ], 2L, var, na.rm = FALSE))
        expect_equal(VarianceColumns(tab, remove.rows = NULL, remove.columns = NULL,
                                     remove.missing = TRUE),
                     apply(tab, 2L, var, na.rm = TRUE))
        expect_equal(VarianceColumns(tab, remove.columns = "NET", remove.missing = TRUE),
                     apply(tab[rownames(tab) != "NET", colnames(tab) != "NET"],
                           2L, var, na.rm = TRUE))
        expect_equal(VarianceColumns(tab, remove.rows = "Pepsi", remove.missing = TRUE),
                     apply(tab[rownames(tab) != "Pepsi", ], 2L, var, na.rm = TRUE))
        blank.input <- sanitizeAttributes(table2D.Percentage)
        dimnames(blank.input) <- NULL
        wgts <- runif(nrow(blank.input))
        all.weights <- apply(blank.input, 2L, sumWeights, weights = wgts)
        expect_equal(VarianceColumns(blank.input),
                     apply(blank.input, 2L, var))
        split.x <- split(blank.input, rep(1:NCOL(blank.input), each = NROW(blank.input)))
        unnamed.split.x <- unname(split.x)
        expect_equal(VarianceColumns(blank.input, weights = wgts),
                     mapply(weighted.variance, unnamed.split.x,
                            MoreArgs = list(remove.missing = TRUE, wgts = wgts)))
        input.with.colnames <- blank.input
        colnames(input.with.colnames) <- names(split.x) <- LETTERS[1:NCOL(blank.input)]
        expect_equal(VarianceColumns(input.with.colnames, weights = wgts),
                     mapply(weighted.variance, split.x,
                            MoreArgs = list(remove.missing = TRUE, wgts = wgts)))

    }
})

test_that("3d array input", {
    load("table2D.PercentageAndCount.rda")
    tab <- table2D.PercentageAndCount

    expect_equal(VarianceColumns(tab, remove.rows = NULL, remove.columns = NULL,
                                 remove.missing = FALSE),
                 apply(tab, 2:3, var, na.rm = FALSE))
    expect_equal(VarianceColumns(tab, remove.columns = "NET", remove.missing = FALSE),
                 apply(tab[rownames(tab) != "NET", colnames(tab) != "NET", ],
                       2:3, var, na.rm = FALSE))
    expect_equal(VarianceColumns(tab, remove.rows = "Pepsi", remove.missing = FALSE),
                 apply(tab[rownames(tab) != "Pepsi", , ], 2:3, var, na.rm = FALSE))
    expect_equal(VarianceColumns(tab, remove.rows = NULL, remove.columns = NULL,
                                 remove.missing = TRUE),
                 apply(tab, 2:3, var, na.rm = TRUE))
    expect_equal(VarianceColumns(tab, remove.columns = "NET", remove.missing = TRUE),
                 apply(tab[rownames(tab) != "NET", colnames(tab) != "NET", ],
                       2:3, var, na.rm = TRUE))
    expect_equal(VarianceColumns(tab, remove.rows = "Pepsi", remove.missing = TRUE),
                 apply(tab[rownames(tab) != "Pepsi", , ], 2:3, var, na.rm = TRUE))
    blank.input <- sanitizeAttributes(table2D.PercentageAndCount)
    expected.error <- capture_error(throwErrorAboutHigherDimArray(3L, quoted.function))[["message"]]
    expect_error(VarianceColumns(blank.input), expected.error, fixed = TRUE)
})

test_that("Warnings", {
    expected.warning <- capture_warnings(throwWarningAboutVarianceCalculationWithSingleElement(table1D.Average, 1L, quoted.function))
    expect_warning(VarianceColumns(table1D.Average[1], warn = TRUE), expected.warning)
    expected.warning <- capture_warnings(warnAboutOppositeInfinities(TRUE, quoted.function))
    expect_warning(VarianceColumns(c(Inf, 1, -Inf), warn = TRUE), expected.warning)
    expected.warning <- capture_warnings(warnAboutOppositeInfinities(c(TRUE, FALSE), quoted.function))
    expect_warning(VarianceColumns(cbind(c(Inf, 1, -Inf), 1:3), warn = TRUE),
                   expected.warning)
})

test_that("EachColumn aliases working", {
    expect_equal(VarianceEachColumn, VarianceColumns)
    expect_equal(VarianceColumns(table2D.Percentage),
                 VarianceEachColumn(table2D.Percentage))
    expect_equal(StandardDeviationEachColumn, StandardDeviationColumns)
    expect_equal(StandardDeviationColumns(table2D.Percentage),
                 StandardDeviationEachColumn(table2D.Percentage))
})
