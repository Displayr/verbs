context("VarianceEachRow")

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

quoted.function <- sQuote("VarianceRows")

test_that("Variables/Variable Sets", {
    n <- length(variable.Binary)
    df <- data.frame(`Coca-Cola` = variable.Binary, Age = variable.Numeric,
                     Age = variable.Nominal, check.names = FALSE)
    df.transformed <- flipTransformations::AsNumeric(df, binary = FALSE)
    for (fun in c(var, pvar))
    {
        sample <- identical(fun, var)
        expect_equal(VarianceRows(df, sample = sample),
                     setNames(apply(df.transformed, 1L, fun, na.rm = TRUE), 1:n))
        expect_equal(StandardDeviationRows(df, sample = sample),
                     setNames(sqrt(apply(df.transformed, 1L, fun, na.rm = TRUE)), 1:n))
        should.be.missing <- VarianceRows(variable.Binary, remove.missing = FALSE, sample = sample)
        expected.out <- if (sample) rep(NA_real_, n) else c(0, NA_real_)[is.na(variable.Binary) + 1L]
        expect_equal(should.be.missing, expected.out)
        expect_true(length(should.be.missing) == n)
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
            sample <- identical(fun, var)
            expect_equal(VarianceRows(tab, remove.rows = NULL, remove.columns = NULL,
                                      remove.missing = FALSE, sample = sample),
                         apply(tab, 1L, fun, na.rm = FALSE))
            expect_equal(VarianceRows(tab, remove.rows = "NET",
                                      remove.missing = FALSE, sample = sample),
                         apply(tab[rownames(tab) != "NET", colnames(tab) != "NET"],
                               1L, fun, na.rm = FALSE))
            expect_equal(VarianceRows(tab, remove.rows = "Pepsi", remove.columns = NULL,
                                      remove.missing = FALSE, sample = sample),
                         apply(tab[rownames(tab) != "Pepsi", ], 1L, fun, na.rm = FALSE))
            expect_equal(VarianceRows(tab, remove.rows = NULL, remove.columns = NULL,
                                      remove.missing = TRUE, sample = sample),
                         apply(tab, 1L, fun, na.rm = TRUE))
            expect_equal(VarianceRows(tab, remove.rows = "NET",
                                      remove.missing = TRUE, sample = sample),
                         apply(tab[rownames(tab) != "NET", colnames(tab) != "NET"],
                               1L, fun, na.rm = TRUE))
            expect_equal(VarianceRows(tab, remove.rows = "Pepsi",
                                      remove.missing = TRUE, sample = sample),
                         apply(tab[rownames(tab) != "Pepsi", colnames(tab) != "NET"], 1L, fun, na.rm = TRUE))
        }
    }
    blank.input <- sanitizeAttributes(table2D.Percentage)
    dimnames(blank.input) <- NULL
    input.with.rownames <- blank.input
    rownames(input.with.rownames) <- LETTERS[1:NROW(blank.input)]
    for (fun in c(var, pvar))
    {
        sample <- identical(fun, var)
        expect_equal(VarianceRows(blank.input, sample = sample),
                     apply(blank.input, 1L, fun))
        expect_equal(VarianceRows(input.with.rownames, sample = sample),
                     apply(input.with.rownames, 1L, fun, na.rm = TRUE))
    }
})

test_that("3d array input", {
    load("table2D.PercentageAndCount.rda")
    tab <- table2D.PercentageAndCount
    tab.degenerate <- tab[, , 1, drop = FALSE]
    attr(tab.degenerate, "questions") <- attr(tab, "questions")
    for (fun in c(var, pvar))
    {
        sample <- identical(fun, var)
        expect_equal(VarianceRows(tab, remove.rows = NULL, remove.columns = NULL,
                                  remove.missing = FALSE, sample = sample),
                     apply(tab, c(1L, 3L), fun, na.rm = FALSE))
        expect_equal(VarianceRows(tab, remove.columns = "NET",
                                  remove.missing = FALSE, sample = sample),
                     apply(tab[rownames(tab) != "NET", colnames(tab) != "NET", ],
                           c(1L, 3L), fun, na.rm = FALSE))
        expect_equal(VarianceRows(tab, remove.rows = "Pepsi",
                                  remove.missing = FALSE, sample = sample),
                     apply(tab[rownames(tab) != "Pepsi", colnames(tab) != "NET", ],
                           c(1L, 3L), fun, na.rm = FALSE))
        expect_equal(VarianceRows(tab, remove.rows = NULL, remove.columns = NULL,
                                  remove.missing = TRUE, sample = sample),
                     apply(tab, c(1L, 3L), fun, na.rm = TRUE))
        expect_equal(VarianceRows(tab, remove.columns = "NET",
                                  remove.missing = TRUE, sample = sample),
                     apply(tab[rownames(tab) != "NET", colnames(tab) != "NET", ],
                           c(1L, 3L), fun, na.rm = TRUE))
        expect_equal(VarianceRows(tab, remove.rows = "Pepsi",
                                  remove.missing = TRUE, sample = sample),
                     apply(tab[rownames(tab) != "Pepsi", colnames(tab) != "NET", ],
                           c(1L, 3L), fun, na.rm = TRUE))
        remove.cols <- colnames(tab)[colnames(tab) != "Never"]
        expected.out <- setNames(rep(if (sample) NA_real_ else 0, nrow(tab)),
                                 rownames(tab.degenerate))
        expect_equal(VarianceRows(tab.degenerate, remove.columns = remove.cols, sample = sample),
                     expected.out)
        blank.input <- sanitizeAttributes(table2D.PercentageAndCount)
        expected.error <- capture_error(throwErrorAboutHigherDimArray(3L, quoted.function))[["message"]]
        expect_error(VarianceRows(blank.input, sample = sample), expected.error, fixed = TRUE)
    }
})

test_that("Warnings", {
    single.element.warning <- capture_warnings(throwWarningAboutVarianceCalculationWithSingleElement(variable.Binary,
                                                                                                     1L,
                                                                                                     quoted.function))
    expect_setequal(capture_warnings(output <- VarianceRows(variable.Binary, warn = TRUE)), single.element.warning)
    expect_equal(output, rep(NA_real_, NROW(variable.Binary)))
    two.vals.warning <- capture_warnings(throwWarningAboutDimWithTooManyMissing(1L, sample = TRUE, quoted.function))
    expect_warning(VarianceRows(data.frame(variable.Binary, variable.Numeric), warn = TRUE),
                   two.vals.warning)
    all.missing.warning <- capture_warnings(throwWarningAboutDimWithTooManyMissing(1L, sample = FALSE, quoted.function))
    expect_warning(VarianceRows(data.frame(variable.Binary, variable.Numeric), sample = FALSE, warn = TRUE),
                   all.missing.warning)
    expect_warning(VarianceRows(data.frame(variable.Binary, variable.Numeric),
                                warn = TRUE,
                                remove.missing = FALSE),
                   NA)
    opp.inf.warning <- capture_warnings(warnAboutOppositeInfinities(c(TRUE, FALSE), quoted.function))
    expect_warning(VarianceRows(rbind(c(Inf, -Inf), 1:2), warn = TRUE), opp.inf.warning)
    opp.inf.warning <- capture_warnings(warnAboutOppositeInfinities(c(TRUE, TRUE), quoted.function))
    expect_warning(VarianceRows(rbind(c(Inf, -Inf), c(Inf, -Inf)), warn = TRUE), opp.inf.warning)
    expect_warning(VarianceRows(array(c(NA, 1:2, NA, 3:4, NA, 5:6), dim = c(3L, 3L)), warn = TRUE),
                   two.vals.warning)
})

test_that("EachRow aliases working", {
    expect_equal(VarianceEachRow, VarianceRows)
    expect_equal(VarianceRows(table2D.Percentage),
                 VarianceEachRow(table2D.Percentage))
    expect_equal(StandardDeviationEachRow, StandardDeviationRows)
    expect_equal(StandardDeviationRows(table2D.Percentage),
                 StandardDeviationEachRow(table2D.Percentage))
})
