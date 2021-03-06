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
    expect_equal(VarianceRows(df),
                 setNames(apply(df.transformed, 1L, var, na.rm = TRUE), 1:n))
    expect_equal(StandardDeviationRows(df),
                 setNames(apply(df.transformed, 1L, sd, na.rm = TRUE), 1:n))
    should.be.missing <- VarianceRows(variable.Binary, remove.missing = FALSE)
    expect_true(all(is.na(should.be.missing)) && length(should.be.missing) == n)
})

load("table2D.Percentage.rda")
load("table1D.Average.rda")
load("table2D.PercentageNaN.rda")

test_that("2d array input", {
    tables <- list(table2D.Percentage, table2D.PercentageNaN)
    for (tab in tables)
    {
        expect_equal(VarianceRows(tab, remove.rows = NULL, remove.columns = NULL,
                                  remove.missing = FALSE),
                     apply(tab, 1L, var, na.rm = FALSE))
        expect_equal(VarianceRows(tab, remove.rows = "NET", remove.missing = FALSE),
                     apply(tab[rownames(tab) != "NET", colnames(tab) != "NET"],
                           1L, var, na.rm = FALSE))
        expect_equal(VarianceRows(tab, remove.rows = "Pepsi", remove.columns = NULL, remove.missing = FALSE),
                     apply(tab[rownames(tab) != "Pepsi", ], 1L, var, na.rm = FALSE))
        expect_equal(VarianceRows(tab, remove.rows = NULL, remove.columns = NULL,
                                     remove.missing = TRUE),
                     apply(tab, 1L, var, na.rm = TRUE))
        expect_equal(VarianceRows(tab, remove.rows = "NET", remove.missing = TRUE),
                     apply(tab[rownames(tab) != "NET", colnames(tab) != "NET"],
                           1L, var, na.rm = TRUE))
        expect_equal(VarianceRows(tab, remove.rows = "Pepsi", remove.missing = TRUE),
                     apply(tab[rownames(tab) != "Pepsi", colnames(tab) != "NET"], 1L, var, na.rm = TRUE))
    }
    blank.input <- sanitizeAttributes(table2D.Percentage)
    dimnames(blank.input) <- NULL
    expect_equal(VarianceRows(blank.input),
                 apply(blank.input, 1L, var))
    input.with.rownames <- blank.input
    rownames(input.with.rownames) <- LETTERS[1:NROW(blank.input)]
    expect_equal(VarianceRows(input.with.rownames),
                 apply(input.with.rownames, 1L, var, na.rm = TRUE))
})

test_that("3d array input", {
    load("table2D.PercentageAndCount.rda")
    tab <- table2D.PercentageAndCount
    tab.degenerate <- tab[, , 1, drop = FALSE]
    attr(tab.degenerate, "questions") <- attr(tab, "questions")
    expect_equal(VarianceRows(tab, remove.rows = NULL, remove.columns = NULL,
                                 remove.missing = FALSE),
                 apply(tab, c(1L, 3L), var, na.rm = FALSE))
    expect_equal(VarianceRows(tab, remove.columns = "NET", remove.missing = FALSE),
                 apply(tab[rownames(tab) != "NET", colnames(tab) != "NET", ],
                       c(1L, 3L), var, na.rm = FALSE))
    expect_equal(VarianceRows(tab, remove.rows = "Pepsi", remove.missing = FALSE),
                 apply(tab[rownames(tab) != "Pepsi", colnames(tab) != "NET", ],
                       c(1L, 3L), var, na.rm = FALSE))
    expect_equal(VarianceRows(tab, remove.rows = NULL, remove.columns = NULL,
                                 remove.missing = TRUE),
                 apply(tab, c(1L, 3L), var, na.rm = TRUE))
    expect_equal(VarianceRows(tab, remove.columns = "NET", remove.missing = TRUE),
                 apply(tab[rownames(tab) != "NET", colnames(tab) != "NET", ],
                       c(1L, 3L), var, na.rm = TRUE))
    expect_equal(VarianceRows(tab, remove.rows = "Pepsi", remove.missing = TRUE),
                 apply(tab[rownames(tab) != "Pepsi", colnames(tab) != "NET", ],
                       c(1L, 3L), var, na.rm = TRUE))
    remove.cols <- colnames(tab)[colnames(tab) != "Never"]
    expect_true(all(is.na(VarianceRows(tab.degenerate, remove.columns = remove.cols))))
    blank.input <- sanitizeAttributes(table2D.PercentageAndCount)
    expected.error <- capture_error(throwErrorAboutHigherDimArray(3L, quoted.function))[["message"]]
    expect_error(VarianceRows(blank.input), expected.error, fixed = TRUE)
})

test_that("Warnings", {
    expected.warnings <- capture_warnings(throwWarningAboutVarianceCalculationWithSingleElement(variable.Binary,
                                                                                                1L,
                                                                                                quoted.function))
    expect_setequal(capture_warnings(output <- VarianceRows(variable.Binary, warn = TRUE)), expected.warnings)
    expect_equal(output, rep(NA, NROW(variable.Binary)))
    expected.warning <- capture_warnings(throwWarningAboutDimWithTooManyMissing(1L, quoted.function))
    expect_warning(VarianceRows(data.frame(variable.Binary, variable.Numeric), warn = TRUE),
                   expected.warning)
    expect_warning(VarianceRows(data.frame(variable.Binary, variable.Numeric),
                                warn = TRUE,
                                remove.missing = FALSE),
                   NA)
    expected.warning <- capture_warnings(warnAboutOppositeInfinities(c(TRUE, FALSE), quoted.function))
    expect_warning(VarianceRows(rbind(c(Inf, -Inf), 1:2), warn = TRUE), expected.warning)
    expected.warning <- capture_warnings(warnAboutOppositeInfinities(c(TRUE, TRUE), quoted.function))
    expect_warning(VarianceRows(rbind(c(Inf, -Inf), c(Inf, -Inf)), warn = TRUE), expected.warning)
})

test_that("EachRow aliases working", {
    expect_equal(VarianceEachRow, VarianceRows)
    expect_equal(VarianceRows(table2D.Percentage),
                 VarianceEachRow(table2D.Percentage))
    expect_equal(StandardDeviationEachRow, StandardDeviationRows)
    expect_equal(StandardDeviationRows(table2D.Percentage),
                 StandardDeviationEachRow(table2D.Percentage))
})
