context("Average")

load("variable.Text.rda")
load("variable.Binary.rda")
load("variable.Nominal.rda")
load("variable.Numeric.rda")
load("variable.Time.rda")
load("variable.Date.rda")

.computeSampleSize <- function(...)
{
    x <- list(...)
    dims <- dim(x[[1L]])
    counts <- lapply(x, Negate(is.na))
    n.sum <- Reduce(`+`, counts)
    n.sum
}

test_that("Variables", {
    expect_error(Average(variable.Text),
                 paste0("Text data has been supplied but ", sQuote("Average"), " requires numeric data."))
    expect_equal(Average(variable.Binary), Sum(variable.Binary) / sum(!is.na(variable.Binary)))
    expect_true(is.na(Average(variable.Binary, remove.missing = FALSE)))
    expect_equal(Average(variable.Numeric), Sum(variable.Numeric) / sum(!is.na(variable.Numeric)))
    expect_error(Average(variable.Date), paste0("Date/Time data has been supplied but ", sQuote("Average"), " requires numeric data.")) # Not that means and the like are defined
    expect_error(Average(variable.Time), paste0("Date/Time data has been supplied but ", sQuote("Average"), " requires numeric data.")) # Not that means and the like are defined
    ## Factors
    # With value attributes
    expect_equal(Average(variable.Nominal), Sum(variable.Nominal) / sum(!is.na(variable.Nominal)))
    expect_warning(Average(variable.Nominal), NA)
    # Without value attributes
    expect_warning(basic.factor <- Average(factor(1:10)),
                   "Data has been automatically converted to numeric")
    expect_equal(basic.factor, sum(1:10)/10)
    # Warnings about missing values
    expect_warning(Average(variable.Binary, warn = TRUE),
                   "Missing values have been ignored in calculation.")
    # Missing values in calculations
    expect_true(is.na(Average(variable.Binary, remove.missing = FALSE)))
    expect_true(is.na(Average(variable.Numeric, remove.missing = FALSE)))
    # Multiple variables
    expected.avg <- as.array(as.vector((variable.Binary + variable.Numeric) / 2))
    expect_equivalent(Average(variable.Binary, variable.Numeric, remove.missing = FALSE),
                      expected.avg)
    inputs <- list(variable.Binary, variable.Numeric)
    expected.inputs <- lapply(inputs, function(x) {
        x[is.na(x)] <- 0
        x
    })
    # Expect no warning about statistics if no missing data is present
    expect_equivalent(Average(variable.Binary, variable.Numeric, remove.missing = FALSE, warn = TRUE),
                      expected.avg)
    n.sum <- apply(vapply(inputs, Negate(is.na), logical(length(variable.Binary))), 1, sum)
    expect_equivalent(Average(variable.Binary, variable.Numeric, remove.missing = TRUE),
                      Sum(variable.Binary, variable.Numeric)/n.sum)
    # Expect Variable sets to be handled ok
    ## Test with none missing
    df1 <- data.frame(x = runif(10), y = runif(10))
    df2 <- data.frame(y = runif(10), z = runif(10))
    edf1 <- cbind(df1, z = df2[["z"]])
    edf2 <- cbind(x = df1[["x"]], df2)
    expected.sum <- as.matrix(data.frame(x = df1[["x"]], y = df1[["y"]] + df2[["y"]], z = df2[["z"]]))
    n.sum <- array(c(rep(1, 10), rep(2, 10), rep(1, 10)), dim = c(10, 3), dimnames = list(1:10, c("x", "y", "z")))
    expected.out <- expected.sum/n.sum
    expect_equivalent(Average(df1, df2), expected.out)
    indices.to.modify <- expand.grid(1:10, 1:2)
    ## Test with some missing from each
    inds.with.missing <- sample(1:nrow(indices.to.modify), size = 2)
    mdf1 <- df1
    for (ind in inds.with.missing)
        mdf1[indices.to.modify[ind, 1], indices.to.modify[ind, 2]] <- NA
    inds.with.missing <- sample(1:nrow(indices.to.modify), size = 2)
    mdf2 <- df2
    inds.with.missing <- sample(setdiff(1:nrow(indices.to.modify), inds.with.missing), size = 2)
    for (ind in inds.with.missing)
        mdf2[indices.to.modify[ind, 1], indices.to.modify[ind, 2]] <- NA
    expected.sum <- Sum(mdf1, mdf2)
    if (anyNA(mdf1[["x"]]))
        n.sum[is.na(mdf1[["x"]]), 1] <- 0
    if (anyNA(mdf1[["y"]]) || anyNA(mdf2[["y"]]))
        n.sum[, 2] <- apply(!is.na(cbind(mdf1[["y"]], mdf2[["y"]])), 1, sum)
    if (anyNA(mdf2[["z"]]))
        n.sum[is.na(mdf2[["z"]]), 3] <- 0
    expect_equal(Average(mdf1, mdf2), Sum(mdf1, mdf2)/n.sum)
})

test_that("Variables with weights, filters (subset), and a combination of the two", {
    subset.missing.out <- !is.na(variable.Numeric)
    expect_equal(Average(variable.Numeric, subset = subset.missing.out),
                 Sum(variable.Numeric)/sum(subset.missing.out))
    transformed.nom <- flipTransformations::AsNumeric(variable.Nominal, binary = FALSE)
    transformed.nom <- transformed.nom[!is.na(transformed.nom)]
    subset.num <- variable.Numeric[!is.na(variable.Numeric)]
    expect_equivalent(Average(variable.Numeric, variable.Nominal, subset = subset.missing.out),
                      (transformed.nom + subset.num)/2)
    expect_error(Average(variable.Numeric[1:10], subset = subset.missing.out),
                 paste0("The subset vector has length 327. However, it needs to ",
                        "have length 10 to match the number of cases in the supplied input data."))
    expect_error(Average(variable.Numeric, 1:10, subset = subset.missing.out),
                 paste0(sQuote('Average'), " requires all input elements to have the same size to be able ",
                        "to apply a filter or weight vector. ",
                        verbs:::determineAppropriateContact()),
                 fixed = TRUE)
    weights <- runif(length(variable.Numeric))
    expect_equal(Average(variable.Numeric, weights = weights),
                 flipStatistics::Mean(variable.Numeric, weights = weights))
    expect_equal(Average(variable.Numeric, variable.Nominal,
                         weights = weights),
                 Average(variable.Numeric, variable.Nominal))
    shuffled.variable.Nominal <- CopyAttributes(sample(variable.Nominal), variable.Nominal)
    sum.w <- sum(computeTotalWeights(data.frame(variable.Numeric, shuffled.variable.Nominal),
                                     weights = weights), na.rm = TRUE)
    expect_equal(Average(data.frame(variable.Numeric, shuffled.variable.Nominal),
                         weights = weights),
                 Sum(data.frame(variable.Numeric, shuffled.variable.Nominal),
                     weights = weights)/sum.w)
    expect_error(Average(variable.Numeric, weights = weights[1:10]),
                 paste0("The weights vector has length 10. However, it needs to ",
                        "have length 327 to match the number of cases in the supplied input data."))
})

load("table1D.Average.rda")
load("table1D.Percentage.rda")
load("table.1D.MultipleStatistics.rda")
test_that("Table 1D", {
    expect_equal(Average(table1D.Percentage, remove.rows = "NET"),
                 mean(table1D.Percentage[names(table1D.Percentage) != "NET"]))
    expect_equal(Average(table1D.Percentage, remove.rows = "NET"),
                 mean(table1D.Percentage[names(table1D.Percentage) != "NET"]))
    expect_true(is.nan(Average(table.1D.MultipleStatistics)))
    captured.warnings <- capture_warnings(Average(table.1D.MultipleStatistics, warn = TRUE))
    stat.names <- dimnames(table.1D.MultipleStatistics)[[2]]
    expect_setequal(captured.warnings,
                    c(paste0("The input data contains statistics of different types (i.e., ",
                             paste0(stat.names, collapse = ", "),
                             "), it may not be appropriate to compute ", sQuote("Average"), "."),
                      paste0(sQuote('Average'), " cannot be computed as the data contains both Inf and -Inf.")))
    # Removal of row categories in a 1D table
    expect_equal(Average(table1D.Average, remove.rows = "SUM"), sum(table1D.Average[1:3])/3)
    expect_equal(Average(table1D.Average,
                         remove.rows = NULL), sum(table1D.Average[1:4])/4)
    # Missing values
    z = table1D.Average
    z[2] = NA
    expect_equal(Average(z, remove.rows = "SUM"), sum(z[1:3], na.rm = TRUE)/2)
    expect_true(is.na(Average(z, remove.missing = FALSE)))
})

load("table2D.Percentage.rda")
load("table2D.PercentageAndCount.rda")
load("table2D.PercentageNaN.rda")
test_that("Table 2D",
{
    # Expect elements in the table to be summed, ignoring the NET
    expect_equal(Average(table2D.Percentage, remove.columns = "NET"),
                 mean(table2D.Percentage[, colnames(table2D.Percentage) != "NET"]))
    expect_equal(Average(table2D.PercentageNaN, remove.columns = "NET", remove.rows = "NET"),
                 mean(table2D.PercentageNaN[-8, -10], na.rm = TRUE))
    # Note that while we represent this as a 3D array, from the user's perspective
    # this is a 2D table, where the third dimension is stacked into the rows.
    expect_equal(Average(table2D.PercentageAndCount, remove.columns = "NET", remove.rows = "NET"),
                 mean(table2D.PercentageAndCount[-8, -10, ]))
    # Warning for dodgy calculation

    expect_warning(Average(table2D.PercentageAndCount, warn = TRUE),
                   paste0("The input data contains statistics of different types ",
                          "(i.e., Row %, Count), it may not be appropriate to compute ",
                          sQuote("Average"), "."), fixed = TRUE)

    # Extra category removed removed
    expect_equal(Average(table2D.PercentageNaN, remove.rows = c("NET", "None of these"), remove.columns = "NET"),
                 mean(table2D.PercentageNaN[-7:-8, -10], na.rm = TRUE))

    # Missing values
    expect_true(is.na(Average(table2D.PercentageNaN, remove.missing = FALSE)))
})

.removeAttributes <- function(x)
{
    attr.out <- setdiff(names(attributes(x)),
                        c("dim", "names", "dimnames"))
    for (a in attr.out)
        attr(x, a) <- NULL
    x
}

test_that("Q Tables: Check warning of different statistics thrown or suppressed", {
    # Matching statistics (No warnings)
    # warning already suppressed by default
    inputs <- list(table2D.Percentage, table2D.PercentageNaN[-(7:8), ])
    inputs <- lapply(inputs, .removeAttributes)
    expected.table.out <- Reduce(`+`, inputs)/2
    dimnames(expected.table.out)[[2L]] <- paste0(colnames(table2D.Percentage), " + ",
                                                 colnames(table2D.PercentageNaN))
    x <- table2D.Percentage
    y <- table2D.PercentageNaN
    row.names(y)[1L] <- "Coca-Cola"
    expect_equal(Average(x, y,
                         remove.missing = FALSE,
                         remove.rows = c("None of these", "NET"),
                         match.rows = "Yes",
                         match.columns = "No"),
                 expected.table.out)
    sanitized.inputs <- lapply(inputs, function(x) {
        x[is.na(x)] <- 0
        x
    })
    n.sum <- .computeSampleSize(x, y[!rownames(y) %in% c("None of these", "NET"), ])
    expected.sanitized.out <- Reduce(`+`, sanitized.inputs)/n.sum
    dimnames(expected.sanitized.out)[[2L]] <- paste0(colnames(x), " + ", colnames(y))
    expect_equal(Average(x, y,
                         remove.missing = TRUE,
                         remove.rows = c("None of these", "NET"),
                         match.rows = "Yes",
                         match.columns = "No"),
                 expected.sanitized.out)
    # No warning even if warn = TRUE
    inputs <- list(table2D.Percentage, table2D.Percentage)
    inputs <- lapply(inputs, .removeAttributes)
    expect_equal(Average(table2D.Percentage, table2D.Percentage,
                         remove.rows = NULL,
                         remove.columns = NULL,
                         warn = TRUE),
                 Reduce(`+`, inputs)/2)
    # Expect warning if statistic of second table isn't matching
    table.with.non.matching.stat <- table2D.Percentage
    attr(table.with.non.matching.stat, "statistic") <- "Column %"
    expect_warning(computed.sum <- Average(table2D.Percentage,
                                           table.with.non.matching.stat,
                                           remove.rows = NULL,
                                           remove.columns = NULL,
                                           warn = TRUE),
                   paste0("The input data contains statistics of different types ",
                          "(i.e., Row %, Column %), it may not be appropriate to ",
                          "compute ", sQuote("Average"), "."),
                   fixed = TRUE)
    inputs <- list(table2D.Percentage, table.with.non.matching.stat)
    inputs <- lapply(inputs, .removeAttributes)
    expected.out <- Reduce(`+`, inputs)/2
    expect_equal(computed.sum, expected.out)
})

test_that("Works with more than two Q Tables", {
    # If elements are congruent, then works as expected
    expect_equal(Average(table1D.Average, table1D.Average, table1D.Average),
                 .removeAttributes(table1D.Average))
})

test_that("NULL or entirely missing inputs handled correctly", {
    expect_true(is.nan(Average(NULL)))
    expect_true(is.nan(Average(NA, remove.missing = TRUE)))
    expect_true(is.na(Average(NA, remove.missing = FALSE)))
})
