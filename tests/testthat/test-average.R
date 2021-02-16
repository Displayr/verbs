context("Average")

load("variable.Text.rda")
load("variable.Binary.rda")
load("variable.Nominal.rda")
load("variable.Numeric.rda")
load("variable.Time.rda")
load("variable.Date.rda")

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
    expected.inputs <- lapply(list(variable.Binary, variable.Numeric), function(x) {
        x[is.na(x)] <- 0
        x
    })
    # Expect no warning about statistics if no missing data is present
    expect_equivalent(Average(variable.Binary, variable.Numeric, remove.missing = FALSE, warn = TRUE),
                      expected.avg)
    expect_equivalent(Average(variable.Binary, variable.Numeric, remove.missing = TRUE),
                      as.vector(Reduce(`+`, expected.inputs))/2)
    # Expect Variable sets to be handled ok
    df1 <- data.frame(x = runif(10), y = runif(10))
    df2 <- data.frame(y = runif(10), z = runif(10))
    expected.out <- as.matrix(data.frame(x = df1[["x"]], y = df1[["y"]] + df2[["y"]], z = df2[["z"]]))/2
    expect_equivalent(Average(df1, df2), expected.out)
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
    expected.error <- capture_error(checkInputAppropriateForSummingAndWeights(list(variable.Numeric, variable.Nominal),
                                                                              sQuote("Average")))
    expect_error(Average(variable.Numeric, variable.Nominal,
                         weights = weights),
                 expected.error[["message"]])
    expected.error <- capture_error(checkInputAppropriateForSummingAndWeights(list(data.frame(variable.Numeric, variable.Nominal)),
                                                                              sQuote("Average")))
    expect_error(Average(data.frame(variable.Numeric, variable.Nominal),
                         weights = weights),
                 expected.error[["message"]])
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
