context("Utilities")

load("variable.Text.rda")
load("variable.Binary.rda")
load("variable.Nominal.rda")
load("variable.Numeric.rda")
load("variable.Date.rda")
load("table1D.Average.rda")
load("table1D.Percentage.rda")
load("table.1D.MultipleStatistics.rda")
load("table2D.Percentage.rda")
load("table2D.PercentageAndCount.rda")
load("table2D.PercentageNaN.rda")

if (flipU::IsRServer())
{
    contact.msg <- "support@displayr.com if you wish this to be changed."
} else
    contact.msg <- paste0("opensource@displayr.com or raise an issue ",
                          "at https://github.com/Displayr/verbs if you wish this to be changed.")

test_that("Dimension checking functions", {
    # QTables
    ## Inspect the table structure
    expect_equal(getDim(table1D.Average), 1L)
    expect_equal(getDim(table1D.Percentage), 1L)
    expect_equal(getDim(table.1D.MultipleStatistics), 2L)
    expect_equal(getDim(table2D.Percentage), 2L)
    expect_equal(getDim(table2D.PercentageAndCount), 3L)
    expect_equal(getDim(table2D.PercentageNaN), 2L)
    # Tables without attributes ok
    stucture1d.no.att <- table1D.Average[TRUE]
    stucture2d.no.att <- table.1D.MultipleStatistics[TRUE, TRUE]
    stucture3d.no.att <- table2D.PercentageAndCount[TRUE, TRUE, TRUE]
    expect_equal(getDim(stucture1d.no.att), 1)
    expect_equal(getDim(1:3), 1)
    expect_equal(getDim(stucture2d.no.att), 2)
    expect_equal(getDim(matrix(1:4, nrow = 2)), 2)
    expect_equal(getDim(matrix(1:4, nrow = 4)), 2)
    expect_equal(getDim(stucture3d.no.att), 3)

    ## Coerce vectors to arrays before attempting to sum them with matrices
    input <- list(1:3, y <- matrix(1:6, nrow = 3))
    output <- lapply(input, as.array)
    expect_equal(coerceToVectorTo1dArrayIfNecessary(input), output)
    input <- list(c(a = 1, b = 2, c = 3), y)
    output <- list(array(1:3, dim = 3, dimnames = list(letters[1:3])), y)
    expect_equal(coerceToVectorTo1dArrayIfNecessary(input), output)

    ## Check dimension utility function throws error when required
    expect_null(checkDimensionsEqual(replicate(2, 1:5, simplify = FALSE)))
    expect_null(checkDimensionsEqual(replicate(2, matrix(1:6, nrow = 3), simplify = FALSE)))
    expect_null(checkDimensionsEqual(replicate(2, array(1:12, dim = c(3, 2, 2)), simplify = FALSE)))

    expect_error(checkDimensionsEqual(list(1:5, matrix(1:6, nrow = 3)), function.name = "Test"),
                 paste0("Test requires inputs to have the same number of rows or the same number ",
                        "of columns. Contact support at ", contact.msg))
})

test_that("Check elements for opposite Infinities", {
    # Check warning thrown when appropriate
    expect_true(checkForOppositeInfinites(c(Inf, -Inf, 1)))
    expect_false(checkForOppositeInfinites(1:3))
    expect_false(checkForOppositeInfinites(c(Inf, 1:3)))
    expect_false(checkForOppositeInfinites(c(1:3, -Inf)))
    # Not confused by NA values
    expect_false(checkForOppositeInfinites(c(NA, -Inf, 1)))
})

test_that("Check vector appropriate", {
    var1 <- runif(5)
    names(var1) <- LETTERS[1:5]
    var2 <- var1
    names(var2) <- letters[1:5]
    expect_equal(vector.names <- lapply(list(var1, var2), rowNames),
                 list(LETTERS[1:5], letters[1:5]))
    expect_error(checkPartiallyNamed(vector.names, "'Test'"), NA)
    expect_error(checkPartiallyNamed(rev(vector.names), "'Test'"), NA)
    names(var2)[1] <- NA
    expect_equal(vector.names <- lapply(list(var1, var2), rowNames),
                 list(LETTERS[1:5], c(NA, letters[2:5])))
    warn.msg <- paste0("'Test' requires inputs with dimensions that are either ",
                       "fully named or unnamed to calculate output. One input element ",
                       "has a dimension with some named values while other values are not named. ",
                       "Please name all elements if you wish to compute 'Test' by ",
                       "matching elements. ",
                       determineAppropriateContact())
    expect_error(checkPartiallyNamed(vector.names, "'Test'"),
                 warn.msg)
    expect_error(checkPartiallyNamed(rev(vector.names), "'Test'"),
                 warn.msg)
})

test_that("Row and column checking functions",{
    # Confirm row name lookup is adequate
    expect_equal(verbs:::rowNames(table1D.Average), names(table1D.Average))
    expect_equal(verbs:::rowNames(table1D.Percentage), names(table1D.Percentage))
    expect_equal(verbs:::rowNames(table.1D.MultipleStatistics), row.names(table.1D.MultipleStatistics))
    expect_equal(verbs:::rowNames(table2D.Percentage), row.names(table2D.Percentage))
    expect_equal(verbs:::rowNames(table2D.PercentageAndCount), row.names(table2D.PercentageAndCount))
    expect_equal(verbs:::rowNames(table2D.PercentageNaN), row.names(table2D.PercentageNaN))
    # Confirm column name lookup is adequate
    expect_equal(verbs:::colNames(table1D.Average), NULL)
    expect_equal(verbs:::colNames(table1D.Percentage), NULL)
    expect_equal(verbs:::colNames(table.1D.MultipleStatistics), colnames(table.1D.MultipleStatistics))
    expect_equal(verbs:::colNames(table2D.Percentage), colnames(table2D.Percentage))
    expect_equal(verbs:::colNames(table2D.PercentageAndCount), colnames(table2D.PercentageAndCount))
    expect_equal(verbs:::colNames(table2D.PercentageNaN), colnames(table2D.PercentageNaN))
    basic.vector <- 1:10
    names(basic.vector) <- LETTERS[1:10]
    expect_equal(rowNames(basic.vector), names(basic.vector))
    expect_equal(colNames(basic.vector), NULL)
    basic.matrix <- matrix(1:12, nrow = 3, ncol = 4, dimnames = list(LETTERS[1:3], letters[1:4]))
    expect_equal(rowNames(basic.matrix), rownames(basic.matrix))
    expect_equal(colNames(basic.matrix), colnames(basic.matrix))
    # Add tests about subsetting tables
    ## Check helper function removeElementsFromArray first, uses logical to subset
    expect_equivalent(removeElementsFromArray(table1D.Average, keep.rows = c(TRUE, TRUE, FALSE, FALSE)),
                      table1D.Average[1:2])
    expect_equivalent(removeElementsFromArray(table1D.Percentage,
                                                      keep.rows = c(rep(FALSE, 3), rep(TRUE, 4), rep(FALSE, 3))),
                      table1D.Percentage[4:7])
    expect_equivalent(removeElementsFromArray(table.1D.MultipleStatistics,
                                                      keep.rows = c(FALSE, TRUE, TRUE, FALSE)),
                      table.1D.MultipleStatistics[2:3, ])
    ## Exoect error if an array with 3 dimensions is input but its not a 2D QTable with multiple statistics
    expect_error(removeElementsFromArray(array(1:24, dim = 2:4), function.name = "'Test'"),
                 paste0("'Test' only supports inputs that have 1 or 2 dimensions. A supplied input has 3 ",
                        "dimensions. ", determineAppropriateContact()),
                 fixed = TRUE)
    ## Check next function, removeRowsAndCols that determines the logical indices
    default.removal <- c("NET", "SUM", "Total")
    expect_equivalent(removeRowsAndCols(table1D.Average,
                                        remove.rows = default.removal,
                                        remove.columns = default.removal),
                      table1D.Average[1:3])
    expect_equivalent(removeRowsAndCols(table1D.Percentage,
                                        remove.rows = default.removal,
                                        remove.columns = default.removal),
                      table1D.Percentage[1:9])
    expect_equivalent(removeRowsAndCols(table.1D.MultipleStatistics,
                                        remove.rows = default.removal,
                                        remove.columns = default.removal),
                      table.1D.MultipleStatistics[1:3, ])
    # Check remove columns as well.
    ## Check internal helper function with the logical indices
    expect_equivalent(removeElementsFromArray(table.1D.MultipleStatistics,
                                              keep.rows = c(FALSE, TRUE, TRUE, FALSE),
                                              keep.columns = c(TRUE, rep(FALSE, 5))),
                      table.1D.MultipleStatistics[2:3, 1])
    expect_equivalent(removeElementsFromArray(table2D.Percentage,
                                              keep.rows = c(rep(FALSE, 2), rep(TRUE, 2), rep(FALSE, 2)),
                                              keep.columns = c(rep(FALSE, 3), rep(TRUE, 4), rep(FALSE, 3))),
                      table2D.Percentage[3:4, 4:7])
    expect_equivalent(removeElementsFromArray(table2D.PercentageAndCount,
                                              keep.rows = c(rep(FALSE, 2), rep(TRUE, 2), rep(FALSE, 2)),
                                              keep.columns = c(rep(FALSE, 3), rep(TRUE, 4), rep(FALSE, 3))),
                      table2D.PercentageAndCount[3:4, 4:7, ])
    ## Check outer function that removes appropriate slices
    expect_equivalent(table.subsetted <- removeRowsAndCols(table.1D.MultipleStatistics,
                                                           remove.rows = default.removal,
                                                           remove.columns = "z-Statistic"),
                      table.1D.MultipleStatistics[1:3, -5])
    # Check entriesToKeep function
    ## If no valid comparisons are made, fall back to the required logical vector
    expect_equal(entriesToKeep(LETTERS[1:3], "B"), c(TRUE, FALSE, TRUE))
    expect_equal(entriesToKeep(NULL, "B", dim.length = 4), rep(TRUE, 4))
})

test_that("QTable: Inspecting Statistics and throwing warnings", {
    # Check statistics name lookup in tables working as expected
    expect_equal(lookupStatistics(table1D.Average), "Average")
    expect_equal(lookupStatistics(table1D.Percentage), "%")
    expect_equal(lookupStatistics(table.1D.MultipleStatistics), colnames(table.1D.MultipleStatistics))
    expect_equal(lookupStatistics(table2D.Percentage), "Row %")
    expect_equal(lookupStatistics(table2D.PercentageAndCount), dimnames(table2D.PercentageAndCount)[[3]])
    expect_equal(lookupStatistics(table2D.PercentageNaN), "%")
    expect_equal(possibleStatistics(table1D.Average), "Average")
    expect_equal(possibleStatistics(table1D.Percentage), "%")
    expect_equal(possibleStatistics(table.1D.MultipleStatistics), colnames(table.1D.MultipleStatistics))
    expect_equal(possibleStatistics(table2D.Percentage), "Row %")
    expect_equal(possibleStatistics(table2D.PercentageAndCount), dimnames(table2D.PercentageAndCount)[[3]])
    expect_equal(possibleStatistics(table2D.PercentageNaN), "%")
    ## Check possible filters out ones that aren't a statistic
    struct1d <- table1D.Average[TRUE]
    struct2d <- table.1D.MultipleStatistics[TRUE, TRUE]
    colnames(struct2d) <- LETTERS[1:NCOL(struct2d)]
    attr(struct1d, "statistic") <- "This is a red herring"
    expect_equal(possibleStatistics(struct1d), "This is a red herring")
    expect_null(lookupStatistics(struct1d))
    expect_equal(possibleStatistics(struct2d), colnames(struct2d))
    expect_null(lookupStatistics(struct2d))
    # Warning function works ok
    input.stats <- c("Average", "Standard Error")
    expect_warning(throwWarningAboutDifferentStatistics(input.stats,
                                                                function.name = "'Hello'"),
                   paste0("The input data contains statistics of different types (i.e., ",
                          paste0(input.stats, collapse = ", "),
                          "), it may not be ",
                          "appropriate to compute 'Hello'."),
                   fixed = TRUE)
    # Check thrown warnings, single input
    expect_warning(checkForMultipleStatistics(table1D.Average, function.name = "'Sum'"), NA)
    expect_warning(checkForMultipleStatistics(table1D.Percentage, function.name = "'Sum'"), NA)
    expect_warning(checkForMultipleStatistics(table.1D.MultipleStatistics, function.name = "'Sum'"),
                   paste0("The input data contains statistics of different types (i.e., Average, Effective Sample Size, ",
                          "t-Statistic, d.f., z-Statistic, Corrected p), it may not be appropriate to compute 'Sum'."),
                   fixed = TRUE)
    expect_warning(checkForMultipleStatistics(table2D.Percentage, function.name = "'Sum'"), NA)
    expect_warning(checkForMultipleStatistics(table2D.PercentageAndCount, function.name = "'Sum'"),
                   paste0("The input data contains statistics of different types (i.e., Row %, Count), it may not be ",
                          "appropriate to compute 'Sum'."),
                   fixed = TRUE)
    expect_warning(checkForMultipleStatistics(table2D.PercentageNaN, function.name = "'Sum'"), NA)
    # Check function name correct
    expect_warning(checkForMultipleStatistics(table.1D.MultipleStatistics, function.name = "'Hello'"),
                   paste0("The input data contains statistics of different types (i.e., Average, ",
                          "Effective Sample Size, t-Statistic, d.f., z-Statistic, Corrected p), it may not be ",
                          "appropriate to compute 'Hello'."),
                   fixed = TRUE)
})


test_that("Contact details correct", {
    # Expect open source contact correct
    expect_equal(determineAppropriateContact(),
                 paste0("Contact support at ", contact.msg),
                 fixed = TRUE)
    # Expect customer support contact correct
    expect_equal(with_mock(IsRServer = function() TRUE,
                           determineAppropriateContact(),
                           .env = "flipU"),
                 "Contact support at support@displayr.com if you wish this to be changed.",
                 fixed = TRUE)
    expect_error(throwErrorContactSupportForRequest("isn't supported. ", "'some func'"),
                 paste0("'some func' isn't supported. ", determineAppropriateContact()))
})

test_that("Check missing data handling", {
    expect_warning(warnIfDataHasMissingValues(remove.missing = FALSE), NA)
    expect_warning(warnIfDataHasMissingValues(list(1:3), remove.missing = TRUE), NA)
    expect_warning(warnIfDataHasMissingValues(list(c(1, NA, 3)), remove.missing = TRUE),
                   "Missing values have been ignored in calculation.")
})

test_that("Subset and Weights handled correctly", {
    # checkWeights
    ## Valid weights pass the check
    n <- 100L
    x <- runif(n)
    weight.test <- runif(n)
    expect_equal(checkWeights(weight.test, n), weight.test)
    ## Check helper function multiples appropriately
    expect_equal(weightInput(x, weight.test),
                 x * weight.test)
    ## Check weights with negative elements are set to zero
    invalid.weights <- weight.test
    rand.negatives <- sample(c(TRUE, FALSE), size = n, replace = TRUE)
    invalid.weights[rand.negatives] <- -runif(sum(rand.negatives))
    expect_warning(output <- checkWeights(invalid.weights, n, warn = TRUE),
                   "Elements with negative weights were set to have weight of zero")
    expect_equal(output[rand.negatives], rep(0, sum(rand.negatives)))
    ## Check weights with missing values are set to zero
    invalid.weights <- weight.test
    rand.negatives <- sample(c(TRUE, FALSE), size = n, replace = TRUE)
    invalid.weights[rand.negatives] <- NA
    expect_warning(output <- checkWeights(invalid.weights, n, warn = TRUE),
                   "Weights with missing elements were set to have a weight of zero")
    expect_equal(output[rand.negatives], rep(0, sum(rand.negatives)))
    ## Check weight vector that is the wrong length will throw an error
    invalid.weight <- weight.test[seq_len(n/2)]
    expect_error(checkWeights(invalid.weight, n),
                 paste0("The weights vector has length ", length(invalid.weight), ". However, it needs to have ",
                 "length ", n, " to match the number of cases in the supplied input data."))
    # checkSubset
    subset.test <- sample(c(TRUE, FALSE), size = n, replace = TRUE)
    expect_error(checkSubset(subset.test, n), NA)
    ## Check missing elements in subset vector converted to FALSE
    broken.subset <- subset.test
    broken.subset[sample(c(TRUE, FALSE), size = n, replace = TRUE)] <- NA
    expect_warning(subset.out <- checkSubset(broken.subset, n, warn = TRUE),
                   paste0("The subset argument contains missing values. ",
                          "Data correspondong to these were filtered out."))
    expected.subset <- broken.subset
    expected.subset[is.na(expected.subset)] <- FALSE
    expect_equal(subset.out, expected.subset)
    ## Check susbet vector that is the wrong length will throw an error
    invalid.subset <- subset.test[seq_len(n/2)]
    expect_error(checkSubset(invalid.subset, n),
                 paste0("The subset vector has length ", length(invalid.weight), ". However, it needs to have ",
                        "length ", n, " to match the number of cases in the supplied input data."))
    subset.input <- seq_along(subset.test)
    attr(subset.input, "foo") <- "bar"
    expect_equal(subsetInput(subset.input, subset.test),
                 structure(which(subset.test), foo = "bar"))
    # Expect errors with invalid subset and/or weight inputs
    ## Expect invalid subset and weight vectors to throw an error
    expect_error(subsetAndWeightInputsIfNecessary(1L, subset = "Hello"),
                 "The subset argument should be a logical vector")
    expect_error(subsetAndWeightInputsIfNecessary(1L, subset = NULL, weights = "Hello"),
                 "The weights argument should be a numeric vector")
    rand.in <- lapply(5:10, rnorm)
    expect_equal(subsetAndWeightInputsIfNecessary(rand.in,
                                                  subset = rep(TRUE, 5),
                                                  weights = NULL,
                                                  function.name = "'Test'"),
                 rand.in)
    expect_error(subsetAndWeightInputsIfNecessary(rand.in,
                                       subset = c(rep(TRUE, 4), FALSE),
                                       weights = NULL,
                                       function.name = "'Test'"),
                 paste0("'Test' requires all input elements to have the same size to be able to ",
                        "apply a filter or weight vector. ", determineAppropriateContact()),
                 fixed = TRUE)
    simple.df <- function()
    {
        out <- data.frame(lapply(1:3, function(x) rnorm(10)))
        names(out) <- LETTERS[1:3]
        attr(out , "foo") <- "bar"
        out
    }
    many.df <- replicate(5, simple.df(), simplify = FALSE)
    subset.test <- rep(c(TRUE, FALSE, TRUE, FALSE, TRUE), 2)
    subsetted.dfs <- lapply(many.df, function(x) {
        out <- x[subset.test, ]
        out
    })
    expect_equal(subsetAndWeightInputsIfNecessary(many.df, subset = subset.test),
                 subsetted.dfs)
    weights.test <- runif(10)
    subsetted.and.weighted.dfs <- lapply(many.df, function(x) {
        out <- x[subset.test, ] * weights.test[subset.test]
        out
    })
    expect_equal(subsetAndWeightInputsIfNecessary(many.df,
                                                  subset = subset.test,
                                                  weights = weights.test),
                 subsetted.and.weighted.dfs)
    ## Tests to see Q Tables ignored and warned when used with subset and weights
    expect_equal(subsetAndWeightInputsIfNecessary(list(table1D.Average, table1D.Percentage),
                                                  subset = rep(c(TRUE, FALSE), c(5, 5))),
                 list(table1D.Average, table1D.Percentage))
    warn.msg <- "'Test' is unable to apply a filter to the input Q Tables since the original variable data is unavailable."
    expect_warning(subsetAndWeightInputsIfNecessary(list(table1D.Average, table1D.Percentage),
                                                    subset = rep(c(TRUE, FALSE), c(5, 5)),
                                                    warn = TRUE,
                                                    function.name = "'Test'"),
                   warn.msg)
    warn.msg <- sub("a filter", "weights", warn.msg)
    expect_warning(subsetAndWeightInputsIfNecessary(list(table1D.Average, table1D.Percentage),
                                                    weights = runif(5),
                                                    warn = TRUE,
                                                    function.name = "'Test'"),
                   warn.msg)
    warn.msg <- sub("weights", "a filter or weights", warn.msg)
    expect_warning(subsetAndWeightInputsIfNecessary(list(table1D.Average, table1D.Percentage),
                                                    subset = c(TRUE, FALSE),
                                                    weights = runif(5),
                                                    warn = TRUE,
                                                    function.name = "'Test'"),
                   warn.msg)
    expect_equal(weightInput(table1D.Average, weights = runif(5)), table1D.Average)
    expect_equal(subsetInput(table1D.Average, subset = runif(5)), table1D.Average)
})

test_that("Data types checked", {
    expect_error(checkIfCharacter(variable.Text, function.name = "'test'"),
                 "Text data has been supplied but 'test' requires numeric data.")
    expect_error(checkIfDateTime(variable.Date, function.name = "'test'"),
                 "Date/Time data has been supplied but 'test' requires numeric data.")
    # Expect error for Date and Text variables
    expect_error(checkInputTypes(variable.Date, "'Test'"),
                 "Date/Time data has been supplied but 'Test' requires numeric data.")
    expect_error(checkInputTypes(variable.Text, "'Test'"),
                 "Text data has been supplied but 'Test' requires numeric data.")
    expect_error(checkInputTypes(variable.Binary, "'Test'"), NA)
    expect_error(checkInputTypes(list(table1D.Average, variable.Numeric), "'Test'"),
                 paste0("'Test' requires input elements to be of the same type. ",
                        "However, both QTables and Variables have been used as inputs. ",
                        "It is not possible to use 'Test' with multiple inputs of ",
                        "different types. Contact support at ", contact.msg))
    ## Check conversion from categorical to numeric occurs
    expect_equal(convertToNumeric(list(variable.Nominal)),
                 list(flipTransformations::AsNumeric(variable.Nominal, binary = FALSE)))
    # Expect error if a QTable and Variables are both used as inputs
    expect_error(checkInputsDontContainTablesAndVariables(list(variable.Numeric, table1D.Average),
                                                          function.name = "'Test'"),
                 paste0("'Test' requires input elements to be of the same type. However, ",
                        "both QTables and Variables have been used as inputs. ",
                        "It is not possible to use 'Test' ",
                        "with multiple inputs of different types. ",
                        determineAppropriateContact()))
})

test_that("ExtractChartData", {
    var1 <- variable.Numeric
    var2 <- runif(length(var1))
    correlation.output <- flipStatistics::CorrelationMatrix(data.frame(var1, var2))
    expect_equivalent(extractChartDataIfNecessary(correlation.output),
                      cor(data.frame(var1, var2), use = "complete.obs"))
})

# Helper function to shuffle second element, useful for the matching tests
.shuffleSecond <- function(x, ind = NULL)
{
    n.second <- length(x[[2]])
    if (is.null(ind))
    {
        ind <- sample(n.second)
        while(any(ind == 1:n.second))
            ind <- sample(n.second)
    }
    x[[2]] <- x[[2]][ind]
    x
}

test_that("exactMatchDimensionNames", {
    # ok for inputs with same size and names
    n <- 5
    inputs.same.names <- replicate(2, letters[1:n], simplify = FALSE)
    expected.mapping <- replicate(2, {x <- 1:n; names(x) <- letters[1:n]; x}, simplify = FALSE)
    expect_equal(exactMatchDimensionNames(inputs.same.names, hide.unmatched = FALSE), expected.mapping)
    # Expect permuted names to be matched correctly
    inputs.same.names.permuted <- .shuffleSecond(inputs.same.names)
    expected.permuted.mapping <- expected.mapping
    expected.permuted.mapping[[2]] <- match(inputs.same.names.permuted[[1]], inputs.same.names.permuted[[2]])
    names(expected.permuted.mapping[[2]]) <- names(expected.permuted.mapping[[1]])
    expect_equal(exactMatchDimensionNames(inputs.same.names.permuted, hide.unmatched = FALSE), expected.permuted.mapping)
    # Check unmatched elements are recognized and take the value NA in the indexing.
    names.with.unmatched <- inputs.same.names
    names.with.unmatched[[1]] <- append(names.with.unmatched[[1]], "A")
    names.with.unmatched[[2]] <- append(names.with.unmatched[[2]], "Z")
    expected.unmatched.mapping <- expected.mapping
    expected.unmatched.mapping[[1]] <- append(expected.unmatched.mapping[[1]],
                                              values = c(A = n + 1, Z = NA))
    expected.unmatched.mapping[[2]] <- append(expected.unmatched.mapping[[2]],
                                              values = c(A = NA, Z = 6))
    expect_equal(exactMatchDimensionNames(names.with.unmatched, hide.unmatched = FALSE),
                 expected.unmatched.mapping)
    # Also works when second input element shuffled
    inputs.names.with.unmatched.permuted <- .shuffleSecond(names.with.unmatched)
    expected.shuffled.unmatched.mapping <- expected.unmatched.mapping
    expected.shuffled.unmatched.mapping[[2]] <- match(c(inputs.same.names[[1]], "A", "Z"),
                                                      inputs.names.with.unmatched.permuted[[2]])
    names(expected.shuffled.unmatched.mapping[[2]]) <- c(inputs.same.names[[1]], "A", "Z")
    expect_equal(exactMatchDimensionNames(inputs.names.with.unmatched.permuted, hide.unmatched = FALSE),
                 expected.shuffled.unmatched.mapping)
    # Hide unmatched tests
    names.with.unmatched <- inputs.same.names
    names.with.unmatched[[1]] <- append(names.with.unmatched[[1]], "A")
    names.with.unmatched[[2]] <- append(names.with.unmatched[[2]], "Z")
    expect_equal(exactMatchDimensionNames(names.with.unmatched,
                                          hide.unmatched = TRUE,
                                          warn = FALSE),
                 expected.mapping)
    expect_warning(expect_equal(exactMatchDimensionNames(names.with.unmatched,
                                                         hide.unmatched = TRUE,
                                                         warn = TRUE,
                                                         function.name = "Test"),
                                expected.mapping),
                   paste0("here were unmatched categories that weere removed from ",
                          "the calculation of Test. They had the category names: ",
                          "A, Z. If you wish these categories to be used in the ",
                          "calculation, consider using the Fuzzy name matching ",
                          "options if the name is similar to an existing category. ",
                          "Alternatively, modify the exact matching options if you ",
                          "wish it to be shown."))
    # Also works when second input element shuffled
    inputs.with.unmatched.permuted <- .shuffleSecond(names.with.unmatched)
    expected.shuffled.unmatched.mapping <- expected.mapping
    expected.shuffled.unmatched.mapping[[2]] <- match(inputs.with.unmatched.permuted[[1]],
                                                      inputs.with.unmatched.permuted[[2]])
    names(expected.shuffled.unmatched.mapping[[2]]) <- inputs.with.unmatched.permuted[[1]]
    expected.shuffled.unmatched.mapping <- lapply(expected.shuffled.unmatched.mapping,
                                                  function(x) x[!is.na(x)])
    expect_equal(exactMatchDimensionNames(inputs.with.unmatched.permuted,
                                          hide.unmatched = TRUE,
                                          warn = FALSE),
                 expected.shuffled.unmatched.mapping)
})

test_that("fuzzyMatchDimensionNames", {
    # Check exact matches work when attemping to fuzzy match
    exact.in <- replicate(2, letters[1:5], simplify = FALSE)
    expected.out <- replicate(2, {
        x <- 1:5
        names(x) <- letters[1:5]
        x
    }, simplify = FALSE)
    expect_equal(fuzzyMatchDimensionNames(exact.in, hide.unmatched = TRUE), expected.out)
    expect_equal(fuzzyMatchDimensionNames(exact.in, hide.unmatched = FALSE), expected.out)
    # Shuffle the 2nd list and check answer is still correct
    shuffled.exact.in <- .shuffleSecond(exact.in)
    shuffled.expected.out <- expected.out
    shuffled.expected.out[[2L]] <- match(exact.in[[1L]], shuffled.exact.in[[2L]])
    names(shuffled.expected.out[[2L]]) <- letters[1:5]
    expect_equal(fuzzyMatchDimensionNames(shuffled.exact.in, hide.unmatched = TRUE),
                 shuffled.expected.out)
    expect_equal(fuzzyMatchDimensionNames(shuffled.exact.in, hide.unmatched = FALSE),
                 shuffled.expected.out)
    ## Make the second use all upper case names requiring fuzzy matching
    names.near.exact <- exact.in
    names.near.exact[[2]] <- toupper(names.near.exact[[2]])
    expected.mapping.list <- replicate(2, 1:5, simplify = FALSE)
    names(expected.mapping.list[[1L]]) <- letters[1:5]
    names(expected.mapping.list[[2L]]) <- LETTERS[1:5]
    names.near.expected.out <- list(mapping.list = expected.mapping.list, unmatched = NULL)
    expect_equal(fuzzyMatchDimensionNames(names.near.exact, hide.unmatched = TRUE),
                 names.near.expected.out)
    expect_equal(fuzzyMatchDimensionNames(names.near.exact, hide.unmatched = FALSE),
                 names.near.expected.out)
    ## Shuffle with case changes
    shuffled.names.near.exact <- shuffled.exact.in
    shuffled.names.near.exact[[2L]] <- toupper(shuffled.names.near.exact[[2L]])
    expected.mapping.list[[2L]] <- shuffled.expected.out[[2L]]
    names(expected.mapping.list[[2L]]) <- LETTERS[1:5]
    shuffled.names.near.expected.out <- list(mapping.list = expected.mapping.list, unmatched = NULL)
    expect_equal(fuzzyMatchDimensionNames(shuffled.names.near.exact, hide.unmatched = TRUE),
                 shuffled.names.near.expected.out)
    expect_equal(fuzzyMatchDimensionNames(shuffled.names.near.exact, hide.unmatched = FALSE),
                 shuffled.names.near.expected.out)
    # Test case that requires fuzzy matching
    test.in <- replicate(2, c("Hello", "Don't know", "None of these", "Other", "Burger",
                              "Sushi", "Pizza"),
                         simplify = FALSE)
    test.only.variants <- test.in
    test.only.variants <- lapply(test.only.variants, function(x) x[1:4])
    # Create variants
    test.only.variants[[2]][test.only.variants[[2]] == "None of these"] <- "none"
    test.only.variants[[2]][test.only.variants[[2]] == "Don't know"] <- "dont know"
    test.only.variants[[2]][test.only.variants[[2]] == "Other"] <- "  other     "
    expected.mapping <- lapply(test.only.variants, function(x) {
        y <- 1:4
        names(y) <- x
        y
    })
    expect_equal(fuzzyMatchDimensionNames(test.only.variants, hide.unmatched = TRUE),
                 list(mapping.list = expected.mapping, unmatched = NULL))
    expect_equal(fuzzyMatchDimensionNames(test.only.variants, hide.unmatched = FALSE),
                 list(mapping.list = expected.mapping, unmatched = NULL))
    test.ind <- c(4, 3, 1, 2)
    shuffled.only.variants <- .shuffleSecond(test.only.variants, ind = test.ind)
    shuffled.expected.mapping <- expected.mapping
    shuffled.expected.mapping[[2L]] <- shuffled.expected.mapping[[2L]][match(1:4, test.ind)]
    names(shuffled.expected.mapping[[2L]]) <- names(expected.mapping[[2L]])
    expect_equal(fuzzyMatchDimensionNames(shuffled.only.variants, hide.unmatched = TRUE),
                 list(mapping.list = shuffled.expected.mapping, unmatched = NULL))
    expect_equal(fuzzyMatchDimensionNames(shuffled.only.variants, hide.unmatched = FALSE),
                 list(mapping.list = shuffled.expected.mapping, unmatched = NULL))
    test.misc <- replicate(2, c("Hello", "Don't know", "None of these", "Other", "Burger",
                                "Sushi", "Pizza"),
                           simplify = FALSE)
    test.misc[[2]][2] <- "not sure"
    test.misc[[2]][3] <- "unsure"
    expected.out <- list(mapping.list = replicate(2, c(Hello = 1, `Don't know` = NA, `None of these` = NA,
                                                       Other = 4, Burger = 5, Sushi = 6, Pizza = 7,
                                                       `not sure` = NA, unsure = NA),
                                                  simplify = FALSE),
                         unmatched = list(c("Don't know", "None of these"),
                                          c("not sure", "unsure")))
    warn.msg <- paste0("Multiple fuzzy matches found with rows named 'Don't know', 'not sure', 'unsure'. ",
                       "Considering merging these categories if they are similar measures.")
    expect_warning(expect_equal(fuzzyMatchDimensionNames(test.misc, hide.unmatched = FALSE, warn = TRUE),
                                expected.out),
                   warn.msg)
    expected.out.when.hidden <- expected.out
    expected.out.when.hidden[["mapping.list"]] <- lapply(expected.out.when.hidden[["mapping.list"]],
                                                         function(x) x[!is.na(x)])
    expect_warning(expect_equal(fuzzyMatchDimensionNames(test.misc, hide.unmatched = TRUE, warn = TRUE),
                                expected.out.when.hidden),
                   warn.msg)
    ## Shuffle the second element
    test.ind <- sample(length(test.misc[[2L]]))
    shuffled.mapping <- expected.out[["mapping.list"]]
    shuffled.mapping[[2L]]
    shuffled.test.misc <- .shuffleSecond(test.misc, ind = test.ind)
    shuffled.expected.mapping <- expected.out[["mapping.list"]]
    rematched <- match(shuffled.expected.mapping[[1L]], test.ind)
    shuffled.expected.mapping[[2L]] <-  rematched
    names(shuffled.expected.mapping[[2L]]) <- names(shuffled.expected.mapping[[1L]])
    expected.shuffled.out <- list(mapping.list = shuffled.expected.mapping,
                                  unmatched = expected.out[["unmatched"]])
    second.unmatched <- shuffled.test.misc[[2L]][which(shuffled.test.misc[[2L]] %in% c("unsure", "not sure"))]
    names(expected.shuffled.out[[1L]][[2L]])[8:9] <- second.unmatched
    names(expected.shuffled.out[[1L]][[1L]])[8:9] <- second.unmatched
    expected.shuffled.out[[2L]][[2L]] <- second.unmatched
    warn.msg <- sub("'not sure', 'unsure'", paste0("'", second.unmatched, "'", collapse = ", "), warn.msg)
    expect_warning(expect_equal(fuzzyMatchDimensionNames(shuffled.test.misc, hide.unmatched = FALSE),
                                expected.shuffled.out),
                   warn.msg)
    shuffled.expected.mapping.when.hidden <- expected.shuffled.out
    shuffled.expected.mapping.when.hidden[["mapping.list"]] <- lapply(shuffled.expected.mapping.when.hidden[["mapping.list"]],
                                                                      function(x) x[!is.na(x)])
    expect_warning(expect_equal(fuzzyMatchDimensionNames(shuffled.test.misc, hide.unmatched = TRUE),
                                shuffled.expected.mapping.when.hidden),
                   warn.msg)
    ## Near match tests
    test.dist <- list(c("Displayr", "qu", "burger", "Ham", "Stew", "kitten", "Honda"),
                      c("displayer", "Q", "Berger", "yam", "stew", "sitten", "Hyundai"))
    expected.mapping <- mapply(function(x, nam) {names(x) <- nam; x}, nam = lapply(test.dist, "[", -7), MoreArgs = list(x = 1:6), SIMPLIFY = FALSE)
    expected.out <- list(mapping.list = expected.mapping,
                         unmatched = lapply(test.dist, "[", 7))
    expect_warning(expect_equal(fuzzyMatchDimensionNames(test.dist, hide.unmatched = TRUE), expected.out),
                   paste0("After a fuzzy matching search there are still names that couldn't be matched without ",
                          "ambiguity. These had the names 'Honda', 'Hyundai'. Consider merging these categories ",
                          "if appropriate or relaxing the matching options to ignore them beforing proceeeding further."),
                   fixed = TRUE)
    ## Ambiguous fuzzy matches, throw a warning
    ambiguous <- test.dist
    ambiguous[[2L]][2] <- "displayar"
    expected.ambiguous.mapping <- replicate(2, c(NA, NA, 3, 4, 5, 6, NA), simplify = FALSE)
    expected.ambiguous.mapping <- mapply(function(x, name) { names(x) <- name; x},
                                         expected.ambiguous.mapping, ambiguous, SIMPLIFY = FALSE)
    expected.unmatched <- lapply(expected.ambiguous.mapping, function(x) names(x)[is.na(x)])
    second.unmatched <- expected.ambiguous.mapping[[2L]][is.na(expected.ambiguous.mapping[[2L]])]
    expected.ambiguous.mapping[[1L]] <- c(expected.ambiguous.mapping[[1L]], second.unmatched)
    names(expected.ambiguous.mapping[[2L]])[c(1, 2, 7)] <- names(expected.ambiguous.mapping[[1L]])[c(1, 2, 7)]
    expected.ambiguous.mapping[[2L]] <- c(expected.ambiguous.mapping[[2L]], second.unmatched)
    expected.out <- list(mapping.list = expected.ambiguous.mapping,
                         unmatched = expected.unmatched)
    expect_equal(fuzzyMatchDimensionNames(ambiguous, hide.unmatched = FALSE),
                 expected.out)
    hidden.expected.out <- expected.out
    hidden.expected.out[[1L]] <- lapply(hidden.expected.out[[1L]], function(x) x[!is.na(x)])
    warn.msg <- paste0("After a fuzzy matching search there are still names that couldn't be ",
                       "matched without ambiguity. These had the names 'Displayr', 'qu', 'Honda', ",
                       "'displayer', 'displayar', 'Hyundai'. Consider merging these categories ",
                       "if appropriate or relaxing the matching options to ignore them ",
                       "beforing proceeeding further.")
    expect_warning(expect_equal(fuzzyMatchDimensionNames(ambiguous, hide.unmatched = TRUE),
                                hidden.expected.out),
                   warn.msg)
    ## Match the punctuation
    punct.match <- test.dist
    punct.match <- lapply(punct.match, function(x) x[1:6]) # Remove Honda and Hyundai
    punct.match[[1L]] <- append(punct.match[[1L]], c("FILET-O-FISH", "Toys'R'Us", "Young @ Heart"))
    punct.match[[2L]] <- append(punct.match[[2L]], c("Filet'o'fish", "ToysRUs", "Young@Heart"))
    punct.mapping <- replicate(2, 1:9, simplify = FALSE)
    punct.mapping <- mapply(function(x, nam) {names(x) <- nam; x}, punct.mapping, punct.match, SIMPLIFY = FALSE)
    expect.punct.out <- list(mapping.list = punct.mapping, unmatched = NULL)
    expect_equal(fuzzyMatchDimensionNames(punct.match, hide.unmatched = FALSE),
                 expect.punct.out)
    shuffled.punct <- punct.match
    test.ind <- sample(length(expect.punct.out[[1L]][[1L]]))
    shuffled.punct <- .shuffleSecond(shuffled.punct, ind = test.ind)
    shuf.mapping <- expect.punct.out[["mapping.list"]]
    shuf.mapping[[2L]] <- match(shuf.mapping[[1L]], shuf.mapping[[2L]][test.ind])
    shuf.expect.out <- expect.punct.out
    names(shuf.mapping[[2L]]) <- names(expect.punct.out[[1L]][[2L]])
    shuf.expect.out[["mapping.list"]] <- shuf.mapping
    expect_equal(fuzzyMatchDimensionNames(shuffled.punct, hide.unmatched = FALSE),
                 shuf.expect.out)
})

test_that("matchDimensionElements", {
    # Inputs ok
    valid.matching <- c("Yes", "Yes - hide unmatched", "Fuzzy", "Fuzzy - hide unmatched", "No")
    expect_error(checkMatchingArguments(valid.matching), NA)
    err.msg <- paste0("The argument match.rows = \"foo\" was requested for Test. ",
                      "However, valid arguments for match.rows are one of ",
                      paste0(valid.matching, collapse = ", "), ". Please choose a ",
                      "valid option before attempting to recalculate Test")
    expect_error(checkMatchingArguments("foo", function.name = "Test"),
                 err.msg)
    expect_error(checkMatchingArguments(list("Yes", "foo"), function.name = "Test"),
                 gsub("match.rows", "match.columns", err.msg))
    # If matching requested when there are two unnamed inputs of the same size doesn't
    # throw an error unless its impossible to create inputs with compatible dimensions.
    input <- replicate(2, runif(5), simplify = FALSE)
    expect_equal(matchDimensionElements(input,
                                        match.rows = "Yes", match.columns = "Yes",
                                        remove.missing = TRUE,
                                        function.name = "Test"),
                 input)
    input[[2L]] <- runif(4)
    expect_error(matchDimensionElements(input,
                                        match.rows = "Yes", match.columns = "Yes",
                                        remove.missing = TRUE,
                                        function.name = "Test"),
                 paste0("Test requires multiple elements to have the same dimension ",
                        "or partially agreeing dimensions. In this case, the inputs ",
                        "are two vectors with 5 rows and 4 rows respectively. Please ",
                        "ensure the inputs have the same or partially agreeing ",
                        "dimensions before attempting to recompute Test"))
    # No error, might be possible to reshape (if one of the inputs has dim length 1)
    input[[2L]] <- runif(1)
    expect_equal(matchDimensionElements(input,
                                        match.rows = "Yes", match.columns = "Yes",
                                        remove.missing = TRUE,
                                        function.name = "Test"),
                 input)
    # Some elements dont have names
    input <- list(x = c(a = 1, b = 2), y = matrix(1:6, nrow = 3))
    err.msg <- paste0("Test requires inputs that have named rows in order to match elements by name. ",
                      "Please provide names for all rows in all input elements or change the matching ",
                      "options to not match row elements before attempting to recalculate. Contact ",
                      "support at ", contact.msg)
    expect_error(matchDimensionElements(input,
                                        match.rows = "Yes", match.columns = "No",
                                        remove.missing = TRUE,
                                        function.name = "Test"),
                 err.msg)
    n <- 5
    exact.in <- replicate(2, {x <- runif(n); names(x) <- letters[1:n]; x}, simplify = FALSE)
    expected.out <- exact.in
    expect_equal(matchDimensionElements(exact.in,
                                        match.rows = "Yes", match.columns = "No",
                                        remove.missing = TRUE),
                 expected.out)
    test.ind <- sample(n)
    shuf.in <- .shuffleSecond(exact.in, ind = test.ind)
    expect_equal(matchDimensionElements(exact.in,
                                        match.rows = "Yes", match.columns = "No",
                                        remove.missing = TRUE),
                 expected.out)
    # Exact matching with no unmatched
    ## Redundant matching, elements already aligned with matching names
    input.v <- replicate(2, {x <- runif(3); names(x) <- letters[1:3]; x}, simplify = FALSE)
    expect_equal(matchDimensionElements(input.v, match.rows = "Yes", match.columns = "No", remove.missing = TRUE),
                 input.v)
    ## Single vectors
    input.v <- replicate(2, {x <- runif(3); names(x) <- letters[1:3]; x}, simplify = FALSE)
    inds <- sample(3)
    input.v[[2L]] <- input.v[[2L]][inds]
    output <- input.v
    output[[2L]] <- output[[2L]][letters[1:3]]
    expect_equal(matchDimensionElements(input.v,
                                        match.rows = "Yes", match.columns = "No",
                                        remove.missing = TRUE),
                 output)
    ## 1d Array
    input.a <- replicate(2, array(runif(3), dim = 3, dimnames = list(letters[1:3])), simplify = FALSE)
    inds <- sample(3)
    input.a[[2L]] <- input.a[[2L]][inds]
    output <- input.a
    output[[2L]] <- output[[2L]][letters[1:3]]
    expect_equal(matchDimensionElements(input.a,
                                        match.rows = "Yes", match.columns = "No",
                                        remove.missing = TRUE),
                 output)
    ## 1d array and vector
    input.both <- input.v
    input.both[[2L]] <- input.a[[2L]]
    output[[1L]] <- input.v[[1L]]
    attr(output[[2L]][[1L]], "dim") <- NULL
    expect_equal(matchDimensionElements(input.both,
                                        match.rows = "Yes", match.columns = "No",
                                        remove.missing = TRUE),
                 output)
    ## Matrix, (2d array)
    input <- replicate(2, matrix(runif(12), nrow = 4, dimnames = list(letters[1:4], LETTERS[1:3])), simplify = FALSE)
    inds <- lapply(4:3, sample)
    dimnames(input[[2L]]) <- mapply(function(lab, ind) lab[ind], dimnames(input[[2L]]), inds, SIMPLIFY = FALSE)
    output <- input
    output[[2L]] <- output[[2L]][letters[1:4], LETTERS[1:3]]
    expect_equal(matchDimensionElements(input,
                                        match.rows = "Yes", match.columns = "Yes",
                                        remove.missing = TRUE),
                 output)
    ## 3d array (via Q Table)
    input <- replicate(2, table2D.PercentageAndCount, simplify = FALSE)
    x <- runif(1)
    input[[2L]] <- x * input[[2L]]
    inds <- lapply(dim(table2D.PercentageAndCount), sample)
    stat.names <- dimnames(table2D.PercentageAndCount)[[3]]
    dimnames(input[[2L]]) <- mapply(function(lab, ind) lab[ind], dimnames(input[[2L]]), inds, SIMPLIFY = FALSE)
    dimnames(input[[2L]])[[3L]] <- stat.names
    output <- input
    tab.names <- dimnames(table2D.PercentageAndCount)
    output[[2L]] <- output[[2L]][tab.names[[1L]], tab.names[[2L]], tab.names[[3L]]]
    expect_equal(matchDimensionElements(input,
                                        match.rows = "Yes", match.columns = "Yes",
                                        remove.missing = TRUE),
                 output)
    # Exact matching with unmatched
    ## Single vectors with showing unmatched, default
    input.v <- replicate(2, runif(3), simplify = FALSE)
    input.v <- mapply(function(x, ind) {names(x) <- letters[ind]; x},
                      input.v,
                      list(1:3, 2:4),
                      SIMPLIFY = FALSE)
    output <- replicate(2, {x <- rep(0, 4); names(x) <- letters[1:4]; x}, simplify = FALSE)
    output <- mapply(function(temp, inp) { temp[names(inp)] <- inp; temp} ,
                     output, input.v, SIMPLIFY = FALSE)
    expect_equal(matchDimensionElements(input.v,
                                        match.rows = "Yes", match.columns = "No",
                                        remove.missing = TRUE),
                 output)
    output.with.NA <- lapply(output, function(x) {x[x == 0] <- NA; x})
    expect_equal(matchDimensionElements(input.v,
                                        match.rows = "Yes", match.columns = "No",
                                        remove.missing = FALSE),
                 output.with.NA)
    ## 2 matrices
    input <- replicate(2, matrix(runif(12), nrow = 4, dimnames = list(letters[1:4], LETTERS[1:3])), simplify = FALSE)
    input <- mapply(function(x, ind) {rownames(x) <- letters[ind]; x},
                    input,
                    list(1:4, 2:5),
                    SIMPLIFY = FALSE)
    input <- mapply(function(x, ind) {colnames(x) <- LETTERS[ind]; x},
                    input,
                    list(1:3, 2:4),
                    SIMPLIFY = FALSE)
    inds <- lapply(4:3, sample)
    input[[2L]] <- input[[2L]][inds[[1L]], inds[[2L]]]
    output <- input
    output[[1L]] <- rbind(cbind(output[[1L]], D = 0), e = 0)
    output[[2L]] <- rbind(cbind(output[[2L]], A = 0), a = 0)
    output[[2L]] <- output[[2L]][order(rownames(output[[2L]])), ]
    output[[2L]] <- output[[2L]][, order(colnames(output[[2L]]))]
    expect_equal(matchDimensionElements(input,
                                        match.rows = "Yes", match.columns = "Yes",
                                        remove.missing = TRUE),
                 output)
    output.with.NA <- lapply(output, function(x) {x[x == 0] <- NA; x})
    expect_equal(matchDimensionElements(input,
                                        match.rows = "Yes", match.columns = "Yes",
                                        remove.missing = FALSE),
                 output.with.NA)
    ## 3d array (via Q Table)
    input <- replicate(2, table2D.PercentageAndCount, simplify = FALSE)
    x <- runif(1)
    input[[2L]] <- x * input[[2L]]
    output <- input
    output[[1L]] <- output[[1L]][-3, , ]
    output[[1L]] <- aperm(apply(output[[1L]], c(2, 3), c, `Coke Zero` = 0), c(1, 2, 3))
    output[[2L]][, 4, ] <- 0
    output <- lapply(output, sanitizeAttributes)
    input[[1L]] <- input[[1L]][-3, , ]
    input[[2L]] <- input[[2L]][, -4, ]
    input[[2L]] <- input[[2L]][sample(NROW(input[[2L]])), sample(NCOL(input[[2L]])), ]
    output[[2L]] <- output[[2L]][rownames(output[[1L]]), , ]
    expect_equal(matchDimensionElements(input,
                                        match.rows = "Yes", match.columns = "Yes",
                                        remove.missing = TRUE),
                 output)
    output.with.NA <- output
    output.with.NA[[1L]]["Coke Zero", , ] <- NA
    output.with.NA[[2L]][, 4, ] <- NA
    expect_equal(matchDimensionElements(input,
                                        match.rows = "Yes", match.columns = "Yes",
                                        remove.missing = FALSE),
                 output.with.NA)
    # Fuzzy checking
    ## Single vectors
    input.v <- replicate(2, runif(3), simplify = FALSE)
    input.v <- mapply(function(x, name) {names(x) <- name; x},
                      input.v, list(letters[1:3], LETTERS[3:1]),
                      SIMPLIFY = FALSE)
    output <- lapply(input.v, function(x) x[sort(names(x))])
    expect_equal(matchDimensionElements(input.v,
                                        match.rows = "Fuzzy", match.columns = "No",
                                        remove.missing = TRUE),
                 output)
    ## 1d Array
    input.a <- replicate(2, array(runif(3), dim = 3), simplify = FALSE)
    input.a <- mapply(function(x, name) {dimnames(x) <- list(name); x},
                      input.a, list(letters[1:3], LETTERS[3:1]),
                      SIMPLIFY = FALSE)
    input.a[[2L]] <- input.a[[2L]]
    output <- lapply(input.a, function(x) x[sort(names(x))])
    expect_equal(matchDimensionElements(input.a,
                                        match.rows = "Fuzzy", match.columns = "Fuzzy",
                                        remove.missing = TRUE),
                 output)
    ## 1d array and vector
    input.both <- input.v
    input.both[[2L]] <- input.a[[2L]]
    output <- input.both
    output <- lapply(output, function(x) x[sort(names(x))])
    expect_equal(matchDimensionElements(input.both,
                                        match.rows = "Fuzzy", match.columns = "Fuzzy",
                                        remove.missing = TRUE),
                 output)
    ## Matrix, (2d array)
    input <- replicate(2, matrix(runif(12), nrow = 4, dimnames = list(letters[1:4], letters[1:3])), simplify = FALSE)
    dimnames(input[[2L]]) <- lapply(dimnames(input[[2L]]), toupper)
    inds <- lapply(4:3, sample)
    output <- input
    input[[2L]] <- input[[2L]][inds[[1L]], inds[[2L]]]
    expect_equal(matchDimensionElements(input,
                                        match.rows = "Fuzzy", match.columns = "Fuzzy",
                                        remove.missing = TRUE),
                 output)
    ## 3d array (via Q Table)
    input <- replicate(2, table2D.PercentageAndCount, simplify = FALSE)
    x <- runif(1)
    input[[2L]] <- x * input[[2L]]
    dimnames(input[[2L]]) <- lapply(dimnames(input[[2L]]), toupper)
    output <- input
    inds <- lapply(dim(table2D.PercentageAndCount), sample)
    stat.names <- dimnames(table2D.PercentageAndCount)[[3]]
    input[[2L]] <- input[[2L]][inds[[1]], inds[[2L]], ]
    expect_equivalent(matchDimensionElements(input,
                                             match.rows = "Fuzzy", match.columns = "Fuzzy",
                                             remove.missing = TRUE),
                      output)
    # Fuzzy matching with unmatched
    ## Single vectors with showing unmatched, default
    input.v <- replicate(2, runif(3), simplify = FALSE)
    names(input.v[[1L]]) <- letters[1:3]
    names(input.v[[2L]]) <- LETTERS[2:4]
    output <- input.v
    output[[1L]] <- c(output[[1L]], D = 0)
    output[[2L]] <- c(a = 0, output[[2L]])
    expect_equal(matchDimensionElements(input.v,
                                        match.rows = "Fuzzy", match.columns = "Fuzzy",
                                        remove.missing = TRUE),
                 output)
    output.with.NA <- lapply(output, function(x) {x[x == 0] <- NA; x})
    expect_equal(matchDimensionElements(input.v,
                                        match.rows = "Fuzzy", match.columns = "Fuzzy",
                                        remove.missing = FALSE),
                 output.with.NA)
    ## 2 matrices
    input <- replicate(2, matrix(runif(12), nrow = 4, dimnames = list(letters[1:4], letters[1:3])), simplify = FALSE)
    input <- mapply(function(x, ind, funct) {rownames(x) <- funct[ind]; x},
                    input,
                    list(1:4, 2:5),
                    funct = list(letters, LETTERS),
                    SIMPLIFY = FALSE)
    input <- mapply(function(x, ind, funct) {colnames(x) <- funct[ind]; x},
                    input,
                    list(1:3, 2:4),
                    funct = list(letters, LETTERS),
                    SIMPLIFY = FALSE)
    inds <- lapply(4:3, sample)
    output <- input
    input[[2L]] <- input[[2L]][inds[[1L]], inds[[2L]]]
    output[[1L]] <- rbind(cbind(output[[1L]], D = 0), E = 0)
    output[[2L]] <- rbind(a = 0, cbind(a = 0, output[[2L]]))
    expect_equal(matchDimensionElements(input,
                                        match.rows = "Fuzzy", match.columns = "Fuzzy",
                                        remove.missing = TRUE),
                 output)
    output.with.NA <- lapply(output, function(x) {x[x == 0] <- NA; x})
    expect_equal(matchDimensionElements(input,
                                        match.rows = "Fuzzy", match.columns = "Fuzzy",
                                        remove.missing = FALSE),
                 output.with.NA)
    ## 3d array (via Q Table)
    input <- replicate(2, table2D.PercentageAndCount, simplify = FALSE)
    x <- runif(1)
    input[[2L]] <- x * input[[2L]]
    dimnames(input[[2L]]) <- lapply(dimnames(input[[2L]]), toupper)
    output <- input
    colnames(output[[2L]])[4] <- "Once a month"
    output[[1L]] <- output[[1L]][-3, , ]
    output[[1L]] <- aperm(apply(output[[1L]], c(2, 3), c, `COKE ZERO` = 0), c(1, 2, 3))
    output[[2L]][, 4, ] <- 0
    output <- lapply(output, sanitizeAttributes)
    input[[1L]] <- input[[1L]][-3, , ]
    input[[2L]] <- input[[2L]][, -4, ]
    input[[2L]] <- input[[2L]][sample(NROW(input[[2L]])), sample(NCOL(input[[2L]])), ]
    output[[2L]] <- output[[2L]][toupper(rownames(output[[1L]])), , ]
    expect_equal(matchDimensionElements(input,
                                        match.rows = "Fuzzy", match.columns = "Fuzzy",
                                        remove.missing = TRUE),
                 output)
    output.with.NA <- output
    output.with.NA[[1L]]["COKE ZERO", , ] <- NA
    output.with.NA[[2L]][, 4, ] <- NA
    expect_equal(matchDimensionElements(input,
                                        match.rows = "Fuzzy", match.columns = "Fuzzy",
                                        remove.missing = FALSE),
                 output.with.NA)
    # Element with no columns
    input <- list(matrix(1:6, nrow = 3), c(1:3))
    expect_error(matchDimensionElements(input,
                                        match.rows = "No", match.columns = "Yes",
                                        remove.missing = TRUE,
                                        function.name = "Test"),
                 paste0("Test requires multiple elements to have the same dimension ",
                        "or partially agreeing dimensions. In this case, the inputs are ",
                        "a matrix and vector with 3 rows and 2 columns and 3 rows ",
                        "respectively. Please ensure the inputs have the same or ",
                        "partially agreeing dimensions before attempting to recompute Test"))
})

test_that("Reshape 1d inputs", {
    # 1d array and column matrix
    x.names <- LETTERS[1:3]
    x.a <- array(1:3, dim = 3, dimnames = list(x.names))
    y <- matrix(4:6, nrow = 3)
    input <- list(x.a, y)
    output <- list(matrix(x.a, nrow = 3, dimnames = list(x.names, NULL)), y)
    dims <- vapply(input, getDim, integer(1L))
    expect_equal(reshapeOneDimensionalInput(input, dims), output)
    expect_equal(reshapeOneDimensionalInput(rev(input), rev(dims)), rev(output))
    # named vector and column matrix
    x.v <- 1:3
    names(x.v) <- x.names
    y <- matrix(4:6, nrow = 3)
    input <- list(x.v, y)
    dims <- vapply(input, getDim, integer(1L))
    expect_equal(reshapeOneDimensionalInput(input, dims), output)
    expect_equal(reshapeOneDimensionalInput(rev(input), rev(dims)), rev(output))
    # 1d array and row matrix
    y <- matrix(4:6, nrow = 1)
    input <- list(x.a, y)
    output <- list(matrix(x.a, nrow = 1, dimnames = list(NULL, x.names)), y)
    dims <- vapply(input, getDim, integer(1L))
    expect_equal(reshapeOneDimensionalInput(input, dims), output)
    expect_equal(reshapeOneDimensionalInput(rev(input), rev(dims)), rev(output))
    # named vector and row matrix
    input <- list(x.v, y)
    dims <- vapply(input, getDim, integer(1L))
    expect_equal(reshapeOneDimensionalInput(input, dims), output)
    expect_equal(reshapeOneDimensionalInput(rev(input), rev(dims)), rev(output))
    # 1d array and matrix with matching number of rows
    x.a <- array(13:16, dim = 3, dimnames = list(x.names))
    y <- matrix(1:12, nrow = 3)
    input <- list(x.a, y)
    output <- list(matrix(x.a, nrow = 3, ncol = 4, dimnames = list(x.names, NULL)), y)
    dims <- vapply(input, getDim, integer(1L))
    expect_equal(reshapeOneDimensionalInput(input, dims), output)
    expect_equal(reshapeOneDimensionalInput(rev(input), rev(dims)), rev(output))
    # named vector and matrix with matching number of rows
    x.v <- c(A = 13, B = 14, C = 15)
    y <- matrix(1:12, nrow = 3)
    input <- list(x.v, y)
    dims <- vapply(input, getDim, integer(1L))
    expect_equal(reshapeOneDimensionalInput(input, dims), output)
    expect_equal(reshapeOneDimensionalInput(rev(input), rev(dims)), rev(output))
    # 1d array and matrix with matching number of columns
    x.a <- array(13:16, dim = 3, dimnames = list(x.names))
    y <- matrix(1:12, ncol = 3)
    input <- list(x.a, y)
    output <- list(matrix(x.a, nrow = nrow(y), ncol = ncol(y),
                          byrow = TRUE, dimnames = list(NULL, x.names)),
                   y)
    dims <- vapply(input, getDim, integer(1L))
    expect_equal(reshapeOneDimensionalInput(input, dims), output)
    expect_equal(reshapeOneDimensionalInput(rev(input), rev(dims)), rev(output))
    # named vector and matrix with matching number of columns
    x.v <- c(A = 13, B = 14, C = 15)
    y <- matrix(1:12, ncol = 3)
    input <- list(x.v, y)
    dims <- vapply(input, getDim, integer(1L))
    expect_equal(reshapeOneDimensionalInput(input, dims), output)
    expect_equal(reshapeOneDimensionalInput(rev(input), rev(dims)), rev(output))
    # 1d array and matrix with matching number of columns and rows, columns matched
    x.a <- array(13:16, dim = 3, dimnames = list(x.names))
    y <- matrix(1:9, ncol = 3)
    input <- list(x.a, y)
    output <- list(matrix(x.a, nrow = nrow(y), ncol = ncol(y),
                          byrow = TRUE, dimnames = list(NULL, x.names)),
                   y)
    dims <- vapply(input, getDim, integer(1L))
    expect_equal(reshapeOneDimensionalInput(input, dims), output)
    expect_equal(reshapeOneDimensionalInput(rev(input), rev(dims)), rev(output))
    # named vector and matrix with matching number of columns and rows, columns matched
    x.v <- c(A = 13, B = 14, C = 15)
    y <- matrix(1:9, ncol = 3)
    input <- list(x.v, y)
    dims <- vapply(input, getDim, integer(1L))
    expect_equal(reshapeOneDimensionalInput(input, dims), output)
    expect_equal(reshapeOneDimensionalInput(rev(input), rev(dims)), rev(output))
    # Use 2D Q Table with multiple statistics for the remaining outputs
    y <- table2D.PercentageAndCount
    dim.req <- dim(table2D.PercentageAndCount)
    # 1d array and 2D Q Table with multiple statistics, matching rows
    x.names <- LETTERS[1:6]
    x.a <- array(1:6, dim = 6, dimnames = list(x.names))
    input <- list(x.a, y)
    output <- list(array(x.a, dim = dim.req, dimnames = list(x.names, NULL, NULL)), y)
    dims <- vapply(input, getDim, integer(1L))
    expect_equal(reshapeOneDimensionalInput(input, dims), output)
    expect_equal(reshapeOneDimensionalInput(rev(input), rev(dims)), rev(output))
    # named.vector and 2D Q Table with multiple statistics, matching rows
    x.names <- LETTERS[1:6]
    x.v <- 1:6; names(x.v) <- x.names
    input <- list(x.v, y)
    dims <- vapply(input, getDim, integer(1L))
    expect_equal(reshapeOneDimensionalInput(input, dims), output)
    expect_equal(reshapeOneDimensionalInput(rev(input), rev(dims)), rev(output))
    # 1d array and 2D Q Table with multiple statistics, matching columns
    x.names <- LETTERS[1:10]
    x.a <- array(1:10, dim = 10, dimnames = list(x.names))
    input <- list(x.a, y)
    output <- list(array(rep(x.a, each = dim.req[1L]), dim = dim.req, dimnames = list(NULL, x.names, NULL)), y)
    dims <- vapply(input, getDim, integer(1L))
    expect_equal(reshapeOneDimensionalInput(input, dims), output)
    expect_equal(reshapeOneDimensionalInput(rev(input), rev(dims)), rev(output))
    # named.vector and 2D Q Table with multiple statistics, matching columns
    x.v <- 1:10; names(x.v) <- x.names
    input <- list(x.v, y)
    dims <- vapply(input, getDim, integer(1L))
    expect_equal(reshapeOneDimensionalInput(input, dims), output)
    expect_equal(reshapeOneDimensionalInput(rev(input), rev(dims)), rev(output))
    # 1d array and 2D Q Table with multiple statistics, matching statistics
    x.names <- LETTERS[1:2]
    x.a <- array(1:2, dim = 2, dimnames = list(x.names))
    input <- list(x.a, y)
    output <- list(array(rep(x.a, each = prod(dim.req[1:2])), dim = dim.req,
                         dimnames = list(NULL, NULL, x.names)),
                   y)
    dims <- vapply(input, getDim, integer(1L))
    expect_equal(reshapeOneDimensionalInput(input, dims), output)
    expect_equal(reshapeOneDimensionalInput(rev(input), rev(dims)), rev(output))
    # named.vector and 2D Q Table with multiple statistics, matching statistics
    x.v <- 1:2; names(x.v) <- x.names
    input <- list(x.v, y)
    dims <- vapply(input, getDim, integer(1L))
    expect_equal(reshapeOneDimensionalInput(input, dims), output)
    expect_equal(reshapeOneDimensionalInput(rev(input), rev(dims)), rev(output))
})

test_that("Reshaping", {
    # Helper function to determine is elements need to be reshaped/expanded.
    # Two scalars
    x.v <- 1
    y.v <- 2
    x.m <- matrix(1)
    y.m <- matrix(2)
    x.a <- array(1, dim = 1)
    y.a <- array(2, dim = 1)
    inputs <- list(x.v, y.v)
    expect_equal(reshapeIfNecessary(inputs), inputs)
    expect_equal(reshapeIfNecessary(rev(inputs)), rev(inputs))
    inputs <- list(x.m, y.m)
    expect_equal(reshapeIfNecessary(inputs), inputs)
    expect_equal(reshapeIfNecessary(rev(inputs)), rev(inputs))
    inputs <- list(x.a, y.a)
    expect_equal(reshapeIfNecessary(inputs), inputs)
    expect_equal(reshapeIfNecessary(rev(inputs)), rev(inputs))
    inputs <- list(x.v, y.m)
    output <- list(matrix(x.v, dimnames = list(NULL, 1)), y.m)
    expect_equal(reshapeIfNecessary(inputs), output)
    expect_equal(reshapeIfNecessary(rev(inputs)), rev(output))
    inputs <- list(x.v, y.a)
    expect_equal(reshapeIfNecessary(inputs), inputs)
    expect_equal(reshapeIfNecessary(rev(inputs)), rev(inputs))
    inputs <- list(x.a, y.a)
    expect_equal(reshapeIfNecessary(inputs), inputs)
    expect_equal(reshapeIfNecessary(rev(inputs)), rev(inputs))
    # Vectors of same size
    x <- 1:5
    y <- 6:10
    inputs <- list(x, y)
    expect_equal(reshapeIfNecessary(inputs), inputs)
    expect_equal(reshapeIfNecessary(rev(inputs)), rev(inputs))
    # Vector and array of same size
    y <- as.array(y)
    inputs <- list(x, y)
    expect_equal(reshapeIfNecessary(inputs), inputs)
    expect_equal(reshapeIfNecessary(rev(inputs)), rev(inputs))
    # Vectors of different size
    x <- 1:2
    y <- 3:5
    inputs <- list(x, y)
    err.msg <- paste0("Test requires multiple elements to have the same dimension ",
                      "or partially agreeing dimensions. In this case, the inputs ",
                      "are two vectors with 2 rows and 3 rows respectively. ",
                      "Please ensure the inputs have the same or partially ",
                      "agreeing dimensions before attempting to recompute Test")
    expect_error(reshapeIfNecessary(inputs, function.name = "Test"),
                 err.msg)
    expect_error(reshapeIfNecessary(rev(inputs), function.name = "Test"),
                 sub("2 rows and 3 rows", "3 rows and 2 rows", err.msg))
    # Vector and 1d array of different length
    y <- as.array(y)
    inputs <- list(x, y)
    expect_error(reshapeIfNecessary(inputs, function.name = "Test"),
                 err.msg)
    expect_error(reshapeIfNecessary(rev(inputs), function.name = "Test"),
                 sub("2 rows and 3 rows", "3 rows and 2 rows", err.msg))
    # Both 1d arrays
    x <- array(1:3, dim = 3)
    inputs <- list(x, y)
    expect_equal(reshapeIfNecessary(inputs), inputs)
    expect_equal(reshapeIfNecessary(rev(inputs)), rev(inputs))
    .scalarElement <- function(x, dims)
        array(x, dim = dims, dimnames = lapply(dims, function(i) rep(x, i)))
    # Vector and scalar
    scalar.val <- 3L
    y <- scalar.val
    input  <- list(x, y)
    output <- list(x, array(3, dim = 3))
    expect_equal(reshapeIfNecessary(input), output)
    expect_warning(expect_equal(reshapeIfNecessary(input, warn = TRUE), output),
                   "A scalar element was reshaped to a vector with 3 rows")
    expect_equal(reshapeIfNecessary(rev(input)), rev(output))
    # Matrix and matrix, same size
    x <- matrix(1:6, nrow = 3)
    input <- list(x, x)
    expect_equal(reshapeIfNecessary(input), input)
    # Matrix with column vector dims, and 1d array, 1d array reshaped to matrix
    x <- matrix(1:3, nrow = 3)
    y <- array(4:6, dim = 3)
    input <- list(x, y)
    output <- list(x, as.matrix(y))
    expect_equal(reshapeIfNecessary(input), output)
    expect_equal(reshapeIfNecessary(rev(input)), rev(output))
    # Matrix and scalar, reshaped to correct size
    x <- matrix(1:6, nrow = 3)
    input <- list(x, scalar.val)
    output <- list(x, array(3, dim = dim(x)))
    expect_equal(reshapeIfNecessary(input), output)
    expect_equal(reshapeIfNecessary(rev(input)), rev(output))
    # 3d array (QTable) and scalar
    x <- table2D.PercentageAndCount
    input <- list(x, scalar.val)
    output <- list(x,
                   array(scalar.val, dim = dim(x),
                         dimnames = list(NULL, rep(3, NCOL(x)), rep(3, 2))))
    expect_equal(reshapeIfNecessary(input), output)
    expect_equal(reshapeIfNecessary(rev(input)), rev(output))
    # Two matrices, different size
    x <- matrix(1:6, nrow = 3)
    y <- matrix(1:6, nrow = 2)
    err.msg <- paste0("Test requires multiple elements to have the same dimension ",
                      "or partially agreeing dimensions. In this case, the inputs ",
                      "are two matrices with 3 rows and 2 columns and 2 rows and 3 ",
                      "columns respectively. Please ensure the inputs have the same ",
                      "or partially agreeing dimensions before attempting to recompute Test")
    expect_error(reshapeIfNecessary(list(x, y), function.name = "Test"),
                 err.msg)
    # Two matrices, col vector reshaped
    x <- matrix(1:6, nrow = 3)
    y <- matrix(7:9, nrow = 3, dimnames = list(letters[1:3], "A"))
    input <- list(x, y)
    output <- list(x, array(y, dim = c(3, 2), dimnames = list(letters[1:3], rep("A", 2))))
    expect_equal(reshapeIfNecessary(input), output)
    expect_warning(expect_equal(reshapeIfNecessary(input, warn = TRUE), output),
                   "An input element with 3 rows and 1 column was reshaped to a matrix with 3 rows and 2 columns")
    expect_equal(reshapeIfNecessary(rev(input)), rev(output))
    # Two matrices, row vector reshaped
    x <- matrix(1:6, nrow = 3)
    y <- matrix(7:8, nrow = 1)
    input <- list(x, y)
    output <- list(x, array(rep(y, each = 3), dim = c(3, 2)))
    expect_equal(reshapeIfNecessary(input), output)
    expect_equal(reshapeIfNecessary(rev(input)), rev(output))
    # Two vectors, 1 column and 1 row
    x <- matrix(1:5, nrow = 5, dimnames = list(letters[1:5], "foo"))
    y <- matrix(6:8, ncol = 3, dimnames = list("bar", LETTERS[1:3]))
    input <- list(x, y)
    left <- do.call(cbind, replicate(3,
                                     array(1:5, dim = c(5, 1), dimnames = list(letters[1:5], "foo")),
                                     simplify = FALSE))
    right <- do.call(rbind, replicate(5,
                                      array(6:8, dim = c(1, 3), dimnames = list("bar", LETTERS[1:3])),
                                      simplify = FALSE))
    output <- list(left, right)
    expect_equal(reshapeIfNecessary(input), output)
    # Matrix with same dims as the 2d part of a 3d array.
    x <- array(1:24, dim = c(3, 4, 2), dimnames = list(letters[1:3], LETTERS[1:4], c("foo", "bar")))
    y <- array(1:12, dim = c(3, 4), dimnames = list(LETTERS[24:26], letters[23:26]))
    input <- list(x, y)
    left <- x
    right <- array(y, dim = dim(x), dimnames = dimnames(y))
    output <- list(left, right)
    expect_equal(reshapeIfNecessary(input), output)
    # Test warnings
    x <- matrix(1:3, nrow = 3)
    y <- matrix(1:5, nrow = 1)
    expected.out <- list(matrix(x, nrow = 3, ncol = 5),
                         matrix(y, byrow = TRUE, nrow = 3, ncol = 5))
    expect_warning(expect_equal(reshapeIfNecessary(list(x, y), warn = TRUE),
                                expected.out),
                   paste0("Two elements with 3 rows and 1 column and 1 row and 5 ",
                          "columns respectively were reshaped to a matrix with ",
                          "3 rows and 5 columns"))
})

test_that("Warnings", {
    # Infinity warnings
    expect_warning(warnAboutOppositeInfinities(TRUE, "Test"),
                   "Test cannot be computed as the data contains both Inf and -Inf.")
    expect_warning(warnAboutOppositeInfinities(c(TRUE, FALSE), "Test"),
                   "Test cannot compute some values as the data contains both Inf and -Inf.")
    expect_equal(determineIfOppositeInfinitiesWereAdded(x = list(data.frame(x = Inf, y = Inf), c(-Inf, -Inf)),
                                                        nan.output = c(TRUE, TRUE),
                                                        match.rows = "No",
                                                        match.columns = "No"),
                 rep(TRUE, 2L))
    expect_equal(determineIfOppositeInfinitiesWereAdded(x = list(data.frame(x = Inf, y = 1), c(-Inf, -Inf)),
                                                        nan.output = c(TRUE, FALSE),
                                                        match.rows = "No",
                                                        match.columns = "No"),
                 c(TRUE, FALSE))
    expect_equal(determineIfOppositeInfinitiesWereAdded(x = list(data.frame(x = Inf, y = 1), c(-Inf, -Inf)),
                                                        nan.output = c(TRUE, FALSE),
                                                        match.rows = "No",
                                                        match.columns = "No"),
                 c(TRUE, FALSE))
    df <- data.frame(x = 1:3, y = c(Inf, 1:2))
    mat <- as.matrix(-df)
    rownames(mat) <- rownames(df)
    nan.output <- matrix(c(rep(FALSE, 3), TRUE, rep(FALSE, 2)), nrow = 3)
    expect_equal(determineIfOppositeInfinitiesWereAdded(x = list(df, mat),
                                                        nan.output = nan.output,
                                                        match.rows = "Yes",
                                                        match.columns = "Yes"),
                 as.vector(nan.output))
    # Reshaping warnings
    expect_warning(throwWarningAboutReshaping(standardized.dims = 1, dims.to.match = 3),
                   "A scalar element was reshaped to a vector with 3 rows")
    expect_warning(throwWarningAboutReshaping(standardized.dims = 1, dims.to.match = c(1, 3)),
                   "A scalar element was reshaped to a matrix with 1 row and 3 columns")
    expect_warning(throwWarningAboutReshaping(standardized.dims = 1, dims.to.match = c(4, 1)),
                   "A scalar element was reshaped to a matrix with 4 rows and 1 column")
    expect_warning(throwWarningAboutReshaping(standardized.dims = 1, dims.to.match = c(4, 2, 3)),
                   "A scalar element was reshaped to a Q Table with 4 rows, 2 columns and 3 statistics")
    ## Vector reshaping
    expect_warning(throwWarningAboutReshaping(standardized.dims = 4, dims.to.match = c(4, 2)),
                   "An input element with 4 rows was reshaped to a matrix with 4 rows and 2 columns")
    expect_warning(throwWarningAboutReshaping(standardized.dims = c(4, 1), dims.to.match = c(4, 3)),
                   "An input element with 4 rows and 1 column was reshaped to a matrix with 4 rows and 3 columns")
    expect_warning(throwWarningAboutReshaping(standardized.dims = 1, dims.to.match = c(4, 1)),
                   "A scalar element was reshaped to a matrix with 4 rows and 1 column")
    # Removal of slices
    warn.msg <- paste0("There was a single unmatched category (foo) that was removed in the ",
                       "calculation of Test. If you wish these categories to be used in the ",
                       "calculation, consider using the Fuzzy name matching options if the ",
                       "name is similar to an existing category. Alternatively, modify the ",
                       "exact matching options if you wish it to be shown.")
    expect_warning(throwWarningAboutUnmatched("foo", "Test"),
                   warn.msg, fixed = TRUE)
    warn.msg <- paste0("There were unmatched categories that weere removed from the ",
                       "calculation of Test. They had the category names: foo, bar. ",
                       "If you wish these categories to be used in the calculation, ",
                       "consider using the Fuzzy name matching options if the ",
                       "name is similar to an existing category. Alternatively, modify the ",
                       "exact matching options if you wish it to be shown.")
    expect_warning(throwWarningAboutUnmatched(c("foo", "bar"), "Test"),
                   warn.msg)
})

load("table2D.with.Column.Comparisons.rds")
test_that("Test Character statistics are removed", {
    input <- list(variable.Binary, table1D.Average, table.1D.MultipleStatistics,
                  table2D.with.Column.Comparisons,
                  table2D.Percentage, table2D.PercentageAndCount)
    output <- input
    expected.trimmed <- table2D.with.Column.Comparisons
    expected.trimmed <- expected.trimmed[, , c(1, 4, 5)]
    storage.mode(expected.trimmed) <- "numeric"
    expected.trimmed <- flipU::CopyAttributes(expected.trimmed, table2D.with.Column.Comparisons)
    output[[4L]] <- expected.trimmed
    expect_equal(removeCharacterStatisticsFromQTables(input), output)
})

test_that("Handling of NULL elements is ok", {
    # NULL survives the processing steps quickly
    expect_equal(processArguments(list(NULL)), list(NULL))
    # NULL inputs are Filtered out
    expect_equal(processArguments(list(1, NULL, 2, NULL, 3, NULL, 4),
                                  remove.missing = FALSE,
                                  remove.rows = NULL,
                                  remove.columns = FALSE,
                                  subset = NULL,
                                  weights = NULL,
                                  warn = FALSE,
                                  check.statistics = FALSE),
                 list(1, 2, 3, 4))
})

test_that("Symbol attribute created when possible", {
    no.symbols <- pairlist(quote(1:4), quote(runif(10)))
    x <- as.list(no.symbols)
    expect_equal(addSymbolAttributeIfPossible(no.symbols, x), x)
    x <- 1:4
    y <- runif(10)
    with.symbols <- pairlist(quote(x), runif(5), quote(y))
    inputs <- list(x, runif(5), y)
    output.with.attr <- inputs
    attr(output.with.attr[[1L]], "symbol") <- "x"
    attr(output.with.attr[[3L]], "symbol") <- "y"
    expect_equal(addSymbolAttributeIfPossible(with.symbols, inputs), output.with.attr)
})

test_that("Check multiple statistics", {
    load("table2D.PercentageAndCount.rda")
    expect_true(qTableHasMultipleStatistics(table.1D.MultipleStatistics))
    expect_true(qTableHasMultipleStatistics(table2D.PercentageAndCount))
    expect_false(qTableHasMultipleStatistics(table2D.Percentage))

})
