context("Counting operations")

quoted.function <- sQuote("Count")

test_that("Valid elements.to.count argument", {
    string <- c("foo", "bar", "baz")
    expect_equal(validateElementsToCount(string, quoted.function),
                 list(categorical = string, numeric = NULL))
    values <- 1:5
    expect_equal(validateElementsToCount(values, quoted.function),
                 list(categorical = NULL, numeric = list(values = values)))
    expected.error <- capture_error(throwErrorAboutElementsToCountArgument(quoted.function))[["message"]]
    nonsense.list <- replicate(3, "Foo", simplify = FALSE)
    expect_error(validateElementsToCount(nonsense.list, quoted.function),
                 expected.error)
    invalid.structure <- sample(c(TRUE, FALSE), size = 10, replace = TRUE)
    expect_error(validateElementsToCount(invalid.structure, quoted.function),
                 expected.error)
    invalid.list <- list(categorical = 1:3, numeric = 1:3)
    expect_error(validateElementsToCount(invalid.list, quoted.function),
                 expected.error)
    valid.list <- list(categorical = letters[1:3], numeric = 1:3)
    expected.list <- list(categorical = letters[1:3], numeric = list(values = 1:3))
    expect_equal(validateElementsToCount(valid.list, quoted.function), expected.list)
    valid.list <- list(categorical = letters[1:3], numeric = "1-3")
    expected.list <- list(categorical = letters[1:3], numeric = list(range = list(c(1L, 3L))))
    expect_equal(validateElementsToCount(valid.list, quoted.function),
                 expected.list)
    invalid.list <- list(categorical = letters[1:3], numeric = c("1-3", "1"))
    expect_error(validateElementsToCount(invalid.list, quoted.function),
                 expected.error)
    invalid.numeric.list <- list(categorical = NULL, numeric = list("foo"))
    expect_error(validateElementsToCount(invalid.numeric.list),
                 paste0("The numeric part of the elements.to.count list be a named list ",
                        "with the possible names: 'values', 'range', 'gt', 'gte', 'lt', 'lte'"))
    invalid.numeric.list <- list(categorical = NULL, numeric = list(foo = "foo"))
    expect_error(validateElementsToCount(invalid.numeric.list),
                 paste0("The numeric part of the elements.to.count list be a named list ",
                        "with the possible names: 'values', 'range', 'gt', 'gte', 'lt', 'lte'"))
    invalid.numeric.list <- list(categorical = NULL, numeric = list(values = "foo"))
    expect_error(validateElementsToCount(invalid.numeric.list),
                 paste0("The ", sQuote("values"), " element of the numeric part ",
                        "of the elements.to.count list needs to contain numeric values"))
    invalid.numeric.list <- list(categorical = NULL, numeric = list(range = "foo"))
    expect_error(validateElementsToCount(invalid.numeric.list),
                 paste0("The ", sQuote("range"), " element of the numeric part ",
                        "of the elements.to.count list needs to be a list where all ",
                        "elements are two numeric values"))
    invalid.numeric.list <- list(categorical = NULL, numeric = list(range = list(c(1, 2), 2)))
    expect_error(validateElementsToCount(invalid.numeric.list),
                 paste0("The ", sQuote("range"), " element of the numeric part ",
                        "of the elements.to.count list needs to be a list where all ",
                        "elements are two numeric values"))
    invalid.numeric.list <- list(categorical = NULL,
                                 numeric = list(range = list(c(1, NA), c(1, 2))))
    expect_error(validateElementsToCount(invalid.numeric.list),
                 paste0("The ", sQuote("range"), " element of the numeric part ",
                        "of the elements.to.count list needs to be a list where all ",
                        "elements are two numeric values"))
    invalid.numeric.list <- list(categorical = NULL,
                                 numeric = list(lt = 2, lte = 3, gt = 2, gte = c(1, 3)))
    expect_error(validateElementsToCount(invalid.numeric.list),
                 paste0("The ", sQuote("gte"), " element of the numeric part ",
                        "of the elements.to.count list needs to be a single numeric ",
                        "value denoting the boundary of the inequality"))
    invalid.numeric.list <- list(categorical = NULL,
                                 numeric = list(lt = 2, lte = 3, gt = c(1, 2), gte = c(1, 3)))
    expect_error(validateElementsToCount(invalid.numeric.list),
                 paste0("The ", paste0(sQuote(c("gt", "gte")), collapse = ", "), " elements ",
                        "of the numeric part of the elements.to.count list each need to be a ",
                        "single numeric value denoting the boundary of each inequality"))
    valid.numeric.list <- list(categorical = NULL,
                               numeric = list(values = c(1, 2, 3, NA)))
    expect_error(validateElementsToCount(valid.numeric.list), NA)
    valid.numeric.list <- list(numeric = "NA", categorical = NULL)
    expect_equal(validateElementsToCount(valid.numeric.list),
                 list(categorical = NULL, numeric = list(values = NA)))
})

test_that("Check range parsing", {
    invalid.ranges <- list("1-3-4", "1-NA", "Inf-Inf", "NA-1", "Inf-1")
    invalid.ranges[[6L]] <- paste0(c("1-3", unlist(invalid.ranges)), collapse = ",")
    for (invalid in invalid.ranges)
    {
        expected.error <- capture_error(throwErrorAboutInvalidCharsInElementsToCount(invalid, quoted.function))[["message"]]
        expect_error(parseRanges(invalid, quoted.function), expected.error)
    }
    # check merging
    mergeable <- list(c(1L, 3L), c(2L, 4L))
    expect_true(canMergeRanges(mergeable[[1L]], mergeable[[2L]]) &&
                    canMergeRanges(mergeable[[2L]], mergeable[[1L]]))
    # Check all ranges parsed and merged if possible.
    valid.ranges <- c("-2--1", "-1-1", "1-3", "4-5", "6-7", "9-10",
                      "2.718282-3.141593", "-3.141593--2.718282")
    expected.merged.ranges <- list(c(-3.141593, -2.718282), c(-2, 3.141593),
                                   c(4L, 5L), c(6L, 7L), c(9L, 10L))
    expect_equal(parseRanges(valid.ranges), expected.merged.ranges)
    disjoint.ranges <- list(c(1L, 3L), c(5L, 8L), c(10L, 12L))
    expect_equal(mergeOverlappingRanges(disjoint.ranges), disjoint.ranges)
    overlapping.ranges <- disjoint.ranges
    overlapping.ranges <- c(overlapping.ranges, list(c(2L, 4L), c(7L, 9L)))
    expected.disjoint <- list(c(1L, 4L), c(5L, 9L), c(10L, 12L))
    expect_equal(mergeOverlappingRanges(overlapping.ranges), expected.disjoint)
    expect_equal(mergeOverlappingRanges(rev(overlapping.ranges)), expected.disjoint)
})

test_that("Check Inequality parsing", {
    valid.lt.ineq <- c("<10", "<-1")
    expect_equal(parseInequalities(valid.lt.ineq, "<", quoted.function),
                 10L)
    valid.lt.ineq <- c(">10", ">-1")
    expect_equal(parseInequalities(valid.lt.ineq, ">", quoted.function),
                 -1L)
    invalid.state <- c("<NA")
    expected.error <- capture_error(throwErrorAboutInvalidCharsInElementsToCount(invalid.state, quoted.function))[["message"]]
    expect_error(parseInequalities(invalid.state, "<", quoted.function), expected.error)
})

test_that("Parsing numeric element strings", {
    invalid.cases <- list("foo", "3>", "4<", "1----")
    valid.situations <- list("1-5,6-10,-5-1,-3--1",
                             "1,2,3,4,5,Inf,-Inf,NA",
                             ">5",
                             ">=6",
                             "<=10",
                             "<Inf")
    completely.valid <- paste0(unlist(valid.situations), collapse = ",")
    edge.cases <- ">Inf,<-Inf,<NA,>NA"
    valid.edge.cases <- "1-5, 1, 3,Inf, inf, INF, NA, na, Na, nA"
    expected.valid <- "1-5,1,3,Inf,Inf,Inf,NA,NA,NA,NA"

    for (situation in valid.situations)
        expect_equal(checkElementsToCountCharactersValid(situation), situation)
    expect_equal(checkElementsToCountCharactersValid(edge.cases), edge.cases)
    expect_equal(checkElementsToCountCharactersValid(valid.edge.cases), expected.valid)
    for (invalid in invalid.cases)
    {
        expected.error <- capture_error(throwErrorAboutInvalidCharsInElementsToCount(invalid, quoted.function))[["message"]]
        expect_error(parseStringOfNumericConditions(invalid, quoted.function),
                     expected.error)
    }
    values <- "-12,0,1,2,3,4,5,Inf,-Inf"
    values.with.na <- paste0(values, ",NA,NA")
    expected.list <- list(values = as.numeric(strsplit(values, ",")[[1L]]))
    expected.list.with.na <- list(values = c(NA, expected.list[["values"]]))
    expect_equal(parseStringOfNumericConditions(values, quoted.function), expected.list)
    expect_equal(parseStringOfNumericConditions(values.with.na, quoted.function),
                 expected.list.with.na)
    inequals <- "<=5, >=10"
    expected.list <- list(lte = 5L, gte = 10L)
    expect_equal(parseStringOfNumericConditions(inequals, quoted.function), expected.list)
    inequals <- gsub("=", "", inequals)
    names(expected.list) <- gsub("e", "", names(expected.list))
    expect_equal(parseStringOfNumericConditions(inequals, quoted.function), expected.list)
    inequals <- "<=10,>=5"
    expected.list <- list(lte = Inf)
    expect_equal(parseStringOfNumericConditions(inequals, quoted.function), expected.list)
    complete.string <- paste0(unlist(valid.situations), collapse = ",")
    expected.valid.situation.list <- list(lte = Inf, values = NA)
    expect_equal(parseStringOfNumericConditions(complete.string, quoted.function),
                 expected.valid.situation.list)
    # Check redundant conditions are omitted/merged
    string.with.redundant.ineq <- "<5,<=5"
    expect_equal(parseStringOfNumericConditions(string.with.redundant.ineq, quoted.function),
                 list(lte = 5))
    string.with.redundant.ineq <- ">5,>=5"
    expect_equal(parseStringOfNumericConditions(string.with.redundant.ineq, quoted.function),
                 list(gte = 5))
    string.with.redundant.ineq <- ">3,>=5"
    expect_equal(parseStringOfNumericConditions(string.with.redundant.ineq, quoted.function),
                 list(gt = 3))
    string.with.redundant.ineq <- ">3,>=5,<0,<=2"
    expect_equal(parseStringOfNumericConditions(string.with.redundant.ineq, quoted.function),
                 list(lte = 2, gt = 3))
    string.with.mergeable.ranges <- "-3--1,0-2,1-3"
    expect_equal(parseStringOfNumericConditions(string.with.mergeable.ranges, quoted.function),
                 list(range = list(c(-3L, -1L), c(0L, 3L))))
    string.with.mergeable.ranges <- "-3--1,0-2,-2-1,1-3"
    expect_equal(parseStringOfNumericConditions(string.with.mergeable.ranges, quoted.function),
                 list(range = list(c(-3L, 3L))))
    string.with.mergeable.output <- "<-5,-3--1,0-2,-2-1,1-3,>10"
    expect_equal(parseStringOfNumericConditions(string.with.mergeable.output, quoted.function),
                 list(lt = -5L, gt = 10L, range = list(c(-3L, 3L))))
    string.with.mergeable.output <- "<-2,-3--1,0-2,-2-1,1-3,>10"
    expect_equal(parseStringOfNumericConditions(string.with.mergeable.output, quoted.function),
                 list(lte = 3L, gt = 10L))
    string.with.mergeable.output <- "<-2,-3--1,0-2,-2-1,1-3,9-12,>10"
    expect_equal(parseStringOfNumericConditions(string.with.mergeable.output, quoted.function),
                 list(lte = 3L, gte = 9L))
    string.with.mergeable.output <- "<-2,-3-3,4-5,9-12,>10"
    expect_equal(parseStringOfNumericConditions(string.with.mergeable.output, quoted.function),
                 list(lte = 3L, gte = 9L, range = list(c(4L, 5L))))
    completely.redundent.string <- "<0,>1,0-1"
    expect_equal(parseStringOfNumericConditions(completely.redundent.string, quoted.function),
                 list(lte = Inf))
    string.with.mergeable.output <- "<-2,-3-3,4-5,9-12,>10,6,7,8,NA"
    expect_equal(parseStringOfNumericConditions(string.with.mergeable.output, quoted.function),
                 list(lte = 3L, gte = 9L, range = list(c(4L, 5L)), values = c(NA, 6:8)))
    string.with.mergeable.output <- "<-2,-3-3,4-5,9-12,>10,0,1,2,3,NA"
    expect_equal(parseStringOfNumericConditions(string.with.mergeable.output, quoted.function),
                 list(lte = 3L, gte = 9L, range = list(c(4L, 5L)), values = NA))
})

test_that("Conversion of values to count to evaluateable conditions", {
    odd.numbers <- seq(from = 1L, to = 9L, by = 2L)
    elements.to.count <- list(categorical = NULL, numeric = list(values = odd.numbers))
    expected.list <- list(categorical = NULL,
                          numeric = list(values = quote(x %in% c(1L, 3L, 5L, 7L, 9L))))
    expect_equal(elementsToCountAsConditions(elements.to.count), expected.list)
    levels.to.count <- c("foo", "bar", "baz", NA)
    elements.to.count <- list(categorical = levels.to.count,
                              numeric = NULL)
    expected.list <- list(categorical = quote(x %in% c("foo", "bar", "baz", NA)),
                          numeric = NULL)
    expect_equal(elementsToCountAsConditions(elements.to.count), expected.list)
    elements.to.count <- list(categorical = NULL,
                              numeric = list(range = list(c(-5L, 0L), c(10L, 100L)),
                                             values = odd.numbers,
                                             gt = 5L,
                                             gte = -2L,
                                             lt = 10L,
                                             lte = 100L))
    expected.list <- list(categorical = NULL,
                          numeric = list(range1 = quote(x >= -5L & x <= 0L),
                                         range2 = quote(x >= 10L & x <= 100L),
                                         values = quote(x %in% c(1L, 3L, 5L, 7L, 9L)),
                                         gt = quote(x > 5L),
                                         gte = quote(x >= -2L),
                                         lt = quote(x < 10L),
                                         lte = quote(x <= 100L)))
    expect_equal(elementsToCountAsConditions(elements.to.count), expected.list)
})

test_that("Convert inputs to boolean with conditions", {
    test <- sample(1:12)
    odd.vals <- seq(from = 1L, to = 11L, by = 2L)
    single.condition <- list(categorical = NULL,
                             numeric = list(values = quote(x %in% seq(from = 1L, to = 11L, by = 2L))))
    expect_equal(inputToBoolean(test, single.condition),
                 test %in% odd.vals)
    two.conditions <- list(categorical = NULL,
                           numeric = list(values = quote(x %in% seq(from = 1L, to = 11L, by = 2L)),
                                          gt = quote(x > 6L)))
    expect_equal(inputToBoolean(test, two.conditions),
                 test %in% odd.vals | test > 6L)
    factor.test <- as.factor(sample(c("fizz", "buzz", "foo", "bar", "baz", NA),
                                    size = 1e2, replace = TRUE))
    fbb <- c("foo", "bar", "baz", NA)
    factor.condition <- list(categorical = quote(x %in% c("foo", "bar", "baz", NA)),
                             numeric = NULL)
    expect_equal(inputToBoolean(factor.test, factor.condition),
                 factor.test %in% fbb)
    test.df <- data.frame(factor.test = factor.test,
                          number.test = sample(1e3, size = 1e2))
    cat.and.num.cond <- list(categorical = quote(x %in% c("foo", "bar", "baz")),
                             numeric = list(range = quote(x >= 500L & x <= 750L)))
    expected.logical <- array(c(test.df[["factor.test"]] %in% c("foo", "bar", "baz"),
                                test.df[["number.test"]] >= 500 &
                                    test.df[["number.test"]] <= 750),
                              dim = c(nrow(test.df), 2L),
                              dimnames = list(NULL, names(test.df)))
    expect_equal(inputToBoolean(test.df, cat.and.num.cond),
                 expected.logical)
    named.vector <- setNames(1:10, letters[1:10])
    expected.vector <- setNames(rep(c(TRUE, FALSE), 5L), letters[1:10])
    counting.condition <- list(categorical = NULL,
                               numeric = list(values = quote(x %in% seq(from = 1, to = 11, by = 2))))
    expect_equal(inputToBoolean(named.vector, counting.condition), expected.vector)
    test.matrix <- array(1:12, dim = 3:4, dimnames = list(letters[1:3], LETTERS[1:4]))
    expected.matrix <- array(c(TRUE, FALSE), dim = dim(test.matrix), dimnames = dimnames(test.matrix))
    expect_equal(inputToBoolean(test.matrix, counting.condition), expected.matrix)
})

load("variable.Text.rda")
load("variable.Binary.rda")
load("variable.Nominal.rda")
load("variable.Numeric.rda")
load("variable.Time.rda")
load("variable.Date.rda")

test_that("Variables", {
    # Count
    levels.to.count <- sample(1:nlevels(variable.Nominal), size = 2L)
    expected.single.count <- sum(table(variable.Nominal)[levels.to.count])
    elements.to.count <- list(categorical = levels(variable.Nominal)[levels.to.count],
                              numeric = NULL)
    count.foo = list(categorical = "Foo", numeric = NULL)
    levels.var <- levels(variable.Nominal)
    randomized.second <- sample(variable.Nominal)
    expected.multiple.count <- (variable.Nominal %in% levels.var[levels.to.count]) +
        (randomized.second %in% levels.var[levels.to.count])
    expect_equal(Count(variable.Nominal, elements.to.count = elements.to.count),
                 expected.single.count)
    expect_true(is.na(Count(variable.Nominal, elements.to.count = elements.to.count,
                            ignore.missing = FALSE)))
    expect_equal(Count(variable.Nominal, randomized.second, elements.to.count = elements.to.count),
                 expected.multiple.count)
    nas <- is.na(variable.Nominal) | is.na(randomized.second)
    expected.multiple.count.with.nas <- expected.multiple.count
    is.na(expected.multiple.count.with.nas) <- nas
    expect_equal(Count(variable.Nominal, randomized.second,
                       elements.to.count = elements.to.count,
                       ignore.missing = FALSE),
                 expected.multiple.count.with.nas)
    expected.error <- capture_error(throwErrorAboutMissingCondition(variable.Nominal, quoted.function))[["message"]]
    expect_error(Count(variable.Nominal, elements.to.count = list(numeric = 1:5)),
                 expected.error)
    # AnyOf
    expect_true(AnyOf(variable.Nominal, elements.to.count = elements.to.count))
    expect_true(AnyOf(variable.Nominal, elements.to.count = elements.to.count, ignore.missing = FALSE))
    expect_true(is.na(AnyOf(variable.Nominal, elements.to.count = count.foo, ignore.missing = FALSE)))
    expect_false(AnyOf(variable.Nominal, elements.to.count = count.foo))
    vars <- list(variable.Nominal, randomized.second)
    expected.anyof <- Reduce(`|`, lapply(vars, function(x) x %in% levels.var[levels.to.count]))
    expect_equal(AnyOf(variable.Nominal, randomized.second, elements.to.count = elements.to.count),
                 expected.anyof)
    expected.anyof.with.na <- Reduce(`|`, lapply(vars,
                                                 function(x)
                                                 {
                                                     output <- x %in% levels.var[levels.to.count]
                                                     is.na(output) <- is.na(x)
                                                     output
                                                  }))
    expect_equal(AnyOf(variable.Nominal, randomized.second,
                       elements.to.count = elements.to.count,
                       ignore.missing = FALSE),
                 expected.anyof.with.na)
    # NoneOf
    expect_true(NoneOf(variable.Nominal, elements.to.count = count.foo))
    expect_false(NoneOf(variable.Nominal, elements.to.count = elements.to.count))
    expect_false(NoneOf(variable.Nominal,
                        elements.to.count = elements.to.count,
                        ignore.missing = FALSE))
    expect_true(is.na(NoneOf(variable.Nominal,
                             elements.to.count = count.foo,
                             ignore.missing = FALSE)))
    expect_equal(NoneOf(variable.Nominal, randomized.second, elements.to.count = elements.to.count),
                 !expected.anyof)
    expect_equal(NoneOf(variable.Nominal, randomized.second,
                        elements.to.count = elements.to.count,
                        ignore.missing = FALSE),
                 !expected.anyof.with.na)
})

load("table1D.Average.rda")
load("table2D.Percentage.rda")
load("table2D.PercentageAndCount.rda")
load("table2D.PercentageNaN.rda")

test_that("Table inputs", {
   # CountzX
    values.to.count <- list(numeric = ">4")
    expect_equal(Count(table1D.Average, elements.to.count = values.to.count),
                 sum(table1D.Average > 4L))
    expect_equal(Count(table1D.Average,
                       remove.rows = "SUM", elements.to.count = values.to.count),
                 sum(table1D.Average[-4L] > 4L))
    values.to.count <- list(numeric = ">=50")
    expect_equal(Count(table2D.PercentageNaN, elements.to.count = values.to.count),
                 sum(table2D.PercentageNaN >= 50L, na.rm = TRUE))
    expect_true(is.na(Count(table2D.PercentageNaN, elements.to.count = values.to.count,
                            ignore.missing = FALSE)))
    second.table <- AverageEachRow(table2D.PercentageNaN)
    values.to.count <- list(numeric = "0-25")
    unmatched <- c("Coca-Cola", "Coke", "None of these", "NET")
    matched.first <- table2D.Percentage[-1, ]
    matched.second <- array(second.table[!names(second.table) %in% unmatched], dim = c(5L, 10L))
    captured.warnings <- capture_warnings(counts <- Count(table2D.Percentage, second.table,
                                                          elements.to.count = values.to.count,
                                                          warn = TRUE))
    expect_equal(counts, (matched.first <= 25) + (matched.second  <= 25))
    expected.warnings <- c(capture_warnings(throwWarningAboutRecycling(5, c(5, 10))),
                           capture_warnings(throwWarningAboutUnmatched(unmatched, quoted.function)))
    expect_setequal(captured.warnings, expected.warnings)
    expected.error <- capture_error(throwErrorAboutMissingCondition(table1D.Average, quoted.function))[["message"]]
    expect_error(Count(table1D.Average, elements.to.count = list(categorical = letters[1:2])),
                 expected.error)
    qtable <- table2D.PercentageAndCount
    expected.out <- qtable %in% 1:50
    dim(expected.out) <- dim(qtable)
    dimnames(expected.out) <- dimnames(qtable)
    expect_equal(Count(qtable, qtable, elements.to.count = list(numeric = 1:50)),
                 2 * (expected.out))
    # edge cases, dont try to restore NAs if NAs are being counted
    na.array <- array(NA, dim = 3:4, dimnames = list(letters[1:3], LETTERS[1:4]))
    expect_equal(Count(na.array), Count(na.array, ignore.missing = FALSE))

    test.dat <- data.frame(x.factor = factor(sample(c("foo", "bar", "baz"), size = 100L, replace = TRUE)),
                           x.numeric = runif(100L))
    expected.error <- capture_error(throwErrorAboutMissingCondition(test.dat[["x.factor"]], quoted.function))[["message"]]
    expect_error(Count(test.dat, elements.to.count = list(numeric = "<0.5")), expected.error)
    expected.error <- capture_error(throwErrorAboutMissingCondition(test.dat[["x.numeric"]], quoted.function))[["message"]]
    expect_error(Count(test.dat, elements.to.count = list(categorical = "foo")), expected.error)
})

test_that("Row and Column variants", {
    checkAllOperations <- function(input, expected.output, ...)
    {
        if (!is.list(expected.output))
        {
            any.of.output <- as.logical(expected.output)
            none.of.output <- !any.of.output
            all.outputs <- list(expected.output, any.of.output, none.of.output)
            for (i in seq_along(all.outputs))
                mostattributes(all.outputs[[i]]) <- attributes(expected.output)
        } else
            all.outputs <- expected.output
        operations <- list(count, anyOf, noneOf)
        args <- list(...)
        dimension <- args[["dimension"]]
        args[["dimension"]] <- NULL
        args[["x"]] <- input
        if (dimension == 1L)
            exported <- list(CountEachRow, AnyOfEachRow, NoneOfEachRow)
        else
            exported <- list(CountEachColumn, AnyOfEachColumn, NoneOfEachColumn)


        for (i in seq_along(operations))
        {
            expect_equal(countEachDimension(input,
                                            operation = operations[[i]],
                                            ...,),
                         all.outputs[[i]])
            expect_equal(do.call(exported[[i]], args),
                         all.outputs[[i]])
        }
    }
    test.array.1d <- array(1:12, dim = 12L, dimnames = list(letters[1:12]))
    test.array.2d <- array(1:12, dim = 3:4, dimnames = list(letters[1:3], LETTERS[1:4]))
    test.array.3d <- array(1:24, dim = c(3:4, 2), dimnames = list(letters[1:3], LETTERS[1:4],
                                                                  c("foo", "bar")))
    attr(test.array.3d, "questions") <- "Hi"
    test.array.1d[12L] <- test.array.2d[12L] <- test.array.3d[12L] <- NA
    even.numbers <- 2L * (1:12)
    counting.condition <- list(numeric = "2,4,6,8,10,12,>=12")
    counting.condition.with.na <- list(numeric = "2,4,6,8,10,12,>=12,NA")
    expected.1d.row.counts <- setNames(1L * (test.array.1d %in% even.numbers),
                                       names(test.array.1d))
    checkAllOperations(test.array.1d, expected.1d.row.counts,
                       dimension = 1L, elements.to.count = counting.condition)
    expected.1d.row.counts.na.included <- expected.1d.row.counts
    expected.1d.row.counts.na.included[12L] <- 1L
    expected.1d.row.counts.with.na <- expected.1d.row.counts
    is.na(expected.1d.row.counts.with.na) <- is.na(test.array.1d)
    checkAllOperations(test.array.1d, expected.1d.row.counts.with.na,
                       dimension = 1L, elements.to.count = counting.condition,
                       ignore.missing = FALSE)

    expected.1d.col.counts <- 5L
    expected.1d.col.counts.with.na <- list(NA_integer_, TRUE, FALSE)
    checkAllOperations(test.array.1d, expected.1d.col.counts,
                       dimension = 2L, elements.to.count = counting.condition)
    checkAllOperations(test.array.1d, expected.1d.col.counts.with.na,
                       dimension = 2L, elements.to.count = counting.condition,
                       ignore.missing = FALSE)

    expect_equal(countEachDimension(test.array.1d,
                                    dimension = 1L,
                                    ignore.missing = FALSE,
                                    elements.to.count = counting.condition.with.na),
                 expected.1d.row.counts.na.included)
    split.by.row <- split(test.array.2d, row(test.array.2d))
    names(split.by.row) <- rownames(test.array.2d)
    split.by.col <- split(test.array.2d, col(test.array.2d))
    names(split.by.col) <- colnames(test.array.2d)

    expected.2d.row.counts <- vapply(split.by.row, Count,
                                     integer(1L),
                                     elements.to.count = counting.condition)
    all.operators <- list(Count, AnyOf, NoneOf)
    expected.2d.row.counts.with.na <- lapply(all.operators,
                                             function(f)
                                             vapply(split.by.row, f,
                                                    if (identical(f, Count)) integer(1L) else logical(1L),
                                                    elements.to.count = counting.condition,
                                                    ignore.missing = FALSE))
    expected.2d.col.counts <- vapply(split.by.col, Count,
                                     integer(1L),
                                     elements.to.count = counting.condition)
    expected.2d.col.counts.with.na <- lapply(all.operators,
                                             function(f)
                                             vapply(split.by.col, f,
                                                    if (identical(f, Count)) integer(1L) else logical(1L),
                                                    elements.to.count = counting.condition,
                                                    ignore.missing = FALSE))

    checkAllOperations(test.array.2d, expected.2d.col.counts,
                       dimension = 2L, elements.to.count = counting.condition)
    checkAllOperations(test.array.2d, expected.2d.col.counts.with.na,
                       dimension = 2L, elements.to.count = counting.condition,
                       ignore.missing = FALSE)

    expected.3d.row.counts <- apply(test.array.3d, c(1L, 3L), Count,
                                    elements.to.count = counting.condition)
    expected.3d.row.counts.with.na <- lapply(all.operators,
                                             function(f)
                                             apply(test.array.3d, c(1L, 3L), f,
                                                   elements.to.count = counting.condition,
                                                   ignore.missing = FALSE))
    expected.3d.col.counts <- apply(test.array.3d, 2:3, Count,
                                    elements.to.count = counting.condition)
    expected.3d.col.counts.with.na <- lapply(all.operators,
                                             function(f)
                                             apply(test.array.3d, 2:3, f,
                                                   elements.to.count = counting.condition,
                                                   ignore.missing = FALSE))
    checkAllOperations(test.array.3d, expected.3d.row.counts,
                       dimension = 1L, elements.to.count = counting.condition)
    checkAllOperations(test.array.3d, expected.3d.row.counts.with.na,
                       dimension = 1L, elements.to.count = counting.condition,
                       ignore.missing = FALSE)
    checkAllOperations(test.array.3d, expected.3d.col.counts,
                       dimension = 2L, elements.to.count = counting.condition)
    checkAllOperations(test.array.3d, expected.3d.col.counts.with.na,
                       dimension = 2L, elements.to.count = counting.condition,
                       ignore.missing = FALSE)
    subsetted.3d.array <- test.array.3d[, , 1, drop = FALSE]
    subsetted.3d.array <- CopyAttributes(subsetted.3d.array, test.array.3d)
    expect_equal(countEachDimension(subsetted.3d.array, dimension = 1L,
                                    elements.to.count = counting.condition,
                                    ignore.missing = TRUE),
                 expected.3d.row.counts[, 1])
    # Correct error if dimension not specified
    expected.error <- capture_error(throwErrorAboutMissingDimensionArgument(substitute(count), quoted.function))[["message"]]
    expect_true(grepl("count", expected.error))
    expect_error(countEachDimension(), expected.error)
})

test_that("Missing values handled correctly", {
    test.array <- array(1:12, dim = 3:4)
    is.na(test.array) <- c(1L, 7L)
    gt.cond.met <- list(numeric = list(gt = 5))
    gt.cond.not <- list(numeric = list(gt = 12))
    values.met <- list(numeric = list(values = 10:12))
    values.not <- list(numeric = list(values = 13:15))
    # AnyOf
    expect_true(AnyOf(test.array, elements.to.count = gt.cond.met))
    expect_true(AnyOf(test.array, elements.to.count = gt.cond.met, ignore.missing = FALSE))
    expect_false(AnyOf(test.array, elements.to.count = gt.cond.not))
    expect_true(is.na(AnyOf(test.array, elements.to.count = gt.cond.not, ignore.missing = FALSE)))
    expect_true(AnyOf(test.array, elements.to.count = values.met))
    expect_false(AnyOf(test.array, elements.to.count = values.not))
    expect_true(AnyOf(test.array, elements.to.count = values.met, ignore.missing = FALSE))
    expect_true(is.na(AnyOf(test.array, elements.to.count = values.not, ignore.missing = FALSE)))
    # Count
    expect_true(Count(test.array, elements.to.count = gt.cond.met) > 0)
    expect_true(is.na(Count(test.array, elements.to.count = gt.cond.met, ignore.missing = FALSE)))
    expect_true(Count(test.array, elements.to.count = gt.cond.not) == 0)
    expect_true(is.na(Count(test.array, elements.to.count = gt.cond.not, ignore.missing = FALSE)))
    expect_true(Count(test.array, elements.to.count = values.met) > 0)
    expect_true(Count(test.array, elements.to.count = values.not) == 0)
    expect_true(is.na(Count(test.array, elements.to.count = values.met, ignore.missing = FALSE)))
    expect_true(is.na(Count(test.array, elements.to.count = values.not, ignore.missing = FALSE)))
    # Displayr level string handled correctly
    test.factor <- factor(sample(c("foo", "bar", "baz", NA), size = 100, replace = TRUE))
    cond.with.reserved.string <- list(categorical = c("foo", "bar", "Missing data used only by Q/Displayr"))
    regular.cond <- list(categorical = c("foo", "bar", NA))
    expect_equal(Count(test.factor, elements.to.count = regular.cond), sum(test.factor %in% c("foo", "bar", NA)))
    expect_equal(Count(test.factor, elements.to.count = regular.cond),
                 Count(test.factor, elements.to.count = cond.with.reserved.string))
})

test_that("More than 2 inputs", {
    first  <- setNames(c(1:3, NA), LETTERS[1:4])
    second <- setNames(c(2L, 1L, NA, 4L), LETTERS[1:4])
    third <- factor(letters[c(2L, 1L, NA, 1L)])
    df.test <- data.frame(first, second, third)
    expectedOutput <- function(input, operation, counting.conditions, ...)
        setNames(countEachDimension(input, dimension = 1L, operation = operation,
                                    elements.to.count = counting.conditions, ...),
                 rowNames(input))
    # Count checks
    counting.conditions <- list(numeric = list(values = 1:2), categorical = c("a", NA))
    expect_equal(Count(first, second, third, elements.to.count = counting.conditions),
                 expectedOutput(df.test, operation = count, counting.conditions))
    counting.conditions <- list(numeric = c(1:2, NA), categorical = c("a", NA))
    expect_equal(Count(first, second, third, elements.to.count = counting.conditions),
                 expectedOutput(df.test, operation = count, counting.conditions))
    # Anyof checks
    counting.conditions <- list(numeric = list(values = 1:2), categorical = "a")
    expect_equal(AnyOf(first, second, third, elements.to.count = counting.conditions),
                 expectedOutput(df.test, operation = anyOf, counting.conditions))
    expect_equal(AnyOf(first, second, third, elements.to.count = counting.conditions, ignore.missing = FALSE),
                 expectedOutput(df.test, operation = anyOf, counting.conditions, ignore.missing = FALSE))
    # NoneOf checks
    counting.conditions <- list(numeric = list(values = 1:2), categorical = "a")
    expect_equal(NoneOf(first, second, third, elements.to.count = counting.conditions),
                 expectedOutput(df.test, operation = noneOf, counting.conditions))
    expect_equal(NoneOf(first, second, third, elements.to.count = counting.conditions, ignore.missing = FALSE),
                 expectedOutput(df.test, operation = noneOf, counting.conditions, ignore.missing = FALSE))
})

test_that("NULL handled", {
    for (fun in c(Count, CountEachColumn, CountEachRow))
        expect_equal(fun(NULL), 0L)
    for (fun in c(AnyOf, AnyOfEachColumn, AnyOfEachRow))
        expect_equal(fun(NULL), FALSE)
    for (fun in c(NoneOf, NoneOfEachColumn, NoneOfEachRow))
        expect_equal(fun(NULL), TRUE)
    expect_equal(Count(1:10, NULL, elements.to.count = list(numeric = 1:5)),
                 (1:10 <= 5) * 1L)
    expect_equal(Count(1:10, 2:11, NULL, elements.to.count = list(numeric = 1:5)),
                 (1:10 <= 5) + (2:11 <= 5))
    expect_equal(AnyOf(1:10, NULL, elements.to.count = list(numeric = 1:5)),
                 (1:10 <= 5))
    expect_equal(NoneOf(1:10, NULL, elements.to.count = list(numeric = 1:5)),
                 !(1:10 <= 5))
})
