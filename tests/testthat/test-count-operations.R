context("Counting operations")

load("variable.Text.rda")
load("variable.Binary.rda")
load("variable.Nominal.rda")
load("variable.Numeric.rda")
load("variable.Time.rda")
load("variable.Date.rda")

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
    expected.error <- capture_error(throwErrorAboutElementsToCountArgument(quoted.function))[["message"]]
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
    expect_true(canMerge(mergeable[[1L]], mergeable[[2L]]) &&
                    canMerge(mergeable[[2L]], mergeable[[1L]]))
    # Check all ranges parsed and merged if possible.
    valid.ranges <- c("-2--1", "-1-1", "1-3", "4-5", "6-7", "9-10")
    expected.merged.ranges <- list(c(-2L, 3), c(4L, 5L), c(6L, 7L), c(9L, 10L))
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
    expected.valid.situation.list <- list(lte = 10L, gte = 6L, lt = Inf, gt = 5L,
                                          range = list(c(-5L, 5L), c(6L, 10L)),
                                          values = c(NA, 1:5, Inf, -Inf))
    completely.valid <- paste0(unlist(valid.situations))
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
    inequals <- "<=10,>=5"
    expected.list <- list(lte = 10, gte = 5)
    expect_equal(parseStringOfNumericConditions(inequals, quoted.function), expected.list)
    inequals <- gsub("=", "", inequals)
    names(expected.list) <- gsub("e", "", names(expected.list))
    expect_equal(parseStringOfNumericConditions(inequals, quoted.function), expected.list)
    complete.string <- paste0(unlist(valid.situations), collapse = ",")

    expect_equal(parseStringOfNumericConditions(complete.string, quoted.function),
                 expected.valid.situation.list)
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
    bool.test <- sample(as.logical(0L, 1L), size = 12L, replace = TRUE)
    expect_equal(inputToBoolean(bool.test), bool.test)
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
})

test_that("Variables", {
    skip("To be completed later")
    levels.to.count <- sample(1:nlevels(variable.Nominal), size = 2L)
    expected.count <- sum(table(variable.Nominal)[levels.to.count])
    elements.to.count <- list(categorical = levels(variable.Nominal)[levels.to.count],
                              numeric = NULL)
    expect_equal(Count(variable.Nominal, elements.to.count = elements.to.count),
                 expected.count)
})

