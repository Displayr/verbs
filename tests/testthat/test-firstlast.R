context("First and Last")

library(flipTime)

test_that("First and last without time period", {
    # vector
    x <- 1:10
    names(x) <- LETTERS[1:10]
    expect_equal(First(x, 3),
                 structure(1:3, .Names = c("A", "B", "C")))

    # vector, matches head and tail
    x <- 1:10
    names(x) <- LETTERS[1:10]
    expect_equal(First(x), head(x, 1))
    expect_equal(Last(x), tail(x, 1))
    expect_equal(First(x, 2), head(x, 2))
    expect_equal(Last(x, 2), tail(x, 2))

    # vector, negative keep
    expect_equal(First(x, -3), head(x, -3))
    expect_equal(Last(x, -3), tail(x, -3))

    # matrix, matches head and tail
    m <- matrix(1:20, 4)
    expect_equal(First(m), head(m, 1))
    expect_equal(Last(m), tail(m, 1))
    expect_equal(First(m, 2), head(m, 2))
    expect_equal(Last(m, 2), tail(m, 2))

    # matrix with multiple keep, matches head and tail
    m <- matrix(1:20, 4)
    expect_equal(First(m, c(2, 3)), head(m, c(2, 3)))
    expect_equal(Last(m, c(2, 3)), tail(m, c(2, 3)))
    expect_equal(First(m, c(NA, 1)), head(m, c(NA, 1)))
    expect_equal(Last(m, c(NA, 1)), tail(m, c(NA, 1)))

    # matrix with negative keep
    expect_equal(First(m, c(-2, 3)), head(m, c(-2, 3)))
    expect_equal(Last(m, c(-2, 3)), tail(m, c(-2, 3)))
    expect_equal(First(m, c(NA, -2)), head(m, c(NA, -2)))
    expect_equal(Last(m, c(NA, -2)), tail(m, c(NA, -2)))

    # array, matches head and tail
    arr <- array(1:24, dim = c(2,3,4))
    expect_equal(First(arr), head(arr, 1))
    expect_equal(Last(arr), tail(arr, 1))
    expect_equal(First(arr, 2), head(arr, 2))
    expect_equal(Last(arr, 2), tail(arr, 2))

    # list, matches head and tail
    lst <- list(1, 1:2, 1:3, 1:4)
    names(lst) <- letters[1:4]
    expect_equal(First(lst, 3), head(lst, 3))
    expect_equal(Last(lst, 3), tail(lst, 3))

    # table, matches head and tail
    tbl <- table(letters)
    expect_equal(First(tbl, 3), head(tbl, 3))
    expect_equal(Last(tbl, 2), tail(tbl, 2))

    # function, matches head and tail
    expect_equal(First(colMeans, 3), head(colMeans, 3))
    expect_equal(Last(colMeans, 2), tail(colMeans, 2))

    # keep = 0
    expect_equal(First(1:10, 0), integer(0))
    expect_equal(Last(1:10, 0), integer(0))
    m <- matrix(1:20, 4)
    expect_equal(First(m, 0), structure(integer(0), .Dim = c(0L, 5L)))
    # The presence of .Dimnames in Last and not First is due to differing
    # behavior from head vs tail (not sure why)
    expect_equal(Last(m, 0), structure(integer(0), .Dim = c(0L, 5L),
                                       .Dimnames = list(NULL, NULL)))
    expect_equal(First(m, c(0, 0)), structure(integer(0), .Dim = c(0L, 0L)))
    expect_equal(Last(m, c(0, 0)), structure(integer(0), .Dim = c(0L, 0L),
                                             .Dimnames = list(NULL, NULL)))
})

test_that("Invalid keep", {
    # Invalid keep: matrix
    expect_error(First(1:10, matrix(1:20, 4)),
                 paste0("The input 'keep' needs to be an integer scalar or vector ",
                 "containing the number of entries to keep corresponding to ",
                 "the dimensions of the input data."))

    # Invalid keep: character
    expect_error(First(1:10, "3"),
                 paste0("The input 'keep' needs to be an integer scalar or vector ",
                        "containing the number of entries to keep corresponding to ",
                        "the dimensions of the input data."))

    # Invalid keep: NA
    expect_error(First(1:10, NA),
                 paste0("The input 'keep' needs to be an integer scalar or vector ",
                        "containing the number of entries to keep corresponding to ",
                        "the dimensions of the input data."))
    expect_error(First(matrix(1:20, 4), c(NA_real_, NA_real_)),
                 paste0("The input 'keep' cannot have values that are all missing."))

    # Invalid keep length
    expect_error(First(1:10, 1:2),
                 paste0("The input 'keep' is a vector with more than one ",
                        "value. It needs to be a scalar when 'x' is a vector."))
    expect_error(First(matrix(1:20, 4), 1:3),
                 paste0("The input 'keep' is a vector with length greater ",
                        "than the number of dimensions of 'x'. Its length ",
                        "needs to be less than or equal to the number of ",
                        "dimensions of 'x'."))
})

test_that("Invalid unit", {
    expect_error(First(1:10, 2, unit = "bad value"),
                 paste0("The input 'unit' needs to be one of 'Row', ",
                        "'Year', 'Quarter', 'Month', 'Week', 'Day', 'Hour', ",
                        "'Minute', 'Second'."))
    expect_error(First(matrix(1:20, 4), c(NA, 2), unit = "bad value"),
                 paste0("The input 'unit' needs to be one of 'Element', ",
                        "'Year', 'Quarter', 'Month', 'Week', 'Day', 'Hour', ",
                        "'Minute', 'Second'."))
})

test_that("Invalid calendar", {
    expect_error(First(1:10, 2, unit = "Year", calendar = 0),
                 "The input 'calendar' needs to be either TRUE or FALSE")
})

test_that("Invalid date names", {
    expect_error(First(1:10, 2, unit = "Year"),
                 paste0("The duration 'Year' cannot be applied as the input ",
                        "data is not labeled with dates."))

    expect_error(First(matrix(1:20, 4), 2, unit = "Year"),
                 paste0("The duration 'Year' cannot be applied as the input ",
                        "data is not labeled with dates."))

    x <- 1:10
    names(x) <- as.character(AsDateTime("2020-11-03") + 1:10)
    names(x)[5] <- "2020-111-08" # badly formatted date
    expect_error(First(x, 2, unit = "Year"),
                 paste0("The duration 'Year' cannot be applied as the input ",
                        "data is not labeled with valid dates."), fixed = TRUE)

    m <- matrix(1:20, 4)
    colnames(m) <- as.character(AsDateTime("2020-11-03") + 1:5)
    colnames(m)[2] <- "2020-111-04" # badly formatted date
    expect_error(First(m, c(NA, 1), unit = "Year"),
                 paste0("The duration 'Year' cannot be applied as the ",
                        "columns in the input data are not labeled with ",
                        "valid dates."), fixed = TRUE)
})

test_that("First and Last with time period", {
    x <- 1:900
    names(x) <- AsDateTime("2000-11-07 13:34:56") + (0:899) * 24 * 60 * 60

    x2 <- 1:9000
    names(x2) <- AsDateTime("2000-11-07 13:34:56") + 0:8999 * 60

    x3 <- 1:900
    names(x3) <- AsDateTime("2000-11-07 13:34:56") + 0:899

    x4 <- 1:90
    names(x4) <- paste0("2000-11-07 13:34:5", (3:92) / 10)

    # calendar year
    result <- First(x, 2, "Year")
    expect_equal(result[1], x[1])
    expect_equal(result[length(result)], c(`2001-12-31 13:34:56` = 420L))
    result <- Last(x, 2, "Year")
    expect_equal(result[1], c(`2002-01-01 13:34:56` = 421L))
    expect_equal(result[length(result)], x[length(x)])

    # non-calendar year
    result <- First(x, 2, "Year", calendar = FALSE)
    expect_equal(result[1], x[1])
    expect_equal(result[length(result)], c(`2002-11-06 13:34:56` = 730L))
    result <- Last(x, 2, "Year", calendar = FALSE)
    expect_equal(result[1], c(`2001-04-26 13:34:56` = 171L))
    expect_equal(result[length(result)], x[length(x)])

    # calendar quarter
    result <- First(x, 3, "Quarter")
    expect_equal(result[1], x[1])
    expect_equal(result[length(result)], c(`2001-06-30 13:34:56` = 236L))
    result <- Last(x, 3, "Quarter")
    expect_equal(result[1], c(`2002-10-01 13:34:56` = 694L))
    expect_equal(result[length(result)], x[length(x)])

    # non-calendar quarter
    result <- First(x, 3, "Quarter", calendar = FALSE)
    expect_equal(result[1], x[1])
    expect_equal(result[length(result)], c(`2001-08-06 13:34:56` = 273L))
    result <- Last(x, 3, "Quarter", calendar = FALSE)
    expect_equal(result[1], c(`2002-07-26 13:34:56` = 627L))
    expect_equal(result[length(result)], x[length(x)])

    # calendar month
    result <- First(x, 5, "Month")
    expect_equal(result[1], x[1])
    expect_equal(result[length(result)], c(`2001-03-31 13:34:56` = 145L))
    result <- Last(x, 5, "Month")
    expect_equal(result[1], c(`2002-12-01 13:34:56` = 755L))
    expect_equal(result[length(result)], x[length(x)])

    # non-calendar month
    result <- First(x, 5, "Month", calendar = FALSE)
    expect_equal(result[1], x[1])
    expect_equal(result[length(result)], c(`2001-04-06 13:34:56` = 151L))
    result <- Last(x, 5, "Month", calendar = FALSE)
    expect_equal(result[1], c(`2002-11-26 13:34:56` = 750L))
    expect_equal(result[length(result)], x[length(x)])

    # calendar week
    result <- First(x, 6, "Week")
    expect_equal(result[1], x[1])
    expect_equal(result[length(result)], c(`2000-12-16 13:34:56` = 40L))
    result <- Last(x, 6, "Week")
    expect_equal(result[1], c(`2003-03-16 13:34:56` = 860L))
    expect_equal(result[length(result)], x[length(x)])

    # non-calendar week
    result <- First(x, 6, "Week", calendar = FALSE)
    expect_equal(result[1], x[1])
    expect_equal(result[length(result)], c(`2000-12-18 13:34:56` = 42L))
    result <- Last(x, 6, "Week", calendar = FALSE)
    expect_equal(result[1], c(`2003-03-15 13:34:56` = 859L))
    expect_equal(result[length(result)], x[length(x)])

    # calendar day
    result <- First(x2, 2, "Day")
    expect_equal(result[1], x2[1])
    expect_equal(result[length(result)], c(`2000-11-08 23:59:56` = 2066L))
    result <- Last(x2, 2, "Day")
    expect_equal(result[1], c(`2000-11-12 00:00:56` = 6387L))
    expect_equal(result[length(result)], x2[length(x2)])

    # non-calendar day
    result <- First(x2, 2, "Day", calendar = FALSE)
    expect_equal(result[1], x2[1])
    expect_equal(result[length(result)], c(`2000-11-09 13:33:56` = 2880L))
    result <- Last(x2, 2, "Day", calendar = FALSE)
    expect_equal(result[1], c(`2000-11-11 19:34:56` = 6121L))
    expect_equal(result[length(result)], x2[length(x2)])

    # calendar hour
    result <- First(x2, 5, "Hour")
    expect_equal(result[1], x2[1])
    expect_equal(result[length(result)], c(`2000-11-07 17:59:56` = 266L))
    result <- Last(x2, 5, "Hour")
    expect_equal(result[1], c(`2000-11-13 15:00:56` = 8727L))
    expect_equal(result[length(result)], x2[length(x2)])

    # non-calendar hour
    result <- First(x2, 5, "Hour", calendar = FALSE)
    expect_equal(result[1], x2[1])
    expect_equal(result[length(result)], c(`2000-11-07 18:33:56` = 300L))
    result <- Last(x2, 5, "Hour", calendar = FALSE)
    expect_equal(result[1], c(`2000-11-13 14:34:56` = 8701L))
    expect_equal(result[length(result)], x2[length(x2)])

    # calendar minute
    result <- First(x3, 5, "Minute")
    expect_equal(result[1], x3[1])
    expect_equal(result[length(result)], c(`2000-11-07 13:38:59` = 244L))
    result <- Last(x3, 5, "Minute")
    expect_equal(result[1], c(`2000-11-07 13:45:00` = 605L))
    expect_equal(result[length(result)], x3[length(x3)])

    # non-calendar minute
    result <- First(x3, 5, "Minute", calendar = FALSE)
    expect_equal(result[1], x3[1])
    expect_equal(result[length(result)], c(`2000-11-07 13:39:55` = 300L))
    result <- Last(x3, 5, "Minute", calendar = FALSE)
    expect_equal(result[1], c(`2000-11-07 13:44:56` = 601L))
    expect_equal(result[length(result)], x3[length(x3)])

    # calendar second
    result <- First(x4, 7, "Second")
    expect_equal(result[1], x4[1])
    expect_equal(result[length(result)], c(`2000-11-07 13:34:56.9` = 67L))
    result <- Last(x4, 7, "Second")
    expect_equal(result[1], c(`2000-11-07 13:34:53` = 28L))
    expect_equal(result[length(result)], x4[length(x4)])

    # non-calendar second
    result <- First(x4, 7, "Second", calendar = FALSE)
    expect_equal(result[1], x4[1])
    expect_equal(result[length(result)], c(`2000-11-07 13:34:57.2` = 70L))
    result <- Last(x4, 7, "Second", calendar = FALSE)
    expect_equal(result[1], c(`2000-11-07 13:34:52.3` = 21))
    expect_equal(result[length(result)], x4[length(x4)])
})

test_that("First and Last with multi-dimensional time period", {
    m <- matrix(1:20, 4)
    colnames(m) <- as.character(AsDateTime("2020-11-03") + (1:5) * 24 * 60 * 60)
    result <- First(m, 2)
    expect_equal(result, head(m, 2))
    result <- Last(m, 2)
    expect_equal(result, tail(m, 2))

    result <- First(m, c(NA, 2), "Day")
    expect_equal(result, m[, 1:2])
    result <- Last(m, c(NA, 2), "Day")
    expect_equal(result, m[, 4:5])
})

test_that("First and last with time period and negative keep", {
    x <- 1:1000
    names(x) <- AsDateTime("2000-11-07 13:34:56") + (1:1000) * 24 * 60 * 60
    expect_equal(First(x, -3, unit = "Year"), First(x, 1, unit = "Year"))
})

test_that("intervalLength", {
    expect_equal(intervalLength(AsDateTime("2000-11-05"),
                                AsDateTime("2020-11-04"), unit = "Year"), 19)
    expect_equal(intervalLength(AsDateTime("2000-11-05"),
                                AsDateTime("2020-11-05"), unit = "Year"), 20)
    expect_equal(intervalLength(AsDateTime("2019-07-31"),
                                AsDateTime("2020-04-30"), unit = "Quarter"), 2)
    expect_equal(intervalLength(AsDateTime("2019-07-31"),
                                AsDateTime("2020-05-01"), unit = "Quarter"), 3)
    expect_equal(intervalLength(AsDateTime("2019-07-31"),
                                AsDateTime("2020-04-30"), unit = "Month"), 8)
    expect_equal(intervalLength(AsDateTime("2019-07-31"),
                                AsDateTime("2020-05-01"), unit = "Month"), 9)
    expect_equal(intervalLength(AsDateTime("2020-10-23"),
                                AsDateTime("2020-11-6"), unit = "Week"), 2)
    expect_equal(intervalLength(AsDateTime("2020-11-1 12:01pm"),
                                AsDateTime("2020-11-6 12:00pm"), unit = "Day"), 4)
    expect_equal(intervalLength(AsDateTime("2020-11-1 12:01pm"),
                                AsDateTime("2020-11-6 12:01pm"), unit = "Day"), 5)
    expect_equal(intervalLength(AsDateTime("2020-11-5 12:01pm"),
                                AsDateTime("2020-11-6 12:00pm"), unit = "Hour"), 23)
    expect_equal(intervalLength(AsDateTime("2020-11-6 11:01am"),
                                AsDateTime("2020-11-6 12:00:59pm"), unit = "Minute"), 59)
    expect_equal(intervalLength(AsDateTime("2020-11-6 12:00:00.1am"),
                                AsDateTime("2020-11-6 12:00:10pm"), unit = "Second"), 9)
})

test_that("periodIntegers", {
    expect_equal(periodIntegers(c(AsDateTime("2019-11-05"), AsDateTime("2020-11-04")),
                                unit = "Year", calendar = TRUE), c(49, 50))
    expect_equal(periodIntegers(c(AsDateTime("2019-11-05"), AsDateTime("2020-11-04")),
                                unit = "Year", calendar = FALSE, from.start = TRUE), c(0, 0))
    expect_equal(periodIntegers(c(AsDateTime("2019-11-05"), AsDateTime("2020-11-04")),
                                unit = "Year", calendar = FALSE, from.start = FALSE), c(0, 0))
    expect_equal(periodIntegers(c(AsDateTime("2019-11-04"), AsDateTime("2020-11-04")),
                                unit = "Year", calendar = FALSE, from.start = TRUE), c(0, 1))
    expect_equal(periodIntegers(c(AsDateTime("2019-11-04"), AsDateTime("2020-11-04")),
                                unit = "Year", calendar = FALSE, from.start = FALSE), c(-1, 0))

    expect_equal(periodIntegers(c(AsDateTime("2019-11-05"), AsDateTime("2020-11-04")),
                                unit = "Quarter", calendar = TRUE), c(199, 203))
    expect_equal(periodIntegers(c(AsDateTime("2019-11-05"), AsDateTime("2020-11-04")),
                                unit = "Quarter", calendar = FALSE, from.start = TRUE), c(0, 3))
    expect_equal(periodIntegers(c(AsDateTime("2019-11-05"), AsDateTime("2020-11-04")),
                                unit = "Quarter", calendar = FALSE, from.start = FALSE), c(-3, 0))
    expect_equal(periodIntegers(c(AsDateTime("2019-11-04"), AsDateTime("2020-11-04")),
                                unit = "Quarter", calendar = FALSE, from.start = TRUE), c(0, 4))
    expect_equal(periodIntegers(c(AsDateTime("2019-11-04"), AsDateTime("2020-11-04")),
                                unit = "Quarter", calendar = FALSE, from.start = FALSE), c(-4, 0))

    expect_equal(periodIntegers(c(AsDateTime("2019-11-05"), AsDateTime("2020-11-04")),
                                unit = "Month", calendar = TRUE), c(598, 610))
    expect_equal(periodIntegers(c(AsDateTime("2019-11-05"), AsDateTime("2020-11-04")),
                                unit = "Month", calendar = FALSE, from.start = TRUE), c(0, 11))
    expect_equal(periodIntegers(c(AsDateTime("2019-11-05"), AsDateTime("2020-11-04")),
                                unit = "Month", calendar = FALSE, from.start = FALSE), c(-11, 0))
    expect_equal(periodIntegers(c(AsDateTime("2019-11-04"), AsDateTime("2020-11-04")),
                                unit = "Month", calendar = FALSE, from.start = TRUE), c(0, 12))
    expect_equal(periodIntegers(c(AsDateTime("2019-11-04"), AsDateTime("2020-11-04")),
                                unit = "Month", calendar = FALSE, from.start = FALSE), c(-12, 0))

    expect_equal(periodIntegers(c(AsDateTime("2020-10-29"), AsDateTime("2020-11-04")),
                                unit = "Week", calendar = TRUE), c(2651, 2652))
    expect_equal(periodIntegers(c(AsDateTime("2020-10-29"), AsDateTime("2020-11-04")),
                                unit = "Week", calendar = FALSE, from.start = TRUE), c(0, 0))
    expect_equal(periodIntegers(c(AsDateTime("2020-10-29"), AsDateTime("2020-11-04")),
                                unit = "Week", calendar = FALSE, from.start = FALSE), c(0, 0))
    expect_equal(periodIntegers(c(AsDateTime("2020-10-28"), AsDateTime("2020-11-04")),
                                unit = "Week", calendar = FALSE, from.start = TRUE), c(0, 1))
    expect_equal(periodIntegers(c(AsDateTime("2020-10-28"), AsDateTime("2020-11-04")),
                                unit = "Week", calendar = FALSE, from.start = FALSE), c(-1, 0))

    expect_equal(periodIntegers(c(AsDateTime("2020-11-03 1:00pm"), AsDateTime("2020-11-04 12:00pm")),
                                unit = "Day", calendar = TRUE), c(18569, 18570))
    expect_equal(periodIntegers(c(AsDateTime("2020-11-03 1:00pm"), AsDateTime("2020-11-04 12:00pm")),
                                unit = "Day", calendar = FALSE, from.start = TRUE), c(0, 0))
    expect_equal(periodIntegers(c(AsDateTime("2020-11-03 1:00pm"), AsDateTime("2020-11-04 12:00pm")),
                                unit = "Day", calendar = FALSE, from.start = FALSE), c(0, 0))
    expect_equal(periodIntegers(c(AsDateTime("2020-11-03 12:00pm"), AsDateTime("2020-11-04 12:00pm")),
                                unit = "Day", calendar = FALSE, from.start = TRUE), c(0, 1))
    expect_equal(periodIntegers(c(AsDateTime("2020-11-03 12:00pm"), AsDateTime("2020-11-04 12:00pm")),
                                unit = "Day", calendar = FALSE, from.start = FALSE), c(-1, 0))

    expect_equal(periodIntegers(c(AsDateTime("2020-11-03 12:30pm"), AsDateTime("2020-11-03 01:29pm")),
                                unit = "Hour", calendar = TRUE), c(445668, 445669))
    expect_equal(periodIntegers(c(AsDateTime("2020-11-03 12:30pm"), AsDateTime("2020-11-03 01:29pm")),
                                unit = "Hour", calendar = FALSE, from.start = TRUE), c(0, 0))
    expect_equal(periodIntegers(c(AsDateTime("2020-11-03 12:30pm"), AsDateTime("2020-11-03 01:29pm")),
                                unit = "Hour", calendar = FALSE, from.start = FALSE), c(0, 0))
    expect_equal(periodIntegers(c(AsDateTime("2020-11-03 12:30pm"), AsDateTime("2020-11-03 01:30pm")),
                                unit = "Hour", calendar = FALSE, from.start = TRUE), c(0, 1))
    expect_equal(periodIntegers(c(AsDateTime("2020-11-03 12:30pm"), AsDateTime("2020-11-03 01:30pm")),
                                unit = "Hour", calendar = FALSE, from.start = FALSE), c(-1, 0))

    expect_equal(periodIntegers(c(AsDateTime("2020-11-03 12:00:30pm"), AsDateTime("2020-11-03 12:01:29pm")),
                                unit = "Minute", calendar = TRUE), c(26740080, 26740081))
    expect_equal(periodIntegers(c(AsDateTime("2020-11-03 12:00:30pm"), AsDateTime("2020-11-03 12:01:29pm")),
                                unit = "Minute", calendar = FALSE, from.start = TRUE), c(0, 0))
    expect_equal(periodIntegers(c(AsDateTime("2020-11-03 12:00:30pm"), AsDateTime("2020-11-03 12:01:29pm")),
                                unit = "Minute", calendar = FALSE, from.start = FALSE), c(0, 0))
    expect_equal(periodIntegers(c(AsDateTime("2020-11-03 12:00:30pm"), AsDateTime("2020-11-03 12:01:30pm")),
                                unit = "Minute", calendar = FALSE, from.start = TRUE), c(0, 1))
    expect_equal(periodIntegers(c(AsDateTime("2020-11-03 12:00:30pm"), AsDateTime("2020-11-03 12:01:30pm")),
                                unit = "Minute", calendar = FALSE, from.start = FALSE), c(-1, 0))

    expect_equal(periodIntegers(c(AsDateTime("2020-11-03 12:00:00.5pm"), AsDateTime("2020-11-03 12:00:01.4pm")),
                                unit = "Second", calendar = TRUE), c(1604404800, 1604404801))
    expect_equal(periodIntegers(c(AsDateTime("2020-11-03 12:00:00.5pm"), AsDateTime("2020-11-03 12:00:01.4pm")),
                                unit = "Second", calendar = FALSE, from.start = TRUE), c(0, 0))
    expect_equal(periodIntegers(c(AsDateTime("2020-11-03 12:00:00.5pm"), AsDateTime("2020-11-03 12:00:01.4pm")),
                                unit = "Second", calendar = FALSE, from.start = FALSE), c(0, 0))
    expect_equal(periodIntegers(c(AsDateTime("2020-11-03 12:00:00.5pm"), AsDateTime("2020-11-03 12:00:01.5pm")),
                                unit = "Second", calendar = FALSE, from.start = TRUE), c(0, 1))
    expect_equal(periodIntegers(c(AsDateTime("2020-11-03 12:00:00.5pm"), AsDateTime("2020-11-03 12:00:01.5pm")),
                                unit = "Second", calendar = FALSE, from.start = FALSE), c(-1, 0))
})

test_that("parseDateTime", {
    expect_equal(as.character(parseDateTime("6 Nov 2020 1:12:34pm", "Hour", 2, 3)),
                 "2020-11-06 13:12:34")
    expect_error(parseDateTime(NULL, "Hour", 1, 1),
                 paste0("The duration 'Hour' cannot be applied as the input ",
                        "data is not labeled with dates."))
    expect_error(parseDateTime(NULL, "Hour", 1, 2),
                 paste0("The duration 'Hour' cannot be applied as the rows ",
                        "in the input data are not labeled with dates."))
    expect_error(parseDateTime(NULL, "Hour", 2, 2),
                 paste0("The duration 'Hour' cannot be applied as the columns ",
                        "in the input data are not labeled with dates."))
    expect_error(parseDateTime(NULL, "Hour", 2, 3),
                 paste0("The duration 'Hour' cannot be applied as dimension 2 ",
                        "in the input data is not labeled with dates."))
    expect_error(parseDateTime("???", "Hour", 2, 3),
                 paste0("The duration 'Hour' cannot be applied as dimension 2 ",
                        "in the input data is not labeled with valid dates."), fixed = TRUE)
})

test_that("Q/Displayr table", {
    q.table <- structure(c(0, 19.8776758409786, 18.6544342507645, 0, 13.7614678899083,
                3.05810397553517, 0, 14.0672782874618, 2.75229357798165, 8.56269113149847,
                17.4311926605505, 2.14067278287462, 10.0917431192661, 9.48012232415902,
                4.58715596330275, 16.8195718654434, 11.6207951070336, 3.97553516819572,
                22.6299694189602, 6.11620795107034, 6.72782874617737, 12.2324159021407,
                3.36391437308868, 5.81039755351682, 29.6636085626911, 4.28134556574923,
                52.2935779816514, 100, 100, 100, 0, 65, 61, 0, 45, 10, 0, 46,
                9, 28, 57, 7, 33, 31, 15, 55, 38, 13, 74, 20, 22, 40, 11, 19,
                97, 14, 171, 327, 327, 327), .Dim = c(3L, 10L, 2L), .Dimnames = list(
                    c("Colas (e.g., Coca Cola, Pepsi Max)?", "Sparkling mineral water",
                      "Coffee"), c("Never", "Once or twice a year", "Once every 3 months",
                                   "Once a month", "Once every 2 weeks", "Once a week", "2 to 3 days a week",
                                   "4 to 5 days a week", "Every or nearly every day", "NET"),
                    c("Row %", "Count")), basedescriptiontext = "sample size = 327", basedescription = list(
                        Minimum = 327L, Maximum = 327L, Range = FALSE, Total = 327L,
                        Missing = 0L, EffectiveSampleSize = 327L, EffectiveSampleSizeProportion = 100,
                        FilteredProportion = 0), questiontypes = "PickOneMulti", span = list(
                            rows = structure(list(c("Colas (e.g., Coca Cola, Pepsi Max)?",
                                                    "Sparkling mineral water", "Coffee")),
                                             class = "data.frame", .Names = "",
                                             row.names = c(NA, 3L)),
                            columns = structure(list(c("Never", "Once or twice a year", "Once every 3 months",
                                                       "Once a month", "Once every 2 weeks", "Once a week",
                                                       "2 to 3 days a week", "4 to 5 days a week",
                                                       "Every or nearly every day", "NET")),
                                                class = "data.frame", .Names = "",
                                                row.names = c(NA, 10L))),
                name = "table.Q4.Frequency.of.drinking.cola",
                questions = c("Q4. Frequency of drinking cola", "SUMMARY"))

    first.q.table <- First(q.table, 2)
    expect_equal(first.q.table, head(q.table, 2))
    last.q.table <- Last(q.table, 2)
    expect_equal(last.q.table, tail(q.table, 2))

})

test_that("Automatic selection of rows or columns with dates", {
    m <- matrix(1:20, 4)
    m2 <- m
    colnames(m2) <- as.character(AsDateTime("2020-11-03") + (1:5) * 24 * 60 * 60)
    m3 <- m2
    rownames(m3) <- as.character(AsDateTime("2020-11-10") + (1:4) * 24 * 60 * 60)
    arr <- array(1:24, dim = c(2,3,4))
    rownames(arr) <- as.character(AsDateTime("2020-11-10") + (1:2) * 24 * 60 * 60)
    colnames(arr) <- as.character(AsDateTime("2020-11-03") + (1:3) * 24 * 60 * 60)
    dimnames(arr)[[3]] <- as.character(AsDateTime("2020-11-15") + (1:4) * 24 * 60 * 60)

    expect_equal(First(m2, 2, unit = "Day"),
                 head(m2, c(NA ,2)))
    expect_equal(Last(m2, 2, unit = "Day"),
                 tail(m2, c(NA ,2)))

    expect_error(First(m, 2, unit = "Day"),
                 paste0("The duration 'Day' cannot be applied as the input ",
                        "data is not labeled with dates."))

    expect_warning(result <- First(m3, 2, unit = "Day"),
                   paste0("Both the rows and columns of the input data are ",
                          "labeled with dates. The duration 'Day' will be ",
                          "applied to the dates in the row labels. For ",
                          "column labels instead, set the row.or.columns ",
                          "parameter to 'columns'."))
    expect_equal(result, head(m3, 2))

    expect_warning(result <- First(arr, 2, unit = "Day"),
                   paste0("Multiple dimensions of the input data are labeled ",
                          "with dates. The duration 'Day' will be applied to ",
                          "the dates in dimension 1. Use the keep parameter ",
                          "to specify a different dimension."))
})

test_that("Row and Column unit", {
    expect_equal(First(1:10, 2, "Row"), 1:2)
    expect_equal(Last(1:10, 2, "Row"), 9:10)
    expect_warning(First(1:10, 2, "Column"),
                 paste0("The unit 'Column' could not be applied as the data ",
                        "is 1-dimensional. Rows have been considered instead."))

    m <- matrix(1:20, 4)
    expect_equal(First(m, 2, "Row"), head(m, 2))
    expect_equal(Last(m, 2, "Row"), tail(m, 2))
    expect_equal(First(m, 2, "Column"), head(m, c(NA, 2)))
    expect_equal(Last(m, 2, "Column"), tail(m, c(NA, 2)))

    arr <- array(1:24, dim = c(2,3,4))
    expect_equal(First(arr, 1, "Row"), head(arr, 1))
    expect_equal(Last(arr, 1, "Row"), tail(arr, 1))
    expect_equal(First(arr, 1, "Column"), head(arr, c(NA, 1)))
    expect_equal(Last(arr, 1, "Column"), tail(arr, c(NA, 1)))
})

test_that("Statistic attribute is copied", {
    q.table.with.statistic <- structure(c(25.0764525993884, 25.6880733944954, 27.3846153846154,
                23.9263803680982, 27.6923076923077, 19.9386503067485, 0, 0.305810397553517,
                0.307692307692308, 0.613496932515337, 0, 0, 0, 0, 0, 0, 0, 0.920245398773006,
                28.7461773700306, 22.0183486238532, 22.4615384615385, 22.6993865030675,
                15.0769230769231, 23.6196319018405, 7.03363914373089, 19.8776758409786,
                15.3846153846154, 16.5644171779141, 23.6923076923077, 17.7914110429448,
                14.3730886850153, 20.4892966360856, 20.9230769230769, 19.3251533742331,
                23.6923076923077, 17.7914110429448, 5.81039755351682, 4.28134556574923,
                2.76923076923077, 2.45398773006135, 3.38461538461538, 3.68098159509202,
                4.28134556574923, 1.8348623853211, 1.53846153846154, 2.14723926380368,
                2.46153846153846, 0.920245398773006, 5.19877675840979, 2.75229357798165,
                4, 5.52147239263804, 2.15384615384615, 8.58895705521472, 2.75229357798165,
                1.52905198776758, 2.46153846153846, 4.29447852760736, 0.615384615384615,
                3.37423312883436, 3.05810397553517, 0.917431192660551, 2.15384615384615,
                2.14723926380368, 0.923076923076923, 3.37423312883436, 3.6697247706422,
                0.305810397553517, 0.615384615384615, 0.306748466257669, 0.307692307692308,
                0, 100, 100, 100, 100, 100, 100), statistic = "Row %",
              .Dim = c(6L, 13L),
              .Dimnames = list(c("Coke", "Diet Coke", "Coke Zero", "Pepsi", "Diet Pepsi", "Pepsi Max"),
                               c("Functional", "Price: Expensive", "Price: Inexpensive",
                                 "Evaluative: Positive", "Evaluative: Negative",
                                 "Other", "Reliable", "Fun", "Energising", "Sexy", "Strong", "Classic", "NET")),
              basedescriptiontext = "base n = from 325 to 327; total n = 327; 2 missing",
              basedescription = list(Minimum = 325L, Maximum = 327L, Range = TRUE, Total = 327L,
                                     Missing = 2L, EffectiveSampleSize = 327L,
                                     EffectiveSampleSizeProportion = 100, FilteredProportion = 0),
              questiontypes = "PickOneMulti",
              span = list(rows = structure(list(c("Coke", "Diet Coke", "Coke Zero", "Pepsi", "Diet Pepsi", "Pepsi Max")),
                                           class = "data.frame", .Names = "", row.names = c(NA, 6L)),
                          columns = structure(list(c("Functional", "Price: Expensive", "Price: Inexpensive",
                                                     "Evaluative: Positive", "Evaluative: Negative",
                                                     "Other", "Reliable", "Fun", "Energising", "Sexy", "Strong", "Classic", "NET")),
                                              class = "data.frame", .Names = "", row.names = c(NA, 13L))),
              name = "Q1.  Fragments coded", questions = c("Q1.  Fragments coded", "SUMMARY"))

    result <- First(q.table.with.statistic)
    expect_equal(attr(result, "statistic"), "Row %")
    result <- Last(q.table.with.statistic)
    expect_equal(attr(result, "statistic"), "Row %")
})
