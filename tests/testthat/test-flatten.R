context("Test flattening of tables")

test_that("DS-3920 Correct dims identified for flattening", {
    expect_error(determineFlatteningRowAndColVars(),
                 "Need question types to resolve row and column variables for flattening")
    expect_error(determineFlatteningRowAndColVars("foo", n.dim = "hi"),
                 "n.dim should be a single integer")
    expect_error(determineFlatteningRowAndColVars("foo", n.dim = 0L),
                 "Flattening only supported for 3D and 4D tables")
    expect_error(determineFlatteningRowAndColVars("foo", n.dim = 2L),
                 "Flattening only supported for 3D and 4D tables")
    all.question.types <- c("Number", "PickOne", "Date", "NumberMulti", "PickOneMulti",
                            "PickAny", "Ranking", "PickAnyCompact", "NumberGrid", "PickAnyGrid")
    multi.grid.types <- c("PickOneMulti", "PickAnyGrid", "NumberGrid")
    for (row.qtype in all.question.types)
        for (col.qtype in all.question.types) {
            row.in.grid.types <- row.qtype %in% multi.grid.types
            expected.row <- if (row.in.grid.types) 1L else 2:1
            expected.col <- if (row.in.grid.types) 2:3 else 3L
            expected.3d.list <- list(row.vars = expected.row, col.vars = expected.col)
            expected.4d.list <- list(row.vars = c(3L, 1L), col.vars = c(2L, 4L))
            qtypes <- c(row.qtype, col.qtype)
            expect_equal(determineFlatteningRowAndColVars(qtypes, n.dim = 3L),
                         expected.3d.list)
            expect_equal(determineFlatteningRowAndColVars(qtypes, n.dim = 4L),
                         expected.4d.list)
        }
})

test_that("DS-3920 Flattening dimnames works as intended", {
    # 1d case
    input <- list(LETTERS[1:3])
    expected.out <- input[[1L]]
    expect_equal(createFlattenedNames(input), expected.out)
    input <- list(letters[1:2], LETTERS[1:3])
    expected.out <- c("a - A", "a - B", "a - C", "b - A", "b - B", "b - C")
    expect_equal(createFlattenedNames(input), expected.out)
})

test_that("DS-3920 Adding Row and Column references to QStatisticsTestingInfo", {
    dimnames <- list(LETTERS[1:3], letters[1:2])
    fake.df <- data.frame(foo = rnorm(6), bar = rnorm(6))
    expected.df <- data.frame(
        Row = c("A", "A", "B", "B", "C", "C"),
        Column = c("a", "b", "a", "b", "a", "b"),
        foo = fake.df[["foo"]],
        bar = fake.df[["bar"]],
        stringsAsFactors = TRUE
    )
    expect_equal(updateFlattenedDimensionsToQStatInfo(fake.df, dimnames),
                 expected.df)
})

test_that("DS-3920 Update QuestionTypes attribute", {
    all.question.types <- c("Number", "PickOne", "Date", "NumberMulti", "PickOneMulti",
                            "PickAny", "Ranking", "PickAnyCompact", "NumberGrid", "PickAnyGrid")
    expect_equal(updateFlattenedQuestionTypes(c("Number", "NumberGrid")),
                 c("Number", "NumberMulti"))
    expect_equal(updateFlattenedQuestionTypes(c("Number", "NumberMulti")),
                 c("Number", "NumberMulti"))
    expect_equal(updateFlattenedQuestionTypes(c("PickAnyGrid", "PickOne")),
                 c("PickAny", "PickOne"))
    expect_equal(updateFlattenedQuestionTypes(c("PickAnyGrid", "NumberGrid")),
                 c("PickAny", "NumberMulti"))
})

test_that("DS-3920 Update dimnames with span information", {
    x <- LETTERS[1:3]
    span <- data.frame(LETTERS[1:3], fix.empty.names = FALSE)
    wrong.span <- data.frame(LETTERS[1:3], fix.empty.names = FALSE)
    expect_equal(joinSpansToNames(x, span), x)
    expect_equal(joinSpansToNames(x, wrong.span), x)
    valid.span <- data.frame(c("foo", "foo", "bar"), LETTERS[1:3], fix.empty.names = FALSE)
    expect_equal(joinSpansToNames(x, valid.span),
                 c("foo - A", "foo - B", "bar - C"))
    valid.span <- data.frame(c("foo", NA_character_, "bar"), LETTERS[1:3], fix.empty.names = FALSE)
    expect_equal(joinSpansToNames(x, valid.span),
                 c("foo - A", "B", "bar - C"))
    valid.span <- data.frame(
        c("fizz", "buzz", NA_character_),
        c("foo", NA_character_, "bar"),
        LETTERS[1:3],
        fix.empty.names = FALSE
    )
    expect_equal(joinSpansToNames(x, valid.span),
                 c("fizz - foo - A", "buzz - B", "bar - C"))
})

#test.token <- Sys.getenv("TEST_TOKEN", unset = NA)
#
#if (!is.na(test.token) && requireNamespace("flipAPI", quietly = TRUE)) {
#    region <- "app"
#    `1234.verbs.tests` <- "verbs-tests"
#    unlockBinding("clientId", envir = getNamespace("flipAPI"))
#    assign("clientId", "1234", envir = getNamespace("flipAPI"))
#    lockBinding("clientId", envir = getNamespace("flipAPI"))
#    tbls <- flipAPI::QLoadData("test-tables.rds", company.token = test.token)
#    table.names <- vapply(tbls, attr, character(1L), "name")
#    tbls <- setNames(tbls, table.names)
#}
tbls <- readRDS("flatten-tests.rds")
table.names <- vapply(tbls, attr, character(1L), "name")
tbls <- setNames(tbls, table.names)

test_that("DS-4716 1d tables", {
#    skip_if(is.na(test.token))
    pure1D <- function(x) getDimensionLength(x) == 1L
    tbls.1d <- Filter(pure1D, tbls)
    for (tbl in tbls.1d) {
        expected.tbl <- tbl
        x.span <- attr(tbl, "span")[[1L]]
        if (NCOL(x.span) > 1L) {
            new.names <- joinSpansToNames(dimnames(tbl)[[1L]], x.span)
            dimnames(expected.tbl) <- list(new.names)
            attr(expected.tbl, "span") <- NULL
        }
        expect_equal(FlattenQTable(tbl), expected.tbl)
        expect_equal(FlattenQTable(tbl, drop = TRUE), expected.tbl)
    }
})

test_that("DS-4716 2d tables", {
#    skip_if(is.na(test.token))
    pure2D <- function(x) getDimensionLength(x) == 2L && !isMultiStatTable(x)
    tbls.2d <- Filter(pure2D, tbls)
    for (tbl in tbls.2d) {
        expected.tbl <- tbl
        x.span <- attr(tbl, "span")
        if (any(vapply(x.span, NCOL, 1L) > 1L)) {
            new.names <- mapply(joinSpansToNames, dimnames(tbl), x.span, SIMPLIFY = FALSE)
            dimnames(expected.tbl) <- new.names
            attr(expected.tbl, "span") <- NULL
        }
        expect_equal(FlattenQTable(tbl), expected.tbl)
    }
})

test_that("DS-4716 1d multi stat (2d)", {
#    skip_if(is.na(test.token))
    multistat1D <- function(x) getDimensionLength(x) == 2L && isMultiStatTable(x)
    tbls.1d <- Filter(multistat1D, tbls)
    for (tbl in tbls.1d) {
        expect_equal(FlattenQTable(tbl), tbl)
    }
})

test_that("DS-4716 Handle spans for flattened tables", {
    # 3d case, binary grid x nominal, both have spans, combine is row = 1, col = 2:3
    tbl <- tbls[["table.binary.grid.with.spans.by.nominal.with.spans"]]
    original.dimnames <- list(
        c("Lunch", "Dinner", "NET"),
        c("Pepsi", "Cola", "Soda", "NET"),
        c(LETTERS[1:3], "NET")
    )
    original.span <- list(
        rows = data.frame(
                c("Meal", "Meal", NA_character_),
                c("Lunch", "Dinner", "NET"),
                fix.empty.names = FALSE
            ),
        columns = data.frame(
                c("PepsiCo", "PepsiCo", NA_character_, NA_character_),
                c("Pepsi", "Cola", "Soda", "NET"),
                fix.empty.names = FALSE
            )
    )
    expect_equal(dimnames(tbl), original.dimnames)
    expect_equal(attr(tbl, "span"), original.span)
    flattened.tbl <- FlattenQTable(tbl)
    expected.flattened.dimnames <- list(
        c("Meal - Lunch", "Meal - Dinner", "NET"),
        paste0(
            rep(c("PepsiCo - Pepsi", "PepsiCo - Cola", "Soda", "NET"), each = 4L),
            " - ",
            rep(c(LETTERS[1:3], "NET"), 4L)
        )
    )
    expect_equal(dimnames(flattened.tbl), expected.flattened.dimnames)
    # 3d case, nominal multi x numeric multi, both with spans, combine settings =
    tbl <- tbls[["table.nominal.with.spans.by.binary.grid.with.spans"]]
    original.dimnames <- list(
        c("A", "B", "C", "NET"),
        c("Lunch", "Dinner", "NET"),
        c("Pepsi", "Cola", "Soda", "NET")
    )
    original.span <- list(
        rows = data.frame(
                c(NA_character_, "Last", "Last", NA_character_),
                c(LETTERS[1:3], "NET"),
                fix.empty.names = FALSE
            ),
        columns = data.frame(
                c("Meal", "Meal", NA_character_),
                c("Lunch", "Dinner", "NET"),
                fix.empty.names = FALSE
            )
    )
    expect_equal(dimnames(tbl), original.dimnames)
    expect_equal(attr(tbl, "span"), original.span)
    flattened.tbl <- FlattenQTable(tbl)
    expected.flattened.dimnames <- list(
        paste0(
            rep(c("Meal - Lunch", "Meal - Dinner", "NET"), each = 4L),
            " - ",
            rep(c("A", "Last - B", "Last - C", "NET"), 3L)
        ),
        c("Pepsi", "Cola", "Soda", "NET")
    )
    expect_equal(dimnames(flattened.tbl), expected.flattened.dimnames)
})

test_that("DS-4716 5d (2dx2d with multi stat)", {
#    skip_if(is.na(test.token))
    x <- tbls[["table.numeric.grid.by.binary.grid.2"]]
    output <- FlattenQTable(x)
    dropped.output <- FlattenQTable(x, drop = TRUE)
    expect_equal(getDimensionLength(output), 3L)
    expect_equal(getDimensionLength(dropped.output), 2L)
    int.subscript <- output[2, 2, 2]
    chr.subscript <- output["Lunch - carrots", "Apple - Cola", "Standard Deviation"]
    expect_equal(as.vector(int.subscript), as.vector(chr.subscript))
    expect_equal(as.vector(int.subscript), 0.884803051724127)
    int.attributes <- attributes(int.subscript)
    chr.attributes <- attributes(chr.subscript)
    expect_equal(int.attributes[["QStatisticsTestingInfo"]],
                 chr.attributes[["QStatisticsTestingInfo"]])
    expect_equal(nrow(int.attributes[["QStatisticsTestingInfo"]]), 1L)
    expect_equal(int.attributes[["QStatisticsTestingInfo"]][["zstatistic"]],
                 0.323765451147663)
    int.subscript <- output[4, 5, 1]
    chr.subscript <- output["Dinner - peas", "Orange - Pepsi", "Average"]
    expect_equal(as.vector(int.subscript), as.vector(chr.subscript))
    expect_equal(as.vector(int.subscript), 91.505170506274)
    int.attributes <- attributes(int.subscript)
    chr.attributes <- attributes(chr.subscript)
    expect_equal(int.attributes[["QStatisticsTestingInfo"]],
                 chr.attributes[["QStatisticsTestingInfo"]])
    expect_equal(nrow(int.attributes[["QStatisticsTestingInfo"]]), 1L)
    expect_equal(int.attributes[["QStatisticsTestingInfo"]][["zstatistic"]],
                 -0.528241151116461)
    int.dropped.subscript <- dropped.output[4, 5]
    expect_equal(as.vector(int.subscript), as.vector(int.dropped.subscript))
    expect_equal(attr(dropped.output, "statistic"), "Average")
    expect_null(attr(output, "statistic"))
    expect_equal(dimnames(dropped.output), dimnames(output)[-3L])
    expected.names <- list(
        paste0(rep(c("Lunch", "Dinner", "NET"), each = 3L),
               " - ",
               rep(c("peas", "carrots", "SUM"), 3L)),
        paste0(rep(c("Apple", "Orange", "Banana", "SUM"), each = 4L),
               " - ",
               rep(c("Pepsi", "Cola", "Soda", "NET"), 4L)),
        c("Average", "Standard Deviation")
    )
    expect_equal(dimnames(output), expected.names)
    expect_false(any(c("Row", "Column") %in% names(attr(x, "QStatisticsTestingInfo"))))
    expect_equal(attr(output, "QStatisticsTestingInfo"),
                 attr(dropped.output, "QStatisticsTestingInfo"))
    expect_equal(names(attr(output, "QStatisticsTestingInfo"))[1:2], c("Row", "Column"))
    x <- tbls[["table.binary.grid.by.nominal.multi.2"]]
    output <- FlattenQTable(x)
    dropped.output <- FlattenQTable(x, drop = TRUE)
    expect_equal(getDimensionLength(output), 3L)
    expect_equal(getDimensionLength(dropped.output), 2L)
    int.subscript <- output[8, 10, 2]
    chr.subscript <- output["Cheese - Dinner", "Soda - Often", "Row %"]
    expect_equal(as.vector(int.subscript), as.vector(chr.subscript))
    expect_equal(as.vector(int.subscript), 50)
    int.attributes <- attributes(int.subscript)
    chr.attributes <- attributes(chr.subscript)
    expect_equal(int.attributes[["QStatisticsTestingInfo"]],
                 chr.attributes[["QStatisticsTestingInfo"]])
    expect_equal(nrow(int.attributes[["QStatisticsTestingInfo"]]), 1L)
    expect_equal(int.attributes[["QStatisticsTestingInfo"]][["zstatistic"]],
                 0.400320384512718)
    int.subscript <- output[5, 7, 1]
    chr.subscript <- output["Milk - Dinner", "Cola - Sometimes", "Column %"]
    expect_equal(as.vector(int.subscript), as.vector(chr.subscript))
    expect_equal(as.vector(int.subscript), 31.8181818181818)
    int.attributes <- attributes(int.subscript)
    chr.attributes <- attributes(chr.subscript)
    expect_equal(int.attributes[["QStatisticsTestingInfo"]],
                 chr.attributes[["QStatisticsTestingInfo"]])
    expect_equal(nrow(int.attributes[["QStatisticsTestingInfo"]]), 1L)
    expect_equal(int.attributes[["QStatisticsTestingInfo"]][["zstatistic"]],
                 0.451622302330946)
    int.dropped.subscript <- dropped.output[5, 7]
    expect_equal(as.vector(int.subscript), as.vector(int.dropped.subscript))
    expect_equal(attr(dropped.output, "statistic"), "Column %")
    expect_null(attr(output, "statistic"))
    expect_equal(dimnames(dropped.output), dimnames(output)[-3L])
    expected.names <- list(
        paste0(rep(c("Bread", "Milk", "Cheese"), each = 3L),
               " - ",
               rep(c("Lunch", "Dinner", "NET"), 3L)),
        paste0(rep(c("Pepsi", "Cola", "Soda", "NET"), each = 4L),
               " - ",
               rep(c("Never", "Often", "Sometimes", "NET"), 4L)),
        c("Column %", "Row %")
    )
    expect_equal(dimnames(output), expected.names)
    expect_equal(attr(output, "QStatisticsTestingInfo"),
                 attr(dropped.output, "QStatisticsTestingInfo"))
    expect_false(any(c("Row", "Column") %in% names(attr(x, "QStatisticsTestingInfo"))))
    expect_equal(names(attr(output, "QStatisticsTestingInfo"))[1:2], c("Row", "Column"))
})
