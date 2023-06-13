sfile <- system.file("tests", "QTables.R", package = "verbs")
source(sfile)

test_that("Flatten 1D and 2D QTables",
{
    qtable.1D.flat <- FlattenTableAndDropStatisticsIfNecessary(qtable.1D)
    expect_identical(qtable.1D.flat, qtable.1D)

    qtable.2D.multi.VS.flat <- FlattenTableAndDropStatisticsIfNecessary(qtable.2D.multi.VS)
    expect_identical(qtable.2D.multi.VS.flat, qtable.2D.multi.VS)

    qtable.2D.multistat.flat <- FlattenTableAndDropStatisticsIfNecessary(qtable.2D.multistat)
    expect_identical(qtable.2D.multistat.flat, qtable.2D.multistat)

    expect_warning(
        qtable.2D.multistat.flat <- FlattenTableAndDropStatisticsIfNecessary(qtable.2D.multistat, TRUE),
        "Multiple statistics detected"
    )
    expect_equivalent(qtable.2D.multistat.flat, qtable.2D.multistat[, 1])
    expect_equal(names(qtable.2D.multistat.flat), rownames(qtable.2D.multistat))
    expect_equal(attr(qtable.2D.multistat.flat, "statistic"), "%")
    qtable.1D.flat <- FlattenQTable(qtable.1D)
    expect_identical(qtable.1D.flat, qtable.1D)

    qtable.2D.multi.VS.flat <- FlattenQTable(qtable.2D.multi.VS)
    expect_identical(qtable.2D.multi.VS.flat, qtable.2D.multi.VS)

    qtable.2D.multistat.flat <- FlattenQTable(qtable.2D.multistat)
    expect_identical(qtable.2D.multistat.flat, qtable.2D.multistat)

    expect_silent(
        qtable.2D.multistat.flat <- FlattenQTable(qtable.2D.multistat, drop = TRUE)
    )
    expect_equivalent(qtable.2D.multistat.flat, qtable.2D.multistat[, 1])
    expect_equal(names(qtable.2D.multistat.flat), rownames(qtable.2D.multistat))
    expect_equal(attr(qtable.2D.multistat.flat, "statistic"), "%")
})

test_that("Flatten 3D QTables",
{
    expect_warning(
        qtable.3D.nominal.multi.multistat.flat <- FlattenTableAndDropStatisticsIfNecessary(qtable.3D.nominal.multi.multistat),
        "Row %")
    expect_equivalent(qtable.3D.nominal.multi.multistat.flat,
                      qtable.3D.nominal.multi.multistat[, , 1])
    expect_equal(rownames(qtable.3D.nominal.multi.multistat.flat),
                 rownames(qtable.3D.nominal.multi.multistat))

    qtable.3D.xtab.1stat.flat <- FlattenTableAndDropStatisticsIfNecessary(qtable.3D.xtab.1stat)
    dim.target <- c(dim(qtable.3D.xtab.1stat)[1],
                    prod(dim(qtable.3D.xtab.1stat)[2:3]))
    expect_equal(dim(qtable.3D.xtab.1stat.flat), dim.target)
    expect_equal(colnames(qtable.3D.xtab.1stat.flat)[6], "Dislike - NET")
    expect_equal(qtable.3D.xtab.1stat.flat[2, 5], 42.43697, tol = 1e-5)

    expect_warning(
        qtable.3D.xtab.multistat.flat <- FlattenTableAndDropStatisticsIfNecessary(qtable.3D.xtab.multistat),
        "Multiple statistics detected in table")
    expect_equivalent(qtable.3D.xtab.multistat.flat,
                      qtable.3D.xtab.multistat[, , 1])
    expect_equivalent(colnames(qtable.3D.xtab.multistat.flat),
                 colnames(qtable.3D.xtab.multistat))
    expect_equivalent(rownames(qtable.3D.xtab.multistat.flat),
                 rownames(qtable.3D.xtab.multistat))
    expect_equal(attr(qtable.3D.xtab.multistat.flat, "name"), "table.Age.by.Gender")
    expect_silent(
        qtable.3D.nominal.multi.multistat.flat <- FlattenQTable(qtable.3D.nominal.multi.multistat, drop = TRUE)
       )
    expect_equivalent(qtable.3D.nominal.multi.multistat.flat,
                      qtable.3D.nominal.multi.multistat[, , 1])
    expect_equal(rownames(qtable.3D.nominal.multi.multistat.flat),
                 rownames(qtable.3D.nominal.multi.multistat))

    qtable.3D.xtab.1stat.flat <- FlattenQTable(qtable.3D.xtab.1stat)
    dim.target <- c(dim(qtable.3D.xtab.1stat)[1],
                    prod(dim(qtable.3D.xtab.1stat)[2:3]))
    expect_equal(dim(qtable.3D.xtab.1stat.flat), dim.target)
    expect_equal(colnames(qtable.3D.xtab.1stat.flat)[6], "Dislike - NET")
    expect_equal(qtable.3D.xtab.1stat.flat[2, 5], 42.43697, tol = 1e-5)

    expect_silent(
        qtable.3D.xtab.multistat.flat <- FlattenQTable(qtable.3D.xtab.multistat, drop = TRUE)
)
    expect_equivalent(qtable.3D.xtab.multistat.flat,
                      qtable.3D.xtab.multistat[, , 1])
    expect_equivalent(colnames(qtable.3D.xtab.multistat.flat),
                 colnames(qtable.3D.xtab.multistat))
    expect_equivalent(rownames(qtable.3D.xtab.multistat.flat),
                 rownames(qtable.3D.xtab.multistat))
    expect_equal(attr(qtable.3D.xtab.multistat.flat, "name"), "FlattenTable(table.Age.by.Gender)")
})

test_that("Flatten 4D QTables",
{
    dim.target <- c(prod(dim(qtable.4D.1stat)[c(1, 3)]),
                    prod(dim(qtable.4D.1stat)[c(2, 4)]))

    expect_silent(qtable.4D.1stat.flat <- FlattenTableAndDropStatisticsIfNecessary(qtable.4D.1stat))
    expect_equal(attr(qtable.4D.1stat.flat, "statistic"), "Average")
    expect_equal(rownames(qtable.4D.1stat.flat)[12], "'at home' - Pepsi Max")
    expect_equal(colnames(qtable.4D.1stat.flat)[1], "Hate - Coca-Cola")
    expect_equal(dim(qtable.4D.1stat.flat), dim.target)

    expect_warning(
        qtable.4D.1var.in.columns.flat <- FlattenTableAndDropStatisticsIfNecessary(qtable.4D.1var.in.columns),
        "Column %")
    dim.target <- c(dim(qtable.4D.1var.in.columns)[1],
                    prod(dim(qtable.4D.1var.in.columns)[2:3]))
    expect_equal(colnames(qtable.4D.1var.in.columns.flat)[4], "Dislike - Male")

    expect_warning(
        qtable.4D.1var.in.rows.flat <- FlattenTableAndDropStatisticsIfNecessary(qtable.4D.1var.in.rows),
        "Multiple statistics detected in table")
    dim.target <- c(prod(dim(qtable.4D.1var.in.rows)[1:2]),
                    dim(qtable.4D.1var.in.rows)[3])
    expect_equal(rownames(qtable.4D.1var.in.rows.flat)[9], "'out and about' - 65 or more")
    expect_equal(dim(qtable.4D.1var.in.rows.flat),
                 dim(qtable.4D.1var.in.rows.flat))
    expect_equal(qtable.4D.1var.in.rows.flat["'at home' - 30 to 34", "Pepsi"],
                 qtable.4D.1var.in.rows["30 to 34", "'at home'", "Pepsi", 1])

    dim.target <- c(prod(dim(qtable.4D.1stat)[c(1, 3)]),
                    prod(dim(qtable.4D.1stat)[c(2, 4)]))

    expect_silent(qtable.4D.1stat.flat <- FlattenQTable(qtable.4D.1stat))
    expect_equal(attr(qtable.4D.1stat.flat, "statistic"), "Average")
    expect_equal(rownames(qtable.4D.1stat.flat)[12], "'at home' - Pepsi Max")
    expect_equal(colnames(qtable.4D.1stat.flat)[1], "Hate - Coca-Cola")
    expect_equal(dim(qtable.4D.1stat.flat), dim.target)

    expect_silent(
        qtable.4D.1var.in.columns.flat <- FlattenQTable(qtable.4D.1var.in.columns, drop = TRUE)
    )
    dim.target <- c(dim(qtable.4D.1var.in.columns)[1],
                    prod(dim(qtable.4D.1var.in.columns)[2:3]))
    expect_equal(colnames(qtable.4D.1var.in.columns.flat)[4], "Dislike - Male")

    expect_silent(
        qtable.4D.1var.in.rows.flat <- FlattenQTable(qtable.4D.1var.in.rows, drop = TRUE)
    )
    dim.target <- c(prod(dim(qtable.4D.1var.in.rows)[1:2]),
                    dim(qtable.4D.1var.in.rows)[3])
    expect_equal(rownames(qtable.4D.1var.in.rows.flat)[9], "'out and about' - 65 or more")
    expect_equal(dim(qtable.4D.1var.in.rows.flat),
                 dim(qtable.4D.1var.in.rows.flat))
    expect_equal(qtable.4D.1var.in.rows.flat["'at home' - 30 to 34", "Pepsi"],
                 qtable.4D.1var.in.rows["30 to 34", "'at home'", "Pepsi", 1])

})

test_that("Flatten 5D QTables",
{
    dim.target <- c(prod(dim(qtable.5D)[c(1, 3)]),
              prod(dim(qtable.5D)[c(2, 4)]))
    expect_warning(
        qtable.5D.flat <- FlattenTableAndDropStatisticsIfNecessary(qtable.5D),
        "Multiple statistics detected")
    expect_equal(attr(qtable.5D.flat, "statistic"), "Average")
    expect_equal(rownames(qtable.5D.flat)[1], "'out and about' - Coke")
    expect_equal(colnames(qtable.5D.flat)[9], "Health-conscious - Diet Coke")
    expect_equal(dim(qtable.5D.flat), dim.target)
    dim.target <- c(prod(dim(qtable.5D)[c(1, 3)]),
              prod(dim(qtable.5D)[c(2, 4)]))
    expect_silent(
        qtable.5D.flat <- FlattenQTable(qtable.5D, drop = TRUE)
    )
    expect_equal(attr(qtable.5D.flat, "statistic"), "Average")
    expect_equal(rownames(qtable.5D.flat)[1], "'out and about' - Coke")
    expect_equal(colnames(qtable.5D.flat)[9], "Health-conscious - Diet Coke")
    expect_equal(dim(qtable.5D.flat), dim.target)
})


test_that("Q Tables with vector selection mode",
{
    idx <- 1:3
    out <- SelectFromTable(qtable.2D.xtab,
                    row.selection.mode = "vector",
                    row.selections = idx)
    expect_equivalent(out, qtable.2D.xtab[idx, ])
    expect_equal(attr(out, "span")$rows,
                 attr(qtable.2D.xtab, "span")$rows[idx, , drop = FALSE])

    idx <- c(2, 8, 5)
    out <- SelectFromTable(qtable.2D.xtab,
                    column.selection.mode = "vector",
                    column.selections = idx)
    expect_equivalent(out, qtable.2D.xtab[, idx])
    expect_equal(attr(out, "span")$columns,
                 attr(qtable.2D.xtab, "span")$columns[idx, , drop = FALSE])

    ridx <- c("18 to 24", "SUM")
    expect_warning(out <- SelectFromTable(qtable.2D.xtab,
                    row.selection.mode = "vector",
                    row.selections = ridx), "ignored: SUM")
    expect_equivalent(out, qtable.2D.xtab[ridx[1], ])
    expect_equal(nrow(out), 1)
    expect_equivalent(attr(out, "span")$rows, "18 to 24")

    ## bad indices ignored, duplicates removed
    idx <- c(-1, 2, 5, 11, 5)
    msg.expect <- paste0("Numeric selections need to be positive integers ",
                         "less than or equal to the number of rows in the ",
                         "table (10). The following selections will be ",
                         "ignored: -1, 11.")
    expect_warning(out <- SelectFromTable(qtable.2D.xtab,
                    row.selection.mode = "vector",
                    row.selections = idx), msg.expect, fixed = TRUE)
    good.idx <- unique(idx[idx > 0 & idx <= nrow(qtable.2D.xtab)])
    expect_equivalent(out, qtable.2D.xtab[good.idx, ])
    expect_equal(nrow(out), length(good.idx))

    expect_error(SelectFromTable(qtable.2D.xtab,
                    column.selection.mode = "vector",
                    column.selections = c(0, 1e4)),
                 "The supplied numeric selections are not valid.")

    ridx <- c("Pepsi", "Diet Pepsi", "Pepsi Max")
    cidx <- c("Older", "Open to new experiences")
    out <- SelectFromTable(qtable.2D.multi.VS,
                    row.selection.mode = "vector",
                    row.selections = ridx,
                    column.selections = cidx)
    expect_equivalent(out, qtable.2D.multi.VS[ridx, cidx])
    expect_equal(dim(out), c(length(ridx), length(cidx)))
    expect_equal(rownames(out), ridx)
    expect_equal(colnames(out), cidx)

    ## logical
    expect_error(SelectFromTable(qtable.1D,
                                            row.selections = logical(11)),
                 "logical selections are not valid")
    expect_error(SelectFromTable(qtable.1D,
                                            row.selections = logical(10)),
                 "output contains no rows")
    idx <- rep(c(TRUE, FALSE), length.out = nrow(qtable.1D))
    expect_equivalent(SelectFromTable(qtable.1D,
                                      row.selections = idx), qtable.1D[idx])

    x <- qtable.2D.xtab
    expect_error(SelectFromTable(x,
                                            column.selections = logical(ncol(x) + 2)),
                 "logical selections are not valid")
    expect_error(SelectFromTable(x,
                                            column.selections = logical(ncol(x))),
                 "output contains no columns")
    idx <- rep(c(TRUE, FALSE), length.out = ncol(x))
    expect_equivalent(SelectFromTable(x,
                                      column.selection.mode = "vector",
                                      column.selections = idx), x[, idx])

})

test_that("DS-3300: Select with named vector",
{
    x <- seq(.5, 2.5, length.out = 5)
    names(x) <- letters[seq_along(x)]
    expect_equal(SelectFromTable(x, row.selections = 2:3),
                 x[2:3], check.attributes = FALSE)
    expect_equal(SelectFromTable(x, row.selections = c("b", "c")),
                 x[2:3], check.attributes = FALSE)
    expect_equal(SelectFromTable(x, row.selections = c(F, T, T, F, F)),
                 x[2:3], check.attributes = FALSE)

    expect_equal(SelectFromTable(x, row.selection.mode = "Range",
                                 row.selections = "3-5"),
                 x[3:5], check.attributes = FALSE)
    expect_equal(SelectFromTable(x, row.selection.mode = "First rows",
                                 row.selections = 2),
                 x[1:2], check.attributes = FALSE)
    expect_equal(SelectFromTable(x, row.selection.mode = "Last rows",
                                 row.selections = 3),
                 x[3:5], check.attributes = FALSE)

    names(x) <- seq(as.Date("2020-12-01"), as.Date("2021-03-01"),
                    length.out = length(x))
    expect_equal(SelectFromTable(x, row.selection.mode = "First date-time periods",
                                 row.selections = 1, unit = "Month"),
                 x[1:2], check.attributes = FALSE)
})

test_that("Select with data.frame",
{
    x <- data.frame(x = 1:10, y = 10:1, z = letters[1:10])
    cidx <- c("z", "x")
    ridx <- 1:3
    out <- SelectFromTable(x, row.selections = ridx,
                           column.selections = cidx)
    expect_equivalent(out, x[ridx, cidx])
})

test_that("Selection with higher dimensional Q Tables",
{
    qtable.flat <- FlattenQTable(qtable.4D.1stat)

    ridx <- grep("at home", rownames(qtable.flat), value = TRUE)
    cidx <- grep("[dD]islike", colnames(qtable.flat))
    out <- SelectFromTable(qtable.4D.1stat, row.selections = ridx,
                           column.selections = cidx)
    expect_equivalent(out, qtable.flat[ridx, cidx])

    qtable.flat <- FlattenQTable(qtable.3D.banner.in.rows)
    cidx <- c(TRUE, TRUE, FALSE)
    select.model <- "Honda"
    ridx <- grepl(select.model, rownames(qtable.flat))
    out <- SelectFromTable(qtable.3D.banner.in.rows, row.selections = ridx,
                           column.selections = cidx)
    expect_equal(dim(out), c(sum(ridx), sum(cidx)))
    expect_equivalent(out, qtable.3D.banner.in.rows[, select.model, cidx])
    span <- attr(qtable.3D.banner.in.rows, "span")$rows
    rnames.expect <- paste0(select.model, " - ",
                            apply(span, 1, paste0, collapse = " - "))
    expect_equal(rownames(out), rnames.expect)

    expect_silent(qtable.flat <- FlattenQTable(qtable.4D.banner.in.rows, drop = TRUE))
    selected.drink <- "Pepsi"
    ridx <- grep(selected.drink, rownames(qtable.flat), value = TRUE)
    expect_warning(out <- SelectFromTable(qtable.4D.banner.in.rows,
                                          row.selections = ridx),
                   "Column %", fixed = TRUE)
    expect_equivalent(out, qtable.flat[ridx, ])
    span <- attr(qtable.4D.banner.in.rows, "span")$rows
    rnames.expect <- paste0(selected.drink, " - ",
                            apply(span, 1, paste0, collapse = " - "))
    expect_equal(rownames(out), rnames.expect)
    expect_equal(colnames(out), dimnames(qtable.4D.banner.in.rows)[[3]])
})

test_that("Q Tables with Last and First rows",
{
    out <- SelectFromTable(qtable.2D.multi.VS,
                    row.selection.mode = "First rows",
                    row.selections = 3)
    expect_equivalent(out, qtable.2D.multi.VS[1:3, ])
    expect_equivalent(out, First(qtable.2D.multi.VS, 3))
    expect_equal(attr(out, "statistic"), "%")

    out <- SelectFromTable(qtable.2D.multi.VS,
                    row.selection.mode = "Last rows",
                    row.selections = 2)
    n <- nrow(qtable.2D.multi.VS)
    expect_equivalent(out, qtable.2D.multi.VS[(n-1):n, ])
    expect_equivalent(out, Last(qtable.2D.multi.VS, 2))
    expect_equal(attr(out, "questiontypes"), "PickAnyGrid")
})

test_that("Q Tables with Last and First columns",
{
    out <- SelectFromTable(qtable.2D.xtab,
                    column.selection.mode = "first columns",
                    column.selections = 3)
    expect_equivalent(out, qtable.2D.xtab[, 1:3])
    expect_equivalent(out, First(qtable.2D.xtab, 3, unit = "Column"))
    expect_equal(attr(out, "statistic"), "Column %")

    out <- SelectFromTable(qtable.2D.xtab,
                    column.selection.mode = "last columns",
                    column.selections = 1)
    n <- ncol(qtable.2D.xtab)
    expect_equivalent(out, qtable.2D.xtab[, n])
    expect_equivalent(out, Last(qtable.2D.xtab, 1, unit = "Column"))
    expect_equal(colnames(out), "$200,001 or more")


    expect_warning(out <- SelectFromTable(qtable.3D.nominal.multi.multistat,
                           row.selection.mode = "Last rows",
                           row.selections = 5,
                           column.selection.mode = "First columns",
                           column.selections = 2), "Row %")
    n <- nrow(qtable.3D.nominal.multi.multistat)
    expect_equivalent(out, qtable.3D.nominal.multi.multistat[(n-4):n, 1:2, 1])
    expect_equivalent(attr(out, "span")$rows,
                      attr(qtable.3D.nominal.multi.multistat,
                           "span")$rows[(n-4):n, , drop = FALSE])
})

test_that("last and first date selection mode",
{
    dates <- seq(as.Date("2000/1/15"), as.Date("2001/01/15"), length.out = 14)
    x <- matrix(seq_len(length(dates)*5), length(dates), 5)
    colnames(x) <- LETTERS[1:ncol(x)]
    date.fmt <- "%Y/%m/%d"
    rownames(x) <- format(dates, date.fmt)
    out <- SelectFromTable(x, row.selection.mode = "first date-time periods",
                           row.selections = 1, unit = "Quarter", calendar = TRUE)
    expect_equal(dim(out), c(3, ncol(x)))
    expect_equivalent(out, First(x, keep = 1, unit = "Quarter"))
    expect_equal(rownames(out), format(dates[1:3], date.fmt))

    out <- SelectFromTable(x, row.selection.mode = "first date-time periods",
                           row.selections = 1, unit = "Quarter", calendar = FALSE)
    expect_equal(dim(out), c(4, ncol(x)))
    expect_equal(out, First(x, keep = 1, unit = "Quarter", calendar = FALSE))

    out <- SelectFromTable(x, row.selection.mode = "first date-time periods",
                           row.selections = 1, unit = "Year", calendar = FALSE)
    expect_equal(dim(out), c(nrow(x) - 1, ncol(x)))


    out <- SelectFromTable(x, row.selection.mode = "last date-time periods",
                           row.selections = 1, unit = "Year", calendar = FALSE)
    expect_equal(out, Last(x, keep = 1, unit = "Year", calendar = FALSE))
    out <- SelectFromTable(x, row.selection.mode = "last date-time periods",
                           row.selections = 1, unit = "Year", calendar = TRUE)
    expect_equal(dim(out), c(1, ncol(x)))

    x <- t(x)
    out <- SelectFromTable(x, column.selection.mode = "first date-time periods",
                           column.selections = 3, unit = "Month")
    expect_equal(dim(out), c(nrow(x), 3))
    out <- SelectFromTable(x, column.selection.mode = "first date-time periods",
                           column.selections = 3, unit = "Month", calendar = FALSE)
    expect_equal(dim(out), c(nrow(x), 4))

    out <- SelectFromTable(x, column.selection.mode = "last date-time periods",
                           column.selections = 5, unit = "Week")
    expect_equal(dim(out), c(nrow(x), 2))
    expect_equal(out, Last(out, keep = 5, unit = "Week"))

    row.keep <- c(2, 4)
    out <- SelectFromTable(x, row.selection.mode = "vector",
                           row.selections = LETTERS[row.keep],
                           column.selection.mode = "last date-time periods",
                           column.selections = 30, unit = "Day", calendar = FALSE)
    expect_equal(dim(out), c(2, 2))
    expect_equivalent(out, Last(x, keep = 30, unit = "Day",
                                calendar = FALSE)[row.keep, ])
})

test_that("range selection mode",
{
    x <- matrix(0, 26, 26)
    colnames(x) <- LETTERS
    rownames(x) <- letters
    out <- SelectFromTable(x, row.selection.mode = "range",
                           row.selections = "1,3,24-26")
    expect_equal(dim(out), c(5, 26))
    expect_equal(rownames(out), c("a", "c", "x", "y", "z"))


    out <- SelectFromTable(x, column.selection.mode = "range",
                           column.selections = "5-8")
    expect_equal(dim(out), c(26, 4))
    expect_equal(colnames(out), c("E", "F", "G", "H"))

    out <- SelectFromTable(x, row.selection.mode = "range",
                           row.selections = "1-3,21",
                           column.selection.mode = "range",
                           column.selections = "13")
    expect_equal(dim(out), c(4, 1))
    expect_equal(colnames(out), "M")
    expect_equal(rownames(out), letters[c(1:3, 21)])

    out <- SelectFromTable(x, row.selection.mode = "range", row.selections = "2-")
    expect_equal(rownames(out), rownames(x)[-1])

    ## order matters
    idx <- "5-6,2,10-8"
    out <- SelectFromTable(x, row.selection.mode = "range",
                           row.selections = idx)
    expect_equal(rownames(out), letters[c(5, 6, 2, 10, 9, 8)])

    ## bad indices removed, duplicates ignored
    idx <- "0-3,2,2-4"
    expect_warning(out <- SelectFromTable(qtable.2D.multi.VS,
                                          row.selection.mode = "range",
                                          row.selections = idx),
                   "ignored: 0.")
    expect_equivalent(out, qtable.2D.multi.VS[1:4, ])

    expect_warning(out <- SelectFromTable(qtable.2D.multi.VS,
                                          column.selection.mode = "range",
                                          column.selections = "8-11"),
                   "ignored: 11.")
    expect_equivalent(out, Last(qtable.2D.multi.VS, 3, unit = "Column"))
})

test_that("date range selection mode",
{
    dates <- seq(as.Date("2000/1/1"), as.Date("2003/1/1"), by = "quarter")
    x <- matrix(seq_len(65), 13, 5)
    colnames(x) <- LETTERS[1:ncol(x)]
    rownames(x) <- format(dates, "%Y-%m-%d")
    out <- SelectFromTable(x, row.selection.mode = "date range",
                           row.selections = "2000-01-01--2000-12-31")
    expect_equal(dim(out), c(4, ncol(x)))
    expect_equivalent(out, First(x, keep = 1, unit = "Year"))
    expect_equivalent(out, First(x, keep = 4, unit = "Quarter"))
    expect_equal(rownames(out), format(dates[1:4], "%Y-%m-%d"))

    dates <- seq(as.Date("2000/1/6"), as.Date("2001/1/15"), length.out = 13)
    x <- matrix(seq_len(52), 4, 13)
    rownames(x) <- LETTERS[1:nrow(x)]
    date.fmt <- "%d/%m/%Y"
    colnames(x) <- format(dates, date.fmt)
    out <- SelectFromTable(x, column.selection.mode = "date range",
                           column.selections = "01/15/2000--11/15/2000")
    ncol.expect <- sum(dates >= as.Date("2000/01/15") & dates <= as.Date("2000/11/15"))
    expect_equal(dim(out), c(4, ncol.expect))
    expect_equal(colnames(out), format(dates[2:11], date.fmt))
})


test_that("checkSelections error handling",
{
    table <- matrix(1:4, 2, 2, dimnames = list(letters[1:2], LETTERS[1:2]))
    expect_error(checkSelections(list("a", "b"), table, 1),
                 "Supplied format for selections is not valid.")
    expect_error(checkSelections(c("foo", "bar"), table, 2),
                 "No valid selections were found in the column labels.")
})

test_that("DS-3886 DropMultipleStatistics", {
    .addQTableClass <- function(x) {
        class(x) <- c("qTable", class(x))
        x
    }
    q3d.multi <- .addQTableClass(qtable.3D.xtab.multistat)
    expected.warning <- paste0("Multiple statistics detected in table, only the first, ",
                               sQuote("Column %"), ", will be shown.")
    expect_warning(output <- DropMultipleStatisticsFromTable(q3d.multi),
                   expected.warning, fixed = TRUE)
    expect_silent(no.wan.out <- DropMultipleStatisticsFromTable(q3d.multi, warn = FALSE))
    expected.output <- qtable.3D.xtab.multistat[, , 1, drop = TRUE]
    # Elements the same, along with dim and dimnames
    output <- unclass(output)
    output <- output[, ]
    expect_equal(output, expected.output)
    expect_equal(dimnames(output), dimnames(qtable.3D.xtab.multistat)[-3])
    # Move the stat to 2nd dim
    q3d.multi.in.middle <- q3d.multi[, , c(2, 1), drop = FALSE]
    # Lower dim tables are not affected
    expect_equal(DropMultipleStatisticsFromTable(qtable.2D.multistat),
                 qtable.2D.multistat)
    # unless forced
    expected.warning <- gsub("Column ", "", expected.warning)
    expect_warning(output <- DropMultipleStatisticsFromTable(qtable.2D.multistat,
                                                             drop.stats.from.2d.table = TRUE),
                   expected.warning, fixed = TRUE)
    expect_equal(output[TRUE], as.array(qtable.2D.multistat[, 1]))
})
