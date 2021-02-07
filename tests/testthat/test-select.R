sfile <- system.file("tests", "QTables.R", package = "verbs")
source(sfile)

test_that("1D and 2D QTables",
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
})

test_that("3D QTables",
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
    expect_equal(colnames(qtable.3D.xtab.1stat.flat)[6], "Dislike: NET")
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
})

test_that("4D QTables",
{
    dim.target <- c(prod(dim(qtable.4D.1stat)[c(1, 3)]),
                    prod(dim(qtable.4D.1stat)[c(2, 4)]))

    expect_silent(qtable.4D.1stat.flat <- FlattenTableAndDropStatisticsIfNecessary(qtable.4D.1stat))
    expect_equal(attr(qtable.4D.1stat.flat, "statistic"), "Average")
    expect_equal(rownames(qtable.4D.1stat.flat)[12], "'at home': Pepsi Max")
    expect_equal(colnames(qtable.4D.1stat.flat)[1], "Hate: Coca-Cola")
    expect_equal(dim(qtable.4D.1stat.flat), dim.target)

    expect_warning(
        qtable.4D.1var.in.columns.flat <- FlattenTableAndDropStatisticsIfNecessary(qtable.4D.1var.in.columns),
        "Column %")
    dim.target <- c(dim(qtable.4D.1var.in.columns)[1],
                    prod(dim(qtable.4D.1var.in.columns)[2:3]))
    expect_equal(colnames(qtable.4D.1var.in.columns.flat)[4], "Dislike: Male")

    expect_warning(
        qtable.4D.1var.in.rows.flat <- FlattenTableAndDropStatisticsIfNecessary(qtable.4D.1var.in.rows),
        "Multiple statistics detected in table")
    dim.target <- c(prod(dim(qtable.4D.1var.in.rows)[1:2]),
                    dim(qtable.4D.1var.in.rows)[3])
    expect_equal(rownames(qtable.4D.1var.in.rows.flat)[9], "'out and about': 65 or more")
    expect_equal(dim(qtable.4D.1var.in.rows.flat),
                 dim(qtable.4D.1var.in.rows.flat))

})

test_that("5D QTables",
{
    dim.target <- c(prod(dim(qtable.5D)[c(1, 3)]),
              prod(dim(qtable.5D)[c(2, 4)]))
    expect_warning(
        qtable.5D.flat <- FlattenTableAndDropStatisticsIfNecessary(qtable.5D),
        "Multiple statistics detected")
    expect_equal(attr(qtable.5D.flat, "statistic"), "Average")
    expect_equal(rownames(qtable.5D.flat)[1], "'out and about': Coke")
    expect_equal(colnames(qtable.5D.flat)[9], "Health-conscious: Diet Coke")
    expect_equal(dim(qtable.5D.flat), dim.target)
})
