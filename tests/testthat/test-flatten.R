context("Test flattening of tables")

tbls <- readRDS("qTablesWithZStatInCells.rds")

test_that("Lower dim tables are returned without problems", {
    lower.dim.tbls <- Filter(function(x) getDimensionLength(x) <= 2, tbls)
    for (tbl in lower.dim.tbls)
        expect_equal(FlattenTableAndDropStatisticsIfNecessary(tbl), tbl)
    # Same tests on legacy outputs that dont have the class
    lower.dim.tbls <- lapply(lower.dim.tbls, unclass)
    for (tbl in lower.dim.tbls)
        expect_equal(FlattenTableAndDropStatisticsIfNecessary(tbl), tbl)
})

test_that("DS-3920 Correct dims identified for flattening", {
    expect_error(determineFlatteningRowAndColVars(),
                 "Need question types to resolve row and column variables for flattening")
    expect_error(determineFlatteningRowAndColVars("foo", n.dim = 0),
                 "n.dim should be a single integer")
    expect_error(determineFlatteningRowAndColVars("foo", n.dim = 2L),
                 "Flattening only supported for 3D and 4D tables")
})
