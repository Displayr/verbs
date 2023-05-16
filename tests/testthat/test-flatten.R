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
