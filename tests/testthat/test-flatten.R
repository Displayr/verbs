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
            expected.row <- if (row.in.grid.types) 1L else 1:2
            expected.col <- if (row.in.grid.types) 2:3 else 3L
            expected.3d.list <- list(row.vars = expected.row, col.vars = expected.col)
            expected.4d.list <- list(row.vars = c(1L, 3L), col.vars = c(2L, 4L))
            qtypes <- c(row.qtype, col.qtype)
            expect_equal(determineFlatteningRowAndColVars(qtypes, n.dim = 3L),
                         expected.3d.list)
            expect_equal(determineFlatteningRowAndColVars(qtypes, n.dim = 4L),
                         expected.4d.list)
        }
})

})
