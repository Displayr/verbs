context("Subscripting QStatisticsTestingInfo and other manipulations")

tb <- structure(c(5.82278481012658, 51.3924050632911, 42.7848101265823,
    100, 10.6172839506173, 51.1111111111111, 38.2716049382716, 100
    ), statistic = "Column %", dim = c(4L, 2L), dimnames = list(c(
    "I am on a diet, so I tend to watch what I eat and drink",
    "I tend watch what I eat and drink, but don’t consider myself",
    "I typically eat and drink whatever I feel like", "NET"), c("Male",
    "Female")), class = c("matrix", "array", "QTable"), dimnets = list(
    4L, integer(0)), dimduplicates = list(4L, integer(0)), span = list(
    rows = structure(list(c("I am on a diet, so I tend to watch what I eat and drink",
    "I tend watch what I eat and drink, but don’t consider myself",
    "I typically eat and drink whatever I feel like", "NET")),
    class = "data.frame", names = "", row.names = c(NA, 4L)),
    columns = structure(list(c("Male", "Female")), class = "data.frame",
    names = "", row.names = 1:2)), basedescriptiontext = "sample size = 800",
    basedescription = list(Minimum = 800L, Maximum = 800L, Range = FALSE, Total = 800L,
    Missing = 0L, EffectiveSampleSize = 800L, EffectiveSampleSizeProportion = 100,
    FilteredProportion = 0), QStatisticsTestingInfo = structure(list(
    significancearrowratio = structure(c(0.246786632390746, 0.246786632390746,
    0, 0, 0, 0, 0, 0), dim = 8L), significancedirection = structure(c("Down",
    "Up", "None", "None", "None", "None", "None", "None"), dim = 8L),
    significancefontsizemultiplier = structure(c(0.510204081632653,
    1.96, 1, 1, 1, 1, 1, 1), dim = 8L), significanceissignificant = structure(c(TRUE,
    TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE), dim = 8L),
    significanceargbcolor = structure(c(-65536L, -16776961L,
    -8355712L, -8355712L, -8355712L, -8355712L, -8355712L, -8355712L
    ), dim = 8L), backgroundargbcolor = structure(c(0L, 0L, 0L,
    0L, 0L, 0L, 0L, 0L), dim = 8L), zstatistic = structure(c(-2.46430410396057,
    2.46430410396057, 0.0795806012424474, -0.0795806012424474,
    1.30011015397555, -1.30011015397555, NaN, NaN), dim = 8L),
    pcorrected = structure(c(0.0137279582372565, 0.0137279582372565,
    0.936570824241361, 0.936570824241361, 0.19356321801095, 0.19356321801095,
    NaN, NaN), dim = 8L)), class = "data.frame", row.names = c(NA, 8L)), questiontypes = c("PickOne", "PickOne"),
    footerhtml = "Weight-consciousness by Gender&lt;br /&gt;sample size = 800; 95% confidence level",
    name = "Weight-consciousness by Gender", questions = c("Weight-consciousness", "Gender"))

test_that("Subscripting and renaming works on QStatistingTestingInfo",
{
    mytable = tb[1:3,]
    rownames(mytable) = paste(rownames(mytable),"!!")
    mytable = mytable[-3,]
    expect_equal(attr(mytable, "QStatisticsTestingInfo")$Row,
        structure(c(1L, 1L, 2L, 2L), levels = c(
        "I am on a diet, so I tend to watch what I eat and drink !!",
        "I tend watch what I eat and drink, but don’t consider myself !!"), class = "factor"))
    expect_equal(attr(mytable, "QStatisticsTestingInfo")$backgroundargbcolor,
         structure(c(0L, 0L, 0L, 0L), dim = 4L))
})
