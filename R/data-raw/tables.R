require(usethis)

table1D.Average <- structure(c(`Colas (e.g., Coca-Cola, Pepsi Max)?` = 7.08868501529052,
                               `Sparkling mineral water` = 3.84709480122324, Coffee = 6.52599388379205,
                               SUM = 17.4617737003058), statistic = "Average", .Dim = 4L, .Dimnames = list(
                                   c("Colas (e.g., Coca-Cola, Pepsi Max)?", "Sparkling mineral water",
                                     "Coffee", "SUM")), basedescriptiontext = "sample size = 327", basedescription = list(
                                         Minimum = 327L, Maximum = 327L, Range = FALSE, Total = 327L,
                                         Missing = 0L, EffectiveSampleSize = 327L, EffectiveSampleSizeProportion = 100,
                                         FilteredProportion = 0), questiontypes = "NumberMulti", span = list(
                                             rows = structure(list(c("Colas (e.g., Coca-Cola, Pepsi Max)?",
                                                                     "Sparkling mineral water", "Coffee", "SUM")), class = "data.frame", .Names = "", row.names = c(NA,
                                                                                                                                                                    4L))), name = "table.Frequency.of.drinking", questions = c("Frequency of drinking",
                                                                                                                                                                                                                               "SUMMARY"))
use_data(table1D.Average, internal = FALSE, overwrite = TRUE)


table1D.Percentage <- structure(c(`18 to 24` = 13.4556574923547, `25 to 29` = 11.9266055045872,
                                  `30 to 34` = 10.0917431192661, `35 to 39` = 11.0091743119266,
                                  `40 to 44` = 10.7033639143731, `45 to 49` = 8.25688073394496,
                                  `50 to 54` = 12.2324159021407, `55 to 64` = 15.5963302752294,
                                  `65 or more` = 6.72782874617737, NET = 100), statistic = "%", .Dim = 10L, .Dimnames = list(
                                      c("18 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44",
                                        "45 to 49", "50 to 54", "55 to 64", "65 or more", "NET")), basedescriptiontext = "sample size = 327", basedescription = list(
                                            Minimum = 327L, Maximum = 327L, Range = FALSE, Total = 327L,
                                            Missing = 0L, EffectiveSampleSize = 327L, EffectiveSampleSizeProportion = 100,
                                            FilteredProportion = 0), questiontypes = "PickOne", span = list(
                                                rows = structure(list(c("18 to 24", "25 to 29", "30 to 34",
                                                                        "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 64",
                                                                        "65 or more", "NET")), class = "data.frame", .Names = "", row.names = c(NA,
                                                                                                                                                10L))), name = "table.Age", questions = c("Age", "SUMMARY"
                                                                                                                                                ))
use_data(table1D.Percentage, internal = FALSE, overwrite = TRUE)

table.1D.MultipleStatistics <- structure(c(7.08868501529052, 3.84709480122324, 6.52599388379205,
                                           17.4617737003058, 327, 327, 327, 327, 12.6589920377122, -20.1358806716876,
                                           5.56386424467369, 71.7422504665908, 326, 326, 326, 326, Inf,
                                           -Inf, 5.43403079378372, Inf, 0, 3.66502922480297e-59, 5.50950665090255e-08,
                                           0), .Dim = c(4L, 6L), .Dimnames = list(c("Colas (e.g., Coca-Cola, Pepsi Max)?",
                                                                                    "Sparkling mineral water", "Coffee", "SUM"), c("Average", "Effective Sample Size",
                                                                                                                                   "t-Statistic", "d.f.", "z-Statistic", "Corrected p")), basedescriptiontext = "sample size = 327", basedescription = list(
                                                                                                                                       Minimum = 327L, Maximum = 327L, Range = FALSE, Total = 327L,
                                                                                                                                       Missing = 0L, EffectiveSampleSize = 327L, EffectiveSampleSizeProportion = 100,
                                                                                                                                       FilteredProportion = 0), questiontypes = "NumberMulti", span = list(
                                                                                                                                           rows = structure(list(c("Colas (e.g., Coca-Cola, Pepsi Max)?",
                                                                                                                                                                   "Sparkling mineral water", "Coffee", "SUM")), class = "data.frame", .Names = "", row.names = c(NA,
                                                                                                                                                                                                                                                                  4L))), name = "table.Frequency.of.drinking.2", questions = c("Frequency of drinking",
                                                                                                                                                                                                                                                                                                                               "SUMMARY"))
use_data(table.1D.MultipleStatistics, internal = FALSE, overwrite = TRUE)

table1D.Text = structure(c(`95` = "People who drink Coke are more tought since they dont worry about sugar free drinks",
                           `107` = "no sugar in coke zero", `108` = "People who don't want to consume sugar",
                           `244` = "-"), statistic = "Text", .Dim = 4L, .Dimnames = list(
                               c("95", "107", "108", "244")), basedescriptiontext = "sample size = 4; 99% filtered out", basedescription = list(
                                   Minimum = 4L, Maximum = 4L, Range = FALSE, Total = 4L, Missing = 0L,
                                   EffectiveSampleSize = 4L, EffectiveSampleSizeProportion = 100,
                                   FilteredProportion = 98.7767584097859), questiontypes = character(0), span = list(
                                       rows = structure(list(c("95", "107", "108", "244")), class = "data.frame", .Names = "", row.names = c(NA,
                                                                                                                                             4L))),
                         name = "table.Main.difference.between.cola.drinkers", questions = c("Main difference between cola drinkers",
                                                                                             "RAW DATA"))
use_data(table1D.Text, internal = FALSE, overwrite = TRUE)

table2D.Percentage = structure(c(0, 25, 25, 0, 25, 25, 0, 25, 50, 0, 50, 25, 0, 25,
                                 0, 25, 25, 0, 25, 0, 25, 50, 0, 25, 0, 25, 0, 0, 0, 25, 0, 0,
                                 0, 0, 0, 0, 50, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 25, 0, 0, 25,
                                 0, 0, 100, 100, 100, 100, 100, 100), statistic = "Row %", .Dim = c(6L, 10L),
                               .Dimnames = list(c("Coca-Cola", "Diet Coke", "Coke Zero",
                                                  "Pepsi", "Diet Pepsi", "Pepsi Max"), c("Never", "Once or twice a year",
                                                                                         "Once every 3 months", "Once a month", "Once every 2 weeks",
                                                                                         "Once a week", "2 to 3 days a week", "4 to 5 days a week", "Every or nearly every day",
                                                                                         "NET")), basedescriptiontext = "sample size = 4; 99% filtered out", basedescription = list(
                                                                                             Minimum = 4L, Maximum = 4L, Range = FALSE, Total = 4L, Missing = 0L,
                                                                                             EffectiveSampleSize = 4L, EffectiveSampleSizeProportion = 100,
                                                                                             FilteredProportion = 98.7767584097859), questiontypes = "PickOneMulti", span = list(
                                                                                                 rows = structure(list(c("Coca-Cola", "Diet Coke", "Coke Zero",
                                                                                                                         "Pepsi", "Diet Pepsi", "Pepsi Max")), class = "data.frame", .Names = "", row.names = c(NA,
                                                                                                                                                                                                                6L)), columns = structure(list(c("Never", "Once or twice a year",
                                                                                                                                                                                                                                                 "Once every 3 months", "Once a month", "Once every 2 weeks",
                                                                                                                                                                                                                                                 "Once a week", "2 to 3 days a week", "4 to 5 days a week",
                                                                                                                                                                                                                                                 "Every or nearly every day", "NET")), class = "data.frame", .Names = "", row.names = c(NA,
                                                                                                                                                                                                                                                                                                                                        10L))), name = "table.Cola.drinking.frequency", questions = c("Cola drinking frequency",
                                                                                                                                                                                                                                                                                                                                                                                                      "SUMMARY"))

use_data(table2D.Percentage, internal = FALSE, overwrite = TRUE)

table2D.PercentageAndCount = structure(c(15.5963302752294, 36.085626911315, 37.6146788990826,
                                         30.5810397553517, 63.302752293578, 39.1437308868502, 7.03363914373089,
                                         15.5963302752294, 8.56269113149847, 18.0428134556575, 12.5382262996942,
                                         11.9266055045872, 7.3394495412844, 9.78593272171254, 6.11620795107034,
                                         13.1498470948012, 6.42201834862385, 11.0091743119266, 13.4556574923547,
                                         12.8440366972477, 11.6207951070336, 17.4311926605505, 6.72782874617737,
                                         10.3975535168196, 9.1743119266055, 7.3394495412844, 7.3394495412844,
                                         8.25688073394496, 3.36391437308868, 6.72782874617737, 16.8195718654434,
                                         3.97553516819572, 8.86850152905199, 7.64525993883792, 2.44648318042813,
                                         6.11620795107034, 14.3730886850153, 4.89296636085627, 7.95107033639144,
                                         3.6697247706422, 2.44648318042813, 5.5045871559633, 5.19877675840979,
                                         5.5045871559633, 2.75229357798165, 0, 0.917431192660551, 3.6697247706422,
                                         11.0091743119266, 3.97553516819572, 9.1743119266055, 1.22324159021407,
                                         1.8348623853211, 5.5045871559633, 100, 100, 100, 100, 100, 100,
                                         51, 118, 123, 100, 207, 128, 23, 51, 28, 59, 41, 39, 24, 32,
                                         20, 43, 21, 36, 44, 42, 38, 57, 22, 34, 30, 24, 24, 27, 11, 22,
                                         55, 13, 29, 25, 8, 20, 47, 16, 26, 12, 8, 18, 17, 18, 9, 0, 3,
                                         12, 36, 13, 30, 4, 6, 18, 327, 327, 327, 327, 327, 327), .Dim = c(6L,
                                                                                                           10L, 2L), .Dimnames = list(c("Coca-Cola", "Diet Coke", "Coke Zero",
                                                                                                                                        "Pepsi", "Diet Pepsi", "Pepsi Max"), c("Never", "Once or twice a year",
                                                                                                                                                                               "Once every 3 months", "Once a month", "Once every 2 weeks",
                                                                                                                                                                               "Once a week", "2 to 3 days a week", "4 to 5 days a week", "Every or nearly every day",
                                                                                                                                                                               "NET"), c("Row %", "Count")), basedescriptiontext = "sample size = 327", basedescription = list(
                                                                                                                                                                                   Minimum = 327L, Maximum = 327L, Range = FALSE, Total = 327L,
                                                                                                                                                                                   Missing = 0L, EffectiveSampleSize = 327L, EffectiveSampleSizeProportion = 100,
                                                                                                                                                                                   FilteredProportion = 0), questiontypes = "PickOneMulti", span = list(
                                                                                                                                                                                       rows = structure(list(c("Coca-Cola", "Diet Coke", "Coke Zero",
                                                                                                                                                                                                               "Pepsi", "Diet Pepsi", "Pepsi Max")), class = "data.frame", .Names = "", row.names = c(NA,
                                                                                                                                                                                                                                                                                                      6L)), columns = structure(list(c("Never", "Once or twice a year",
                                                                                                                                                                                                                                                                                                                                       "Once every 3 months", "Once a month", "Once every 2 weeks",
                                                                                                                                                                                                                                                                                                                                       "Once a week", "2 to 3 days a week", "4 to 5 days a week",
                                                                                                                                                                                                                                                                                                                                       "Every or nearly every day", "NET")), class = "data.frame", .Names = "", row.names = c(NA,
                                                                                                                                                                                                                                                                                                                                                                                                                              10L))), name = "table.Cola.drinking.frequency.2", questions = c("Cola drinking frequency",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              "SUMMARY"))


use_data(table2D.PercentageAndCount, internal = FALSE, overwrite = TRUE)

table2D.PercentageNaN = structure(c(6.42201834862385, 57.4923547400612, 22.3241590214067,
                                    8.56269113149847, 60.5504587155963, 10.0917431192661, 9.78593272171254,
                                    100, 1.8348623853211, 58.7155963302752, 55.045871559633, NA,
                                    57.7981651376147, 30.8868501529052, 17.4311926605505, NA, 9.1743119266055,
                                    22.9357798165138, 12.8440366972477, 9.78593272171254, 43.4250764525994,
                                    7.3394495412844, 29.9694189602446, 100, 65.1376146788991, 21.7125382262997,
                                    4.58715596330275, 37.9204892966361, 9.1743119266055, 6.42201834862385,
                                    8.56269113149847, 100, 22.6299694189602, 9.1743119266055, 51.9877675840979,
                                    15.5963302752294, 16.2079510703364, 50.4587155963303, 11.9266055045872,
                                    100, 26.2996941896024, 4.58715596330275, 31.4984709480122, 17.737003058104,
                                    3.97553516819572, 44.6483180428135, 15.9021406727829, 100, 9.1743119266055,
                                    23.5474006116208, 9.1743119266055, 14.3730886850153, 29.6636085626911,
                                    6.42201834862385, 38.8379204892966, 100, 92.3547400611621, 14.6788990825688,
                                    3.05810397553517, 53.822629969419, 3.36391437308868, 3.97553516819572,
                                    2.75229357798165, 100, 0.611620795107034, 76.4525993883792, 64.5259938837921,
                                    0, 76.4525993883792, 40.6727828746177, 5.5045871559633, 100,
                                    98.1651376146789, 92.3547400611621, 90.8256880733945, NA, 95.1070336391437,
                                    86.8501529051988, 57.4923547400612, NA), statistic = "%", .Dim = c(8L,
                                                                                                       10L), .Dimnames = list(c("Coke", "Diet Coke", "Coke Zero", "Pepsi",
                                                                                                                                "Diet Pepsi", "Pepsi Max", "None of these", "NET"), c("Feminine",
                                                                                                                                                                                      "Health-conscious", "Innocent", "Older", "Open to new experiences",
                                                                                                                                                                                      "Rebellious", "Sleepy", "Traditional", "Weight-conscious", "NET"
                                                                                                                                )), basedescriptiontext = "sample size = from 0 to 327; total sample size = 327; 327 missing", basedescription = list(
                                                                                                                                    Minimum = 0L, Maximum = 327L, Range = TRUE, Total = 327L,
                                                                                                                                    Missing = 327L, EffectiveSampleSize = 327L, EffectiveSampleSizeProportion = 100,
                                                                                                                                    FilteredProportion = 0), questiontypes = "PickAnyGrid", span = list(
                                                                                                                                        rows = structure(list(c("Coke", "Diet Coke", "Coke Zero",
                                                                                                                                                                "Pepsi", "Diet Pepsi", "Pepsi Max", "None of these", "NET"
                                                                                                                                        )), class = "data.frame", .Names = "", row.names = c(NA,
                                                                                                                                                                                             8L)), columns = structure(list(c("Feminine", "Health-conscious",
                                                                                                                                                                                                                              "Innocent", "Older", "Open to new experiences", "Rebellious",
                                                                                                                                                                                                                              "Sleepy", "Traditional", "Weight-conscious", "NET")), class = "data.frame", .Names = "", row.names = c(NA,
                                                                                                                                                                                                                                                                                                                                     10L))), name = "table.Q5", questions = c("Q5", "SUMMARY"))
use_data(table2D.PercentageNaN, internal = FALSE, overwrite = TRUE)
