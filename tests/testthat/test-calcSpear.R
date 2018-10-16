context("calcSpear")

test_that("creates dataframe", {
  ecologyResults <- filterSpear(demoEcologyResults, taxaList = "TL2")
  metricResults <- calcSpear(ecologyResults, taxaList = "TL2")
  expect_equal(class(metricResults), expected = "data.frame")
})

test_that("compare TL2 against aquaMetric package scores", {
  ecologyResults <- demoEcologyResults
  ecologyResults <- ecologyResults[ecologyResults$SAMPLE_NUMBER == 3201863,]
  ecologyResults <- ecologyResults[ecologyResults$ANALYSIS_NAME == "FW_TAX_ID",]
  ecologyResults <- filterSpear(ecologyResults, taxaList = "TL2")
  spearOutput <- calcSpear(ecologyResults, recoveryArea = "unknown", taxaList = "TL2")

  # results taken from DAVE Ecology tool 16/10/2018 which uses aquaMetrics package
  expect_equal(round(as.numeric(as.character(spearOutput$RESULT[1])), 2), 32.24)
  expect_equal(round(as.numeric(as.character(spearOutput$RESULT[2])), 2), -3.08)
  expect_equal(as.character(spearOutput$RESULT[3]), "Moderate")
})

test_that("compare TL2 against aquaMetric package scores", {
  # currently DAVE Ecology not working for TL5?
  ecologyResults <- demoEcologyResults
  ecologyResults <- ecologyResults[ecologyResults$ANALYSIS_NAME == "MIXTAX_TST",]
  ecologyResults <- filterSpear(ecologyResults, taxaList = "TL5")
  spearOutput <- calcSpear(ecologyResults, recoveryArea = "unknown", taxaList = "TL5")
})
