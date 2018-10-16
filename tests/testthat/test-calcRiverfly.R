context("calcRiverfly")


test_that("test riverfly scores", {
  ecologyResults <- demoEcologyResults
  ecologyResults <- ecologyResults[ecologyResults$ANALYSIS_NAME == "FW_TAX_ID",]
  riverflyOutput <- calcRiverfly(ecologyResults)

  expect_equal(riverflyOutput$RESULT[1], 10)
  expect_equal(riverflyOutput$RESULT[2], 7)
  expect_equal(riverflyOutput$RESULT[3], 10)
})
