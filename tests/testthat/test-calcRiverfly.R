context("calc_riverfly")


test_that("test riverfly scores", {
  ecologyResults <- demoEcologyResults
  ecologyResults <- ecologyResults[ecologyResults$ANALYSIS_NAME == "FW_TAX_ID", ]
  riverflyOutput <- calc_riverfly(ecologyResults)

  expect_equal(riverflyOutput$RESULT[1], 10)
  expect_equal(riverflyOutput$RESULT[2], 7)
  expect_equal(riverflyOutput$RESULT[3], 10)
})


test_that("test riverfly returns nothing if no relevantdata", {
  ecologyResults <- demoEcologyResults
  ecologyResults <- ecologyResults[ecologyResults$ANALYSIS_NAME == "MAC_R_TST", ]
  riverflyOutput <- calc_riverfly(ecologyResults)

  expect_equal(riverflyOutput$RESULT[1], NULL)
})
