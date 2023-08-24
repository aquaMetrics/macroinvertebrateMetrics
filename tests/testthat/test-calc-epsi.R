context("calc_epsi")

test_that("creates dataframe", {
  ecology_results <- filter_psi(demo_data, taxa_list = "TL2")
  metric_results <- calc_epsi(ecology_results)
  expect_equal(class(metric_results), expected = c("tbl_df", "tbl","data.frame"))

  ecology_results <- filter_psi(demo_data, taxa_list = "TL5")
  metric_results <- calc_epsi(ecology_results, taxa_list = "TL5")
  expect_equal(class(metric_results), expected = c("tbl_df", "tbl","data.frame"))
})

test_that("test internal to SEPA", {
  skip("Requires internal SEPA package to be installed")
  library(sepaTools)
  # old manually calculated example from sepaTools:
  ecologyResults <- getEcologyResults(sampleNumber = c(91977))
  ecologyResults$RESULT <- ecologyResults$VALUE
  ecologyResults <- filter_psi(ecologyResults, taxa_list = "TL2")
  metricResults <- calc_epsi(ecologyResults)
  expect_equal(round(as.numeric(as.character(metricResults$RESULT[1])),
    digits = 5
  ), 82.38532)


  # test standard TL2  / FW_TAX_ID samples
  ecologyResults <- getEcologyResults(sampleNumber = 2031713)
  ecologyResults$RESULT <- ecologyResults$VALUE
  ecologyResults <- filter_psi(ecologyResults, taxaList = "TL2")
  metricResults <- calc_epsi(ecologyResults)

  # hand calculated example
  ecologyResults <- getEcologyResults(sampleNumber = 3686335)
  ecologyResults <- filter_psi(ecologyResults, taxa_list = "TL2")
  metricResults <- calc_epsi(ecologyResults)
  expect_equal(round(as.numeric(as.character(metricResults$RESULT[1])),
    digits = 0
  ), 90)

  # hand calculated example
  ecologyResults <- getEcologyResults(sampleNumber = 3686337)
  ecologyResults <- filter_psi(ecologyResults, taxa_list = "TL2")
  metricResults <- calc_epsi(ecologyResults)
  expect_equal(round(as.numeric(as.character(metricResults$RESULT[1])),
    digits = 0
  ), 91)

  # hand calculated example
  ecologyResults <- getEcologyResults(sampleNumber = 3686339)
  ecologyResults <- filter_psi(ecologyResults, taxa_list = "TL2")
  metricResults <- calc_epsi(ecologyResults)
  expect_equal(round(as.numeric(as.character(metricResults$RESULT[1])),
    digits = 0
  ), 93)

  # worked example PSI score should be 82.96107211
  ecologyResults <- getEcologyResults(locations = 9599, sampleNumber = 2495120)
  ecologyResults <- filter_psi(ecologyResults, taxa_list = "TL2")
  metricResults <- calc_epsi(ecologyResults)
  expect_equal(round(as.numeric(as.character(metricResults$RESULT[1])),
    digits = 2
  ), 82.96)

  # test random field sample is working
  ecologyResults <- getEcologyResults(sampleNumber = c("2430420", "2430489"))
  ecologyResults <- filter_psi(ecologyResults, taxa_list = "TL2")
  metricResults <- calc_epsi(ecologyResults)

  # no EPSI taxa present in sample
  ecologyResults <- getEcologyResults(sampleNumber = c("3077042"))
  ecologyResults <- filter_psi(ecologyResults, taxa_list = "TL2")
  metricResults <- calc_epsi(ecologyResults)
  expect_equal(as.character(metricResults$DETERMINAND), "Error")


  testEcologyResults <- getEcologyResults(sampleNumber = c("41614"))
  testEcologyResults$RESULT <- testEcologyResults$VALUE
  testEcologyResults <- filter_psi(testEcologyResults, taxa_list = "TL2")
  metricResults <- calc_epsi(testEcologyResults)
})
