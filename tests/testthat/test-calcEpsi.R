context("calcEpsi")

test_that("creates dataframe", {
  library(sepaTools)
  ecologyResults <- filterPsi(demoEcologyResults, taxaList = "TL2")
  metricResults <- calcEpsi(ecologyResults)
  expect_equal(class(metricResults), expected = "data.frame")

  ecologyResults <- filterPsi(demoEcologyResults, taxaList = "TL5")
  metricResults <- calcEpsi(ecologyResults, taxaList = "TL5")
  expect_equal(class(metricResults), expected = "data.frame")

  ecologyResults <- getEcologyResults(sampleNumber = 2031713)
  ecologyResults <- filterPsi(ecologyResults, taxaList = "TL2")
  metricResults <- calcEpsi(ecologyResults)

  # worked example PSI score should be 82.96107211
  ecologyResults <- getEcologyResults(locations = 9599, sampleNumber = 2495120)
  ecologyResults <- filterPsi(ecologyResults, taxaList = "TL2")
  metricResults <- calcEpsi(ecologyResults)
  expect_equal(round(as.numeric(as.character(metricResults$RESULT[1])),digits = 2), 82.96)

  # test random field sample
  ecologyResults <- getEcologyResults(sampleNumber = c("2430420", "2430489"))
  ecologyResults <- filterPsi(ecologyResults, taxaList = "TL2")
  metricResults <- calcEpsi(ecologyResults)

  # no EPSI taxa present in sample
  ecologyResults <- getEcologyResults(sampleNumber = c("3077042"))
  ecologyResults <- filterPsi(ecologyResults, taxaList = "TL2")
  metricResults <- calcEpsi(ecologyResults)
  expect_equal(as.character(metricResults$DETERMINAND),"Error")

  ecologyResults <- getEcologyResults(sampleNumber = c("3077042"))
  ecologyResults <- filterPsi(ecologyResults, taxaList = "TL3")
  metricResults <- calcEpsi(ecologyResults)

})
