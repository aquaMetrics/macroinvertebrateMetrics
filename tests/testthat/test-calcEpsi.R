context("calcEpsi")

test_that("creates dataframe", {

  ecologyResults <- filterPsi(demoEcologyResults, taxaList = "TL2")
  metricResults <- calcEpsi(ecologyResults)
  expect_equal(class(metricResults), expected = "data.frame")

  ecologyResults <- filterPsi(demoEcologyResults, taxaList = "TL5")
  metricResults <- calcEpsi(ecologyResults, taxaList = "TL5")
  expect_equal(class(metricResults), expected = "data.frame")

})

test_that("test internal to SEPA", {
  skip("Requires internal SEPA package to be installed")
  library(sepaTools)
# old manually calculated example from sepaTools:
  ecologyResults <- getEcologyResults(sampleNumber = c(91977))
  ecologyResults$RESULT <- ecologyResults$VALUE
  ecologyResults <- filterPsi(ecologyResults, taxaList = "TL2")
  metricResults <- calcEpsi(ecologyResults)
  expect_equal(round(as.numeric(as.character(metricResults$RESULT[1])),
                     digits = 5), 82.38532)


# test standard TL2  / FW_TAX_ID samples
ecologyResults <- getEcologyResults(sampleNumber = 2031713)
ecologyResults$RESULT <- ecologyResults$VALUE
ecologyResults <- filterPsi(ecologyResults, taxaList = "TL2")
metricResults <- calcEpsi(ecologyResults)

# hand calculated example
ecologyResults <- getEcologyResults(sampleNumber = 3686335)
ecologyResults <- filterPsi(ecologyResults, taxaList = "TL2")
metricResults <- calcEpsi(ecologyResults)
expect_equal(round(as.numeric(as.character(metricResults$RESULT[1])),
                   digits = 0), 90)

# hand calculated example
ecologyResults <- getEcologyResults(sampleNumber = 3686337)
ecologyResults <- filterPsi(ecologyResults, taxaList = "TL2")
metricResults <- calcEpsi(ecologyResults)
expect_equal(round(as.numeric(as.character(metricResults$RESULT[1])),
                   digits = 0), 91)

# hand calculated example
ecologyResults <- getEcologyResults(sampleNumber = 3686339)
ecologyResults <- filterPsi(ecologyResults, taxaList = "TL2")
metricResults <- calcEpsi(ecologyResults)
expect_equal(round(as.numeric(as.character(metricResults$RESULT[1])),
                   digits = 0), 93)

# worked example PSI score should be 82.96107211
ecologyResults <- getEcologyResults(locations = 9599, sampleNumber = 2495120)
ecologyResults <- filterPsi(ecologyResults, taxaList = "TL2")
metricResults <- calcEpsi(ecologyResults)
expect_equal(round(as.numeric(as.character(metricResults$RESULT[1])),
                   digits = 2), 82.96)

# test random field sample is working
ecologyResults <- getEcologyResults(sampleNumber = c("2430420", "2430489"))
ecologyResults <- filterPsi(ecologyResults, taxaList = "TL2")
metricResults <- calcEpsi(ecologyResults)

# no EPSI taxa present in sample
ecologyResults <- getEcologyResults(sampleNumber = c("3077042"))
ecologyResults <- filterPsi(ecologyResults, taxaList = "TL2")
metricResults <- calcEpsi(ecologyResults)
expect_equal(as.character(metricResults$DETERMINAND), "Error")


testEcologyResults <- getEcologyResults(sampleNumber = c("41614"))
testEcologyResults$RESULT <- testEcologyResults$VALUE
testEcologyResults <- filterPsi(testEcologyResults, taxaList = "TL2")
metricResults <- calcEpsi(testEcologyResults)

})
