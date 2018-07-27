context("calcPsi")

test_that("creates dataframe", {
  library(sepaTools)
  ecologyResults <- filterPsi(demoEcologyResults)
  metricResults <- calcPsi(ecologyResults)
  expect_equal(class(metricResults), expected = "data.frame")

  ecologyResults <- filterPsi(demoEcologyResults, taxaList = "TL5")
  metricResults <- calcPsi(ecologyResults, taxaList = "TL5")
  expect_equal(class(metricResults), expected = "data.frame")
  
  demoEcologyResults <- demoEcologyResults

  ecologyResults <- filterPsi(demoEcologyResults[demoEcologyResults$LOCATION_CODE == 8175 &
                                                   demoEcologyResults$SAMPLE_NUMBER == 3201863 ,])
  
  metricResults <- calcPsi(ecologyResults)
  
  
  ecologyResults <- getEcologyResults(locations = 122812, sampleNumber = 3291629)
  ecologyResults <- filterPsi(ecologyResults)
  
  metricResults <- calcPsi(ecologyResults)
  
})
