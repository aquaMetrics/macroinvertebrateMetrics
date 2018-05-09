context("calcEpsi")

test_that("creates dataframe", {
  ecologyResults <- filterPsi(demoEcologyResults)
  metricResults <- calcPsi(ecologyResults)
  expect_equal(class(metricResults), expected = "data.frame")

  ecologyResults <- filterPsi(demoEcologyResults, taxaList = "TL5")
  metricResults <- calcPsi(ecologyResults, taxaList = "TL5")
  expect_equal(class(metricResults), expected = "data.frame")
})
