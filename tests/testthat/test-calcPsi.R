context("calcEpsi")

test_that("creates dataframe", {
  ecologyResults <- filterPsi(demoEcologyResults)
  metricResults <- calcPsi(ecologyResults)
  expect_equal(class(metricResults), expected = "data.frame")
})
