context("calcSpear")

test_that("creates dataframe", {
  ecologyResults <- filterSpear(demoEcologyResults)
  metricResults <- calcSpear(ecologyResults)
  expect_equal(class(metricResults), expected = "data.frame")
})
