context("calcPsi")

test_that("creates dataframe", {
  metricResults <-
    calcWhpt(demoEcologyResults)

  expect_equal(class(metricResults), expected = c("tbl_df", "tbl", "data.frame"))


})
