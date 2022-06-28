context("calcWhpt")

test_that("WHPT scores match previously calculated scores in demo dataset", {
  results <- macroinvertebrateMetrics::demoEcologyResults
  results <- dplyr::filter(results, ANALYSIS_REPNAME == "Invert Taxa Family Lab" & DETERMINAND == "Taxon abundance")
  results <- dplyr::select(results, SAMPLE_ID, TAXON, RESULT)
  metricResults <- calcWhpt(results)

  results <- demoEcologyResults
  results <- dplyr::filter(results, ANALYSIS_REPNAME == "Invert Summary Whpt")

  results <- dplyr::filter(results, DETERMINAND %in% c(
    "WHPT ASPT Abund",
    "WHPT NTAXA Abund",
    "WHPT Score"
  )) %>%
    dplyr::select(SAMPLE_ID, DETERMINAND, RESULT)

  results$DETERMINAND <- as.character(results$DETERMINAND)
  results$RESULT <- as.character(results$RESULT)
  results$RESULT <- as.numeric(results$RESULT)

  results <- tidyr::spread(results, key = DETERMINAND, value = RESULT)

  results <- dplyr::arrange(results, SAMPLE_ID)

  metricResults <- dplyr::arrange(metricResults, SAMPLE_ID)
  metricResults <- tidyr::spread(metricResults, key = DETERMINAND, value = RESULT)

  test <- dplyr::inner_join(metricResults, results)

  # remove know errors in demo data WHPT scores and then compare:
  expect_equal(
    test$WHPT_NTAXA[c(1:2, 4, 6:11, 13:14, 16:19, 21:26, 28:32)],
    test$`WHPT NTAXA Abund`[c(1:2, 4, 6:11, 13:14, 16:19, 21:26, 28:32)]
  )
})
