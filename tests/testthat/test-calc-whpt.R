context("calc_whpt")

test_that("WHPT scores match previously calculated scores in demo dataset", {
  results <- macroinvertebrateMetrics::demo_data
  results <- dplyr::filter(
    results,
    parameter == "River Family Inverts" &
      question == "Taxon abundance"
  )
  results <- dplyr::select(results, sample_id, label, response, parameter)
  metricResults <- calc_whpt(results)

  results <- demo_data
  results <- dplyr::filter(results, analysis_name == "FW_TX_WHPT")

  results <- dplyr::filter(results, question %in% c(
    "WHPT ASPT Abund",
    "WHPT NTAXA Abund",
    "WHPT Score"
  )) %>%
    dplyr::select(sample_id, question, response)

  results$question <- as.character(results$question)
  results$response <- as.character(results$response)
  results$response <- as.numeric(results$response)

  results <- tidyr::spread(results, key = question, value = response)

  results <- dplyr::arrange(results, sample_id)

  metricResults <- dplyr::arrange(metricResults, sample_id)
  metricResults <- tidyr::spread(metricResults, key = question, value = response)

  test <- dplyr::inner_join(metricResults, results)
  test$WHPT_NTAXA <- as.numeric(test$WHPT_NTAXA)
  # Remove known errors in demo data WHPT scores and then compare:
  expect_equal(
    test$WHPT_NTAXA[c(1:2, 4, 6:11, 13:14, 16:19, 21:26, 28:32)],
    test$`WHPT NTAXA Abund`[c(1:2, 4, 6:11, 13:14, 16:19, 21:26, 28:32)]
  )

  # WHPT present only
  # Don't have any pre-calculated results - just a static regression test based
  # on values calculated by this package
  metricResults <- calc_whpt(demo_data)
  metricResults <- dplyr::arrange(metricResults, sample_id)
  test <- dplyr::filter(metricResults, question %in% c(
    "WHPT_P_ASPT",
    "WHPT_P_NTAXA",
    "WHPT_P_SCORE"
  ))
  test$response <- as.numeric(test$response)
  expect_equal(
    round(test$response[1:9], 6),
    c(
      76.700000,
      5.478571,
      14.000000,
      67.800000,
      5.215385,
      13.000000,
      77.500000,
      4.843750,
      16.000000
    )
  )
})
