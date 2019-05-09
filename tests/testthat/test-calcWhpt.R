context("calcWhpt")

test_that("WHPT scores match previously calculated scores in demo dataset", {

  results  <- demoEcologyResults
  results  <- filter(results, ANALYSIS_REPNAME == "Invert Taxa Family Lab")

  metricResults <-
    calcWhpt(results)

  results <- demoEcologyResults
  results <- filter(results, ANALYSIS_REPNAME == "Invert Summary Whpt")

  results <- dplyr::filter(results, DETERMINAND %in% c("WHPT ASPT Abund",
                                                       "WHPT NTAXA Abund",
                                                       "WHPT Score")) %>%
  dplyr::select(SAMPLE_ID, DETERMINAND, RESULT)

  results$DETERMINAND <- as.character(results$DETERMINAND)
  results$RESULT <- as.character(results$RESULT)
  results$RESULT <- as.numeric(results$RESULT)


  results <- results[!duplicated(results),]
  metricResults <- metricResults[!duplicated(metricResults),]
  results <- tidyr::spread(results, key = DETERMINAND, value = RESULT)

  results <- arrange(results, SAMPLE_ID)

  metricResults <- arrange(metricResults, SAMPLE_ID)
  metricResults <- tidyr::spread(metricResults, key = DETERMINAND, value = RESULT)

  test <- inner_join(metricResults, results)

   # remove know errors in demo data WHPT scores and then compare:
  expect_equal(test$WHPT_NTAXA[c(1:2, 4, 6:11, 13:14, 16:19, 21:26, 28:32)],
       test$`WHPT NTAXA Abund`[c(1:2, 4, 6:11, 13:14, 16:19, 21:26, 28:32)])

})
