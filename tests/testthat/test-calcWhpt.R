context("calcPsi")

test_that("creates dataframe", {
  metricResults <-
    calcWhpt(demoEcologyResults)

  expect_equal(class(metricResults), expected = c("tbl_df", "tbl", "data.frame"))

  # library(sepaTools)
  #
  # results <- getNemsResults(sampleNumber = 3436402)
  #
  # resultsMetric <- sepaTools:::transformNemsToMetric(results)
  # calcWhpt(resultsMetric)
  # resultsMetric <- resultsMetric[resultsMetric$DETERMINAND == "Taxon abundance",]
  # macroinvertebrateTaxa <- macroinvertebrateTaxa
  #
  # resultsTaxa <- merge(resultsMetric,macroinvertebrateTaxa,by.x = "TAXON",by.y = "TAXON_NAME")
  #
  # groupTaxa <- group_by(resultsTaxa, TL2_TAXON)
  #
  #
  # groupTaxa <- select(groupTaxa, RESULT = VALUE, TAXON = TL2_TAXON, SAMPLE_ID = SAMPLE_NUMBER, DETERMINAND)
  #
  # groupTaxa <- mutate(groupTaxa, SUM = sum(RESULT))
  #
  # groupTaxa <- select(groupTaxa, RESULT = SUM, TAXON, SAMPLE_ID, DETERMINAND)
  #
  # groupTaxa <- distinct(groupTaxa)
  #
  # groupTaxa <- groupTaxa[groupTaxa$TAXON != "",]
  #
  # groupTaxa <- ungroup(groupTaxa)
  #
  # calcWhpt(groupTaxa)
})
