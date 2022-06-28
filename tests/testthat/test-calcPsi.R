context("calcPsi")

test_that("creates dataframe", {
  ecologyResults <- filterPsi(macroinvertebrateMetrics::demoEcologyResults)
  metricResults <- calcPsi(ecologyResults)
  expect_equal(class(metricResults), expected = "data.frame")

  ecologyResults <- filterPsi(demoEcologyResults, taxaList = "TL5")
  metricResults <- calcPsi(ecologyResults, taxaList = "TL5")
  expect_equal(class(metricResults), expected = "data.frame")
})

test_that("test against old aquaMetric value", {
  ecologyResults <- macroinvertebrateMetrics::demoEcologyResults

  ecologyResults <- filterPsi(ecologyResults[ecologyResults$LOCATION_CODE == 8175 &
    ecologyResults$SAMPLE_NUMBER == 3201863, ])

  metricResults <- calcPsi(ecologyResults)
  ### Compare against value from older aquaMetrics package (now defunct)
  expect_equal(as.character(metricResults$RESULT[1]), "62.5")
})

test_that("test directly against old aquaMetric function", {
  skip("Requires defunct aquaMetrics package to be installed")
  ###  Code below to convert ecologyResults into dataframe used in aquaMetric package 'CalcPSI' function
  ### for testing against old package only - no longer used
  library(aquaMetrics)
  ecologyResults <- macroinvertebrateMetrics::demoEcologyResults[
    demoEcologyResults$SEASON == "SPR",
  ]
  #  demoEcologyResults$LOCATION_CODE == 8175 &
  #  demoEcologyResults$SAMPLE_NUMBER == 3201863, ]
  standardTaxa <- ecologyResults[
    ecologyResults$DETERMINAND == "Taxon abundance" &
      ecologyResults$ANALYSIS_NAME == "FW_TAX_ID",
    c(
      "LOCATION_CODE",
      "DATE_TAKEN",
      "DATE_TAKEN",
      "MAITLAND_CODE",
      "TAXON",
      "RESULT"
    )
  ]
  standardTaxa$Season <- calcSeason(standardTaxa$DATE_TAKEN)
  standardTaxa$Year <- format.Date(standardTaxa$DATE_TAKEN, "%Y")
  standardTaxa <- standardTaxa[, c(
    "LOCATION_CODE",
    "Season",
    "Year",
    "MAITLAND_CODE",
    "TAXON",
    "RESULT"
  )]
  names(standardTaxa) <- c(
    "Site",
    "Season",
    "Year",
    "Maitland Code",
    "Maitland Name",
    "Abundance"
  )
  standardTaxa$Season <- as.integer(standardTaxa$Season)
  standardTaxa$`Maitland Name` <- as.character(standardTaxa$"Maitland Name")
  standardTaxa$Abundance <- as.character(standardTaxa$Abundance)
  standardTaxa$Abundance <- as.numeric(standardTaxa$Abundance)
  test <- aquaMetrics::StandardiseRawTaxa(
    family.df = standardTaxa,
    species.df = standardTaxa,
    aggregate = "season"
  )
  testResults <- aquaMetrics::CalcPSI(test$standard.taxa, season = 1)



  checkResults <- filterPsi(macroinvertebrateMetrics::demoEcologyResults)
  checkResults <- calcPsi(ecologyResults)
  checkResults <- inner_join(checkResults,
    ecologyResults[, c("SAMPLE_ID", "DATE_TAKEN")],
    by = c("SAMPLE_ID" = "SAMPLE_ID")
  ) %>% unique()
  test <- spread(checkResults, DETERMINAND, RESULT) %>% arrange(DATE_TAKEN)
  ### Compare against value from older aquaMetrics package (now defunct)
})
