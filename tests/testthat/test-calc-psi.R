context("calc_psi")

test_that("creates dataframe", {
  metricResults <- calc_psi(demo_data)
  expect_equal(class(metricResults), expected = c("tbl_df", "tbl", "data.frame"))

  metricResults <- calc_psi(demo_data, taxa_list = "TL5")
  expect_equal(class(metricResults), expected = c("tbl_df", "tbl", "data.frame"))
})

test_that("test against old aquaMetric value", {
  data <- macroinvertebrateMetrics::demo_data

  data <- data[data$location_id == 8175 &
    data$sample_id == 3201863, ]

  metricResults <- calc_psi(data, taxa_list = "TL3")
  ### Compare against value from older aquaMetrics package (now defunct)
  expect_equal(as.character(metricResults$response[1]), "62.5")
})

test_that("test directly against old aquaMetric function", {
  skip("Requires defunct aquaMetrics package to be installed")
  ###  Code below to convert ecologyResults into dataframe used in aquaMetric package 'calc_psi' function
  ### for testing against old package only - no longer used
  library(aquaMetrics)
  ecologyResults <- macroinvertebrateMetrics::demoEcologyResults[
    demoEcologyResults$SEASON == "SPR",
  ]
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



  checkResults <- filter_psi(macroinvertebrateMetrics::demoEcologyResults)
  checkResults <- calc_psi(ecologyResults)
  checkResults <- inner_join(checkResults,
    ecologyResults[, c("SAMPLE_ID", "DATE_TAKEN")],
    by = c("SAMPLE_ID" = "SAMPLE_ID")
  ) %>% unique()
  test <- spread(checkResults, DETERMINAND, RESULT) %>% arrange(DATE_TAKEN)
  ### Compare against value from older aquaMetrics package (now defunct)
})
