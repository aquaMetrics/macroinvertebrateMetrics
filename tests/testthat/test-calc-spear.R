context("calc_spear")

test_that("creates dataframe", {
  metricResults <- calc_spear(demo_data, taxa_list = "TL2")
  expect_equal(class(metricResults), expected = c("tbl_df", "tbl", "data.frame"))
})

test_that("compare TL2 against aquaMetric package scores", {
  # check first sample
  ecologyResults <- demo_data
  ecologyResults <- ecologyResults[ecologyResults$sample_id == 3201863, ]
  ecologyResults <- ecologyResults[ecologyResults$analysis_name == "FW_TAX_ID", ]
  spearOutput <- calc_spear(ecologyResults, recoveryArea = "unknown", taxa_list = "TL2")

  # results taken from internal DAVE Ecology tool 16/10/2018 which uses aquaMetrics package
  expect_equal(round(as.numeric(as.character(purrr::pluck(spearOutput, column_attributes$name[3], 1))), 2), 32.24)
  expect_equal(round(as.numeric(as.character(purrr::pluck(spearOutput, column_attributes$name[3], 2))), 2), -3.08)
  expect_equal(as.character(purrr::pluck(spearOutput, column_attributes$name[3], 3)), "Moderate")
})


test_that("compare TL5 against aquaMetric package scores", {
  skip("Requires defunct aquaMetrics package to be installed")
  # check first sample
  ecologyResults <- demoEcologyResults
  # Spring 8175 2016
  ecologyResults <- ecologyResults[ecologyResults$SAMPLE_ID == 2959448, ]
  ecologyResults <- ecologyResults[ecologyResults$ANALYSIS_NAME == "MIXTAX_TST", ]
  standardTaxa <- ecologyResults[
    ecologyResults$DETERMINAND == "Taxon abundance",
    c(
      "LOCATION_CODE", "DATE_TAKEN", "DATE_TAKEN", "MAITLAND_CODE",
      "TAXON", "RESULT"
    )
  ]
  standardTaxa$Season <- sepaTools::calcSeason(standardTaxa$DATE_TAKEN)
  standardTaxa$Year <- format.Date(standardTaxa$DATE_TAKEN, "%Y")
  standardTaxa <- standardTaxa[, c("LOCATION_CODE", "Season", "Year", "MAITLAND_CODE", "TAXON", "RESULT")]
  names(standardTaxa) <- c("Site", "Season", "Year", "Maitland Code", "Maitland Name", "Abundance")
  standardTaxa$Season <- as.integer(standardTaxa$Season)
  standardTaxa$`Maitland Name` <- as.character(standardTaxa$"Maitland Name")
  standardTaxa$Abundance <- as.character(standardTaxa$Abundance)
  standardTaxa$Abundance <- as.numeric(standardTaxa$Abundance)
  test <- aquaMetrics::StandardiseRawTaxa(family.df = standardTaxa, species.df = standardTaxa, aggregate = "season")
  aquaMetricOutput <- aquaMetrics::Calc_SPEAR(
    data = test$standard.taxa, season = 1,
    TL = 5L, recovery.area.info = FALSE
  )

  ecologyResults <- filter_spear(ecologyResults, taxaList = "TL5")
  spearOutput <- calc_spear(ecologyResults, recoveryArea = "unknown", taxaList = "TL5")

  # results taken from internal DAVE Ecology tool 16/10/2018 which uses aquaMetrics package
  expect_equal(round(as.numeric(as.character(spearOutput$RESULT[1])), 2), 29.19)
  expect_equal(round(as.numeric(as.character(spearOutput$RESULT[2])), 2), -2.73)
  expect_equal(as.character(spearOutput$RESULT[3]), "Moderate")
})

test_that("compare directly against aquaMetric package", {
  skip("Requires defunct aquaMetrics package to be installed")
  ### Code below to convert ecologyResults into dataframe used in aquaMetric package 'CalcPSI' function
  ### for testing against old package only - no longer used
  ### Also required internal sepaTools package
  ecologyResults <- demoEcologyResults[demoEcologyResults$LOCATION_CODE == 8175 &
    demoEcologyResults$SAMPLE_NUMBER == 3294945, ]
  ecologyResults <- ecologyResults[ecologyResults$ANALYSIS_NAME == "FW_TAX_ID", ]
  standardTaxa <- ecologyResults[
    ecologyResults$DETERMINAND == "Taxon abundance",
    c(
      "LOCATION_CODE", "DATE_TAKEN", "DATE_TAKEN", "MAITLAND_CODE",
      "TAXON", "RESULT"
    )
  ]
  standardTaxa$Season <- sepaTools::calcSeason(standardTaxa$DATE_TAKEN)
  standardTaxa$Year <- format.Date(standardTaxa$DATE_TAKEN, "%Y")
  standardTaxa <- standardTaxa[, c("LOCATION_CODE", "Season", "Year", "MAITLAND_CODE", "TAXON", "RESULT")]
  names(standardTaxa) <- c("Site", "Season", "Year", "Maitland Code", "Maitland Name", "Abundance")
  standardTaxa$Season <- as.integer(standardTaxa$Season)
  standardTaxa$`Maitland Name` <- as.character(standardTaxa$"Maitland Name")
  standardTaxa$Abundance <- as.character(standardTaxa$Abundance)
  standardTaxa$Abundance <- as.numeric(standardTaxa$Abundance)
  test <- aquaMetrics::StandardiseRawTaxa(family.df = standardTaxa, species.df = standardTaxa, aggregate = "season")
  aquaMetricOutput <- aquaMetrics::Calc_SPEAR(test$standard.taxa, season = 3)

  ecologyResults <- filter_spear(ecologyResults, taxaList = "TL2")
  spearOutput <- calc_spear(ecologyResults, recoveryArea = "unknown", taxaList = "TL2")

  expect_equal(
    round(as.numeric(as.character(spearOutput$RESULT[1])), 2),
    round(aquaMetricOutput$Sratio, 2)
  )
  expect_equal(
    round(as.numeric(as.character(spearOutput$RESULT[2])), 2),
    round(aquaMetricOutput$Texp, 2)
  )
  expect_equal(as.character(spearOutput$RESULT[3]), as.character(aquaMetricOutput$Wq))
})

test_that("compare TL2 against aquaMetric package scores", {
  # currently DAVE Ecology not working for TL5?
  ecologyResults <- demo_data
  ecologyResults <- ecologyResults[ecologyResults$analysis_name == "MIXTAX_TST", ]
  spearOutput <- calc_spear(ecologyResults, recoveryArea = "unknown", taxa_list = "TL5")
})
