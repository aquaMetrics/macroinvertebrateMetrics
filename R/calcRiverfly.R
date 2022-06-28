#' Riverfly metric
#'
#' Function to calculate riverfly scores
#'
#' @param ecologyResults
#' A data frame of ecology data
#' @return
#' A data frame 5 variables
#' @export
#' @importFrom rlang .data
#' @importFrom magrittr "%>%"
#' @examples
#' ecologyResults <- demoEcologyResults
#' ecologyResults <- ecologyResults[ecologyResults$ANALYSIS_NAME == "FW_TAX_ID", ]
#' spearOutput <- calcRiverfly(ecologyResults)
#'
calcRiverfly <- function(ecologyResults) {
  # need to create riverfly score for each sample number
  macroinvertebrates <- macroinvertebrateMetrics::macroinvertebrateTaxa

  # need 'Taxon abundance' results to calculate Riverfly score
  ecologyResults <- ecologyResults[
    ecologyResults$DETERMINAND == "Taxon abundance" |
      ecologyResults$DETERMINAND == "Taxon Abundance",
  ]

  ecologyResults$TAXON <- trimws(ecologyResults$TAXON)

  SEPAresults <-
    merge(ecologyResults,
      macroinvertebrates,
      by.x = "TAXON",
      by.y = "TAXON_NAME"
    )


  # this table has a lookup list for riverfly taxon groups against TL2 families
  taxonTable <- utils::read.csv(system.file("extdata",
    "riverfly.csv",
    package = "macroinvertebrateMetrics"
  ),
  stringsAsFactors = FALSE
  )

  # merge only works for TL2 currently - will need to merge with invert taxa
  # table to do Tl3, Tl5 etc
  riverflyTaxa <-
    merge(
      SEPAresults[, c("TL2_5_TAXON", "TAXON", "VALUE", "SAMPLE_ID")],
      taxonTable,
      by.x = "TL2_5_TAXON",
      by.y = "REPORTED_NAME"
    )
  # Issue with linting "RIVERFLY_GROUP" below - 'no visible binding for global
  # variable'
  # Begin Exclude Linting
  riverflySum <- dplyr::group_by(
    riverflyTaxa,
    .data$RIVERFLY_GROUP,
    .data$SAMPLE_ID
  ) %>%
    dplyr::summarise(VALUE = sum(.data$VALUE))

  # riverfly abundance categories
  category <- function(x) {
    ifelse(x > 99, 3,
      ifelse(x > 9, 2,
        ifelse(x > 0, 1, 0)
      )
    )
  }
  riverflySum$VALUE_LOG <- c(apply(riverflySum[, "VALUE"], 2, category))

  # group_by sample_id and sum log abundance
  riverflyScore <- dplyr::group_by(riverflySum, .data$SAMPLE_ID) %>%
    dplyr::summarise(RESULT = sum(.data$VALUE_LOG))
  # if no relevant data return NULL object
  if (nrow(riverflyScore) == 0) {
    return()
  }

  riverflyScore$DETERMINAND <- "Riverfly Score"
  riverflyScore$ANALYSIS_NAME <- "METRIC Riverfly"
  riverflyScore$ANALYSIS_REPNAME <- "Riverfly"
  # End Exclude Linting


  return(riverflyScore)
}
