#' Filter data before calculating SPEAR metric
#'
#' @param data
#' Dataframe with ecology results...
#' @param taxa_list
#' The taxonomic level the sample(s) have been identified at according to
#' specified taxa lists as described in WFD100 Further Development of River
#' Invertebrate Classification Tool. Either "TL2" - Taxa List 2, "TL4" - Taxa
#' List 4 or  "TL5" - Taxa List 5.
#' @return
#' Dataframe of filtered and aggregated results (based on Taxa List) with four
#' columns: sample_id, label, SPEAR_SPECIES, response
filter_spear <- function(data, taxa_list = NULL) {
  taxaMetricValues <- data
  # Remove oligochaeta - not counted at TL2 in SPEAR metric
  if (taxa_list %in% c("TL2", "TL3")) {
    taxaMetricValues <- taxaMetricValues[taxaMetricValues$label != "Oligochaeta", ]
  }
  # if nothing in data.frame
  if (nrow(taxaMetricValues) == 0) {
    return(taxaMetricValues)
  }
  # Remove SPEAR_SPECIES is NA
  taxaMetricValues <- taxaMetricValues[!is.na(taxaMetricValues$SPEAR_SPECIES), ]
  # aggregate to correct Taxa List (TL) level
  taxaMetricValues$response <- as.character(taxaMetricValues$response)
  taxaMetricValues$response <- as.numeric(taxaMetricValues$response)
  if (taxa_list == "TL2") {
    taxaMetricValues <- stats::aggregate(
      taxaMetricValues[, c("response")],
      by = list(
        taxaMetricValues$sample_id,
        taxaMetricValues$TL2_TAXON,
        taxaMetricValues$SPEAR_SPECIES
      ),
      FUN = sum
    )
  } else if (taxa_list == "TL5") {
    taxaMetricValues <- taxaMetricValues %>%
      dplyr::group_by(.data$sample_id, .data$TL5_TAXON, .data$SPEAR_SPECIES) %>%
      dplyr::summarise(value = sum(response))
    # merge SPEAR_SPECIES back in because of NULL SPEAR_SPECIES not pick up if aggregated
    taxaMetricValues <-
      merge(taxaMetricValues,
        macroinvertebrates[, c("TAXON_NAME", "SPEAR_SPECIES")],
        by.x = "TL5_TAXON",
        by.y = "TAXON_NAME"
      )
    taxaMetricValues <- taxaMetricValues[, c("sample_id", "TL5_TAXON", "SPEAR_SPECIES.x", "value")]
  } else {
    taxaMetricValues <- stats::aggregate(
      taxaMetricValues[, c("response")],
      by = list(
        taxaMetricValues$sample_id,
        taxaMetricValues$TL4_TAXON,
        taxaMetricValues$SPEAR_SPECIES
      ),
      FUN = sum
    )
  }
  # update names after aggregation
  names(taxaMetricValues) <-
    c("sample_id", "label", "SPEAR_SPECIES", "response")
  # only return results for records that match TL
  taxaMetricValues <- taxaMetricValues[taxaMetricValues$label != "", ]

  if (nrow(taxaMetricValues) == 0) {
    return(NULL)
  }
  return(taxaMetricValues)
}
