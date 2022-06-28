#' Filter data before calculating SPEAR metric
#'
#' @param ecologyResults
#' Dataframe with ecology results...
#' @param taxaList
#' The taxonomic level the sample(s) have been identified at according to specificed taxa lists
#' as described in WFD100 Further Development of River Invertebrate Classification Tool.
#' Either "TL2" - Taxa list 2, "TL4" - Taxa list 4 or  "TL5" - Taxa list 5.
#' @return
#' Dataframe of filtered and aggregated results (based on Taxa List) with four columns:
#' SAMPLE_NUMBER, TAXON, SPEAR_SPECIES, RESULT
#' @export
#'
#' @examples
#' filtered <- filterSpear(demoEcologyResults, taxaList = "TL2")
filterSpear <- function(ecologyResults, taxaList = NULL) {
  # only need Taxon abundance determinand
  ecologyResults <-
    ecologyResults[ecologyResults$DETERMINAND == "Taxon abundance" |
      ecologyResults$DETERMINAND == "Taxon Abundance", ]
  # merge ecology results with taxa metric scores based on taxon name
  macroinvertebrates <- macroinvertebrateMetrics::macroinvertebrateTaxa
  ecologyResults$TAXON <- trimws(ecologyResults$TAXON)
  taxaMetricValues <-
    merge(ecologyResults,
      macroinvertebrates,
      by.x = "TAXON",
      by.y = "TAXON_NAME"
    )

  # Remove oligochaeta - not counted at TL2 in SPEAR metric
  if (taxaList %in% c("TL2", "TL3")) {
    taxaMetricValues <- taxaMetricValues[taxaMetricValues$TAXON != "Oligochaeta", ]
  }
  # if nothing in data.frame
  if (length(taxaMetricValues$TAXON) == 0) {
    return(taxaMetricValues)
  }
  # aggregate to correct Taxa List (TL) level
  taxaMetricValues$RESULT <- as.character(taxaMetricValues$RESULT)
  taxaMetricValues$RESULT <- as.numeric(taxaMetricValues$RESULT)
  if (taxaList == "TL2") {
    taxaMetricValues <- stats::aggregate(
      taxaMetricValues[, c("RESULT")],
      by = list(
        taxaMetricValues$SAMPLE_ID,
        taxaMetricValues$TL2_TAXON,
        taxaMetricValues$SPEAR_SPECIES
      ),
      FUN = sum
    )
  } else if (taxaList == "TL5") {
    taxaMetricValues <- stats::aggregate(
      taxaMetricValues[, c("RESULT")],
      by = list(
        taxaMetricValues$SAMPLE_ID,
        taxaMetricValues$TL5_TAXON
      ),
      FUN = sum
    )
    # merge SPEAR_SPECIES back in because of NULL SPEAR_SPECIES not pick up if aggregated
    taxaMetricValues <-
      merge(taxaMetricValues,
        macroinvertebrates[, c("TAXON_NAME", "SPEAR_SPECIES")],
        by.x = "Group.2",
        by.y = "TAXON_NAME"
      )
    taxaMetricValues <- taxaMetricValues[, c("Group.1", "Group.2", "SPEAR_SPECIES", "x")]
  } else {
    taxaMetricValues <- stats::aggregate(
      taxaMetricValues[, c("RESULT")],
      by = list(
        taxaMetricValues$SAMPLE_ID,
        taxaMetricValues$TL4_TAXON,
        taxaMetricValues$SPEAR_SPECIES
      ),
      FUN = sum
    )
  }
  # update names after aggregation
  names(taxaMetricValues) <-
    c("SAMPLE_ID", "TAXON", "SPEAR_SPECIES", "RESULT")
  # only return results for records that match TL
  taxaMetricValues <- taxaMetricValues[taxaMetricValues$TAXON != "", ]

  return(taxaMetricValues)
}
