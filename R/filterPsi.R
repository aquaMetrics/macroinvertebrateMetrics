#' Filter results for PSI metric
#'
#' @param ecologyResults
#' Dataframe with ecology results...
#' @param taxaList
#' The taxonomic level the sample(s) have been identified at according to specificed taxa lists
#' as described in WFD100 Further Development of River Invertebrate Classification Tool.
#' Either "TL2" - Taxa list 2 or "TL5" Taxa list 3.
#' @return
#' Dataframe
#' @export
#'
#' @examples
#' filterPsi(demoEcoloyResults, taxaList = "TL2")
filterPsi <- function(ecologyResults, taxaList = "TL2") {
  if (!taxaList %in% c("TL2", "TL5", "TL4")) {
    stop("taxaList arugment must be either 'TL2', 'TL4' or 'TL5'")
  }
  # only need Taxon abundance determinand
  ecologyResults <-
    ecologyResults[ecologyResults$DETERMINAND == "Taxon abundance" |
                     ecologyResults$DETERMINAND == "Taxon Abundance", ]
  # merge ecology results with taxa metric scores based on taxon name
  macroinvertebrateTaxa <- macroinvertebrateMetrics::macroinvertebrateTaxa
  taxaMetricValues <-
    merge(ecologyResults,
         macroinvertebrateTaxa,
          by.x = "TAXON",
          by.y = "TAXON_NAME")
  # if nothing in data.frame
  if (length(taxaMetricValues$TAXON) == 0) {
    return(taxaMetricValues)
  }
  # aggregate to correct Taxa List (TL) level
  taxaMetricValues$RESULT <- as.character(taxaMetricValues$RESULT)
  taxaMetricValues$RESULT <- as.numeric(taxaMetricValues$RESULT)
  if (taxaList == "TL2") {
    taxaMetricValues <- aggregate(
      taxaMetricValues[, c("RESULT")],
      by = list(
        taxaMetricValues$SAMPLE_ID,
        taxaMetricValues$TL2_TAXON,
        taxaMetricValues$PSI_GROUP
      ),
      FUN = sum
    )
  }

  if (taxaList == "TL4") {
    taxaMetricValues <- aggregate(
      taxaMetricValues[, c("RESULT")],
      by = list(
        taxaMetricValues$SAMPLE_ID,
        taxaMetricValues$TL4_TAXON,
        taxaMetricValues$PSI_GROUP
      ),
      FUN = sum
    )
  }

 if (taxaList == "TL5") {
    taxaMetricValues <- aggregate(
      taxaMetricValues[, c("RESULT")],
      by = list(
        taxaMetricValues$SAMPLE_ID,
        taxaMetricValues$TL5_TAXON,
        taxaMetricValues$PSI_GROUP
      ),
      FUN = sum
    )
  }

  if (taxaList == "TL4"){
    taxaMetricValues <- aggregate(
      taxaMetricValues[, c("RESULT")],
      by = list(
        taxaMetricValues$SAMPLE_ID,
        taxaMetricValues$TL5_TAXON,
        taxaMetricValues$PSI_GROUP
      ),
      FUN = sum
    )
  }
  # update names after aggregation
  names(taxaMetricValues) <- c("SAMPLE_ID", "TAXON", "PSI_GROUP")
  # PSI_GROUP not required by calcPSI function - only used to do
  taxaMetricValues$PSI_GROUP <- NULL
  return(taxaMetricValues)
}