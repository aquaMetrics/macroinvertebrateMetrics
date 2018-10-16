#' Filter results for PSI metric
#'
#' @param ecologyResults
#' Dataframe with ecology results...
#' @param taxaList
#' The taxonomic level the sample(s) have been identified at according to specificed taxa lists
#' as described in WFD100 Further Development of River Invertebrate Classification Tool.
#' Either "TL3" - Taxa list 2 or "TL5" Taxa list 3 or "TL4". PSI TL lists don't match RIVPACS species e.g.
#' TL3 list includes 'Oligochaeta' even though it is not a TL3 taxa according to WFD100.
#' @return
#' Dataframe
#' @export
#'
#' @examples
#' filterPsi(demoEcoloyResults, taxaList = "TL2")
filterPsi <- function(ecologyResults, taxaList = "TL3") {
  if (!taxaList %in% c("TL2", "TL3", "TL5", "TL4")) {
    stop("taxaList arugment must be either 'TL2', 'TL3', 'TL4' or 'TL5'")
  }
  # only need Taxon abundance determinand
  ecologyResults <-
    ecologyResults[ecologyResults$DETERMINAND == "Taxon abundance" |
                     ecologyResults$DETERMINAND == "Taxon Abundance", ]
  # merge ecology results with taxa metric scores based on taxon name
  macroinvertebrateTaxa <- macroinvertebrateMetrics::macroinvertebrateTaxa
  ecologyResults$TAXON <- trimws(ecologyResults$TAXON)
  taxaMetricValues <-
    merge(ecologyResults,
         macroinvertebrateTaxa,
          by.x = "TAXON",
          by.y = "TAXON_NAME")
  # if nothing in data.frame
  if (length(taxaMetricValues$TAXON) == 0) {
    return(taxaMetricValues)
  }
  # change class for aggregation
  taxaMetricValues$RESULT <-  as.numeric(taxaMetricValues$RESULT)
  taxaMetricValues$PSI_GROUP <- as.character(taxaMetricValues$PSI_GROUP)
  # aggregate to correct Taxa List (TL) level
  taxaMetricValues$RESULT <- as.character(taxaMetricValues$RESULT)
  taxaMetricValues$RESULT <- as.numeric(taxaMetricValues$RESULT)
  if (taxaList == "TL3") {
    taxaMetricValues$TL3_TAXON <- as.character(taxaMetricValues$TL3_TAXON)
    taxaMetricValues$TL3_TAXON[taxaMetricValues$TAXON == "Oligochaeta"] <- "Oligochaeta"

    taxaMetricValues <- aggregate(
      taxaMetricValues[, c("RESULT")],
      by = list(
        taxaMetricValues$SAMPLE_ID,
        taxaMetricValues$TL3_TAXON,
        taxaMetricValues$PSI_GROUP
      ),
      FUN = sum
    )
  }

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
  names(taxaMetricValues) <- c("SAMPLE_ID", "TAXON", "PSI_GROUP", "RESULT")
  # PSI_GROUP not required by calcPSI function
  taxaMetricValues$PSI_GROUP <- NULL
  # remove 'blank' taxa if not scores for PSI
  taxaMetricValues <- taxaMetricValues[taxaMetricValues$TAXON != "",]
  taxaMetricValues <- taxaMetricValues[! is.na(taxaMetricValues$TAXON),]
  return(taxaMetricValues)
}
